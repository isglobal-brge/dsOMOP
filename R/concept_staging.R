.conceptEncode <- function(x) {
  objects <- function(v) {
    if (is.atomic(v) && !is.null(names(v))) v <- as.list(v)
    if (is.list(v)) v <- lapply(v, objects)
    v
  }
  json <- if(is.list(x)&&length(x$numbers)) .numberJson(.categoryMetadataPlain(objects(x))) else
    as.character(jsonlite::toJSON(objects(x), auto_unbox = TRUE, null = "null", digits = NA))
  paste0("B64:", sub("=+$", "", chartr("+/", "-_", gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json))))))
}

# A-27 uses the existing descriptor and controlled factor transport. The
# descriptor's presentation capsule stays server-side and is manifest-bound.
.conceptStageCapture <- function(df, descriptor) {
  # Ordinary categorical columns also need their complete domain in CSV and
  # Parquet. This capsule is private and bound by the descriptor manifest.
  contracts <- attr(df, "omop_concepts") %||% list()
  controlled <- names(contracts)
  mapping <- attr(df, "omop_column_names")
  if (length(mapping)) controlled <- unname(mapping[controlled])
  # Reconciled concept/date labels can change later. Only their own current
  # capsule may restore them, otherwise an old domain would turn new labels NA.
  factors <- df[setdiff(names(df)[vapply(df, is.factor, logical(1))], controlled)]
  if (length(factors)) descriptor$metadata$factor_domains <- .conceptEncode(
    lapply(factors, function(x) list(levels = levels(x), ordered = is.ordered(x))))
  generated <- attr(df, "omop_generated_columns") %||% list()
  numbers <- attr(df,"omop_numbers") %||% list()
  schema <- attr(df,"omop_codebook_schema")
  if (!length(contracts) && !length(generated) && !length(numbers) && is.null(schema)) return(descriptor)
  totals <- nrow(df)
  observations <- lapply(contracts, function(c) .densityObservations(df, c$ids))
  support <- lapply(contracts, function(c) {
    ids <- unique(c$ids[!is.na(c$ids)])
    setNames(rep(1L, length(ids)), ids)
  })
  reference_support <- if(isTRUE(attr(df,"omop_concept_reference")))attr(df,"omop_reference_support")
  contracts <- lapply(contracts, function(c) { c$ids <- NULL; c })
  descriptor$metadata$concept_presentation <- .conceptEncode(list(
    protocol = 1L, contracts = contracts, generated = generated, numbers = numbers, schema = schema,
    handle = attr(df, "omop_presentation_handle"),
    names = attr(df, "omop_column_names") %||% setNames(names(df), names(df)),
    reference_support = reference_support,
    support = support, total = totals, observations = observations))
  descriptor
}

.conceptStageData <- function(descriptor) {
  .omopStagedDatasetPath(descriptor)
  .ds_arg(descriptor$metadata$concept_presentation)
}

.conceptStageReport <- function(descriptor, discover = TRUE, fingerprint = FALSE) {
  meta <- .conceptStageData(descriptor)
  refreshed <- .conceptRefresh(meta$contracts, meta$generated, meta$handle)
  meta$contracts <- refreshed$contracts; meta$generated <- refreshed$generated
  settings <- .omopDisclosureSettings()
  out <- list(concept_protocol = 1L, levels = list(), concepts = list(),
              unsafe = character(), unsafe_rules = list(), nfilter_levels_max = settings$nfilter_levels_max,
              keys = character(), generated = list())
  for (col in names(meta$contracts)) {
    c <- meta$contracts[[col]]
    support <- unlist(meta$support[[col]], use.names = TRUE)
    a <- .conceptAssessIds(c, names(support), meta$observations[[col]] %||% meta$total, meta$reference_support, discover)
    if (!a$safe) {
      out$unsafe <- c(out$unsafe, col)
      out$unsafe_rules[[col]] <- c(a$rules, if (identical(c$auto_levels, "too_long")) "dsomop.auto_declared_levels_max")
      next
    }
    ids <- union(a$levels, as.character(unlist(c$declared, use.names = FALSE)))
    listed <- if (isTRUE(fingerprint)) .conceptListPart(c) else character()
    if (length(listed)) {
      out$lists[[col]] <- list(n = length(listed), digest = .conceptListDigest(c$records, listed))
      ids <- setdiff(ids, listed)
    }
    out$levels[[col]] <- ids
    out$concepts[[col]] <- unname(c$records[ids])
    if (identical(c$kind, "date")) out$calendars[[col]] <- c$calendar
    if (identical(c$kind, "category")) out$categories[[col]] <- c$category
    if (length(c$reference_keys)) out$reference_names[[col]] <- unique(as.character(unlist(c$reference_keys,use.names=FALSE)))
  }
  refused_keys <- character()
  for (key in names(meta$generated)) {
    g <- meta$generated[[key]]
    if (!isTRUE(g$declared) && !isTRUE(g$permitted)) {
      # Generated keys embed concept IDs: report the class, never the key.
      refused_keys <- c(refused_keys, key)
      out$unsafe <- union(out$unsafe, "generated concept columns")
      out$unsafe_rules[["generated concept columns"]] <- "generated-column admission"
      next
    }
    g[c("declared", "permitted")] <- NULL
    out$generated[[key]] <- g
  }
  out$keys <- setdiff(names(meta$names), refused_keys)
  out$numbers <- meta$numbers %||% list()
  if(!is.null(meta$schema))out$schema<-meta$schema
  out$evidence <- .conceptEvidence(out)
  out
}

.conceptCollectChunk <- function(frame, descriptor) {
  encoded <- descriptor$metadata$concept_presentation
  if (is.null(encoded)) return(frame)
  meta <- .ds_arg(encoded)
  for (col in names(meta$contracts)) {
    physical <- as.character(meta$names[[col]])
    levels <- as.character(unlist(meta$contracts[[col]]$resolved$levels, use.names = FALSE))
    if (physical %in% names(frame)) frame[[physical]] <- if (identical(meta$contracts[[col]]$kind,"category")) .categoryFactor(as.character(frame[[physical]]),meta$contracts[[col]]$resolved) else if ((meta$contracts[[col]]$kind %||% "") %in% c("date","number")) .dateOrdered(as.character(frame[[physical]]), levels = levels) else .conceptFactor(as.character(frame[[physical]]), levels = levels)
  }
  frame
}

.conceptRelevelStaged <- function(descriptor, spec) {
  if (!identical(as.integer(spec$concept_protocol), 1L)) {
    stop("A complete concept presentation protocol is required for this staged output.", call. = FALSE)
  }
  report <- .conceptStageReport(descriptor, fingerprint = isTRUE(spec$fingerprint))
  .conceptSiteVerdict(report)
  meta <- .conceptStageData(descriptor)
  .conceptCheckEvidence(report, spec, !is.null(meta$handle))
  # Protocol-1 factor-only calls preserve numeric columns and their capsule.
  # A supplied numeric reconciliation contract must still match exactly.
  if ("numbers" %in% names(spec)) .numberValidateShared(report$numbers, spec$numbers)
  if (length(report$unsafe) || !setequal(names(meta$contracts), names(spec$columns))) {
    stop("Incomplete disclosure-safe staged concept contract.", call. = FALSE)
  }
  mappings <- list()
  # Fingerprints are checked against the same refreshed records as the report
  # (read again only when a compact column needs them).
  compact <- any(vapply(names(meta$contracts), function(col) !is.null(spec$columns[[col]]$list), logical(1)))
  refreshed <- if (compact) .conceptRefresh(meta$contracts, meta$generated, meta$handle)$contracts
  for (col in names(meta$contracts)) {
    shared <- spec$columns[[col]]
    if (!is.null(shared$list)) {
      shared <- .conceptExpandShared(refreshed[[col]], shared)
      # Later steps (the chunk rewrite) read the complete shared column.
      spec$columns[[col]] <- shared
    }
    if (identical(meta$contracts[[col]]$kind, "date") && !identical(.jcsCanonical(shared$calendar), .jcsCanonical(meta$contracts[[col]]$calendar))) .dateReject("Changed staged calendar identity.")
    if(identical(meta$contracts[[col]]$kind,"category")&&!identical(.jcsCanonical(shared$category),.jcsCanonical(meta$contracts[[col]]$category))) .categoryReject("Changed staged category contract.","category_domain_incompatible")
    .conceptValidateResolution(shared, meta$contracts[[col]]$records[union(report$levels[[col]], .conceptListPart(meta$contracts[[col]]))])
    ids <- as.character(unlist(shared$ids, use.names = FALSE))
    labels <- unname(.categoryNullableStrings(shared$labels))
    levels <- as.character(unlist(shared$levels, use.names = FALSE))
    if (length(ids) != length(labels) || anyDuplicated(ids) || (!identical(meta$contracts[[col]]$kind,"category") && anyNA(labels)) ||
        anyDuplicated(levels) || !setequal(unique(labels[!is.na(labels)]), levels) ||
        !all(report$levels[[col]] %in% ids)) stop("Invalid staged concept union.", call. = FALSE)
    old <- meta$contracts[[col]]$resolved
    old_ids <- as.character(unlist(old$ids, use.names = FALSE))
    old_labels <- unname(.categoryNullableStrings(old$labels))
    # A merged alias member stored under its group's label follows its
    # effective identity when the shared union lists only that identity.
    effective <- old_ids
    old_aliases <- .categoryNullableStrings(old$aliases)
    member <- !(old_ids %in% ids) & old_ids %in% names(old_aliases)
    effective[member] <- unname(old_aliases[old_ids[member]])
    new_labels <- labels[match(effective, ids)]
    mappings[[col]] <- setNames(new_labels, old_labels)
    meta$contracts[[col]]$resolved <- shared
  }
  if (length(meta$generated) && !length(spec$names)) stop("Incomplete shared staged generated-column names.", call. = FALSE)
  old_names <- unlist(meta$names, use.names = TRUE)
  new_names <- if (length(spec$names)) unlist(spec$names, use.names = TRUE) else old_names
  if (!setequal(names(new_names), names(old_names)) || anyDuplicated(new_names) ||
      anyNA(new_names) || any(!.conceptValidNames(new_names))) {
    stop("Invalid staged generated-column names.", call. = FALSE)
  }
  .conceptValidateNames(new_names, names(old_names), meta$generated %||% list())
  path <- .omopStagedDatasetPath(descriptor)
  directory <- dirname(path)
  manifest <- .readStagingManifest(file.path(directory, "manifest.json"))
  pending <- tempfile(".concept-", tmpdir = directory)
  on.exit(unlink(pending), add = TRUE)
  writer <- sink <- NULL
  on.exit({ if (!is.null(writer)) try(writer$Close(), silent = TRUE)
            if (!is.null(sink)) try(sink$close(), silent = TRUE) }, add = TRUE)
  first <- TRUE
  types <- NULL
  .publishReadChunks(descriptor, function(frame) {
    names(frame) <- names(old_names)[match(names(frame), old_names)]
    for (col in names(mappings)) {
      source <- as.character(frame[[col]])
      values <- unname(mappings[[col]][source])
      if (any(!is.na(source) & is.na(values))) stop("Staged factor coverage changed.", call. = FALSE)
      frame[[col]] <- if (identical(meta$contracts[[col]]$kind,"category")) .categoryFactor(values,spec$columns[[col]]) else if ((meta$contracts[[col]]$kind %||% "") %in% c("date","number")) .dateOrdered(values, levels = as.character(unlist(spec$columns[[col]]$levels, use.names = FALSE))) else .conceptFactor(values, levels = as.character(unlist(spec$columns[[col]]$levels, use.names = FALSE)))
    }
    frame <- .conceptReferenceNames(frame, meta$contracts)
    if (!isFALSE(meta$contracts$concept_id$translate) && "concept_id" %in% names(mappings) && "concept_name" %in% names(frame)) frame$concept_name <- as.character(frame$concept_id)
    names(frame) <- unname(new_names[names(frame)])
    types <<- vapply(frame, function(x) paste(typeof(x), paste(class(x), collapse = "/"), sep = "|"), "")
    if (descriptor$metadata$format == "parquet") {
      table <- arrow::Table$create(frame)
      if (is.null(writer)) {
        sink <<- arrow::FileOutputStream$create(pending)
        writer <<- arrow::ParquetFileWriter$create(schema = table$schema, sink = sink,
          properties = arrow::ParquetWriterProperties$create(column_names = names(frame)))
      }
      writer$WriteTable(table, chunk_size = max(1L, nrow(frame)))
    } else .stagedWriteCsv(frame, pending,
                              col.names = first, append = !first)
    first <<- FALSE
  })
  if (!is.null(writer)) { writer$Close(); writer <- NULL; sink$close(); sink <- NULL }
  Sys.chmod(pending, "0600")
  updated <- descriptor
  if (updated$metadata$format == "csv") updated$metadata$csv_encoding <- "backslash-v1"
  meta$names <- as.list(new_names)
  updated$metadata$columns <- unname(new_names[names(old_names)])
  updated$metadata$column_types <- types
  updated$metadata$concept_presentation <- .conceptEncode(meta)
  # Existing federation rollback owns the destination and all staged files.
  if (!file.rename(pending, path)) stop("Staged concept replacement failed.", call. = FALSE)
  name <- sub("^omop.plan.", "", descriptor$dataset_id)
  manifest$outputs[[name]] <- updated
  .writeStagingManifest(directory, manifest$outputs)
  .publishUpdateOwnedValue(descriptor, updated)
  updated
}

# Legacy SQL streams and typed SQL components are presented in two bounded
# passes. Keep distinct identities and the output row count privately.
.conceptStagePresent <- function(ctx, descriptor, columns = NULL, root, component,
                                 reference = FALSE) {
  if (!is.null(descriptor$metadata$concept_presentation)) return(descriptor)
  prototype <- NULL
  reference_rows <- list()
  identities <- list()
  total_rows <- 0L
  keys <- list(); bound <- .densityBound()
  .publishReadChunks(descriptor, function(frame) {
    if (is.null(prototype)) {
      prototype <<- frame[FALSE, , drop = FALSE]
      if (is.null(columns)) columns <<- .conceptLegacyColumns(ctx, frame, root, component)
    }
    if (reference && all(c("covariateId", "analysisId", "conceptId", "covariateName") %in% names(frame))) {
      reference_rows[[length(reference_rows) + 1L]] <<- frame
    }
    concept_cols <- names(columns)[vapply(columns, .conceptColumn, logical(1))]
    total_rows <<- total_rows + nrow(frame)
    key <- .omopPersonKeys(frame)
    for (col in intersect(concept_cols, names(frame))) {
      ids <- .conceptIdText(frame[[col]])
      observed <- unique(ids[!is.na(ids)])
      identities[[col]] <<- union(identities[[col]], observed)
      # A person's repeated value counts once over every chunk.
      if (length(key)) keys[[col]] <<- .densityKeys(frame[[key[[1L]]]], ids, keys[[col]] %||% character(), bound)

    }
  })
  if (is.null(prototype)) return(descriptor)
  n <- max(c(0L, lengths(identities)))
  sample <- prototype[rep(NA_integer_, n), , drop = FALSE]
  for (col in names(identities)) sample[[col]] <- c(identities[[col]], rep(NA_character_, n - length(identities[[col]])))
  if (length(reference_rows)) sample <- do.call(rbind, reference_rows)
  reference_support <- attr(descriptor, "omop_reference_support")
  attr(sample, "omop_reference_support") <- reference_support
  attr(sample, "omop_concept_reference") <- reference
  # The sample holds distinct identities; the level rule uses the output's
  # observations (its rows, a person's repeated value once).
  attr(sample, "omop_total_rows") <- total_rows
  observations <- lapply(setNames(names(identities), names(identities)), function(col)
    if (is.null(keys[[col]])) total_rows else length(keys[[col]]))
  attr(sample, "omop_density_observations") <- observations
  raw_sample <- sample
  sample <- .conceptPresent(ctx, sample, columns)
  updated <- .conceptStageCapture(sample, descriptor)
  if (is.null(updated$metadata$concept_presentation)) return(descriptor)
  meta <- .ds_arg(updated$metadata$concept_presentation)
  mappings <- lapply(names(meta$contracts), function(col) {
    physical <- as.character(meta$names[[col]])
    setNames(as.character(sample[[physical]]), .conceptIdText(raw_sample[[col]]))
  })
  names(mappings) <- names(meta$contracts)
  meta$total <- total_rows
  meta$observations <- observations[intersect(names(observations), names(meta$contracts))]
  if (!is.null(reference_support)) {
    meta$reference_support <- reference_support
  }
  updated$metadata$concept_presentation <- .conceptEncode(meta)
  updated <- .conceptRewritePrivate(descriptor, updated, function(frame) {
    for (col in names(meta$contracts)) {
      resolved <- meta$contracts[[col]]$resolved
      ids <- as.character(unlist(resolved$ids, use.names = FALSE))
      labels <- as.character(unlist(resolved$labels, use.names = FALSE))
      levels <- as.character(unlist(resolved$levels, use.names = FALSE))
      values <- unname(mappings[[col]][.conceptIdText(frame[[col]])])
      frame[[col]] <- if(identical(meta$contracts[[col]]$kind,"category")) .categoryFactor(values,resolved) else
        if((meta$contracts[[col]]$kind %||% "") %in% c("date","number")) .dateOrdered(values,levels) else .conceptFactor(values, levels = levels)
    }
    frame <- .conceptReferenceNames(frame, meta$contracts)
    if (!isFALSE(meta$contracts$concept_id$translate) && "concept_id" %in% names(meta$contracts) && "concept_name" %in% names(frame)) frame$concept_name <- as.character(frame$concept_id)
    mapping <- unlist(meta$names, use.names = TRUE)
    names(frame) <- unname(mapping[names(frame)])
    frame
  })
  updated
}

.conceptRewritePrivate <- function(original, updated, transform) {
  if (updated$metadata$format == "csv") updated$metadata$csv_encoding <- "backslash-v1"
  path <- original$metadata$file
  pending <- tempfile(".concept-", tmpdir = dirname(path))
  on.exit(unlink(pending), add = TRUE)
  writer <- sink <- NULL
  on.exit({ if (!is.null(writer)) try(writer$Close(), silent = TRUE)
            if (!is.null(sink)) try(sink$close(), silent = TRUE) }, add = TRUE)
  first <- TRUE
  .publishReadChunks(original, function(frame) {
    frame <- transform(frame)
    updated$metadata$columns <<- names(frame)
    updated$metadata$column_types <<- vapply(frame, function(x) paste(typeof(x), paste(class(x), collapse = "/"), sep = "|"), "")
    if (original$metadata$format == "parquet") {
      table <- arrow::Table$create(frame)
      if (is.null(writer)) {
        sink <<- arrow::FileOutputStream$create(pending)
        writer <<- arrow::ParquetFileWriter$create(schema = table$schema, sink = sink,
          properties = arrow::ParquetWriterProperties$create(column_names = names(frame)))
      }
      writer$WriteTable(table, chunk_size = max(1L, nrow(frame)))
    } else .stagedWriteCsv(frame, pending,
      col.names = first, append = !first)
    first <<- FALSE
  })
  if (!is.null(writer)) { writer$Close(); writer <- NULL; sink$close(); sink <- NULL }
  Sys.chmod(pending, "0600")
  if (!file.rename(pending, path)) stop("Private concept presentation failed.", call. = FALSE)
  updated
}
