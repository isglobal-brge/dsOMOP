.conceptEncode <- function(x) {
  objects <- function(v) {
    if (is.atomic(v) && !is.null(names(v))) v <- as.list(v)
    if (is.list(v)) v <- lapply(v, objects)
    v
  }
  json <- as.character(jsonlite::toJSON(objects(x), auto_unbox = TRUE, null = "null", digits = NA))
  paste0("B64:", sub("=+$", "", chartr("+/", "-_", gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json))))))
}

# A-27 uses the existing descriptor and controlled factor transport. The
# descriptor's presentation capsule stays server-side and is manifest-bound.
.conceptStageCapture <- function(df, descriptor) {
  contracts <- attr(df, "omop_concepts") %||% list()
  generated <- attr(df, "omop_generated_columns") %||% list()
  if (!length(contracts) && !length(generated)) return(descriptor)
  keys <- intersect(.PERSON_KEY_COLS(), names(df))
  totals <- if (length(keys)) length(unique(df[[keys[[1L]]]])) else 0L
  support <- lapply(contracts, function(c) {
    ids <- unique(c$ids[!is.na(c$ids)])
    setNames(vapply(ids, function(id) {
      if (length(keys)) length(unique(df[[keys[[1L]]]][which(c$ids == id)])) else 0L
    }, 0L), ids)
  })
  reference_support <- attr(df, "omop_reference_support")
  if (!is.null(reference_support)) {
    totals <- reference_support$total
    support <- lapply(contracts, function(c) reference_support$counts)
  }
  contracts <- lapply(contracts, function(c) { c$ids <- NULL; c })
  descriptor$metadata$concept_presentation <- .conceptEncode(list(
    protocol = 1L, contracts = contracts, generated = generated,
    handle = attr(df, "omop_presentation_handle"),
    names = attr(df, "omop_column_names") %||% setNames(names(df), names(df)),
    reference = isTRUE(attr(df, "omop_concept_reference")) && is.null(reference_support),
    support = support, total = totals))
  descriptor
}

.conceptStageData <- function(descriptor) {
  .omopStagedDatasetPath(descriptor)
  .ds_arg(descriptor$metadata$concept_presentation)
}

.conceptStageReport <- function(descriptor) {
  meta <- .conceptStageData(descriptor)
  refreshed <- .conceptRefresh(meta$contracts, meta$generated, meta$handle)
  meta$contracts <- refreshed$contracts; meta$generated <- refreshed$generated
  settings <- .omopDisclosureSettings()
  out <- list(concept_protocol = 1L, levels = list(), concepts = list(),
              unsafe = character(), nfilter_levels_max = settings$nfilter_levels_max,
              keys = names(meta$names), generated = list())
  for (col in names(meta$contracts)) {
    c <- meta$contracts[[col]]
    support <- unlist(meta$support[[col]], use.names = TRUE)
    safe <- (identical(c$kind, "date") && all(names(support) %in% unlist(c$declared))) || isTRUE(meta$reference) || (!length(support) ||
      (all(support >= settings$nfilter_tab) && tryCatch({
        .assertSafeLevels(length(support), meta$total); TRUE
      }, error = function(e) FALSE)))
    if (!safe) { out$unsafe <- c(out$unsafe, col); next }
    ids <- union(names(support), as.character(unlist(c$declared, use.names = FALSE)))
    out$levels[[col]] <- ids
    out$concepts[[col]] <- unname(c$records[ids])
    if (identical(c$kind, "date")) out$calendars[[col]] <- c$calendar
    if (length(c$reference_keys)) out$reference_names[[col]] <- unique(as.character(unlist(c$reference_keys,use.names=FALSE)))
  }
  for (key in names(meta$generated)) {
    g <- meta$generated[[key]]
    if (!isTRUE(g$declared) && !isTRUE(g$permitted)) { out$unsafe <- c(out$unsafe, key); next }
    g[c("declared", "permitted")] <- NULL
    out$generated[[key]] <- g
  }
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
    if (physical %in% names(frame)) frame[[physical]] <- if (identical(meta$contracts[[col]]$kind, "date")) .dateOrdered(as.character(frame[[physical]]), levels = levels) else .conceptFactor(as.character(frame[[physical]]), levels = levels)
  }
  frame
}

.conceptRelevelStaged <- function(descriptor, spec) {
  if (!identical(as.integer(spec$concept_protocol), 1L)) {
    stop("A complete concept presentation protocol is required for this staged output.", call. = FALSE)
  }
  report <- .conceptStageReport(descriptor)
  meta <- .conceptStageData(descriptor)
  .conceptCheckEvidence(report, spec, !is.null(meta$handle))
  if (length(report$unsafe) || !setequal(names(meta$contracts), names(spec$columns))) {
    stop("Incomplete disclosure-safe staged concept contract.", call. = FALSE)
  }
  mappings <- list()
  for (col in names(meta$contracts)) {
    shared <- spec$columns[[col]]
    if (identical(meta$contracts[[col]]$kind, "date") && !identical(.jcsCanonical(shared$calendar), .jcsCanonical(meta$contracts[[col]]$calendar))) .dateReject("Changed staged calendar identity.")
    .conceptValidateResolution(shared, meta$contracts[[col]]$records)
    ids <- as.character(unlist(shared$ids, use.names = FALSE))
    labels <- as.character(unlist(shared$labels, use.names = FALSE))
    levels <- as.character(unlist(shared$levels, use.names = FALSE))
    if (length(ids) != length(labels) || anyDuplicated(ids) || anyNA(labels) ||
        anyDuplicated(levels) || !setequal(unique(labels), levels) ||
        !all(report$levels[[col]] %in% ids)) stop("Invalid staged concept union.", call. = FALSE)
    density_levels <- if (identical(meta$contracts[[col]]$kind,"date")) length(setdiff(ids,unlist(meta$contracts[[col]]$declared))) else length(ids)
    .assertSafeLevels(density_levels, if (isTRUE(meta$reference)) 0L else meta$total)
    old <- meta$contracts[[col]]$resolved
    old_ids <- as.character(unlist(old$ids, use.names = FALSE))
    old_labels <- as.character(unlist(old$labels, use.names = FALSE))
    new_labels <- labels[match(old_ids, ids)]
    if (anyNA(new_labels)) stop("Incomplete staged concept coverage.", call. = FALSE)
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
      frame[[col]] <- if (identical(meta$contracts[[col]]$kind, "date")) .dateOrdered(values, levels = as.character(unlist(spec$columns[[col]]$levels, use.names = FALSE))) else .conceptFactor(values, levels = as.character(unlist(spec$columns[[col]]$levels, use.names = FALSE)))
    }
    frame <- .conceptReferenceNames(frame, meta$contracts)
    if ("concept_id" %in% names(mappings) && "concept_name" %in% names(frame)) frame$concept_name <- as.character(frame$concept_id)
    names(frame) <- unname(new_names[names(frame)])
    types <<- vapply(frame, function(x) paste(typeof(x), paste(class(x), collapse = "/"), sep = "|"), "")
    if (descriptor$metadata$format == "parquet") {
      table <- arrow::Table$create(frame)
      if (is.null(writer)) {
        sink <<- arrow::FileOutputStream$create(pending)
        writer <<- arrow::ParquetFileWriter$create(schema = table$schema, sink = sink,
          properties = arrow::ParquetWriterProperties$create(column_names = names(frame)))
      }
      writer$WriteTable(table)
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
# passes. Keep distinct person/ID support privately, never full clinical rows.
.conceptStagePresent <- function(ctx, descriptor, columns = NULL, root, component,
                                 reference = FALSE) {
  if (!.reduceTranslate(ctx) || !is.null(descriptor$metadata$concept_presentation)) return(descriptor)
  prototype <- NULL
  reference_rows <- list()
  identities <- persons <- list()
  all_persons <- character()
  .publishReadChunks(descriptor, function(frame) {
    if (is.null(prototype)) {
      prototype <<- frame[FALSE, , drop = FALSE]
      if (is.null(columns)) columns <<- .conceptLegacyColumns(ctx, frame, root, component)
    }
    if (reference && all(c("covariateId", "analysisId", "conceptId", "covariateName") %in% names(frame))) {
      reference_rows[[length(reference_rows) + 1L]] <<- frame
    }
    concept_cols <- names(columns)[vapply(columns, .conceptColumn, logical(1))]
    person_col <- intersect(.PERSON_KEY_COLS(), names(frame))
    if (length(person_col)) all_persons <<- union(all_persons, as.character(frame[[person_col[[1L]]]]))
    for (col in intersect(concept_cols, names(frame))) {
      ids <- as.character(frame[[col]])
      observed <- unique(ids[!is.na(ids)])
      identities[[col]] <<- union(identities[[col]], observed)
      for (id in observed) {
        keys <- if (length(person_col)) as.character(frame[[person_col[[1L]]]][which(ids == id)]) else character()
        persons[[col]][[id]] <<- union(persons[[col]][[id]], keys)
      }
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
  sample <- .conceptPresent(ctx, sample, columns)
  updated <- .conceptStageCapture(sample, descriptor)
  if (is.null(updated$metadata$concept_presentation)) return(descriptor)
  meta <- .ds_arg(updated$metadata$concept_presentation)
  meta$total <- length(all_persons)
  meta$support <- lapply(persons, function(x) as.list(lengths(x)))
  if (!is.null(reference_support)) {
    meta$total <- reference_support$total
    meta$support <- lapply(meta$contracts, function(c) as.list(reference_support$counts))
  }
  updated$metadata$concept_presentation <- .conceptEncode(meta)
  updated <- .conceptRewritePrivate(descriptor, updated, function(frame) {
    for (col in names(meta$contracts)) {
      resolved <- meta$contracts[[col]]$resolved
      ids <- as.character(unlist(resolved$ids, use.names = FALSE))
      labels <- as.character(unlist(resolved$labels, use.names = FALSE))
      levels <- as.character(unlist(resolved$levels, use.names = FALSE))
      frame[[col]] <- .conceptFactor(labels[match(as.character(frame[[col]]), ids)], levels = levels)
    }
    frame <- .conceptReferenceNames(frame, meta$contracts)
    if ("concept_id" %in% names(meta$contracts) && "concept_name" %in% names(frame)) frame$concept_name <- as.character(frame$concept_id)
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
      writer$WriteTable(table)
    } else .stagedWriteCsv(frame, pending,
      col.names = first, append = !first)
    first <<- FALSE
  })
  if (!is.null(writer)) { writer$Close(); writer <- NULL; sink$close(); sink <- NULL }
  Sys.chmod(pending, "0600")
  if (!file.rename(pending, path)) stop("Private concept presentation failed.", call. = FALSE)
  updated
}
