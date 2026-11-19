# A federation category contract includes locally unused levels. stats::glm()
# asks model.frame to drop them; retain that contract during subsetting so its
# design matrix has the same columns at every site. This is still an unordered
# R factor, with no synthetic rows or change to its values.
.conceptFactor <- function(x, levels) {
  out <- factor(x, levels = levels)
  class(out) <- c("dsomop_concept_factor", "factor")
  out
}

# OMOP IDs use decimal identity text even when a database returns doubles and
# R would otherwise choose scientific notation. Character/factor labels and
# integer64 values keep their exact text without a lossy numeric conversion.
.conceptIdText <- function(x) {
  if (inherits(x, "integer64") || is.factor(x) || is.character(x)) return(as.character(x))
  out <- as.character(x)
  if (is.numeric(x)) {
    whole <- !is.na(x) & is.finite(x) & x == trunc(x)
    out[whole] <- sprintf("%.0f", x[whole])
  }
  out
}

#' @export
`[.dsomop_concept_factor` <- function(x, ..., drop = FALSE) {
  NextMethod("[", drop = FALSE)
}

# Server-side lineage and presentation. Clinical values stay numeric until the
# final publication boundary, after joins, reducers and support decisions.
.conceptDeclaredColumn <- function(column, registry) {
  if (!is.null(registry) && "semantic_class" %in% names(registry)) {
    return(identical(registry$semantic_class[match(tolower(column), tolower(registry$column_name))], "concept"))
  }
  if (is.null(registry) || !"concept_role" %in% names(registry)) return(FALSE)
  role <- registry$concept_role[match(tolower(column), tolower(registry$column_name))]
  length(role) == 1L && !is.na(role) && nzchar(role) && role != "non_concept"
}

.conceptColumn <- function(column) {
  isTRUE(attr(column, "concept")) || column$role %in% c("concept", "unit", "operator")
}

.conceptVocabulary <- function(handle, ids) {
  out <- setNames(lapply(ids, function(id) list(id = id, name = NULL,
    vocabulary = NULL, version = NULL, origin = "fallback", label = id)), ids)
  if (!length(ids)) return(out)
  bp <- .buildBlueprint(handle)
  tables <- bp$tables$table_name[bp$tables$present_in_db]
  if (!"concept" %in% tables) return(out)
  # Do not use the legacy reader's invented concept-zero row.
  table <- .qualifyTable(handle, "concept", .resolveTableSchema(handle, "concept", "Vocabulary"))
  cols <- bp$columns$concept$column_name
  fields <- intersect(c("concept_id", "concept_name", "vocabulary_id"), cols)
  if (!all(c("concept_id", "concept_name") %in% fields)) return(out)
  info <- .executeQuery(handle, paste0("SELECT ", paste(fields, collapse = ", "),
    " FROM ", table, " WHERE concept_id IN (", .sqlIdList(ids), ")"))
  versions <- character()
  if ("vocabulary" %in% tables && all(c("vocabulary_id", "vocabulary_version") %in%
                                      bp$columns$vocabulary$column_name)) {
    vt <- .qualifyTable(handle, "vocabulary", .resolveTableSchema(handle, "vocabulary", "Vocabulary"))
    v <- .executeQuery(handle, paste0("SELECT vocabulary_id, vocabulary_version FROM ", vt))
    versions <- setNames(as.character(v$vocabulary_version), as.character(v$vocabulary_id))
  }
  for (i in seq_len(nrow(info))) {
    id <- .conceptIdText(info$concept_id[[i]])
    name <- as.character(info$concept_name[[i]])
    if (is.na(name) || !nzchar(trimws(name))) next
    vocab <- if ("vocabulary_id" %in% names(info)) as.character(info$vocabulary_id[[i]]) else ""
    version <- unname(versions[vocab]); if (!length(version) || is.na(version)) version <- NULL
    out[[id]] <- list(id = id, name = name, vocabulary = vocab, version = version,
                      origin = "vocabulary", label = name)
  }
  out
}

# Translation selects labels only. Identity and the factor release contract do
# not depend on whether a vocabulary name is requested.
.conceptIdRecords <- function(ids) {
  setNames(lapply(ids, function(id) list(id = id, name = NULL,
    vocabulary = NULL, version = NULL, origin = "fallback", label = id)), ids)
}

# Observed concept values whose levels fail the DataSHIELD rule declare the
# public vocabulary list that holds them, when a short one exists: every valid
# concept of the vocabularies, domains, classes and standard flags those values
# have. Those kinds are the only data-derived part, and they pass the same level
# rule. The list itself is read from the vocabulary tables only, as
# omop_concept_levels() would declare it; it does not depend on which concepts
# occur. Lists longer than dsomop.auto_declared_levels_max are left undeclared.
.conceptAutoDeclared <- function(handle, ids, n) {
  ids <- unique(as.character(ids[!is.na(ids)]))
  limit <- .omopDisclosureSettings()$auto_declared_levels_max
  if (is.null(handle) || !length(ids) || !is.finite(limit) || limit < 1 ||
      any(!grepl("^[0-9]+$", ids))) return(character())
  bp <- .buildBlueprint(handle)
  if (!"concept" %in% bp$tables$table_name[bp$tables$present_in_db]) return(character())
  fields <- c("concept_id", "vocabulary_id", "domain_id", "concept_class_id", "standard_concept", "invalid_reason")
  if (!all(fields %in% bp$columns$concept$column_name)) return(character())
  table <- .qualifyTable(handle, "concept", .resolveTableSchema(handle, "concept", "Vocabulary"))
  valid <- "(invalid_reason IS NULL OR invalid_reason = '')"
  kinds <- do.call(rbind, lapply(split(ids, ceiling(seq_along(ids) / 500L)), function(chunk)
    .executeQuery(handle, paste0("SELECT DISTINCT vocabulary_id, domain_id, concept_class_id, standard_concept FROM ",
      table, " WHERE ", valid, " AND concept_id IN (", .sqlIdList(chunk), ")"))))
  if (is.null(kinds) || !nrow(kinds)) return(character())
  kinds <- unique(as.data.frame(lapply(kinds, as.character), stringsAsFactors = FALSE))
  if (anyNA(kinds[c("vocabulary_id", "domain_id", "concept_class_id")])) return(character())
  if (!.omopConceptDiscovery(do.call(paste, c(unname(kinds), sep = "\r")), n)$safe) return(character())
  same <- vapply(seq_len(nrow(kinds)), function(i) {
    standard <- kinds$standard_concept[[i]]
    paste0("(vocabulary_id = ", .quoteLiteral(kinds$vocabulary_id[[i]], handle),
           " AND domain_id = ", .quoteLiteral(kinds$domain_id[[i]], handle),
           " AND concept_class_id = ", .quoteLiteral(kinds$concept_class_id[[i]], handle), " AND ",
           if (is.na(standard) || !nzchar(standard)) "(standard_concept IS NULL OR standard_concept = '')"
           else paste0("standard_concept = ", .quoteLiteral(standard, handle)), ")")
  }, "")
  where <- paste0(valid, " AND (", paste(same, collapse = " OR "), ")")
  size <- suppressWarnings(as.numeric(.executeQuery(handle,
    paste0("SELECT COUNT(*) AS n FROM ", table, " WHERE ", where))$n))
  if (length(size) != 1L || is.na(size)) return(character())
  # Too long a public list is reported as such (vocabulary metadata only).
  if (size > limit) return(structure(character(), too_long = TRUE))
  .conceptIdText(.executeQuery(handle, paste0("SELECT concept_id FROM ", table, " WHERE ", where))$concept_id)
}

.conceptPresent <- function(ctx, frame, columns) {
  n <- attr(frame, "omop_total_rows") %||% nrow(frame)
  out <- .conceptPresentOnce(ctx, frame, columns)
  # A column that would be refused, and only such a column, is presented again
  # with its public list declared; recipes that publish are unchanged.
  if (isTRUE(attr(frame, "omop_concept_reference"))) return(out)
  contracts <- attr(out, "omop_concepts") %||% list()
  extended <- FALSE
  too_long <- character()
  for (col in intersect(intersect(names(contracts), names(columns)), names(frame))) {
    c <- contracts[[col]]
    if (isTRUE(.conceptAssessIds(c, c$ids, n)$safe)) next
    if (identical(c$kind, "date")) {
      dated <- .dateAutoDeclared(frame[[col]], n)
      if (isTRUE(attr(dated, "too_long"))) too_long <- c(too_long, col)
      if (!is.factor(dated)) next
      frame[[col]] <- dated
      extended <- TRUE
      next
    }
    if (!.conceptColumn(columns[[col]]) || !(is.null(c$kind) || identical(c$kind, "category"))) next
    declared <- as.character(unlist(attr(columns[[col]], "concept_declared"), use.names = FALSE))
    auto <- .conceptAutoDeclared(ctx$handle, .conceptIdText(frame[[col]]), n)
    if (isTRUE(attr(auto, "too_long"))) too_long <- c(too_long, col)
    auto <- setdiff(auto, declared)
    if (!length(auto)) next
    attr(columns[[col]], "concept_declared") <- c(declared, auto)
    extended <- TRUE
  }
  # A presentation the declared list does not fit stays refused, as before.
  if (extended) out <- tryCatch(.conceptPresentOnce(ctx, frame, columns), error = function(e) out)
  if (length(too_long)) {
    contracts <- attr(out, "omop_concepts")
    for (col in intersect(too_long, names(contracts))) contracts[[col]]$auto_levels <- "too_long"
    attr(out, "omop_concepts") <- contracts
  }
  out
}

.conceptPresentOnce <- function(ctx, frame, columns) {
  for(n in intersect(names(frame),names(columns))) {
    mask<-attr(frame[[n]],"category_defined")
    if(!is.null(mask))attr(columns[[n]],"category_defined")<-mask
  }
  for (col in intersect(names(frame), names(columns))) {
    domain <- attr(columns[[col]], "category_domain")
    if (!is.null(domain)) {
      values <- as.character(frame[[col]])
      if (!is.null(domain$calendar)) {
        levels <- domain$levels %||% as.character(sort(unique(frame[[col]][!is.na(frame[[col]])])))
        ranks <- if (is.null(domain$levels) && all(grepl("^-?[0-9]+$", levels))) as.numeric(levels) else NULL
        frame[[col]] <- .dateFactor(values, levels, domain$calendar, order = ranks,
          public = domain$levels %||% character())
      } else frame[[col]] <- if (isTRUE(domain$ordered))
        .dateOrdered(values, domain$levels) else .conceptFactor(values, domain$levels)
    }
  }
  frame <- .dateCapture(frame, columns)
  for(col in names(frame)) {
    consumed<-attr(frame[[col]],"category_consumed")
    if(!is.null(consumed)) {
      contracts<-attr(frame,"omop_concepts") %||% list();contracts[[col]]<-consumed
      attr(frame,"omop_concepts")<-contracts
    }
  }
  frame <- .numberCapture(frame, columns)
  translate <- .reduceTranslate(ctx)
  contracts <- attr(frame, "omop_concepts") %||% list()
  for (col in intersect(names(columns), names(frame))) {
    if (!.conceptColumn(columns[[col]])) next
    owner <- attr(columns[[col]], "concept_owner")
    variable <- if (length(owner)) ctx$ir$variables[[owner]] else NULL
    recipe_map <- .conceptMap(ctx$ir$options$labels, "options.labels", wire = TRUE)
    variable_map <- .conceptMap(variable$labels, paste0("variables.", owner, ".labels"), wire = TRUE)
    map <- .conceptMapOverlay(recipe_map, variable_map)
    ids <- .conceptIdText(frame[[col]])
    declared <- union(names(map), attr(columns[[col]], "concept_declared"))
    all_ids <- unique(c(ids[!is.na(ids)], declared))
    records <- if (translate) .conceptVocabulary(ctx$handle, all_ids) else .conceptIdRecords(all_ids)
    for (id in if (translate) names(map) else character()) {
      records[[id]]$origin <- if (id %in% names(variable_map)) "variable" else "recipe"
      records[[id]]$label <- unname(map[[id]])
    }
    resolved <- .conceptResolve(unname(records))
    values <- resolved$labels[match(ids, resolved$ids)]
    frame[[col]] <- .conceptFactor(values, levels = resolved$levels)
    contracts[[col]] <- list(ids = ids, records = records,
                              declared = declared,
                              reference_admitted = isTRUE(attr(columns[[col]], "concept_reference_admitted")),
                              translate = translate, owner = owner, resolved = resolved)
    if (col == "conceptId" && "covariateId" %in% names(frame)) {
      contracts[[col]]$reference_keys <- setNames(ids, as.character(frame$covariateId))
      contracts[[col]]$resolved$naming_ids <- unique(ids[!is.na(ids)])
    }
  }
  if (length(contracts)) {
    attr(frame, "omop_concepts") <- contracts
    attr(frame, "omop_concept_cols") <- names(contracts)
    if (translate && "concept_id" %in% names(contracts) && "concept_name" %in% names(frame)) {
      frame$concept_name <- as.character(frame$concept_id)
    }
  }
  frame <- .categoryPresent(ctx, frame, columns)
  contracts <- attr(frame,"omop_concepts") %||% contracts
  if (translate) frame <- .conceptReferenceNames(frame, contracts)
  generated <- list()
  for (key in intersect(names(columns), names(frame))) {
    g <- attr(columns[[key]], "concept_name")
    if (is.null(g)) next
    # A generated column name says that its concept occurs. A column this site
    # does not admit is refused here, at publication, translated or not: it is
    # never assigned, so no client has to revoke it.
    if (!isTRUE(g$declared) && !isTRUE(g$permitted)) .conceptLevelReject()
    if (!translate) next
    owner <- g$owner
    variable <- if (length(owner)) ctx$ir$variables[[owner]] else NULL
    maps <- .conceptMapOverlay(
      .conceptMap(ctx$ir$options$column_names, "options.column_names", TRUE, TRUE),
      .conceptMap(variable$column_names, paste0("variables.", owner, ".column_names"), TRUE, TRUE))
    g$candidates <- unname(.conceptVocabulary(ctx$handle, g$id))
    g$explicit <- unname(maps[key]); if (!length(g$explicit) || is.na(g$explicit)) g$explicit <- NULL
    generated[[key]] <- g
  }
  if (length(generated)) {
    mapping <- .conceptGeneratedNames(names(frame), generated)
    attr(frame, "omop_generated_columns") <- generated
    attr(frame, "omop_column_names") <- mapping
    names(frame) <- unname(mapping)
  }
  if (length(contracts) || length(generated)) {
    attr(frame, "omop_presentation_handle") <- .conceptHandleKey(ctx)
  }
  if(.categoryNative(ctx$ir)||length(.categoryCapabilities(ctx$ir)))attr(frame,"omop_codebook_schema")<-.categoryReleaseSchema(ctx,frame,columns)
  frame
}

.conceptAssessIds <- function(contract, observed, n, reference_support = NULL, discover = TRUE) {
  observed <- unique(as.character(observed[!is.na(observed)]))
  # These references name generated columns whose identities were admitted by
  # their existing name policy. They do not discover an additional value domain.
  if (isTRUE(contract$reference_admitted)) return(list(levels = observed, safe = TRUE, rules = character()))
  if (!is.null(reference_support)) {
    safe <- all(observed %in% names(reference_support$counts))
    # Naming the concepts behind generated covariates is a level release: the
    # DataSHIELD level rule applies, without a per-concept person minimum.
    if (discover) safe <- safe && tryCatch({ .assertSafeLevels(length(observed), reference_support$total); TRUE },
      error = function(e) FALSE)
    return(list(levels = observed, safe = safe,
                rules = if(safe)character() else "generated-column admission"))
  }
  # A category contract can carry locally loaded hierarchy metadata in addition
  # to row values. Those identities are observed too, unless explicitly public.
  discovered <- as.character(unlist(contract$discovered, use.names = FALSE))
  reported <- union(observed, intersect(discovered, names(contract$records)))
  assessment <- .omopConceptDiscovery(union(observed, discovered), n, contract$declared)
  assessment$levels <- setdiff(reported, as.character(unlist(contract$declared, use.names = FALSE)))
  if (!discover) { assessment$safe <- TRUE; assessment$rules <- character() }
  assessment
}

.conceptLevelReject <- function() {
  stop(structure(list(message = paste0("Concept factor publication refused: generated concept ",
                        "columns fail this site's factor-level rule."), call = NULL,
                      code = "validation_error", reason = "concept_level_rule"),
                 class = c("dsomop_plan_rejection", "error", "condition")))
}

.conceptSiteVerdict <- function(report) {
  if (length(report$unsafe)) {
    stop("Concept factor assignment refused by this site's factor-level rule.", call. = FALSE)
  }
  invisible(TRUE)
}

.conceptAssess <- function(df, col, contract, discover = TRUE) {
  if (length(contract$ids) != nrow(df)) stop("Incomplete concept identity metadata.", call. = FALSE)
  .conceptAssessIds(contract, contract$ids, nrow(df),
    if(isTRUE(attr(df,"omop_concept_reference")))attr(df,"omop_reference_support"), discover)
}

.conceptLevelReport <- function(df, discover = TRUE) {
  contracts <- attr(df, "omop_concepts") %||% list()
  refreshed <- .conceptRefresh(contracts, attr(df, "omop_generated_columns") %||% list(), attr(df, "omop_presentation_handle"))
  contracts <- refreshed$contracts
  cap <- .omopDisclosureSettings()$nfilter_levels_max
  out <- list(levels = list(), unsafe = character(), unsafe_rules = list(), nfilter_levels_max = cap,
              concept_protocol = 1L, concepts = list())
  for (col in names(contracts)) {
    c <- contracts[[col]]
    a <- .conceptAssess(df, col, c, discover)
    if (!isTRUE(a$safe)) {
      out$unsafe <- c(out$unsafe, col)
      out$unsafe_rules[[col]] <- c(a$rules, if (identical(c$auto_levels, "too_long")) "dsomop.auto_declared_levels_max")
      # No observed identity/name metadata for a refused column.
      next
    }
    permitted <- union(a$levels, c$declared)
    out$levels[[col]] <- permitted
    out$concepts[[col]] <- unname(c$records[permitted])
    if (identical(c$kind, "date")) out$calendars[[col]] <- c$calendar
    if (identical(c$kind, "category")) out$categories[[col]] <- c$category
    if (length(c$reference_keys)) out$reference_names[[col]] <- unique(unname(c$reference_keys))
  }
  generated <- refreshed$generated
  out$generated <- list()
  refused_keys <- character()
  for (key in names(generated)) {
    g <- generated[[key]]
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
  out$numbers <- attr(df,"omop_numbers") %||% list()
  out$keys <- setdiff(names(attr(df, "omop_column_names")) %||% names(df), refused_keys)
  if(!is.null(attr(df,"omop_codebook_schema")))out$schema<-attr(df,"omop_codebook_schema")
  out$evidence <- .conceptEvidence(out)
  out
}

.conceptRelevel <- function(df, spec, table_column = NULL) {
  if (!is.null(table_column) &&
      (!.is_omop.table(df) || !is.character(table_column) ||
       length(table_column) != 1L || is.na(table_column) ||
       !table_column %in% names(attr(df, "omop_concepts")))) {
    stop("Table analysis requires a published concept column in an omop.table.", call. = FALSE)
  }
  provenance <- NULL
  if (!is.null(attr(df, "dsomop_dp_provenance"))) {
    policy <- .dsomopDpPolicy()
    provenance <- .dsomopDpVerifyPersonLocal(df, policy = policy)
  }
  original <- df
  # The site's own verdict on its observed levels applies at assignment too:
  # a column this site refuses is never relevelled, whatever evidence is sent.
  report <- .conceptLevelReport(df)
  .conceptSiteVerdict(report)
  .conceptCheckEvidence(report, spec, !is.null(attr(df, "omop_presentation_handle")))
  # Legacy factor-only calls leave numeric values and semantics untouched.
  if ("numbers" %in% names(spec)) .numberValidateShared(attr(df,"omop_numbers"), spec$numbers)
  refreshed <- .conceptRefresh(attr(df, "omop_concepts") %||% list(), attr(df, "omop_generated_columns") %||% list(), attr(df, "omop_presentation_handle"))
  attr(df, "omop_concepts") <- refreshed$contracts
  attr(df, "omop_generated_columns") <- refreshed$generated
  physical <- attr(df, "omop_column_names")
  if (length(physical)) names(df) <- names(physical)
  contracts <- attr(df, "omop_concepts") %||% list()
  if (!setequal(names(contracts), names(spec$columns))) {
    stop("Incomplete shared concept-column contract.", call. = FALSE)
  }
  for (col in names(contracts)) {
    c <- contracts[[col]]
    shared <- spec$columns[[col]]
    if (identical(c$kind, "date") && !identical(.jcsCanonical(shared$calendar), .jcsCanonical(c$calendar))) .dateReject("Changed shared calendar identity.")
    if(identical(c$kind,"category")&&!identical(.jcsCanonical(shared$category),.jcsCanonical(c$category))) .categoryReject("Changed category contract.","category_domain_incompatible")
    a <- .conceptAssess(df, col, c, discover = FALSE)
    .conceptValidateResolution(shared, c$records[union(a$levels,c$declared)])
    ids <- as.character(unlist(shared$ids, use.names = FALSE))
    labels <- unname(.categoryNullableStrings(shared$labels))
    levels <- as.character(unlist(shared$levels, use.names = FALSE))
    if (length(ids) != length(labels) || anyNA(ids) || (!identical(c$kind,"category") && anyNA(labels)) ||
        anyDuplicated(ids) || anyDuplicated(levels) || !setequal(unique(labels[!is.na(labels)]), levels)) {
      stop("Invalid shared concept levels.", call. = FALSE)
    }
    if (!a$safe || !all(c(a$levels, c$declared) %in% ids)) {
      stop("Concept reconciliation cannot certify complete disclosure-safe coverage.", call. = FALSE)
    }
    values <- labels[match(c$ids, ids)]
    df[[col]] <- if (identical(c$kind,"category")) .categoryFactor(values,shared) else if ((c$kind %||% "") %in% c("date","number")) .dateOrdered(values, levels = levels) else .conceptFactor(values, levels = levels)
    if (identical(col, table_column)) {
      # One token per resolved category, including deliberate recode merges.
      # Only ASCII tokens cross dsBase's comma-delimited level transport.
      # Relabelling levels preserves every factor code and actual NA exactly.
      levels(df[[col]]) <- paste0("C", seq_along(levels))
    }
    c$resolved <- shared
    contracts[[col]] <- c
  }
  attr(df, "omop_concepts") <- contracts
  df <- .conceptReferenceNames(df, contracts)
  if (!isFALSE(contracts$concept_id$translate) && "concept_id" %in% names(contracts) && "concept_name" %in% names(df)) {
    df$concept_name <- as.character(df$concept_id)
  }
  if (length(attr(df, "omop_generated_columns")) && !length(spec$names)) stop("Incomplete shared generated-column names.", call. = FALSE)
  if (length(spec$names)) {
    mapping <- unlist(spec$names, use.names = TRUE)
    if (!setequal(names(mapping), names(df)) || anyDuplicated(mapping) ||
        any(!.conceptValidNames(mapping))) {
      stop("Invalid final generated-column contract.", call. = FALSE)
    }
    .conceptValidateNames(mapping, names(df), attr(df, "omop_generated_columns") %||% list())
    names(df) <- unname(mapping[names(df)])
    attr(df, "omop_column_names") <- mapping
  }
  if (!is.null(provenance)) {
    # Only the verified ID-preserving transformation above reaches this seal.
    # Rows, keys and numeric measures are untouched; deliberate merges are
    # restricted to the recodes already bound by the source plan's lineage.
    df <- .dsomopDpSetFilterState(df, .dsomopDpFilterState(original, provenance = provenance, policy = policy))
    df <- .dsomopDpSealInheritedLineage(df, provenance, provenance$producer, policy,
                                         source = original)
  }
  if (is.null(table_column)) .publishUpdateOwnedValue(original, df)
  df
}

# The compatibility adapter derives semantic classes from the original table
# declarations and feature specifications. Output spellings never classify IDs.
.conceptLegacyColumns <- function(ctx, frame, root, component = root) {
  existing <- attr(frame, "omop_column_contract")
  columns <- setNames(lapply(frame, function(x) .fragmentColumn(
    if (inherits(x, "Date")) "date" else if (is.character(x) || is.factor(x)) "text" else "numeric")), names(frame))
  if (length(existing)) columns[intersect(names(existing), names(columns))] <- existing[intersect(names(existing), names(columns))]
  domains <- attr(frame, "omop_category_columns") %||% list()
  for (col in intersect(names(columns), names(domains))) attr(columns[[col]], "category_domain") <- domains[[col]]
  out <- ctx$ir$outputs[[root]]
  if (is.null(out)) return(columns)
  bp <- .buildBlueprint(ctx$handle)
  mark <- function(target, source, table) {
    if (target %in% names(columns) && .conceptDeclaredColumn(source, bp$columns[[table]])) {
      attr(columns[[target]], "concept") <<- TRUE
    }
  }
  raw <- function(spec, table) {
    cs <- .colSpec(spec)
    if (is.null(cs$source)) {
      for (col in names(frame)) mark(col, col, table)
    } else for (i in seq_along(cs$source)) mark(cs$alias[[i]], cs$source[[i]], table)
  }
  features <- function(specs, table) {
    for (name in names(specs)) {
      spec <- specs[[name]]
      if ((spec$type %||% "boolean") %in% c("first_value", "latest_value", "last_value", "min", "max", "min_value", "max_value", "mode")) {
        mark(name, spec$value_column %||% "value_as_number", table)
      }
    }
  }
  type <- out$type %||% "event_level"
  if (identical(type, "person_level")) for (table in names(out$tables)) {
    entry <- out$tables[[table]]
    if (is.list(entry) && !is.null(entry$features)) features(entry$features, table) else raw(entry, table)
  }
  if (identical(type, "baseline")) {
    raw(out$columns, "person")
    if ("age_group" %in% names(columns) && "age_at_index" %in% unlist(out$derived))
      attr(columns$age_group, "category_domain") <- .ageBandDomain(out$age_breaks)
  }
  if (identical(type, "person_level")) for (d in .normalizeDerivedColumnSpecs(out$derived_columns)) {
    if (!d$name %in% names(columns)) next
    if (identical(d$kind, "sex_mf")) attr(columns[[d$name]], "category_domain") <- list(levels = c("F", "M"), ordered = FALSE)
  }
  if (identical(type, "event_level")) {
    format <- out$representation$format %||% "long"
    if (format == "long") raw(out$columns, out$table)
    if (format == "features") features(out$representation$features, out$table)
  }
  if (identical(type, "concept_dictionary") && "concept_id" %in% names(columns)) {
    columns$concept_id$role <- "concept"
    attr(columns$concept_id, "concept_declared") <- attr(frame, "omop_dictionary_declared")
    # The dictionary lists the complete declared sets of its source outputs
    # plus the values those outputs release in the same plan, whose concept
    # columns pass this site's discovery or are refused with the whole plan.
    # Its rows are references, not a new domain: counting them against the
    # dictionary's own length would refuse every dictionary.
    attr(columns$concept_id, "concept_reference_admitted") <- TRUE
  }
  if (identical(component, "covariateRef") && "conceptId" %in% names(columns)) columns$conceptId$role <- "concept"
  if (identical(type, "intervals_long") && "concept_id" %in% names(columns)) {
    # Interval rows carry their source concept; its concept filters resolve on
    # the vocabulary tables only, so their IDs are declared levels.
    attr(columns$concept_id, "concept") <- TRUE
    declared <- .declaredOutputConceptIds(ctx$handle, out)
    if (length(declared)) attr(columns$concept_id, "concept_declared") <- .conceptIdText(declared)
  }
  columns
}

.conceptHandleKey <- function(ctx) {
  for (frame in rev(sys.frames())) {
    keys <- grep('^\\.dsomop_handle_', ls(frame, all.names = TRUE), value = TRUE)
    for (key in keys) if (identical(get(key, frame), ctx$handle)) return(sub('^\\.dsomop_handle_', '', key))
  }
  keys <- grep('^handle_', ls(.dsomop_env, all.names = TRUE), value = TRUE)
  matches <- keys[vapply(keys, function(key) identical(get(key, .dsomop_env), ctx$handle), logical(1))]
  if (length(matches)) sub('^handle_', '', matches[[1L]]) else NULL
}

.conceptRefresh <- function(contracts, generated, key) {
  if (is.null(key)) return(list(contracts = contracts, generated = generated))
  handle <- .getHandle(key)
  for (col in names(contracts)) {
    old <- contracts[[col]]$records
    if ((contracts[[col]]$kind %||% "") %in% c("date","number","category")) next
    if (isFALSE(contracts[[col]]$translate)) next
    fresh <- .conceptVocabulary(handle, names(old))
    for (id in names(old)) if (old[[id]]$origin %in% c('variable','recipe')) {
      fresh[[id]]$origin <- old[[id]]$origin; fresh[[id]]$label <- old[[id]]$label
    }
    contracts[[col]]$records <- fresh
  }
  for (col in names(generated)) generated[[col]]$candidates <- unname(.conceptVocabulary(handle, generated[[col]]$id))
  list(contracts = contracts, generated = generated)
}

.conceptEvidence <- function(report) {
  keys<-c("concepts","generated","keys","reference_names","numbers","schema","categories")
  evidence<-setNames(lapply(keys,function(k)report[[k]]),keys)
  as.character(openssl::sha256(charToRaw(.conceptEncode(evidence))))
}

.conceptCheckEvidence <- function(report, spec, required = FALSE) {
  if (required || length(spec$evidence)) {
    if (!.conceptEvidence(report) %in% as.character(unlist(spec$evidence, use.names = FALSE))) {
      stop('Concept presentation evidence changed; reconcile again.', call. = FALSE)
    }
  }
}

# Legacy sparse feature descriptions are names, independent of value recodes.
.conceptReferenceNames <- function(frame, contracts) {
  c <- contracts$conceptId
  if (isFALSE(c$translate)) return(frame)
  if (is.null(c) || !all(c("covariateId", "covariateName", "analysisId") %in% names(frame))) return(frame)
  ids <- as.character(unlist(c$reference_keys, use.names = TRUE)[as.character(frame$covariateId)])
  shared <- c$resolved
  vocabulary <- as.character(unlist(shared$vocabulary, use.names = FALSE))
  if (!length(vocabulary) && length(ids)) stop("Incomplete shared vocabulary names for sparse reference.", call. = FALSE)
  shared_ids <- as.character(unlist(shared$ids, use.names = FALSE))
  all_ids <- as.character(unlist(shared$naming_ids %||% unique(unname(c$reference_keys)), use.names = FALSE))
  if (!all(all_ids %in% shared_ids) || !all(ids %in% all_ids)) stop("Incomplete sparse naming contract.", call. = FALSE)
  # Resolve over the full federation identity set before selecting local rows.
  # Otherwise a missing local concept would change collision suffixes.
  tokens <- .conceptNameToken(vocabulary[match(all_ids,shared_ids)])
  tokens <- .conceptFinalNames(all_ids, setNames(tokens,all_ids), character(), setNames(all_ids,all_ids))
  token <- tokens[match(ids,all_ids)]
  suffix <- c("binary", "count", "mean", "min", "max")[as.integer(frame$analysisId)]
  if (anyNA(token) || anyNA(suffix)) stop("Invalid sparse reference identity contract.", call. = FALSE)
  frame$covariateName <- paste0(token,"_",suffix)
  frame
}

.conceptExplainClasses <- function(ir, handle = NULL) {
  bp <- if (!is.null(handle)) .buildBlueprint(handle) else NULL
  registry <- utils::read.csv(system.file('column_classes.csv',package='dsOMOP'),stringsAsFactors=FALSE)
  declared <- function(table,column) {
    if (is.null(table) || is.null(column)) return(FALSE)
    if (!is.null(bp)) return(.conceptDeclaredColumn(column,bp$columns[[table]]))
    any(registry$table == table & registry$column == column & registry$semantic_class == 'concept')
  }
  setNames(lapply(names(ir$variables),function(name) {
    v <- ir$variables[[name]]
    concept <- FALSE
    if (!is.null(v$raw)) concept <- declared(v$raw$table,v$raw$column)
    if (!is.null(v$derived)) concept <- v$derived$kind %in% c('race','ethnicity')
    if (!is.null(v$reducer) && v$reducer$fn %in% c('first_value','last_value','nth_value','closest_value','mode','min','max')) {
      column <- v$reducer$value %||% if(v$reducer$fn == 'mode') 'concept_id' else 'value'
      table <- ir$events[[v$source$event %||% '']]$table
      concept <- identical(column,'concept_id') || declared(table,column)
    }
    list(semantic_class=if(concept) 'concept' else 'ordinary',origin_variable=name,
      labels=v$labels %||% ir$options$labels %||% list())
  }),names(ir$variables))
}
