# A federation category contract includes locally unused levels. stats::glm()
# asks model.frame to drop them; retain that contract during subsetting so its
# design matrix has the same columns at every site. This is still an unordered
# R factor, with no synthetic rows or change to its values.
.conceptFactor <- function(x, levels) {
  out <- factor(x, levels = levels)
  class(out) <- c("dsomop_concept_factor", "factor")
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
    id <- as.character(info$concept_id[[i]])
    name <- as.character(info$concept_name[[i]])
    if (is.na(name) || !nzchar(trimws(name))) next
    vocab <- if ("vocabulary_id" %in% names(info)) as.character(info$vocabulary_id[[i]]) else ""
    version <- unname(versions[vocab]); if (!length(version) || is.na(version)) version <- NULL
    out[[id]] <- list(id = id, name = name, vocabulary = vocab, version = version,
                      origin = "vocabulary", label = name)
  }
  out
}

.conceptPresent <- function(ctx, frame, columns) {
  if (!.reduceTranslate(ctx)) return(frame)
  attr(frame, "omop_presentation_handle") <- .conceptHandleKey(ctx)
  contracts <- list()
  for (col in intersect(names(columns), names(frame))) {
    if (!.conceptColumn(columns[[col]])) next
    owner <- attr(columns[[col]], "concept_owner")
    variable <- if (length(owner)) ctx$ir$variables[[owner]] else NULL
    recipe_map <- .conceptMap(ctx$ir$options$labels, "options.labels", wire = TRUE)
    variable_map <- .conceptMap(variable$labels, paste0("variables.", owner, ".labels"), wire = TRUE)
    map <- .conceptMapOverlay(recipe_map, variable_map)
    ids <- as.character(frame[[col]])
    all_ids <- unique(c(ids[!is.na(ids)], names(map)))
    records <- .conceptVocabulary(ctx$handle, all_ids)
    for (id in names(map)) {
      records[[id]]$origin <- if (id %in% names(variable_map)) "variable" else "recipe"
      records[[id]]$label <- unname(map[[id]])
    }
    resolved <- .conceptResolve(unname(records))
    values <- resolved$labels[match(ids, resolved$ids)]
    frame[[col]] <- .conceptFactor(values, levels = resolved$levels)
    contracts[[col]] <- list(ids = ids, records = records, declared = names(map),
                              owner = owner, resolved = resolved)
    if (col == "conceptId" && "covariateId" %in% names(frame)) {
      contracts[[col]]$reference_keys <- setNames(ids, as.character(frame$covariateId))
    }
  }
  if (length(contracts)) {
    attr(frame, "omop_concepts") <- contracts
    attr(frame, "omop_concept_cols") <- names(contracts)
    if ("concept_id" %in% names(contracts) && "concept_name" %in% names(frame)) {
      frame$concept_name <- as.character(frame$concept_id)
    }
  }
  frame <- .conceptReferenceNames(frame, contracts)
  generated <- list()
  for (key in intersect(names(columns), names(frame))) {
    g <- attr(columns[[key]], "concept_name")
    if (is.null(g)) next
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
  frame
}

.conceptAssess <- function(df, col, contract) {
  ids <- contract$ids
  if (length(ids) != nrow(df)) stop("Incomplete concept identity metadata.", call. = FALSE)
  observed <- unique(ids[!is.na(ids)])
  if (isTRUE(attr(df, "omop_concept_reference"))) {
    support <- attr(df, "omop_reference_support")
    if (is.null(support)) return(list(levels = observed, safe = TRUE))
    settings <- .omopDisclosureSettings()
    safe <- all(observed %in% names(support$counts)) && all(support$counts[observed] >= settings$nfilter_tab) &&
      tryCatch({ .assertSafeLevels(length(observed), support$total); TRUE }, error = function(e) FALSE)
    return(list(levels = observed, safe = safe))
  }
  keys <- intersect(.PERSON_KEY_COLS(), names(df))
  if (!length(observed)) return(list(levels = character(), safe = TRUE))
  if (!length(keys)) return(list(levels = observed, safe = FALSE))
  probe <- df
  probe[[col]] <- ids
  .omopConceptLevelAssessment(probe, col, keys[[1L]])
}

.conceptLevelReport <- function(df) {
  contracts <- attr(df, "omop_concepts") %||% list()
  refreshed <- .conceptRefresh(contracts, attr(df, "omop_generated_columns") %||% list(), attr(df, "omop_presentation_handle"))
  contracts <- refreshed$contracts
  cap <- .omopDisclosureSettings()$nfilter_levels_max
  out <- list(levels = list(), unsafe = character(), nfilter_levels_max = cap,
              concept_protocol = 1L, concepts = list())
  for (col in names(contracts)) {
    c <- contracts[[col]]
    a <- .conceptAssess(df, col, c)
    if (!isTRUE(a$safe)) {
      out$unsafe <- c(out$unsafe, col)
      # No observed identity/name metadata for a refused column.
      next
    }
    permitted <- union(a$levels, c$declared)
    out$levels[[col]] <- permitted
    out$concepts[[col]] <- unname(c$records[permitted])
  }
  generated <- refreshed$generated
  out$generated <- list()
  for (key in names(generated)) {
    g <- generated[[key]]
    if (!isTRUE(g$declared) && !isTRUE(g$permitted)) {
      out$unsafe <- union(out$unsafe, key)
      next
    }
    g[c("declared", "permitted")] <- NULL
    out$generated[[key]] <- g
  }
  out$keys <- names(attr(df, "omop_column_names")) %||% names(df)
  out$evidence <- .conceptEvidence(out)
  out
}

.conceptRelevel <- function(df, spec) {
  provenance <- NULL
  if (!is.null(attr(df, "dsomop_dp_provenance"))) {
    policy <- .dsomopDpPolicy()
    provenance <- .dsomopDpVerifyPersonLocal(df, policy = policy)
  }
  original <- df
  .conceptCheckEvidence(.conceptLevelReport(df), spec, !is.null(attr(df, "omop_presentation_handle")))
  refreshed <- .conceptRefresh(attr(df, "omop_concepts") %||% list(), attr(df, "omop_generated_columns") %||% list(), attr(df, "omop_presentation_handle"))
  attr(df, "omop_concepts") <- refreshed$contracts
  attr(df, "omop_generated_columns") <- refreshed$generated
  physical <- attr(df, "omop_column_names")
  if (length(physical)) names(df) <- names(physical)
  contracts <- attr(df, "omop_concepts") %||% list()
  if (!setequal(names(contracts), names(spec$columns))) {
    stop("Incomplete shared concept-column contract.", call. = FALSE)
  }
  cap <- .omopDisclosureSettings()$nfilter_levels_max
  for (col in names(contracts)) {
    c <- contracts[[col]]
    shared <- spec$columns[[col]]
    .conceptValidateResolution(shared, c$records)
    ids <- as.character(unlist(shared$ids, use.names = FALSE))
    labels <- as.character(unlist(shared$labels, use.names = FALSE))
    levels <- as.character(unlist(shared$levels, use.names = FALSE))
    if (length(ids) != length(labels) || anyNA(ids) || anyNA(labels) ||
        anyDuplicated(ids) || anyDuplicated(levels) || !setequal(unique(labels), levels) ||
        length(ids) > cap || length(levels) > cap) {
      stop("Invalid or excessive shared concept levels.", call. = FALSE)
    }
    a <- .conceptAssess(df, col, c)
    if (!a$safe || !all(c(a$levels, c$declared) %in% ids)) {
      stop("Concept reconciliation cannot certify complete disclosure-safe coverage.", call. = FALSE)
    }
    keys <- intersect(.PERSON_KEY_COLS(), names(df))
    if (length(keys) && nrow(df)) .assertSafeLevels(length(ids), length(unique(df[[keys[[1L]]]])))
    reference_support <- attr(df, "omop_reference_support")
    if (!is.null(reference_support)) .assertSafeLevels(length(ids), reference_support$total)
    values <- labels[match(c$ids, ids)]
    df[[col]] <- .conceptFactor(values, levels = levels)
    c$resolved <- shared
    contracts[[col]] <- c
  }
  attr(df, "omop_concepts") <- contracts
  df <- .conceptReferenceNames(df, contracts)
  if ("concept_id" %in% names(contracts) && "concept_name" %in% names(df)) {
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
    df <- .dsomopDpSealInheritedLineage(df, provenance, provenance$producer, policy)
  }
  .publishUpdateOwnedValue(original, df)
  df
}

# The compatibility adapter derives semantic classes from the original table
# declarations and feature specifications. Output spellings never classify IDs.
.conceptLegacyColumns <- function(ctx, frame, root, component = root) {
  existing <- attr(frame, "omop_column_contract")
  columns <- setNames(lapply(frame, function(x) .fragmentColumn(
    if (inherits(x, "Date")) "date" else if (is.character(x) || is.factor(x)) "text" else "numeric")), names(frame))
  if (length(existing)) columns[intersect(names(existing), names(columns))] <- existing[intersect(names(existing), names(columns))]
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
  if (identical(type, "baseline")) raw(out$columns, "person")
  if (identical(type, "event_level")) {
    format <- out$representation$format %||% "long"
    if (format == "long") raw(out$columns, out$table)
    if (format == "features") features(out$representation$features, out$table)
  }
  if (identical(type, "concept_dictionary") && "concept_id" %in% names(columns)) columns$concept_id$role <- "concept"
  if (identical(component, "covariateRef") && "conceptId" %in% names(columns)) columns$conceptId$role <- "concept"
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
  as.character(openssl::sha256(charToRaw(.conceptEncode(report[c('concepts','generated','keys')]))))
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
  if (is.null(c) || !all(c("covariateId", "covariateName", "analysisId") %in% names(frame))) return(frame)
  ids <- as.character(unlist(c$reference_keys, use.names = TRUE)[as.character(frame$covariateId)])
  shared <- c$resolved
  vocabulary <- as.character(unlist(shared$vocabulary, use.names = FALSE))
  if (!length(vocabulary) && length(ids)) stop("Incomplete shared vocabulary names for sparse reference.", call. = FALSE)
  all_ids <- as.character(unlist(shared$ids, use.names = FALSE))
  # Resolve over the full federation identity set before selecting local rows.
  # Otherwise a missing local concept would change collision suffixes.
  tokens <- .conceptNameToken(vocabulary)
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
