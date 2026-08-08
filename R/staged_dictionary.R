# Concept-reference support for SQL-streamed staged outputs.

.declaredOutputConceptIds <- function(handle, output) {
  if (!is.list(output)) return(integer(0))
  type <- tolower(output$type %||% "event_level")
  sets <- list()
  if (identical(type, "event_level")) {
    sets <- c(sets, list(
      output$filters$concept_set$ids %||% output$concept_set
    ))
    features <- output$representation$features %||% list()
    sets <- c(sets, lapply(features, function(feature) feature$concept_set))
  } else if (identical(type, "intervals_long")) {
    sets <- c(sets, unname(output$concept_filter %||% list()))
  } else if (type %in% c("temporal_covariates", "person_period")) {
    sets <- c(sets, list(output$concept_set))
  } else if (identical(type, "survival")) {
    outcomes <- output$outcomes
    if (is.null(outcomes)) outcomes <- list(output$outcome)
    sets <- c(sets, lapply(outcomes, function(outcome) outcome$concept_set))
  }
  ids <- unlist(lapply(sets, function(set) {
    if (is.null(set)) return(integer(0))
    .resolveConceptSet(handle, set)
  }), use.names = FALSE)
  sort(unique(as.integer(ids[!is.na(ids)])))
}
.buildDeclaredConceptDictionary <- function(handle, outputs,
                                             source_outputs = NULL) {
  if (is.null(source_outputs)) {
    source_outputs <- names(outputs)[vapply(outputs, function(output) {
      !identical(tolower(output$type %||% "event_level"), "concept_dictionary")
    }, logical(1))]
  }
  if (!is.character(source_outputs) || anyNA(source_outputs) ||
      any(!nzchar(source_outputs)) || anyDuplicated(source_outputs)) {
    stop("concept_dictionary source_outputs must be unique output names.",
         call. = FALSE)
  }
  missing <- setdiff(source_outputs, names(outputs))
  if (length(missing) > 0L) {
    stop("concept_dictionary references unknown source output(s): ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  synthetic <- stats::setNames(lapply(source_outputs, function(name) {
    data.frame(
      concept_id = .declaredOutputConceptIds(handle, outputs[[name]])
    )
  }), source_outputs)
  .buildConceptDictionary(
    handle, results = synthetic, source_outputs = source_outputs
  )
}
