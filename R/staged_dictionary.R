# Concept dictionary identities from declared and observed source outputs.

.declaredOutputConceptIds <- function(handle, output, expand = TRUE) {
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
    if (!expand) return(if (is.list(set) && !is.null(names(set)))
      unlist(set$concepts %||% set$ids, use.names = FALSE) else unlist(set, use.names = FALSE))
    .resolveConceptSet(handle, set)
  }), use.names = FALSE)
  sort(unique(as.integer(ids[!is.na(ids)])))
}
.dictionaryContractIds <- function(contract, ids) {
  if(is.null(contract$kind))return(as.character(ids))
  if(!identical(contract$kind,"category") || !.categoryConceptProducer(contract$category))return(character())
  aliases <- .categoryNullableStrings(contract$resolved$aliases)
  effective <- unname(aliases[as.character(ids)])
  sub("^concept:","",effective[!is.na(effective) & grepl("^concept:",effective)])
}

.dictionaryObservedIds <- function(handle, value, output, root, component=root) {
  ids <- character()
  append_ids <- function(x) {
    if(is.numeric(x) && !inherits(x,"integer64")) {
      if(any(!is.na(x) & (!is.finite(x) | x<0 | x>2147483647 | x!=trunc(x))))
        stop("Concept dictionary requires valid OMOP concept identities.",call.=FALSE)
      x <- as.integer(x)
    }
    x <- as.character(x[!is.na(x)])
    if(length(x) && any(!grepl("^[0-9]+$",x)))stop("Concept dictionary requires original concept identities.",call.=FALSE)
    ids <<- union(ids,x)
  }
  read_frame <- function(frame,skip=character()) {
    columns <- .conceptLegacyColumns(list(handle=handle,ir=list(outputs=setNames(list(output),root))),frame,root,component)
    # Only columns this output classifies as concepts: a count or other reducer
    # alias named *_concept_id holds numbers, not concept identities.
    known <- union(attr(frame,"omop_concept_cols"),names(columns)[vapply(columns,.conceptColumn,logical(1))])
    if("conceptId" %in% names(frame))known <- union(known,"conceptId")
    contracts <- attr(frame,"omop_concepts") %||% list()
    for(col in setdiff(union(known,names(contracts)),skip)) {
      contract <- contracts[[col]]
      append_ids(if(is.null(contract))frame[[col]] else .dictionaryContractIds(contract,contract$ids))
    }
  }
  if(inherits(value,"FlowerDatasetDescriptor")) {
    capsule <- if(!is.null(value$metadata$concept_presentation)) .ds_arg(value$metadata$concept_presentation)
    contracts <- capsule$contracts %||% list()
    for(col in names(contracts))append_ids(.dictionaryContractIds(contracts[[col]],names(capsule$support[[col]])))
    physical <- as.character(unlist(capsule$names[names(contracts)],use.names=FALSE))
    .publishReadChunks(value,function(frame)read_frame(frame,skip=physical))
  } else if(is.data.frame(value))read_frame(value)
  else if(inherits(value,"omop_temporal_sql_components")) {
    for(name in names(value$components)) {
      item <- value$components[[name]]
      if(identical(item$kind,"data"))append_ids(.dictionaryObservedIds(handle,item$data,output,root,name))
      else {
        fields <- intersect(item$columns,c("conceptId",grep("(^|_)concept_id$",item$columns,value=TRUE)))
        if(length(fields)) {
          frame <- .executeQuery(handle,paste0("SELECT DISTINCT ",paste(fields,collapse=", ")," FROM (",item$sql,") dsomop_dictionary_concepts"))
          append_ids(.dictionaryObservedIds(handle,frame,output,root,name))
        }
      }
    }
  } else if(is.list(value))for(name in names(value))append_ids(.dictionaryObservedIds(handle,value[[name]],output,root,name))
  ids
}

.buildDeclaredConceptDictionary <- function(handle, outputs,
                                             source_outputs = NULL, results = list()) {
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
  # Strict publication (the default) releases every source or nothing, so the
  # values a published source carries are references the dictionary may name.
  # A server that publishes partially could refuse a source and still publish
  # its dictionary: there the dictionary names declared concepts only.
  observed <- isTRUE(.omopDisclosureSettings()$query_strict)
  synthetic <- stats::setNames(lapply(source_outputs, function(name) {
    data.frame(
      concept_id = as.integer(union(.declaredOutputConceptIds(handle, outputs[[name]]),
        if (observed) .dictionaryObservedIds(handle,results[[name]],outputs[[name]],name)))
    )
  }), source_outputs)
  result <- .buildConceptDictionary(
    handle, results = synthetic, source_outputs = source_outputs
  )
  # Concept-set expansion reads only the public vocabulary tables, so expanded
  # identities are declared like the literal ones.
  attr(result, "omop_dictionary_declared") <- as.character(unique(unlist(lapply(
    outputs[source_outputs], function(output) .declaredOutputConceptIds(handle, output, expand = TRUE)),
    use.names = FALSE)))
  result
}
