# Module: Vocabulary Queries
# OMOP CDM vocabulary search, concept lookup, and concept set expansion.

#' Search concepts by name pattern
#'
#' @param handle CDM handle
#' @param pattern Character; search pattern
#' @param domain Character; filter by domain
#' @param vocabulary Character; filter by vocabulary
#' @param standard_only Logical; only standard concepts
#' @param limit Integer; max results
#' @return Data frame with concept results
#' @keywords internal
.vocabSearchConcepts <- function(handle, pattern, domain = NULL,
                                  vocabulary = NULL, standard_only = TRUE,
                                  limit = 50) {
  bp <- .buildBlueprint(handle)

  if (!"concept" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    stop("Concept table not found in database.", call. = FALSE)
  }

  schema <- .resolveTableSchema(handle, "concept", "Vocabulary")
  concept_table <- .qualifyTable(handle, "concept", schema)

  cols <- bp$columns[["concept"]]
  avail_cols <- cols$column_name

  select_cols <- intersect(
    c("concept_id", "concept_name", "domain_id", "vocabulary_id",
      "standard_concept", "concept_class_id"),
    avail_cols
  )
  select_str <- paste(select_cols, collapse = ", ")

  # Use TOP for OHDSI SQL limit convention
  effective_limit <- min(as.integer(limit), 500L)
  sql <- paste0("SELECT TOP ", effective_limit, " ", select_str,
                " FROM ", concept_table)

  where <- character(0)
  where <- c(where, paste0(
    "LOWER(concept_name) LIKE LOWER(",
    .quoteLiteral(paste0("%", pattern, "%")), ")"
  ))

  if (standard_only && "standard_concept" %in% avail_cols) {
    where <- c(where, "standard_concept = 'S'")
  }

  if (!is.null(domain) && "domain_id" %in% avail_cols) {
    where <- c(where, paste0("LOWER(domain_id) = LOWER(", .quoteLiteral(domain), ")"))
  }

  if (!is.null(vocabulary) && "vocabulary_id" %in% avail_cols) {
    where <- c(where, paste0("vocabulary_id = ", .quoteLiteral(vocabulary)))
  }

  sql <- paste0(sql, " WHERE ", paste(where, collapse = " AND "))

  # Translate to target dialect (TOP -> LIMIT for sqlite/postgresql)
  translated <- .renderSql(handle, sql)
  result <- DBI::dbGetQuery(handle$conn, translated)
  names(result) <- tolower(names(result))
  .coerce_integer64(result)
}

#' Lookup concepts by ID
#'
#' @param handle CDM handle
#' @param concept_ids Numeric vector of concept IDs
#' @return Data frame with concept details
#' @keywords internal
.vocabLookupConcepts <- function(handle, concept_ids) {
  if (is.null(concept_ids) || length(concept_ids) == 0) {
    return(data.frame(concept_id = integer(0), concept_name = character(0),
                      stringsAsFactors = FALSE))
  }

  bp <- .buildBlueprint(handle)

  schema <- .resolveTableSchema(handle, "concept", "Vocabulary")
  concept_table <- .qualifyTable(handle, "concept", schema)
  ids <- paste(as.integer(concept_ids), collapse = ", ")

  sql <- paste0(
    "SELECT concept_id, concept_name, domain_id, vocabulary_id, standard_concept",
    " FROM ", concept_table,
    " WHERE concept_id IN (", ids, ")"
  )

  .executeQuery(handle, sql)
}

#' Get descendant concepts via concept_ancestor table
#'
#' @param handle CDM handle
#' @param ancestor_ids Numeric vector of ancestor concept IDs
#' @param include_self Logical; include the ancestors themselves
#' @return Data frame with descendant concept details
#' @keywords internal
.vocabGetDescendants <- function(handle, ancestor_ids, include_self = TRUE) {
  bp <- .buildBlueprint(handle)

  if (!"concept_ancestor" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    stop("concept_ancestor table not found. Descendant lookup not available.",
         call. = FALSE)
  }

  schema <- .resolveTableSchema(handle, "concept_ancestor", "Vocabulary")
  ca_table <- .qualifyTable(handle, "concept_ancestor", schema)
  c_table <- .qualifyTable(handle, "concept",
                           .resolveTableSchema(handle, "concept", "Vocabulary"))

  ids <- paste(as.integer(ancestor_ids), collapse = ", ")

  min_sep <- if (include_self) 0L else 1L

  sql <- paste0(
    "SELECT c.concept_id, c.concept_name, c.domain_id, c.vocabulary_id, ",
    "ca.min_levels_of_separation",
    " FROM ", ca_table, " AS ca",
    " INNER JOIN ", c_table, " AS c",
    " ON ca.descendant_concept_id = c.concept_id",
    " WHERE ca.ancestor_concept_id IN (", ids, ")",
    " AND ca.min_levels_of_separation >= ", min_sep
  )

  .executeQuery(handle, sql)
}

#' Get ancestor concepts via concept_ancestor table
#'
#' @param handle CDM handle
#' @param descendant_ids Numeric vector of descendant concept IDs
#' @param include_self Logical; include the descendants themselves
#' @return Data frame with ancestor concept details
#' @keywords internal
.vocabGetAncestors <- function(handle, descendant_ids, include_self = TRUE) {
  bp <- .buildBlueprint(handle)

  if (!"concept_ancestor" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    stop("concept_ancestor table not found.", call. = FALSE)
  }

  schema <- .resolveTableSchema(handle, "concept_ancestor", "Vocabulary")
  ca_table <- .qualifyTable(handle, "concept_ancestor", schema)
  c_table <- .qualifyTable(handle, "concept",
                           .resolveTableSchema(handle, "concept", "Vocabulary"))

  ids <- paste(as.integer(descendant_ids), collapse = ", ")
  min_sep <- if (include_self) 0L else 1L

  sql <- paste0(
    "SELECT c.concept_id, c.concept_name, c.domain_id, c.vocabulary_id, ",
    "ca.min_levels_of_separation",
    " FROM ", ca_table, " AS ca",
    " INNER JOIN ", c_table, " AS c",
    " ON ca.ancestor_concept_id = c.concept_id",
    " WHERE ca.descendant_concept_id IN (", ids, ")",
    " AND ca.min_levels_of_separation >= ", min_sep
  )

  .executeQuery(handle, sql)
}

#' Get non-standard source concepts that map to a set of concepts
#'
#' Reverse "Maps to" lookup: returns the source concepts (\code{concept_id_1})
#' whose mapping target (\code{concept_id_2}) lies in \code{target_ids}. This
#' lets a standard concept set also match records stored with source
#' vocabularies (e.g. ICD9CM/ICD10CM), mirroring the ATLAS "source codes" view.
#'
#' @param handle CDM handle
#' @param target_ids Numeric vector of (typically standard) concept IDs
#' @return Integer vector of source concept IDs; empty if none or no table
#' @keywords internal
.vocabGetMappedConcepts <- function(handle, target_ids) {
  target_ids <- as.integer(target_ids)
  if (length(target_ids) == 0) return(integer(0))

  bp <- .buildBlueprint(handle)
  if (!"concept_relationship" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    return(integer(0))
  }

  schema <- .resolveTableSchema(handle, "concept_relationship", "Vocabulary")
  cr_table <- .qualifyTable(handle, "concept_relationship", schema)
  ids <- paste(target_ids, collapse = ", ")

  sql <- paste0(
    "SELECT DISTINCT cr.concept_id_1 AS concept_id",
    " FROM ", cr_table, " AS cr",
    " WHERE cr.concept_id_2 IN (", ids, ")",
    " AND cr.relationship_id = 'Maps to'",
    " AND cr.invalid_reason IS NULL"
  )

  df <- .executeQuery(handle, sql)
  if (nrow(df) == 0) return(integer(0))
  as.integer(df$concept_id)
}

#' Expand a concept set specification to a full list of concept IDs
#'
#' Resolves a spec into a flat vector by, in order: starting from
#' \code{concepts}; optionally adding hierarchical descendants
#' (\code{include_descendants}); optionally adding mapped source concepts
#' (\code{include_mapped}); then removing \code{exclude}. The broadening steps
#' only grow the matched population; the resulting cohort is still size-checked.
#'
#' @param handle CDM handle
#' @param concept_set List with \code{concepts}, \code{include_descendants},
#'   \code{include_mapped}, \code{exclude}
#' @return Integer vector of expanded concept IDs
#' @keywords internal
.vocabExpandConceptSet <- function(handle, concept_set) {
  base_ids <- as.integer(concept_set$concepts %||% integer(0))
  include_descendants <- concept_set$include_descendants %||% FALSE
  include_mapped <- concept_set$include_mapped %||% FALSE
  exclude_ids <- as.integer(concept_set$exclude %||% integer(0))

  result_ids <- base_ids

  if (include_descendants && length(base_ids) > 0) {
    tryCatch({
      descendants <- .vocabGetDescendants(handle, base_ids, include_self = TRUE)
      if (nrow(descendants) > 0) {
        result_ids <- unique(c(result_ids, descendants$concept_id))
      }
    }, error = function(e) {
      warning("Descendant expansion failed: ", e$message,
              ". Using base concept IDs only.")
    })
  }

  if (include_mapped && length(result_ids) > 0) {
    tryCatch({
      mapped_ids <- .vocabGetMappedConcepts(handle, result_ids)
      if (length(mapped_ids) > 0) {
        result_ids <- unique(c(result_ids, mapped_ids))
      }
    }, error = function(e) {
      warning("Mapped-concept expansion failed: ", e$message,
              ". Using unmapped concept IDs.")
    })
  }

  if (length(exclude_ids) > 0) {
    result_ids <- setdiff(result_ids, exclude_ids)
  }

  as.integer(result_ids)
}

#' Resolve a concept_set field into a flat vector of concept IDs
#'
#' Accepts either a concept-set spec (a list carrying \code{concepts}, expanded
#' via \code{\link{.vocabExpandConceptSet}}) or an already-flat vector/list of
#' IDs. Lets cohort and plan code share one concept_set entry style.
#'
#' @param handle CDM handle
#' @param x Concept-set spec list or flat vector/list of concept IDs
#' @return Integer vector of concept IDs (deduplicated, NA-free)
#' @keywords internal
.resolveConceptSet <- function(handle, x) {
  if (is.null(x)) return(integer(0))
  if (is.list(x) && !is.null(x$concepts)) {
    return(.vocabExpandConceptSet(handle, x))
  }
  v <- suppressWarnings(as.integer(unlist(x, use.names = FALSE)))
  unique(v[!is.na(v)])
}

#' Translate concept_id columns in a data frame to concept names
#'
#' @param handle CDM handle
#' @param df Data frame
#' @return Data frame with concept IDs replaced by names
#' @keywords internal
.vocabTranslateColumns <- function(handle, df) {
  concept_cols <- grep("_concept_id$", names(df), value = TRUE)
  if (length(concept_cols) == 0) return(df)

  all_ids <- unique(unlist(lapply(concept_cols, function(col) {
    vals <- df[[col]]
    vals[!is.na(vals)]
  })))

  if (length(all_ids) == 0) return(df)

  concepts <- tryCatch(
    .vocabLookupConcepts(handle, all_ids),
    error = function(e) data.frame(concept_id = integer(0),
                                    concept_name = character(0),
                                    stringsAsFactors = FALSE)
  )

  if (nrow(concepts) == 0) return(df)

  concept_map <- stats::setNames(concepts$concept_name,
                                  as.character(concepts$concept_id))

  for (col in concept_cols) {
    original <- as.character(df[[col]])
    translated <- concept_map[original]
    translated[is.na(translated) & !is.na(original)] <-
      paste0("concept_", original[is.na(translated) & !is.na(original)])
    # Keep the human-readable concept_name verbatim, so the VALUE shown here
    # matches the catalog (value.counts / concept.prevalence / concept.summary)
    # exactly. Do NOT standardize the value: .standardizeName is for turning a
    # concept into a syntactically-valid COLUMN NAME, which .toWide/.toFeatures
    # apply independently at the naming step (and idempotently), so wide/feature
    # headers stay clean regardless.
    df[[col]] <- unname(translated)
  }

  df
}
