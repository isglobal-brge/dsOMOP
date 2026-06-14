# Module: Vocabulary Queries
# OMOP CDM vocabulary search, concept lookup, and concept set expansion.

#' Search concepts by name pattern
#'
#' Reference-data reader over the \code{concept} table. Beyond the original
#' free-text name match, this also accepts an exact \code{concept_id} list and
#' the standard validity facets (\code{invalid_reason}, \code{valid}) so callers
#' can reproduce the Athena/ATLAS concept search filters. All filters are
#' optional and ANDed together; at least one of \code{pattern} or
#' \code{concept_id} should be supplied to keep the result bounded.
#'
#' @param handle CDM handle
#' @param pattern Character; search pattern (NULL to skip name matching)
#' @param domain Character; filter by domain
#' @param vocabulary Character; filter by vocabulary
#' @param standard_only Logical; only standard concepts (legacy convenience)
#' @param limit Integer; max results
#' @param concept_id Numeric vector; restrict to these exact concept IDs
#' @param standard Character; explicit \code{standard_concept} value to match
#'   (e.g. "S", "C"); overrides \code{standard_only} when supplied
#' @param valid Logical; \code{TRUE} keeps only currently-valid concepts
#'   (\code{invalid_reason IS NULL}), \code{FALSE} keeps only invalidated ones
#' @return Data frame with concept results
#' @keywords internal
.vocabSearchConcepts <- function(handle, pattern, domain = NULL,
                                  vocabulary = NULL, standard_only = TRUE,
                                  limit = 50, concept_id = NULL,
                                  standard = NULL, valid = NULL) {
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
      "standard_concept", "concept_class_id", "concept_code",
      "valid_start_date", "valid_end_date", "invalid_reason"),
    avail_cols
  )
  select_str <- paste(select_cols, collapse = ", ")

  # Use TOP for OHDSI SQL limit convention
  effective_limit <- min(as.integer(limit), 500L)
  sql <- paste0("SELECT TOP ", effective_limit, " ", select_str,
                " FROM ", concept_table)

  where <- character(0)

  if (!is.null(pattern) && nzchar(pattern)) {
    where <- c(where, paste0(
      "LOWER(concept_name) LIKE LOWER(",
      .quoteLiteral(paste0("%", pattern, "%")), ")"
    ))
  }

  concept_id <- as.integer(concept_id)
  concept_id <- concept_id[!is.na(concept_id)]
  if (length(concept_id) > 0) {
    where <- c(where, paste0("concept_id IN (", .sqlIdList(concept_id), ")"))
  }

  if ("standard_concept" %in% avail_cols) {
    if (!is.null(standard)) {
      where <- c(where, paste0("standard_concept = ", .quoteLiteral(standard)))
    } else if (standard_only) {
      where <- c(where, "standard_concept = 'S'")
    }
  }

  if (!is.null(valid) && "invalid_reason" %in% avail_cols) {
    where <- c(where, if (isTRUE(valid)) {
      "invalid_reason IS NULL"
    } else {
      "invalid_reason IS NOT NULL"
    })
  }

  if (!is.null(domain) && "domain_id" %in% avail_cols) {
    where <- c(where, paste0("LOWER(domain_id) = LOWER(", .quoteLiteral(domain), ")"))
  }

  if (!is.null(vocabulary) && "vocabulary_id" %in% avail_cols) {
    where <- c(where, paste0("vocabulary_id = ", .quoteLiteral(vocabulary)))
  }

  if (length(where) > 0) {
    sql <- paste0(sql, " WHERE ", paste(where, collapse = " AND "))
  }

  # Translate to target dialect (TOP -> LIMIT for sqlite/postgresql)
  translated <- .renderSql(handle, sql)
  result <- .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, translated))
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

#' Get the ancestors and descendants of a set of concepts
#'
#' Hierarchy view over \code{concept_ancestor}: returns both directions in one
#' frame (an Athena "concept hierarchy" style answer). Each row carries a
#' \code{direction} of \code{"ancestor"} or \code{"descendant"} relative to the
#' query concepts plus the level columns, so the caller can rebuild the tree.
#' The query concepts themselves are excluded (\code{include_self = FALSE}) to
#' avoid duplicating them on both sides.
#'
#' @param handle CDM handle
#' @param concept_ids Numeric vector of concept IDs
#' @return Data frame with \code{direction}, concept columns, and
#'   \code{min_levels_of_separation} / \code{max_levels_of_separation}
#' @keywords internal
.vocabConceptAncestors <- function(handle, concept_ids) {
  concept_ids <- as.integer(concept_ids)
  concept_ids <- concept_ids[!is.na(concept_ids)]

  empty <- data.frame(direction = character(0), concept_id = integer(0),
                      concept_name = character(0), domain_id = character(0),
                      vocabulary_id = character(0),
                      min_levels_of_separation = integer(0),
                      stringsAsFactors = FALSE)
  if (length(concept_ids) == 0) return(empty)

  bp <- .buildBlueprint(handle)
  if (!"concept_ancestor" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    stop("concept_ancestor table not found. Hierarchy lookup not available.",
         call. = FALSE)
  }

  ca_table <- .qualifyTable(handle, "concept_ancestor",
                            .resolveTableSchema(handle, "concept_ancestor", "Vocabulary"))
  c_table <- .qualifyTable(handle, "concept",
                           .resolveTableSchema(handle, "concept", "Vocabulary"))
  ids <- .sqlIdList(concept_ids)

  sql <- paste0(
    "SELECT 'ancestor' AS direction, c.concept_id, c.concept_name, ",
    "c.domain_id, c.vocabulary_id, ",
    "ca.min_levels_of_separation, ca.max_levels_of_separation",
    " FROM ", ca_table, " AS ca",
    " INNER JOIN ", c_table, " AS c ON ca.ancestor_concept_id = c.concept_id",
    " WHERE ca.descendant_concept_id IN (", ids, ")",
    " AND ca.min_levels_of_separation >= 1",
    " UNION ALL ",
    "SELECT 'descendant' AS direction, c.concept_id, c.concept_name, ",
    "c.domain_id, c.vocabulary_id, ",
    "ca.min_levels_of_separation, ca.max_levels_of_separation",
    " FROM ", ca_table, " AS ca",
    " INNER JOIN ", c_table, " AS c ON ca.descendant_concept_id = c.concept_id",
    " WHERE ca.ancestor_concept_id IN (", ids, ")",
    " AND ca.min_levels_of_separation >= 1"
  )

  .executeQuery(handle, sql)
}

#' Get synonyms for a set of concepts
#'
#' Reference-data reader over \code{concept_synonym}, returning each concept's
#' alternative names (and language where present), mirroring the Athena concept
#' "synonyms" panel.
#'
#' @param handle CDM handle
#' @param concept_ids Numeric vector of concept IDs
#' @return Data frame with \code{concept_id}, \code{concept_synonym_name}, and
#'   \code{language_concept_id} when available
#' @keywords internal
.vocabGetSynonyms <- function(handle, concept_ids) {
  concept_ids <- as.integer(concept_ids)
  concept_ids <- concept_ids[!is.na(concept_ids)]

  empty <- data.frame(concept_id = integer(0),
                      concept_synonym_name = character(0),
                      stringsAsFactors = FALSE)
  if (length(concept_ids) == 0) return(empty)

  bp <- .buildBlueprint(handle)
  if (!"concept_synonym" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    stop("concept_synonym table not found. Synonym lookup not available.",
         call. = FALSE)
  }

  cs_table <- .qualifyTable(handle, "concept_synonym",
                            .resolveTableSchema(handle, "concept_synonym", "Vocabulary"))
  avail_cols <- bp$columns[["concept_synonym"]]$column_name
  select_cols <- intersect(
    c("concept_id", "concept_synonym_name", "language_concept_id"),
    avail_cols
  )
  ids <- .sqlIdList(concept_ids)

  sql <- paste0(
    "SELECT ", paste(select_cols, collapse = ", "),
    " FROM ", cs_table,
    " WHERE concept_id IN (", ids, ")"
  )

  .executeQuery(handle, sql)
}

#' Get concept relationships for a set of concepts
#'
#' Full \code{concept_relationship} reader: returns every relationship in
#' \strong{both} directions (the query concept as \code{concept_id_1} or as
#' \code{concept_id_2}), not just "Maps to". The neighbour concept name is
#' joined in for readability and a \code{direction} column marks whether the row
#' is outgoing (\code{"forward"}) or incoming (\code{"reverse"}) relative to the
#' query set. An optional \code{relationship_id} narrows to one relationship.
#'
#' @param handle CDM handle
#' @param concept_ids Numeric vector of concept IDs
#' @param relationship_id Character; optional single relationship_id filter
#'   (e.g. "Maps to", "Subsumes", "Is a")
#' @return Data frame with the relationship rows and the related concept name
#' @keywords internal
.vocabGetRelationships <- function(handle, concept_ids, relationship_id = NULL) {
  concept_ids <- as.integer(concept_ids)
  concept_ids <- concept_ids[!is.na(concept_ids)]

  empty <- data.frame(direction = character(0), concept_id = integer(0),
                      relationship_id = character(0),
                      related_concept_id = integer(0),
                      related_concept_name = character(0),
                      stringsAsFactors = FALSE)
  if (length(concept_ids) == 0) return(empty)

  bp <- .buildBlueprint(handle)
  if (!"concept_relationship" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    stop("concept_relationship table not found. Relationship lookup not available.",
         call. = FALSE)
  }

  cr_table <- .qualifyTable(handle, "concept_relationship",
                            .resolveTableSchema(handle, "concept_relationship", "Vocabulary"))
  c_table <- .qualifyTable(handle, "concept",
                           .resolveTableSchema(handle, "concept", "Vocabulary"))
  ids <- .sqlIdList(concept_ids)

  rel_clause <- ""
  if (!is.null(relationship_id) && nzchar(relationship_id)) {
    rel_clause <- paste0(" AND cr.relationship_id = ",
                         .quoteLiteral(relationship_id))
  }

  sql <- paste0(
    "SELECT 'forward' AS direction, cr.concept_id_1 AS concept_id, ",
    "cr.relationship_id, cr.concept_id_2 AS related_concept_id, ",
    "c.concept_name AS related_concept_name, cr.invalid_reason",
    " FROM ", cr_table, " AS cr",
    " INNER JOIN ", c_table, " AS c ON cr.concept_id_2 = c.concept_id",
    " WHERE cr.concept_id_1 IN (", ids, ")", rel_clause,
    " UNION ALL ",
    "SELECT 'reverse' AS direction, cr.concept_id_2 AS concept_id, ",
    "cr.relationship_id, cr.concept_id_1 AS related_concept_id, ",
    "c.concept_name AS related_concept_name, cr.invalid_reason",
    " FROM ", cr_table, " AS cr",
    " INNER JOIN ", c_table, " AS c ON cr.concept_id_1 = c.concept_id",
    " WHERE cr.concept_id_2 IN (", ids, ")", rel_clause
  )

  .executeQuery(handle, sql)
}

#' List concepts with server-side pagination
#'
#' Browse the \code{concept} catalog filtered by the standard facets and paged
#' with OFFSET/LIMIT, mirroring Athena's paged concept list. This lifts the
#' 500-row cap of \code{\link{.vocabSearchConcepts}} by letting the caller walk
#' the result in pages. Returns a list with the current \code{rows} page and the
#' \code{total_count} of matching concepts (one extra COUNT query) so the client
#' can render pagination controls.
#'
#' \code{LIMIT ... OFFSET ...} is portable across the realistically-targeted
#' analytic dialects (PostgreSQL, SQLite/DuckDB, MySQL, Spark, BigQuery,
#' Snowflake, Redshift); it is rendered directly rather than via the TOP
#' convention, which has no offset notion. The page size is still capped to keep
#' a single response bounded.
#'
#' @param handle CDM handle
#' @param domain Character; filter by domain_id
#' @param vocabulary Character; filter by vocabulary_id
#' @param concept_class Character; filter by concept_class_id
#' @param standard Character; filter by standard_concept value (e.g. "S")
#' @param valid Logical; TRUE = only valid (invalid_reason IS NULL)
#' @param offset Integer; number of rows to skip
#' @param limit Integer; page size (capped at 1000)
#' @param order Character; column to order by (must be a selectable column)
#' @return List with \code{rows} (data frame), \code{total_count},
#'   \code{offset}, and \code{limit}
#' @keywords internal
.vocabListConcepts <- function(handle, domain = NULL, vocabulary = NULL,
                                concept_class = NULL, standard = NULL,
                                valid = NULL, offset = 0L, limit = 100L,
                                order = "concept_id") {
  bp <- .buildBlueprint(handle)
  if (!"concept" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    stop("Concept table not found in database.", call. = FALSE)
  }

  concept_table <- .qualifyTable(handle, "concept",
                                 .resolveTableSchema(handle, "concept", "Vocabulary"))
  avail_cols <- bp$columns[["concept"]]$column_name

  select_cols <- intersect(
    c("concept_id", "concept_name", "domain_id", "vocabulary_id",
      "concept_class_id", "standard_concept", "concept_code",
      "valid_start_date", "valid_end_date", "invalid_reason"),
    avail_cols
  )

  where <- character(0)
  if (!is.null(domain) && "domain_id" %in% avail_cols) {
    where <- c(where, paste0("LOWER(domain_id) = LOWER(", .quoteLiteral(domain), ")"))
  }
  if (!is.null(vocabulary) && "vocabulary_id" %in% avail_cols) {
    where <- c(where, paste0("vocabulary_id = ", .quoteLiteral(vocabulary)))
  }
  if (!is.null(concept_class) && "concept_class_id" %in% avail_cols) {
    where <- c(where, paste0("concept_class_id = ", .quoteLiteral(concept_class)))
  }
  if (!is.null(standard) && "standard_concept" %in% avail_cols) {
    where <- c(where, paste0("standard_concept = ", .quoteLiteral(standard)))
  }
  if (!is.null(valid) && "invalid_reason" %in% avail_cols) {
    where <- c(where, if (isTRUE(valid)) "invalid_reason IS NULL"
                      else "invalid_reason IS NOT NULL")
  }
  where_clause <- if (length(where) > 0) {
    paste0(" WHERE ", paste(where, collapse = " AND "))
  } else ""

  # total_count for pagination controls (matching filters, not the page).
  count_sql <- paste0("SELECT COUNT(*) AS n FROM ", concept_table, where_clause)
  total_count <- tryCatch(
    as.numeric(.executeQuery(handle, count_sql)$n[1]),
    error = function(e) NA_real_
  )

  # Order must be a real selectable column; default and fall back to concept_id
  # to avoid ORDER BY on an unknown identifier.
  order_col <- if (!is.null(order) && order %in% select_cols) order else "concept_id"

  offset <- max(0L, as.integer(offset))
  limit <- min(max(1L, as.integer(limit)), 1000L)

  page_sql <- paste0(
    "SELECT ", paste(select_cols, collapse = ", "),
    " FROM ", concept_table, where_clause,
    " ORDER BY ", order_col,
    " LIMIT ", limit, " OFFSET ", offset
  )
  rows <- .executeQuery(handle, page_sql)

  list(rows = rows, total_count = total_count,
       offset = offset, limit = limit)
}

#' Distinct values from a vocabulary metadata table
#'
#' Shared helper for the \code{vocabulary} / \code{domain} / \code{concept_class}
#' listers. Prefers the dedicated vocabulary table when present (returning its
#' full descriptive rows); otherwise falls back to \code{SELECT DISTINCT} on the
#' corresponding \code{concept} column so the lister still works on a minimal
#' vocabulary load.
#'
#' @param handle CDM handle
#' @param table Character; dedicated table name (e.g. "vocabulary")
#' @param fallback_col Character; concept column to distinct on if absent
#' @return Data frame
#' @keywords internal
.vocabDistinctMeta <- function(handle, table, fallback_col) {
  bp <- .buildBlueprint(handle)
  present <- bp$tables$table_name[bp$tables$present_in_db]

  if (table %in% present) {
    qualified <- .qualifyTable(handle, table,
                               .resolveTableSchema(handle, table, "Vocabulary"))
    return(.executeQuery(handle, paste0("SELECT * FROM ", qualified)))
  }

  if (!"concept" %in% present) {
    stop("Neither ", table, " nor concept table found in database.", call. = FALSE)
  }
  if (!fallback_col %in% bp$columns[["concept"]]$column_name) {
    stop(table, " table not found and concept.", fallback_col,
         " unavailable.", call. = FALSE)
  }

  concept_table <- .qualifyTable(handle, "concept",
                                 .resolveTableSchema(handle, "concept", "Vocabulary"))
  sql <- paste0("SELECT DISTINCT ", fallback_col, " FROM ", concept_table,
                " WHERE ", fallback_col, " IS NOT NULL ORDER BY ", fallback_col)
  .executeQuery(handle, sql)
}

#' Return the full cdm_source row(s)
#'
#' @param handle CDM handle
#' @return Data frame of cdm_source rows (empty frame if table absent)
#' @keywords internal
.vocabCdmSource <- function(handle) {
  bp <- .buildBlueprint(handle)
  if (!"cdm_source" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    return(data.frame())
  }
  qualified <- .qualifyTable(handle, "cdm_source",
                             .resolveTableSchema(handle, "cdm_source", "CDM"))
  .executeQuery(handle, paste0("SELECT * FROM ", qualified))
}

#' Resolve the CDM version
#'
#' Returns the authoritative \code{cdm_source.cdm_version} when available (via
#' the blueprint's \code{\link{.detectCDMInfo}} result), otherwise the version
#' inferred from the table structure (\code{spec_version}). Mirrors the
#' precedence used when building the blueprint.
#'
#' @param handle CDM handle
#' @return List with \code{cdm_version}, \code{source} ("cdm_source",
#'   "structure", or "unknown"), and the available \code{vocabulary_version}
#' @keywords internal
.vocabCdmVersion <- function(handle) {
  bp <- .buildBlueprint(handle)
  cdm_info <- bp$cdm_info

  if (!is.null(cdm_info) && !is.null(cdm_info$cdm_version)) {
    return(list(cdm_version = cdm_info$cdm_version,
                source = "cdm_source",
                vocabulary_version = cdm_info$vocabulary_version %||% NA_character_))
  }
  if (!is.null(bp$spec_version)) {
    return(list(cdm_version = bp$spec_version,
                source = "structure",
                vocabulary_version = NA_character_))
  }
  list(cdm_version = NA_character_, source = "unknown",
       vocabulary_version = NA_character_)
}
