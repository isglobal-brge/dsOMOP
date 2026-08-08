# Module: executable OHDSI QueryLibrary sticky redesign catalog
#
# This catalog never renders or runs the upstream SQL.  It joins the pinned
# upstream audit to a small set of person-bounded primitive mappings that can be
# executed only after a typed Recipe/Plan has produced a protected omop.table.

.omopQueryLibraryStickyCatalog <- function(
    registry_path = system.file(
      "queries", "dp_redesign_registry.json", package = "dsOMOP"
    ),
    audit_path = system.file(
      "queries", "upstream_querylibrary_audit.json", package = "dsOMOP"
    )) {
  if (!nzchar(registry_path) || !file.exists(registry_path) ||
      !nzchar(audit_path) || !file.exists(audit_path)) {
    stop("The installed OHDSI QueryLibrary redesign metadata is unavailable.",
         call. = FALSE)
  }

  registry <- tryCatch(
    jsonlite::fromJSON(registry_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  audit <- tryCatch(
    jsonlite::fromJSON(audit_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  catalog <- if (is.list(registry)) registry$executable_catalog else NULL
  groups <- if (is.list(catalog)) catalog$mapping_groups else NULL
  if (!is.list(registry) || !is.list(audit) || !is.list(catalog) ||
      !is.list(groups) || length(groups) == 0L ||
      !identical(registry$source$commit, audit$commit) ||
      !identical(catalog$source_commit, audit$commit) ||
      !identical(registry$source$audit_manifest_sha256,
                 audit$manifest_sha256) ||
      !identical(catalog$literal_upstream_sql_authorized, FALSE)) {
    stop("The installed OHDSI QueryLibrary redesign metadata is inconsistent.",
         call. = FALSE)
  }

  allowed_statistics <- c(
    "count", "bounded_record_count", "categorical_histogram",
    "numeric_histogram", "bounded_distinct", "bounded_mean", "binary_rate"
  )
  rows <- lapply(groups, function(group) {
    required <- c(
      "id", "statistic", "reducer", "contribution_mode", "upstream_ids"
    )
    if (!is.list(group) || any(!required %in% names(group)) ||
        !is.character(group$id) || length(group$id) != 1L ||
        !nzchar(group$id) ||
        !is.character(group$statistic) || length(group$statistic) != 1L ||
        !group$statistic %in% allowed_statistics ||
        !is.character(group$reducer) || length(group$reducer) != 1L ||
        !nzchar(group$reducer) ||
        !is.character(group$contribution_mode) ||
        length(group$contribution_mode) != 1L ||
        !nzchar(group$contribution_mode)) {
      stop("The installed QueryLibrary mapping group is malformed.",
           call. = FALSE)
    }
    ids <- unlist(group$upstream_ids, use.names = FALSE)
    if (!is.character(ids) || length(ids) == 0L || anyNA(ids) ||
        any(!nzchar(ids)) || anyDuplicated(ids)) {
      stop("The installed QueryLibrary mapping IDs are malformed.",
           call. = FALSE)
    }
    data.frame(
      upstream_id = ids,
      family = rep(group$id, length(ids)),
      statistic = rep(group$statistic, length(ids)),
      reducer = rep(group$reducer, length(ids)),
      contribution_mode = rep(group$contribution_mode, length(ids)),
      record_cap_required = rep(
        identical(group$record_cap_required, TRUE), length(ids)
      ),
      order_by_required = rep(
        identical(group$order_by_required, TRUE), length(ids)
      ),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  if (nrow(result) != as.integer(catalog$mapped_query_count) ||
      anyDuplicated(result$upstream_id)) {
    stop("The installed QueryLibrary executable catalog count is invalid.",
         call. = FALSE)
  }

  audited <- stats::setNames(
    audit$queries,
    vapply(audit$queries, `[[`, character(1L), "upstream_id")
  )
  missing <- setdiff(result$upstream_id, names(audited))
  invalid <- result$upstream_id[vapply(result$upstream_id, function(id) {
    query <- audited[[id]]
    is.null(query) || !identical(query$dp_candidate, TRUE) ||
      query$triage_class %in% c(
        "patient_rows_assignment_only", "unsafe_as_written"
      )
  }, logical(1L))]
  if (length(missing) > 0L || length(invalid) > 0L) {
    stop("The executable catalog references an unaudited or blocked query.",
         call. = FALSE)
  }

  result$title <- vapply(
    result$upstream_id, function(id) audited[[id]]$title, character(1L)
  )
  result$path <- vapply(
    result$upstream_id, function(id) audited[[id]]$path, character(1L)
  )
  result$sha256 <- vapply(
    result$upstream_id, function(id) audited[[id]]$sha256, character(1L)
  )
  result$source_commit <- rep(audit$commit, nrow(result))
  result$source_url <- paste0(
    registry$source$repository, "/blob/", audit$commit, "/",
    audit$query_root, "/", result$path
  )
  result$status <- rep("mapped_to_bounded_sticky_primitive", nrow(result))
  result$literal_sql_authorized <- rep(FALSE, nrow(result))
  result[order(result$upstream_id, method = "radix"), , drop = FALSE]
}

#' List executable sticky redesigns of OHDSI QueryLibrary questions
#'
#' Returns public catalog metadata only. The endpoint does not execute or
#' return any upstream SQL. Each row identifies a pinned upstream question and
#' the person-bounded sticky primitive that may be used on a protected
#' \code{omop.table} prepared through the typed Recipe/Plan path.
#'
#' This catalog is a semantic redesign map, not a general QueryLibrary SQL
#' executor and not a formal-DP certification surface. It exposes no SQL text,
#' seed, epsilon control, reroll control, or \code{formal_dp} mode. Runtime
#' releases use the single sticky privacy contract advertised by
#' \code{omopDpStatusDS()} and remain subject to that contract's documented
#' accounting boundary.
#'
#' @return A data frame of audited redesign mappings.
#' @export
omopQueryLibraryStickyCatalogDS <- function() {
  .omopQueryLibraryStickyCatalog()
}
