# Module: OHDSI TreatmentPatterns catalog entries (native live compute)
#
# Group OHDSI-B2-treatmentpatterns-new. Three NEW canonical analyses that the
# OHDSI TreatmentPatterns package would otherwise produce, re-implemented as
# native, live-computing catalog entries. None of these is represented by any
# .ohdsi_tool_registry result table (TreatmentPatterns writes no result table the
# precomputed adapter ever read), so there is no legacy id to alias: the three
# canonical ids below are introduced fresh and COMPUTE their metric LIVE from the
# CDM over the SCOPED cohort. Nothing here reads a precomputed results table.
#
#   * dsomop:txpath.pathways           (Stage-2 showcase: treatment sequences)
#   * dsomop:txpath.percentage_treated (Stage-3: % treated per drug per layer)
#   * dsomop:txpath.duration_eras      (Stage-3: treatment-era duration dist)
#
# The scoped cohort IS the treatment population: each member's qualifying drug
# events (domain selected by domain_code, default drug) are ordered by start date
# into a treatment sequence. The treatment universe is optionally narrowed to a
# concept set (filter_treatments, descendants expanded server-side). Drug
# concepts are translated to their names by default (LEFT JOIN concept); no
# *_source_value / free-text is ever selected. Every fn self-gates the scoped
# population via .omopDiagAssertPersons BEFORE materialising, returns an
# AGGREGATE-ONLY frame (no person key), and is cohort-relative (returns a
# gate-safe empty frame when un-scoped).
#
# Disclosure (the SINGLE .omopAnalysisGate does all suppression/banding/coupling
# from the declared spec; nothing here pre-gates a FINAL count except the two
# sanctioned exceptions documented below):
#   * pathways: unit="person" with a bespoke NODE-AND-EDGE HIERARCHICAL gate
#     implemented INSIDE the fn (the one analysis whose gate logic exceeds the
#     standard branches). Every prefix node and every edge whose distinct-person
#     count is below nfilter.tab is suppressed; suppressed siblings under the same
#     parent are collapsed into an "Other" node; every surviving node/edge count
#     is banded to a multiple of nfilter_band. The suppression threshold BINDS to
#     the disclosure nfilter (never the TreatmentPatterns package default of 5,
#     never exposed as a param) and the path length is capped at <= 5. The fn
#     returns an already-hierarchically-consistent frame; the gate's universal
#     small-cell + band pass over person_count then re-applies idempotently.
#   * percentage_treated: unit="person"; n_treated (numerator) and cohort_size
#     (denominator) are gated and pct_treated is recomputed from the banded
#     counts (or NA'd) via .omopAnalysisReconcileRatio inside the fn (the ONE
#     sanctioned ratio exception), so a surviving percentage never re-derives a
#     sub-threshold count.
#   * duration_eras: unit="dist"; the gate strips min/max and MASKS the summary
#     statistics on the distinct-PERSON companion (count_value = persons
#     contributing, NOT the era count) below nfilter_dist. Quantiles are emitted
#     as p10/p25/median/p75/p90 only (never native 0%/100% = min/max).
#
# Scope on every entry: accepts_cohort=TRUE, accepts_tables=TRUE, max_tables=1L
# (the target cohort IS the scope), requires_cohort=FALSE (whole-DB /
# database-characterization semantics; the fns return a gate-safe empty frame
# un-scoped). meta$adapter="diagnostic" (these are native canonicals, not
# precomputed-results delegates).
#
# The group registrar .ohdsiPackTreatmentPatternsEntries(handle) returns the
# three entries keyed by their stable ids; it is concatenated at the single OHDSI
# aggregation point (.omopAnalysisOhdsiEntries), exactly as the parallel ported
# OHDSI groups are. handle is taken for signature parity ONLY and is never
# queried at build time (catalog build is cached on handle$analysis_catalog; all
# DB I/O happens later inside each compute$fn).

# --- Shared treatment-event helpers ------------------------------------------

#' Resolve the optional treatment-universe concept-set predicate
#'
#' The treatment universe is optionally narrowed to a concept set
#' (\code{filter_treatments} = concept ids, descendants expanded server-side via
#' \code{\link{.resolveConceptSet}} so an ingredient/ATC node pulls its whole
#' sub-tree). Returns the SQL IN-list predicate fragment (no leading \code{AND})
#' on the given concept column, or \code{""} when no valid id was supplied (the
#' whole domain).
#'
#' @param handle CDM handle.
#' @param concept_col_expr Character; qualified concept column (e.g. "e.x").
#' @param filter_treatments Character/int scalar or NULL (sanitized literal).
#' @return Character; a SQL predicate fragment or "".
#' @keywords internal
.omopTxPathFilterPredicate <- function(handle, concept_col_expr,
                                       filter_treatments) {
  if (is.null(filter_treatments) ||
      !nzchar(as.character(filter_treatments))) {
    return("")
  }
  ids <- .resolveConceptSet(handle,
                            list(concepts = as.integer(filter_treatments),
                                 include_descendants = TRUE))
  if (length(ids) == 0) {
    ids <- suppressWarnings(as.integer(filter_treatments))
    ids <- ids[!is.na(ids)]
  }
  if (length(ids) == 0) return("")
  paste0(concept_col_expr, " IN (", paste(ids, collapse = ", "), ")")
}

#' Pull the per-person ordered treatment sequence into R (dialect-safe)
#'
#' Returns one row per (person, treatment event) for cohort members, ordered by
#' event start date then concept id, with the per-person sequence position
#' (\code{ROW_NUMBER() OVER (PARTITION BY person_id ...)}) and the treatment
#' concept id + translated name. We build the ordered SEQUENCE in R (not via a
#' dialect-divergent \code{STRING_AGG}/\code{GROUP_CONCAT}), so this stays
#' identical on every backend; the caller summarises the rows into aggregate-only
#' node/edge or layer frames. The scoped population is self-gated by the caller
#' BEFORE this pull. Only the standard concept id + name are selected — never a
#' \code{*_source_value} / free-text column.
#'
#' @param handle CDM handle.
#' @param cohort Character; the validated scoped cohort temp-table name.
#' @param src Covariate source list (\code{\link{.omopCovariateSource}}).
#' @param filter_pred Character; the optional treatment-universe predicate or "".
#' @return Data frame (person_id, seq_pos, concept_id, concept_name) or empty.
#' @keywords internal
.omopTxPathSequenceRows <- function(handle, cohort, src, filter_pred) {
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  where <- paste0("e.", src$concept_col, " IS NOT NULL",
                  if (nzchar(filter_pred)) paste0(" AND ", filter_pred) else "")
  rn <- paste0("ROW_NUMBER() OVER (PARTITION BY e.", src$person_col,
               " ORDER BY e.", src$date_col, ", e.", src$concept_col, ")")
  inner <- paste0(
    "SELECT e.", src$person_col, " AS person_id, ", rn, " AS seq_pos, ",
    "e.", src$concept_col, " AS concept_id ",
    "FROM ", src$table, " e ",
    "INNER JOIN ", cohort, " c ON c.subject_id = e.", src$person_col,
    " WHERE ", where)
  sql <- .sql_translate(paste0(
    "SELECT s.person_id, s.seq_pos, s.concept_id, cc.concept_name ",
    "FROM (", inner, ") s",
    " LEFT JOIN ", concept, " cc ON cc.concept_id = s.concept_id",
    " ORDER BY s.person_id, s.seq_pos"),
    handle$target_dialect)
  df <- .executeQuery(handle, sql)
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
  df
}

#' Collapse a person's ordered concept names into a treatment path
#'
#' Applies the TreatmentPatterns era shaping in R: drops consecutive identical
#' treatments (era collapse) and caps the path at \code{max_len} steps
#' (\code{<= 5}). Returns the ordered character vector of treatment-layer labels
#' for one person.
#'
#' @param names_ordered Character; the person's concept names in sequence order.
#' @param max_len Integer; maximum path length (already floored to \code{<= 5}).
#' @return Character vector of treatment labels (length \code{<= max_len}).
#' @keywords internal
.omopTxPathCollapse <- function(names_ordered, max_len) {
  labs <- as.character(names_ordered)
  labs[is.na(labs) | !nzchar(labs)] <- "Unmapped"
  if (length(labs) > 1) {
    keep <- c(TRUE, labs[-1] != labs[-length(labs)])
    labs <- labs[keep]
  }
  if (length(labs) > max_len) labs <- labs[seq_len(max_len)]
  labs
}

# --- pathways: bespoke hierarchical node/edge gate ----------------------------

#' Hierarchically gate per-prefix node + edge person counts (the bespoke gate)
#'
#' The one analysis whose disclosure needs gate logic beyond the standard
#' branches. Given each person's collapsed treatment path, this computes the
#' distinct-person frequency of every PREFIX node (the first k treatments) and
#' every EDGE (a parent prefix -> its next treatment) and enforces a
#' parent-before-child consistent suppression so a released sunburst/sankey can
#' never betray a sub-threshold sub-population:
#' \enumerate{
#'   \item the suppression threshold is the disclosure \code{nfilter.tab} (NEVER
#'     the TreatmentPatterns package default of 5, and never a user param);
#'   \item processing depth-by-depth from the root, every node/edge whose
#'     distinct-person count is below the threshold is removed and its persons
#'     are folded into an \code{"Other"} sibling under the SAME parent, so the
#'     released children never sum past a banded parent and a small branch is
#'     hidden behind one collapsed node rather than dropped (which would leak its
#'     existence by a sum gap);
#'   \item descendants of a path that has already diverged into \code{"Other"}
#'     are not emitted (an \code{"Other"} branch is a leaf);
#'   \item every surviving node and edge count is banded to a multiple of
#'     \code{nfilter_band} (the differencing defence), so the frame is already
#'     band-consistent and the gate's universal band pass re-applies idempotently.
#' }
#' Returns a single long frame (one row per node and per edge) the
#' \code{unit="person"} gate then small-cell-suppresses + bands on
#' \code{person_count} (a no-op on the already-gated values, or a fail-closed drop
#' of an \code{"Other"} node still below \code{nfilter.tab}).
#'
#' @param paths List of character vectors; one collapsed path per person.
#' @param max_len Integer; the path-length cap (\code{<= 5}).
#' @return Data frame (row_type, depth, parent_path, path, treatment,
#'   person_count) — aggregate-only, hierarchically consistent.
#' @keywords internal
.omopTxPathHierarchicalGate <- function(paths, max_len) {
  settings <- .omopDisclosureSettings()
  thr  <- settings$nfilter_tab
  band <- settings$nfilter_band
  sep  <- " > "

  # Persons keyed by their full collapsed path; a fast membership test per node.
  n_persons <- length(paths)
  if (n_persons == 0) return(data.frame())

  rows <- list()
  add_row <- function(row_type, depth, parent_path, path, treatment, cnt) {
    rows[[length(rows) + 1L]] <<- data.frame(
      row_type     = row_type,
      depth        = as.integer(depth),
      parent_path  = parent_path,
      path         = path,
      treatment    = treatment,
      person_count = as.numeric(cnt),
      stringsAsFactors = FALSE)
  }

  # `survivors` maps a surviving parent-prefix string -> the integer indices of
  # the persons still routed through it (those NOT folded into an ancestor's
  # "Other"). The root holds everyone. We descend one depth at a time: within a
  # parent, group its persons by their next treatment, suppress small groups into
  # one "Other" leaf, band the survivors, and recurse into the kept (non-Other)
  # children only.
  survivors <- list(`__root__` = seq_len(n_persons))
  parent_label <- c(`__root__` = "")

  for (depth in seq_len(max_len)) {
    next_survivors <- list()
    next_parent_label <- character(0)
    for (pkey in names(survivors)) {
      idx <- survivors[[pkey]]
      if (length(idx) == 0) next
      plabel <- parent_label[[pkey]]
      # Next treatment for each person at this depth ("" when the path ended).
      nxt <- vapply(idx, function(i) {
        p <- paths[[i]]
        if (length(p) >= depth) p[[depth]] else ""
      }, character(1))
      ended <- nxt == ""
      # Persons whose path ENDS at this depth contribute to the parent node only
      # (already emitted as a node below); they start no child.
      groups <- split(idx[!ended], nxt[!ended])
      if (length(groups) == 0) next

      small_idx <- integer(0)
      kept <- list()
      for (treat in names(groups)) {
        members <- groups[[treat]]
        if (length(members) < thr) {
          small_idx <- c(small_idx, members)
        } else {
          kept[[treat]] <- members
        }
      }

      # Emit each kept child as a NODE (the prefix) and an EDGE (parent->child),
      # band both counts, and queue it as a surviving parent for the next depth.
      for (treat in names(kept)) {
        members <- kept[[treat]]
        node_path <- if (nzchar(plabel)) paste0(plabel, sep, treat) else treat
        cnt <- .bandCount(length(members), band_width = band)
        add_row("node", depth, plabel, node_path, treat, cnt)
        add_row("edge", depth, plabel, node_path, treat, cnt)
        ckey <- node_path
        next_survivors[[ckey]] <- members
        next_parent_label[[ckey]] <- node_path
      }

      # Fold every suppressed sibling under this parent into ONE "Other" node +
      # edge. It is a LEAF (no descendants emitted), so a small branch is hidden
      # behind a single collapsed node rather than leaking via a sum gap. The
      # gate's universal pass drops it fail-closed if it is itself < nfilter.tab.
      if (length(small_idx) > 0) {
        node_path <- if (nzchar(plabel)) paste0(plabel, sep, "Other") else "Other"
        cnt <- .bandCount(length(small_idx), band_width = band)
        add_row("node", depth, plabel, node_path, "Other", cnt)
        add_row("edge", depth, plabel, node_path, "Other", cnt)
      }
    }
    if (length(next_survivors) == 0) break
    survivors <- next_survivors
    parent_label <- unlist(next_parent_label)
  }

  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  out <- out[order(out$row_type, out$depth, -out$person_count), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Live compute fn for the treatment-pathways node/edge frame
#'
#' Pulls each cohort member's ordered treatment events, collapses them into a
#' treatment path in R, and routes the per-person paths through the bespoke
#' hierarchical node/edge gate (\code{\link{.omopTxPathHierarchicalGate}}).
#' Returns the already-hierarchically-consistent node/edge frame (aggregate-only;
#' \code{person_count} the gate then re-suppresses + re-bands idempotently).
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param params Sanitized params (see the entry's param specs).
#' @return Node/edge data frame or empty.
#' @keywords internal
.omopTxPathPathwaysFn <- function(handle, ctx, params) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  src     <- .omopCovariateSource(handle, params$domain_code %||% "1")
  max_len <- min(max(as.integer(params$max_path_length %||% "5"), 1L), 5L)
  filter_pred <- .omopTxPathFilterPredicate(
    handle, paste0("e.", src$concept_col), params$filter_treatments)

  # Self-gate the (optionally filtered) treatment population BEFORE materialising.
  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col,
                         where_sql = if (nzchar(filter_pred)) filter_pred else NULL)

  rows <- .omopTxPathSequenceRows(handle, cohort, src, filter_pred)
  if (!is.data.frame(rows) || nrow(rows) == 0) return(data.frame())

  rows <- rows[order(rows$person_id, suppressWarnings(as.integer(rows$seq_pos))),
               , drop = FALSE]
  per_person <- split(rows$concept_name, rows$person_id)
  paths <- lapply(per_person, .omopTxPathCollapse, max_len = max_len)

  .omopTxPathHierarchicalGate(paths, max_len)
}

# --- percentage_treated -------------------------------------------------------

#' Live compute fn for the per-(layer, drug) percentage treated
#'
#' For each treatment LAYER (1..max_path_length) and each drug appearing at that
#' layer, computes \code{n_treated} = distinct cohort persons whose collapsed
#' treatment path has that drug at that layer, with \code{cohort_size} = distinct
#' persons in the scoped cohort as the denominator and a derived
#' \code{pct_treated}. The path is collapsed in R (consecutive-duplicate eras
#' removed, capped at \code{max_path_length}). \code{unit="person"}: the gate
#' small-cell-suppresses + bands \code{n_treated} and \code{cohort_size}; the
#' percentage is the ONE sanctioned ratio exception, reconciled here against the
#' banded counts via \code{\link{.omopAnalysisReconcileRatio}} (scale 100) so a
#' surviving percentage never re-derives a sub-threshold count.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param params Sanitized params (see the entry's param specs).
#' @return Data frame (treatment_layer, treatment, n_treated, cohort_size,
#'   pct_treated) or empty.
#' @keywords internal
.omopTxPathPercentageTreatedFn <- function(handle, ctx, params) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  src     <- .omopCovariateSource(handle, params$domain_code %||% "1")
  max_len <- min(max(as.integer(params$max_path_length %||% "5"), 1L), 5L)
  top_n   <- max(as.integer(params$top_n %||% "50"), 1L)
  filter_pred <- .omopTxPathFilterPredicate(
    handle, paste0("e.", src$concept_col), params$filter_treatments)

  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col,
                         where_sql = if (nzchar(filter_pred)) filter_pred else NULL)

  # Cohort denominator (distinct persons), one scalar reused for every row.
  cohort_size <- as.numeric(.executeQuery(handle, .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))$n[1])

  rows <- .omopTxPathSequenceRows(handle, cohort, src, filter_pred)
  if (!is.data.frame(rows) || nrow(rows) == 0) return(data.frame())

  rows <- rows[order(rows$person_id, suppressWarnings(as.integer(rows$seq_pos))),
               , drop = FALSE]
  per_person <- split(rows$concept_name, rows$person_id)
  paths <- lapply(per_person, .omopTxPathCollapse, max_len = max_len)

  # One (layer, drug) key per person per layer: a person is counted ONCE per
  # layer for the drug they take at that layer, so n_treated is a distinct-person
  # count by construction.
  layer_drug <- list()
  for (p in paths) {
    for (k in seq_along(p)) {
      key <- paste0(k, "\t", p[[k]])
      layer_drug[[key]] <- (layer_drug[[key]] %||% 0L) + 1L
    }
  }
  if (length(layer_drug) == 0) return(data.frame())

  keys  <- names(layer_drug)
  parts <- strsplit(keys, "\t", fixed = TRUE)
  out <- data.frame(
    treatment_layer = vapply(parts, function(x) as.integer(x[[1]]), integer(1)),
    treatment       = vapply(parts, function(x) x[[2]], character(1)),
    n_treated       = vapply(keys, function(k) as.numeric(layer_drug[[k]]),
                             numeric(1)),
    cohort_size     = cohort_size,
    pct_treated     = NA_real_,
    stringsAsFactors = FALSE)
  out <- out[order(out$treatment_layer, -out$n_treated), , drop = FALSE]
  if (nrow(out) > top_n) out <- out[seq_len(top_n), , drop = FALSE]
  rownames(out) <- NULL

  # The ONE sanctioned ratio exception: recompute the percentage from the banded
  # numerator + denominator (NA when either side is suppressed) BEFORE returning.
  .omopAnalysisReconcileRatio(out, numerator_col = "n_treated",
                              denominator_col = "cohort_size",
                              ratio_col = "pct_treated", scale = 100)
}

# --- duration_eras ------------------------------------------------------------

#' Live compute fn for the per-drug treatment-era duration distribution
#'
#' One dist row per drug: the distribution (in days) of the per-person mean
#' treatment-era duration among cohort members. Each person contributes ONE value
#' per drug (their mean era length), so \code{count_value} is a distinct-PERSON
#' companion (NOT an era count) and the gate masks the summary statistics below
#' \code{nfilter_dist} on it. Era duration = end - start, with the end date
#' COALESCEd to the start date (a single-day fallback when no end is recorded).
#' Quantiles are emitted as p10/p25/median/p75/p90 only; \code{min_value} /
#' \code{max_value} are carried but stripped by the gate. Concepts translated to
#' names; no \code{*_source_value} ever selected.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param params Sanitized params (see the entry's param specs).
#' @return Data frame of dist rows (one per drug) or empty.
#' @keywords internal
.omopTxPathDurationErasFn <- function(handle, ctx, params) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  src     <- .omopCovariateSource(handle, params$domain_code %||% "1")
  top_n   <- max(as.integer(params$top_n %||% "50"), 1L)
  filter_pred <- .omopTxPathFilterPredicate(
    handle, paste0("e.", src$concept_col), params$filter_treatments)

  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col,
                         where_sql = if (nzchar(filter_pred)) filter_pred else NULL)

  # End-date column for the chosen domain: drug_exposure carries an explicit end
  # date; the other domains have only a single event date, so COALESCE to the
  # start (a single-day era). The start column is the covariate source date_col.
  end_col <- switch(as.character(params$domain_code %||% "1"),
    "1" = "drug_exposure_end_date",
    src$date_col)
  dur <- .omopDateDiffDays(
    handle, paste0("COALESCE(e.", end_col, ", e.", src$date_col, ")"),
    paste0("e.", src$date_col))

  where <- paste0("e.", src$concept_col, " IS NOT NULL",
                  if (nzchar(filter_pred)) paste0(" AND ", filter_pred) else "")
  # One numeric value per (drug, person): the person's MEAN era duration, so each
  # person contributes one value to the per-drug distribution (a distinct-person
  # basis for count_value).
  per_person <- paste0(
    "SELECT e.", src$concept_col, " AS covariate_id, e.", src$person_col,
    " AS person_id, AVG(CAST(", dur, " AS FLOAT)) AS v ",
    "FROM ", src$table, " e ",
    "INNER JOIN ", cohort, " c ON c.subject_id = e.", src$person_col,
    " WHERE ", where,
    " GROUP BY e.", src$concept_col, ", e.", src$person_col)
  vsql <- .sql_translate(paste0(
    "SELECT pp.covariate_id, cc.concept_name AS covariate_name, pp.v ",
    "FROM (", per_person, ") pp",
    " LEFT JOIN ", concept, " cc ON cc.concept_id = pp.covariate_id"),
    handle$target_dialect)
  raw <- .executeQuery(handle, vsql)
  if (!is.data.frame(raw) || nrow(raw) == 0) return(data.frame())

  raw$v <- suppressWarnings(as.numeric(raw$v))
  raw <- raw[!is.na(raw$v), , drop = FALSE]
  if (nrow(raw) == 0) return(data.frame())

  parts <- split(raw, raw$covariate_id)
  rows <- lapply(parts, function(p) {
    v  <- p$v
    qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE, type = 7)
    data.frame(
      covariate_id   = p$covariate_id[1],
      covariate_name = p$covariate_name[1],
      count_value    = length(v),
      min_value      = min(v),   # stripped by the gate
      max_value      = max(v),   # stripped by the gate
      avg_value      = mean(v),
      stdev_value    = stats::sd(v),
      p10_value      = qs[1], p25_value = qs[2], median_value = qs[3],
      p75_value      = qs[4], p90_value = qs[5],
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$count_value), , drop = FALSE]
  if (nrow(out) > top_n) out <- out[seq_len(top_n), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# --- Shared param specs -------------------------------------------------------

#' Parameter specs shared by the treatment-pattern entries
#'
#' The treatment universe is the scoped cohort's events in \code{domain_code}
#' (default drug), optionally narrowed to \code{filter_treatments} (a concept set,
#' descendants expanded). \code{max_path_length} caps the treatment sequence at
#' \code{<= 5} (the disclosure cap; values above 5 are clamped). \code{top_n}
#' bounds the breadth of the per-drug / per-layer outputs.
#'
#' @param include_path_length Logical; include \code{max_path_length}.
#' @param include_top_n Logical; include \code{top_n}.
#' @return List of parameter specs.
#' @keywords internal
.omopTxPathParams <- function(include_path_length = TRUE, include_top_n = TRUE) {
  specs <- list(
    list(name = "domain_code", type = "enum", required = FALSE, default = "1",
         choices = c("0", "1", "2", "3", "4"),
         description = paste0("Treatment domain (0 condition,1 drug,2 procedure,",
                              "3 measurement,4 observation).")),
    list(name = "filter_treatments", type = "concept_id", required = FALSE,
         default = NULL,
         description = paste0("Restrict the treatment universe to this concept id ",
                              "and its descendants (all concepts when omitted)."))
  )
  if (include_path_length) {
    specs <- c(specs, list(
      list(name = "max_path_length", type = "int", required = FALSE,
           default = "5", min = 1L, max = 5L,
           description = "Maximum treatment-sequence length (capped at 5).")
    ))
  }
  if (include_top_n) {
    specs <- c(specs, list(
      list(name = "top_n", type = "int", required = FALSE, default = "50")
    ))
  }
  specs
}

# --- Entry builders -----------------------------------------------------------

#' Build the treatment-pathways entry (\code{dsomop:txpath.pathways})
#' @keywords internal
.omopTxPathPathwaysEntry <- function() {
  plot_code <- paste(
    "function(df, params) {",
    "  edges <- df[df$row_type == 'edge', ]",
    "  ggplot2::ggplot(edges, ggplot2::aes(x = depth, y = person_count,",
    "                                      fill = treatment)) +",
    "    ggplot2::geom_col() +",
    "    ggplot2::labs(x = 'Treatment layer', y = 'Persons', fill = 'Treatment')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = "dsomop:txpath.pathways",
    description = paste0("Treatment-sequence pathways over the scoped cohort: ",
                         "distinct-person frequency of each ordered treatment ",
                         "prefix (node) and transition (edge), hierarchically ",
                         "suppressed + banded for a disclosure-safe sunburst/",
                         "sankey. Path length capped at 5."),
    domain      = "drug",
    params      = .omopTxPathParams(include_path_length = TRUE,
                                    include_top_n = FALSE),
    compute     = list(kind = "r", sql = NULL, fn = .omopTxPathPathwaysFn,
                       plot = list(type = "sankey", code = plot_code)),
    dependencies = list(tables = c("drug_exposure", "concept"),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit = "person", count_cols = "person_count"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

#' Build the percentage-treated entry (\code{dsomop:txpath.percentage_treated})
#' @keywords internal
.omopTxPathPercentageTreatedEntry <- function() {
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = factor(treatment_layer),",
    "                                   y = pct_treated, fill = treatment)) +",
    "    ggplot2::geom_col(position = 'dodge') +",
    "    ggplot2::labs(x = 'Treatment layer', y = '% treated', fill = 'Drug')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = "dsomop:txpath.percentage_treated",
    description = paste0("Percentage of the scoped cohort treated with each drug ",
                         "at each treatment layer, with the distinct-person ",
                         "numerator + cohort denominator gated and the percentage ",
                         "reconciled from the banded counts."),
    domain      = "drug",
    params      = .omopTxPathParams(include_path_length = TRUE,
                                    include_top_n = TRUE),
    compute     = list(kind = "r", sql = NULL,
                       fn = .omopTxPathPercentageTreatedFn,
                       plot = list(type = "bar", code = plot_code)),
    dependencies = list(tables = c("drug_exposure", "concept"),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit = "person", count_cols = c("n_treated", "cohort_size")),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

#' Build the duration-eras entry (\code{dsomop:txpath.duration_eras})
#' @keywords internal
.omopTxPathDurationErasEntry <- function() {
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(covariate_name,",
    "                                                      median_value),",
    "                                   y = median_value)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Drug', y = 'Median era duration (days)')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = "dsomop:txpath.duration_eras",
    description = paste0("Per-drug distribution (days) of the per-person mean ",
                         "treatment-era duration over the scoped cohort. ",
                         "Min/max are never emitted; summary stats are masked on ",
                         "the distinct-person companion."),
    domain      = "drug",
    params      = .omopTxPathParams(include_path_length = FALSE,
                                    include_top_n = TRUE),
    compute     = list(kind = "r", sql = NULL, fn = .omopTxPathDurationErasFn,
                       plot = list(type = "bar", code = plot_code)),
    dependencies = list(tables = c("drug_exposure", "concept"),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit = "dist", count_cols = "count_value", min_max = TRUE),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Group registrar ----------------------------------------------------------

#' Emit the OHDSI TreatmentPatterns catalog entries (native live compute)
#'
#' The group registrar for \code{ohdsi_pack_treatmentpatterns.R}: the three NEW
#' canonical treatment-pattern analyses, each COMPUTING its metric LIVE from the
#' CDM over the scoped cohort (no precomputed results-table read; TreatmentPatterns
#' has no registry result table the precomputed adapter ever consumed). Returns a
#' named list keyed by stable entry id, exactly as
#' \code{\link{.omopAnalysisOhdsiEntries}} expects to concatenate (these ids are
#' disjoint from the precomputed registry ids, so the overlay simply ADDS them).
#' All ids are globally unique by construction. \code{handle} is taken for
#' signature parity ONLY and is never queried at build time (catalog build is
#' cached on \code{handle$analysis_catalog}; all DB I/O happens later inside each
#' \code{compute$fn}).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry id.
#' @keywords internal
.ohdsiPackTreatmentPatternsEntries <- function(handle) {
  entries <- list(
    .omopTxPathPathwaysEntry(),
    .omopTxPathPercentageTreatedEntry(),
    .omopTxPathDurationErasEntry()
  )
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}
