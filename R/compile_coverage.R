# Module: Observation-period coverage (design-2.10.3 sections 1.3, 2.5, 6.13;
# fix of defect A-E6)
#
# Coverage is a per-population policy `omop_coverage(policy, on_ambiguous,
# on_missing, gap)` evaluated for each roster row at its index date (after
# offsets). Missing coverage is resolved before ambiguity:
#   unique    the observation periods containing the index are counted:
#             0 -> on_missing; 1 -> its bounds; > 1 -> on_ambiguous (drop, or
#             the user-chosen typed failure coverage_ambiguous; never an
#             abort of the whole plan as before 2.10.3);
#   earliest  one complete record by (start, end, observation_period_id),
#             start and end always from that record;
#   union     the person's periods are first coalesced into components (gaps
#             and islands, a next period merges when start <= previous max
#             end + gap + 1); the index must lie inside an ACTUAL recorded
#             period, and the component containing that period is selected
#             (records [1,2] and [10,11] with gap 7 form one component, but an
#             index on day 5 is uncovered).
# Three fragments per population (and per criterion-specific request):
#   opc_<pop>      one row per roster row: row_key, person_id, op_count
#                  (private: 0 = missing, 1 = covered, > 1 = ambiguous),
#                  op_start, op_end (the ELIGIBILITY span: the record, or the
#                  coalesced component), component_id;
#   seg_<pop>      the eligibility components of the roster's persons;
#   seg_<pop>_rec  the ACTUAL recorded periods of the roster's persons, each
#                  with the component it belongs to (unique / earliest: every
#                  record is its own component).
# Consumers read eligibility spans from the roster (op_start / op_end) and
# actually observed days from seg_<pop>_rec through the roster row's
# component (.compileObservedSegments()): a gap allowance may satisfy a
# continuity requirement but never adds observed person-time, denominator days
# or outcome ascertainment. Observation rows with a NULL bound or an end
# before the start never cover anything (the one data-quality policy).

.COVERAGE_POLICIES <- c("unique", "earliest", "union")

#' Validated coverage policy (defaults applied)
#'
#' @param coverage NULL or list(policy, on_ambiguous, on_missing, gap).
#' @param path JSON path used in rejections.
#' @return list(policy, on_ambiguous, on_missing, gap).
#' @keywords internal
.coveragePolicy <- function(coverage, path = "/coverage") {
  coverage <- coverage %||% list()
  if (!is.list(coverage) || (length(coverage) && is.null(names(coverage)))) {
    .irReject("coverage is an object", field = path)
  }
  unknown <- setdiff(names(coverage), c("policy", "on_ambiguous", "on_missing", "gap"))
  if (length(unknown)) {
    .irReject(paste0("unknown coverage field(s): ", paste(unknown, collapse = ", ")), field = path)
  }
  list(
    policy = .intervalEnum(coverage[["policy"]] %||% "unique", .COVERAGE_POLICIES, "policy", path),
    on_ambiguous = .intervalEnum(coverage[["on_ambiguous"]] %||% "drop", c("drop", "error"),
                                 "on_ambiguous", path),
    on_missing = .intervalEnum(coverage[["on_missing"]] %||% "drop", c("drop", "keep"),
                               "on_missing", path),
    gap = .intervalInt(coverage[["gap"]] %||% 0L, "gap", path))
}

#' The observation periods of the site as a fragment
#'
#' \code{op_periods}: person_id, period_id (observation_period_id),
#' start_date, end_date of every record with both bounds and end >= start.
#' One per plan; the table must be available (an indexed population needs
#' observation semantics).
#' @keywords internal
.coverageSource <- function(ctx) {
  nm <- .fragmentName("op", "periods", ctx = ctx)
  existing <- .fragmentGet(ctx, nm)
  snapshot <- isTRUE(ctx[["population_snapshots"]])
  if (!is.null(existing)) {
    if (!snapshot || isTRUE(attr(existing, "coverage_snapshot", exact = TRUE))) return(existing)
    # A source compiled by a standalone/rendering helper can precede native
    # populations. Promote it once before roster/component materialisation.
    # Dependencies resolve this identity through the registry, so existing
    # coverage fragments now read the same snapshot. Restore the original
    # entry if materialisation fails; never leave a half-replaced registry.
    store <- .fragmentStore(ctx)
    rm(list = nm, envir = store)
    return(tryCatch({
      frozen <- .populationFreezeFragment(ctx, existing)
      attr(frozen, "coverage_snapshot") <- TRUE
      .fragmentRegister(ctx, frozen)
    }, error = function(e) {
      assign(nm, existing, envir = store)
      stop(e)
    }))
  }
  h <- .compileDialect(ctx)
  handle <- ctx[["handle"]]
  role <- .roleOf(handle, "observation_period")
  if (is.null(role) || is.null(role[["end"]])) {
    .irReject("coverage needs the observation_period table", code = "unavailable_table",
              field = "/populations")
  }
  bp <- .buildBlueprint(handle)
  qualified <- .qualifyTable(handle, "observation_period", .resolveTableSchema(
    handle, "observation_period",
    bp$tables$schema_category[bp$tables$table_name == "observation_period"][1L]))
  col_df <- bp$columns[["observation_period"]]
  person_type <- .eventColumnType(col_df, role[["person"]]) %||% "integer"
  if (!person_type %in% c("integer", "bigint")) person_type <- "integer"
  s <- paste0("o.", role[["start"]])
  e <- paste0("o.", role[["end"]])
  body <- paste0("SELECT o.", role[["person"]], " AS person_id, ",
                 .sqlLosslessKey(h, paste0("o.", role[["event_key"]])), " AS period_id, ",
                 s, " AS start_date, ", e, " AS end_date FROM ", qualified, " o WHERE ",
                 s, " IS NOT NULL AND ", e, " IS NOT NULL AND ", e, " >= ", s)
  f <- .fragment(nm, "coverage", "row", body,
                 columns = list(person_id = .fragmentColumn(person_type, FALSE, "person_key"),
                                period_id = .fragmentColumn("bigint", FALSE, "origin_identity"),
                                start_date = .fragmentColumn("date", FALSE, "start"),
                                end_date = .fragmentColumn("date", FALSE, "end")),
                 unique_key = "period_id",
                 person_lineage = list(person_col = "person_id", via_roster = NULL),
                 private_columns = "period_id", ordering = c("start_date", "period_id"))
  .sqlAssertTypedSql(h, f$select_body)
  if (snapshot) {
    f <- .populationFreezeFragment(ctx, f)
    attr(f, "coverage_snapshot") <- TRUE
  }
  .fragmentRegister(ctx, f)
}

#' Compile the coverage of a roster (design section 6.13)
#'
#' @param ctx Execution context.
#' @param pop Population id (fragment names).
#' @param coverage Coverage policy object (defaults applied when absent).
#' @param roster_fragment The candidates fragment (candidate coverage lookup,
#'   section 6.1) or a final roster.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @param suffix Optional name suffix for a criterion-specific request (e.g.
#'   \code{omop_observed(gap = )}).
#' @return list(coverage = opc_<pop>, segments = seg_<pop>, recorded =
#'   seg_<pop>_rec, policy, pop, suffix). Fragment identities \code{("opc",
#'   pop, suffix)}, \code{("seg", pop, suffix)}, \code{("seg", pop,
#'   "[<suffix>_]rec")} and, for union, \code{("seg", pop, "[<suffix>_]mk")},
#'   allocated per context (A-17): \code{suffix} is NULL for the population's
#'   policy and \code{g<gap>} for an \code{omop_observed(gap =)} request.
#' @keywords internal
.compileCoverage <- function(ctx, pop, coverage, roster_fragment, phase = "candidate",
                             suffix = NULL) {
  policy <- .coveragePolicy(coverage, paste0("/populations/", pop, "/coverage"))
  spec <- .rosterSpec(roster_fragment, phase)
  if (is.null(spec$index)) {
    stop("Coverage is looked up at the roster's index (cohort_start_date).", call. = FALSE)
  }
  h <- .compileDialect(ctx)
  stage <- function(x) paste(c(suffix, x), collapse = "_")
  nm_opc <- .fragmentName("opc", pop, suffix, ctx = ctx)
  memo <- .compileMemo(ctx, "coverage_meta")
  if (exists(nm_opc, envir = memo, inherits = FALSE)) {
    meta <- get(nm_opc, envir = memo, inherits = FALSE)
    if (!identical(meta$policy, policy) || !identical(meta$roster, roster_fragment$name)) {
      stop("Coverage '", nm_opc, "' was compiled with another policy or roster.", call. = FALSE)
    }
    return(list(coverage = .fragmentGet(ctx, meta$fragments[["coverage"]]),
                segments = .fragmentGet(ctx, meta$fragments[["segments"]]),
                recorded = .fragmentGet(ctx, meta$fragments[["recorded"]]), policy = policy,
                pop = pop, suffix = suffix))
  }
  op <- .coverageSource(ctx)
  person_type <- op$columns$person_id$type
  lineage <- list(person_col = "person_id", via_roster = NULL)
  register <- function(f) {
    .sqlAssertTypedSql(h, f$select_body)
    .fragmentRegister(ctx, f)
  }
  persons <- paste0("EXISTS (SELECT 1 FROM ", spec$name, " q WHERE q.", spec$person, " = p.person_id)")
  seg_cols <- list(person_id = .fragmentColumn(person_type, FALSE, "person_key"),
                   component_id = .fragmentColumn("bigint", FALSE, "group"),
                   start_date = .fragmentColumn("date", FALSE, "start"),
                   end_date = .fragmentColumn("date", FALSE, "end"))
  rec_cols <- list(person_id = .fragmentColumn(person_type, FALSE, "person_key"),
                   period_id = .fragmentColumn("bigint", FALSE, "origin_identity"),
                   component_id = .fragmentColumn("bigint", FALSE, "group"),
                   start_date = .fragmentColumn("date", FALSE, "start"),
                   end_date = .fragmentColumn("date", FALSE, "end"))
  nm_seg <- .fragmentName("seg", pop, suffix, ctx = ctx)
  nm_rec <- .fragmentName("seg", pop, stage("rec"), ctx = ctx)
  if (identical(policy$policy, "union")) {
    nm_mk <- .fragmentName("seg", pop, stage("mk"), ctx = ctx)
    base <- paste0("SELECT p.person_id AS person_id, p.period_id AS period_id, ",
                   "p.start_date AS start_date, p.end_date AS end_date FROM ", op$name,
                   " p WHERE ", persons)
    mk_body <- .intervalMarksSql(h, paste0("(", base, ")"),
                                 c("person_id", "period_id", "start_date", "end_date"),
                                 "person_id", c("end_date", "period_id"), policy$gap, 0L)
    f_mk <- register(.fragment(
      nm_mk, "coverage", "row", mk_body,
      columns = list(person_id = .fragmentColumn(person_type, FALSE, "person_key"),
                     period_id = .fragmentColumn("bigint", FALSE, "origin_identity"),
                     start_date = .fragmentColumn("date", FALSE, "start"),
                     end_date = .fragmentColumn("date", FALSE, "end"),
                     prev_max_end = .fragmentColumn("date", TRUE, "private"),
                     new_era = .fragmentColumn("integer", FALSE, "private"),
                     era_no = .fragmentColumn("integer", FALSE, "era_no")),
      dependencies = c(op$name, spec$name), unique_key = "period_id", person_lineage = lineage,
      private_columns = c("period_id", "prev_max_end", "new_era", "era_no"),
      ordering = c("start_date", "period_id")))
    rec_body <- paste0("SELECT m.person_id AS person_id, m.period_id AS period_id, ",
                       .sqlLosslessKey(h, "m.era_no"), " AS component_id, m.start_date AS start_date, ",
                       "m.end_date AS end_date FROM ", f_mk$name, " m")
    f_rec <- register(.fragment(nm_rec, "coverage", "row", rec_body, rec_cols,
                                dependencies = f_mk$name, unique_key = "period_id",
                                person_lineage = lineage, private_columns = c("period_id", "component_id"),
                                ordering = c("start_date", "period_id")))
    seg_body <- paste0("SELECT o.person_id AS person_id, o.component_id AS component_id, ",
                       "MIN(o.start_date) AS start_date, MAX(o.end_date) AS end_date FROM ",
                       f_rec$name, " o GROUP BY o.person_id, o.component_id")
    f_seg <- register(.fragment(nm_seg, "coverage", "row", seg_body, seg_cols,
                                dependencies = f_rec$name, unique_key = c("person_id", "component_id"),
                                person_lineage = lineage, private_columns = "component_id",
                                ordering = c("start_date", "component_id")))
  } else {
    rec_body <- paste0("SELECT p.person_id AS person_id, p.period_id AS period_id, ",
                       "p.period_id AS component_id, p.start_date AS start_date, p.end_date AS end_date FROM ",
                       op$name, " p WHERE ", persons)
    f_rec <- register(.fragment(nm_rec, "coverage", "row", rec_body, rec_cols,
                                dependencies = c(op$name, spec$name), unique_key = "period_id",
                                person_lineage = lineage, private_columns = c("period_id", "component_id"),
                                ordering = c("start_date", "period_id")))
    seg_body <- paste0("SELECT o.person_id AS person_id, o.component_id AS component_id, ",
                       "o.start_date AS start_date, o.end_date AS end_date FROM ", f_rec$name, " o")
    f_seg <- register(.fragment(nm_seg, "coverage", "row", seg_body, seg_cols,
                                dependencies = f_rec$name, unique_key = c("person_id", "component_id"),
                                person_lineage = lineage, private_columns = "component_id",
                                ordering = c("start_date", "component_id")))
  }
  idx <- paste0("r.", spec$index)
  inside <- function(o) paste0(idx, " >= ", o, ".start_date AND ", idx, " <= ", o, ".end_date")
  count_int <- function(x) .sqlIntegerCast(h, x)
  opc_body <- switch(policy$policy,
    unique = paste0(
      "SELECT r.", spec$key, " AS row_key, r.", spec$person, " AS person_id, ",
      count_int("COUNT(o.period_id)"), " AS op_count, ",
      "CASE WHEN COUNT(o.period_id) = 1 THEN MIN(o.start_date) END AS op_start, ",
      "CASE WHEN COUNT(o.period_id) = 1 THEN MIN(o.end_date) END AS op_end, ",
      "CASE WHEN COUNT(o.period_id) = 1 THEN MIN(o.component_id) END AS component_id FROM ",
      spec$name, " r LEFT JOIN ", f_rec$name, " o ON o.person_id = r.", spec$person, " AND ",
      inside("o"), " GROUP BY r.", spec$key, ", r.", spec$person),
    earliest = {
      rn <- .sqlRowNumber(h, paste0("r.", spec$key), c("o.start_date", "o.end_date", "o.period_id"))
      ranked <- paste0("SELECT r.", spec$key, " AS row_key, o.start_date AS start_date, ",
                       "o.end_date AS end_date, o.component_id AS component_id, ", rn,
                       " AS rn FROM ", spec$name, " r INNER JOIN ", f_rec$name, " o ON o.person_id = r.",
                       spec$person, " AND ", inside("o"))
      paste0("SELECT r.", spec$key, " AS row_key, r.", spec$person, " AS person_id, ",
             "CASE WHEN x.component_id IS NULL THEN 0 ELSE 1 END AS op_count, ",
             "x.start_date AS op_start, x.end_date AS op_end, x.component_id AS component_id FROM ",
             spec$name, " r LEFT JOIN (SELECT y.row_key, y.start_date, y.end_date, y.component_id FROM (",
             ranked, ") y WHERE y.rn = 1) x ON x.row_key = r.", spec$key)
    },
    union = paste0(
      "SELECT r.", spec$key, " AS row_key, r.", spec$person, " AS person_id, ",
      "CASE WHEN MIN(o.component_id) IS NULL THEN 0 ELSE 1 END AS op_count, ",
      "MIN(s.start_date) AS op_start, MIN(s.end_date) AS op_end, ",
      "MIN(o.component_id) AS component_id FROM ", spec$name, " r LEFT JOIN ", f_rec$name,
      " o ON o.person_id = r.", spec$person, " AND ", inside("o"), " LEFT JOIN ", f_seg$name,
      " s ON s.person_id = o.person_id AND s.component_id = o.component_id",
      " GROUP BY r.", spec$key, ", r.", spec$person))
  f_opc <- register(.fragment(
    nm_opc, "coverage", "episode", opc_body,
    columns = list(row_key = .fragmentColumn(spec$key_type, FALSE, "episode_key"),
                   person_id = .fragmentColumn(spec$person_type, FALSE, "person_key"),
                   op_count = .fragmentColumn("integer", FALSE, "private"),
                   op_start = .fragmentColumn("date", TRUE, "start"),
                   op_end = .fragmentColumn("date", TRUE, "end"),
                   component_id = .fragmentColumn("bigint", TRUE, "group")),
    dependencies = unique(c(spec$name, f_rec$name, if (identical(policy$policy, "union")) f_seg$name)),
    unique_key = "row_key", person_lineage = lineage, roster_namespace = spec$namespace,
    private_columns = c("row_key", "op_count", "component_id")))
  assign(nm_opc, list(policy = policy, roster = roster_fragment$name,
                      fragments = c(coverage = f_opc$name, segments = f_seg$name,
                                    recorded = f_rec$name)), envir = memo)
  list(coverage = f_opc, segments = f_seg, recorded = f_rec, policy = policy, pop = pop,
       suffix = suffix)
}

# Suffixes of the coverage requests a population can make: its own policy
# (NULL) and one criterion-specific union request per distinct
# omop_observed(gap =) in its criteria ("g<gap>").
.coverageSuffixes <- function(pop) {
  gaps <- integer(0)
  walk <- function(x) {
    if (!is.list(x)) return(invisible(NULL))
    obs <- x[["observed"]]
    if (is.list(obs) && !is.null(obs[["gap"]]) && !is.list(obs[["gap"]])) {
      gaps <<- c(gaps, as.integer(obs[["gap"]]))
    }
    for (v in x) walk(v)
    invisible(NULL)
  }
  walk(pop[["where"]])
  c(list(NULL), lapply(sort(unique(gaps)), function(g) paste0("g", g)))
}

#' Identities of the coverage fragments of a plan (amendment A-17)
#'
#' \code{op_periods} once; per population and coverage request (the
#' population's policy, and each criterion-specific \code{omop_observed(gap =)}
#' union request, suffix \code{g<gap>}): \code{opc}, \code{seg} and its
#' stages \code{rec}, \code{mk}, and the observed segments of the candidate
#' and final rosters (\code{obs_cand}, \code{obs_final}).
#' @keywords internal
.coverageFragmentIdentities <- function(ir) {
  pops <- names(ir[["populations"]] %||% list())
  if (!length(pops)) return(list())
  out <- list(.fragmentIdentity("op", "periods"))
  for (p in pops) {
    for (sfx in .coverageSuffixes(ir[["populations"]][[p]])) {
      st <- function(x) paste(c(sfx, x), collapse = "_")
      out <- c(out, list(.fragmentIdentity("opc", p, sfx), .fragmentIdentity("seg", p, sfx),
                         .fragmentIdentity("seg", p, st("rec")), .fragmentIdentity("seg", p, st("mk")),
                         .fragmentIdentity("seg", p, st("obs_cand")),
                         .fragmentIdentity("seg", p, st("obs_final"))))
    }
  }
  out
}

#' Rows a coverage policy keeps
#'
#' Covered rows always; uncovered rows only under \code{on_missing = "keep"}
#' (with NULL bounds); ambiguous rows never (under \code{on_ambiguous =
#' "error"} the plan has failed before, \code{.coverageAssertUnambiguous()}).
#'
#' @param policy A validated policy (\code{.coveragePolicy()}).
#' @param alias SQL alias of the \code{opc_<pop>} relation.
#' @return SQL predicate.
#' @keywords internal
.coverageKeepSql <- function(policy, alias = "o") {
  if (identical(policy$on_missing, "keep")) {
    paste0("(", alias, ".op_count = 1 OR ", alias, ".op_count = 0)")
  } else {
    paste0(alias, ".op_count = 1")
  }
}

#' Refuse an ambiguous coverage when the policy asks for it
#'
#' Under \code{policy = "unique"} and \code{on_ambiguous = "error"}, a roster
#' row whose index is covered by more than one observation period is the
#' typed, user-chosen failure \code{coverage_ambiguous} (no counts; the row
#' count never leaves the server).
#'
#' @param ctx Execution context.
#' @param cov The result of \code{.compileCoverage()}.
#' @param pop Population id (for the message).
#' @return TRUE invisibly when the coverage is unambiguous.
#' @keywords internal
.coverageAssertUnambiguous <- function(ctx, cov, pop) {
  if (!identical(cov$policy$policy, "unique") || !identical(cov$policy$on_ambiguous, "error")) {
    return(invisible(TRUE))
  }
  f <- cov$coverage
  n <- .adapterCount(ctx, f, paste0("SELECT COUNT(*) AS n FROM ", f$name, " o WHERE o.op_count > 1"))
  if (n > 0) {
    .compileReject(paste0("population '", pop, "': an index date is covered by more than one ",
                          "observation period (coverage on_ambiguous = \"error\")"),
                   reason = "coverage_ambiguous", code = "coverage_ambiguous",
                   field = paste0("/populations/", pop, "/coverage"))
  }
  invisible(TRUE)
}

#' Observation-continuity predicate (criterion \code{omop_observed})
#'
#' \code{op_start <= index - before} and \code{op_end >= index + after} on the
#' eligibility span of the row's coverage (NULL bounds: UNKNOWN).
#'
#' @param h Handle, context, profile or dialect name.
#' @param alias Alias carrying \code{op_start}/\code{op_end}.
#' @param index_expr SQL expression of the index date.
#' @param before,after NULL or non-negative integers.
#' @return SQL predicate (\code{1 = 1} when neither is given).
#' @keywords internal
.coverageObservedSql <- function(h, alias, index_expr, before = NULL, after = NULL) {
  parts <- character(0)
  if (!is.null(before)) {
    parts <- c(parts, paste0(alias, ".op_start <= ", .sqlDateAddDays(h, index_expr, -as.integer(before))))
  }
  if (!is.null(after)) {
    parts <- c(parts, paste0(alias, ".op_end >= ", .sqlDateAddDays(h, index_expr, as.integer(after))))
  }
  if (!length(parts)) {
    return(paste0("(", alias, ".op_start IS NOT NULL AND ", alias, ".op_end IS NOT NULL)"))
  }
  paste(parts, collapse = " AND ")
}

#' The actually observed segments of each roster row
#'
#' The recorded observation periods of the row's selected coverage component
#' (\code{seg_<pop>_rec} joined through the component), for observed days,
#' \code{covered_days(observed_only = TRUE)}, panels and survival risk
#' segments. The roster carries the component as \code{op_component_id};
#' otherwise the coverage fragment of the same roster supplies it.
#'
#' @param ctx Execution context.
#' @param cov The result of \code{.compileCoverage()}.
#' @param roster_fragment Roster or candidates fragment.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @param name Optional fragment name (default: the allocated identity
#'   \code{("seg", <pop>, "[<suffix>_]obs_cand" | "[<suffix>_]obs_final")}).
#' @return Fragment with \code{row_key}, \code{person_id}, \code{start_date},
#'   \code{end_date}: one row per distinct observed segment of each roster row
#'   (two observation-period records with the same dates in one component are
#'   one segment; consumers take the union of overlapping segments).
#' @keywords internal
.compileObservedSegments <- function(ctx, cov, roster_fragment, phase, name = NULL) {
  spec <- .rosterSpec(roster_fragment, phase)
  name <- name %||% .fragmentName("seg", cov$pop,
                                  paste(c(cov$suffix, "obs", if (identical(phase, "candidate")) "cand" else "final"),
                                        collapse = "_"), ctx = ctx)
  existing <- .fragmentGet(ctx, name)
  if (!is.null(existing)) return(existing)
  h <- .compileDialect(ctx)
  rec <- cov$recorded
  if (!is.null(spec$op_component)) {
    body <- paste0("SELECT DISTINCT r.", spec$key, " AS row_key, r.", spec$person, " AS person_id, ",
                   "o.start_date AS start_date, o.end_date AS end_date FROM ", spec$name,
                   " r INNER JOIN ", rec$name, " o ON o.person_id = r.", spec$person,
                   " AND o.component_id = r.", spec$op_component)
    deps <- c(spec$name, rec$name)
  } else {
    opc <- cov$coverage
    if (!spec$name %in% opc$dependencies) {
      stop("The roster carries no op_component_id and the coverage was compiled for another roster.",
           call. = FALSE)
    }
    body <- paste0("SELECT DISTINCT r.", spec$key, " AS row_key, r.", spec$person, " AS person_id, ",
                   "o.start_date AS start_date, o.end_date AS end_date FROM ", spec$name,
                   " r INNER JOIN ", opc$name, " c ON c.row_key = r.", spec$key,
                   " INNER JOIN ", rec$name, " o ON o.person_id = r.", spec$person,
                   " AND o.component_id = c.component_id")
    deps <- c(spec$name, opc$name, rec$name)
  }
  f <- .fragment(name, "coverage", "row", body,
                 columns = list(row_key = .fragmentColumn(spec$key_type, FALSE, "episode_key"),
                                person_id = .fragmentColumn(spec$person_type, FALSE, "person_key"),
                                start_date = .fragmentColumn("date", FALSE, "start"),
                                end_date = .fragmentColumn("date", FALSE, "end")),
                 dependencies = unique(deps), unique_key = c("row_key", "start_date", "end_date"),
                 person_lineage = list(person_col = "person_id", via_roster = NULL),
                 roster_namespace = spec$namespace, private_columns = "row_key",
                 ordering = c("row_key", "start_date", "end_date"))
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
}

#' Row keys of a roster that have a coverage component
#'
#' Rows retained without coverage (\code{on_missing = "keep"}) have none:
#' observation-dependent consumers (observed days, \code{covered_days(
#' observed_only = TRUE)}) give them NA, never 0.
#'
#' @param ctx Execution context.
#' @param cov The result of \code{.compileCoverage()}.
#' @param roster_fragment Roster or candidates fragment.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @param field JSON path of the consumer (the bounded fetch's rejection).
#' @return Vector of row keys (a private fetch of at most
#'   \code{dsomop.max_memory_rows} rows; one more is the typed rejection
#'   \code{reducer_row_cap}, A-20 (j)).
#' @keywords internal
.coverageCoveredKeys <- function(ctx, cov, roster_fragment, phase, field = "/reducer") {
  spec <- .rosterSpec(roster_fragment, phase)
  select <- if (!is.null(spec$op_component)) {
    paste0("SELECT r.", spec$key, " AS row_key FROM ", spec$name, " r WHERE r.",
           spec$op_component, " IS NOT NULL")
  } else {
    if (!spec$name %in% cov$coverage$dependencies) {
      stop("The roster carries no op_component_id and the coverage was compiled for another roster.",
           call. = FALSE)
    }
    paste0("SELECT o.row_key AS row_key FROM ", cov$coverage$name, " o WHERE o.op_count = 1")
  }
  final <- if (!is.null(spec$op_component)) roster_fragment else cov$coverage
  sql <- .buildStatement(ctx, final, select = select, dialect = .compileDialect(ctx))
  cap <- .extractionCap("dsomop.max_memory_rows", 1000000L)
  .fetchBounded(ctx[["handle"]], sql, cap, function() {
    .compileReject("a reducer's roster exceeds the server in-memory row cap (dsomop.max_memory_rows)",
                   reason = "reducer_row_cap", field = field)
  })$row_key
}
