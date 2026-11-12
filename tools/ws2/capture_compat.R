# WS-2 tooling (dsOMOP 2.10.3), Astra's WS-2 re-review R1/R3 and amendment
# A-19 item 7: v1 COMPATIBILITY differential capture.
#
# Not part of the package tests. It produces independent ground truth of what a
# dsOMOP build does with v1 plans whose scalar fields or predicate literals are
# written in a historically coercible or invalid form, and builds the durable
# fixture tests/testthat/fixtures/ir-v1-compat.json that
# tests/testthat/test-ir-v1-compat.R replays on the current code.
#
#   Rscript tools/ws2/capture_compat.R run <dsOMOP source dir> <out.json> [label]
#       Loads the source dir with pkgload::load_all(dir, helpers = TRUE,
#       export_all = TRUE) (so create_test_handle() and the safe-bin helpers
#       exist) and runs every case of the catalogue below, each on a FRESH
#       create_test_handle(), with dsOMOP:::.planExecute(handle, plan,
#       out_symbols) under the case's options (default nfilter.subset = 0,
#       dsomop.query_strict = TRUE). Works on the 2.10.2 baseline and on any
#       2.10.3 snapshot; on a build that has .irPrepare() it also records
#       .irPrepare(plan) without a handle.
#       (CC_FILTER=<regex> restricts a development run to the matching case ids.)
#   Rscript tools/ws2/capture_compat.R fixture <run-a.json> <run-b.json> <fixture.json>
#       Builds the fixture from two independent BASELINE runs (two fresh R
#       processes) after checking that both executed exactly this probe's case
#       catalogue (its sha256 is recorded). A case whose two outcomes differ is
#       kept but flagged `nondeterministic` and is not replayed.
#   Rscript tools/ws2/capture_compat.R compare <fixture.json> <run.json>
#       Prints, per case, the baseline outcome next to the outcome of a run of
#       another build (e.g. the current snapshot) and whether they differ.
#   Rscript tools/ws2/capture_compat.R diff <run-a.json> <run-b.json> [id regex]
#       For every case whose outcomes differ between two runs (e.g. the
#       baseline and the current code), the first differing path of the two
#       canonical results, or the two refusals. The fixture keeps only
#       the sha256 of a baseline result; this recomputes the full forms.
#
# Plans are written in the TRANSPORT form: the JSON a client sends, decoded
# exactly as the server's .ds_arg() decodes it (jsonlite::fromJSON(json,
# simplifyVector = FALSE)): JSON integers are R integers, `30.0` a double,
# strings character, `true` logical, `[]` an empty list, `null` a NULL member.
#
# Per case the run records:
#   outcome   `ok` with the canonical result -- the b10-canonical .b10_canon()
#             below (copied from tools/ws2/capture_results.R at cb5907c) at 17
#             significant digits (lossless for doubles). Date/POSIX objects
#             still use formatted values here; the replay test explicitly
#             refuses those types pending numeric-payload baseline recapture,
#             rather than claiming an exact temporal comparison -- or `error`
#             with the condition classes, the message (volatile temp-table
#             suffixes neutralised) and:
#               refused_before_query  TRUE when no data statement had been
#                   issued when the error was signalled (catalogue reads -- the
#                   blueprint's sqlite_master / PRAGMA / cdm_source probe -- do
#                   not count; every DBI statement is traced);
#               origin  the innermost package functions on the stack at the
#                   FIRST stop() with the final message (base::stop is traced:
#                   the 2.10.2 executor re-raises an output's error from its own
#                   tryCatch), i.e. the coercion that fired;
#               stage   `plan` (a property of the plan alone: the value/shape
#                   rule of a v1 executor), `site` (needs the site's catalogue
#                   or dialect, including its blocked columns), `executor` (the
#                   database or R failed on the value while running: no
#                   validator owns it), `data` (needs the data: disclosure
#                   gates, vocabulary resolution, empty populations) or
#                   `session` (server-issued session state: safe-bin
#                   contracts, handle-owned temp cohorts), from the reviewed
#                   message rules of .cc_stage();
#             and the warnings of the call.
#   summary   per output: rows, columns, distinct persons and rows per person
#             (the fixture keeps the result's sha256 with rows and columns per
#             output -- for sections C and D also persons and rows per person
#             -- and the concept_col cases, section C, keep their data SQL).
#   ir        on a build with .irPrepare(): accept / refuse (classes, message)
#             of .irPrepare(plan) without a handle.
#
# Sections (Astra re-review R1/R3; A-19 item 7):
#   A  every integer/number scalar of the closed v1 envelope (the .v1sInt(),
#      .v1sScalar("integer"), .v1sNum(), .v1sScalar("number"), .v1sCohortId()
#      and .v1sPlanCohortId() positions of R/ir_v1_shape.R, the filter-tree leaf
#      parameters and the concept-id lists), each on a minimal plan that makes
#      the 2.10.2 executor read the field (fields no 2.10.2 path reads --
#      version, options.min_persons, harmonization.version, output options --
#      are included and say so in `reads`), with the variants of
#      .cc_variants(); the gate-dependent leaf coercions are captured again at
#      nfilter.subset = 3 (`.k3`) and the age grid at nfilter.tab = 0 (`.tab0`);
#   B  predicate literals of v1 custom filters (concept, numeric, text and date
#      columns; ==, !=, >, in, not_in, between) and value_bin string bounds;
#   C  the event_level concept_col override;
#   D  set operations and private intermediates;
#   E  predicate-cap accounting at the depth boundary;
#   F  malformed and inactive shapes (R1).

`%||%` <- function(a, b) if (is.null(a)) b else a

# >>> b10-canonical: the .b10_canon() family copied verbatim from the
# b10-canonical block of tools/ws2/capture_results.R / tests/testthat/
# test-ir-results.R (at cb5907c); this probe always passes 17 significant
# digits. The same copy is in tests/testthat/test-ir-v1-compat.R.
.b10_num <- function(v, digits) {
  v <- as.vector(unclass(v))
  s <- sprintf(paste0("%.", digits, "g"), v)
  s[is.na(v)] <- NA_character_
  s[is.nan(v)] <- "NaN"
  s
}
.b10_attrs <- function(x, drop, digits) {
  a <- attributes(x)
  a <- a[setdiff(names(a), drop)]
  if (!length(a)) return(NULL)
  a <- a[order(names(a))]
  lapply(a, .b10_canon, digits = digits)
}
.b10_canon <- function(x, digits = 15L) {
  if (is.null(x)) return(list(kind = "null"))
  if (is.environment(x)) return(list(kind = "environment", class = I(class(x))))
  if (is.function(x)) return(list(kind = "function"))
  if (is.data.frame(x)) {
    rn <- if (.row_names_info(x, type = 1L) < 0L) NULL else
      I(as.character(attr(x, "row.names")))
    return(list(kind = "data.frame", class = I(class(x)),
                names = I(as.character(names(x))), nrow = nrow(x),
                row_names = rn,
                columns = lapply(seq_along(x), function(j) .b10_canon(x[[j]], digits)),
                attrs = .b10_attrs(x, c("names", "row.names", "class"), digits)))
  }
  if (is.list(x)) {
    return(list(kind = "list", class = I(class(x)),
                names = if (is.null(names(x))) NULL else I(as.character(names(x))),
                items = lapply(seq_along(x), function(i) .b10_canon(x[[i]], digits)),
                attrs = .b10_attrs(x, c("names", "class"), digits)))
  }
  if (is.atomic(x)) {
    lev <- NULL
    if (is.factor(x)) {
      lev <- I(as.character(levels(x)))
      vals <- as.character(x)
    } else if (inherits(x, "Date")) {
      vals <- format(x, "%Y-%m-%d")
    } else if (inherits(x, "POSIXt")) {
      vals <- format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
    } else if (inherits(x, "integer64")) {
      vals <- if (requireNamespace("bit64", quietly = TRUE)) {
        bit64::as.character.integer64(x)
      } else paste0("integer64-bits:", .b10_num(unclass(x), 17L))
    } else if (is.double(x)) {
      vals <- .b10_num(x, digits)
    } else if (is.complex(x)) {
      vals <- paste0(.b10_num(Re(x), digits), "|", .b10_num(Im(x), digits))
      vals[is.na(x)] <- NA_character_
    } else if (is.raw(x)) {
      vals <- as.character(x)
    } else if (is.character(x)) {
      vals <- enc2utf8(as.vector(x))
    } else {
      vals <- as.vector(unclass(x))
    }
    vals <- as.vector(vals)
    attributes(vals) <- NULL
    return(list(kind = "atomic", class = I(class(x)), type = typeof(x),
                length = length(x), levels = lev, values = I(vals),
                names = if (is.null(names(x))) NULL else I(as.character(names(x))),
                attrs = .b10_attrs(x, c("names", "class", "levels"), digits)))
  }
  list(kind = "other", class = I(class(x)), type = typeof(x))
}
# <<< b10-canonical

# ---- small helpers --------------------------------------------------------------

.cc_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null",
                                digits = NA, force = TRUE))
}
.cc_sha <- function(txt) unclass(as.character(openssl::sha256(charToRaw(enc2utf8(txt)))))
# sha256 of the lossless canonical form of an R object (plans and results)
.cc_canon_sha <- function(x) .cc_sha(.cc_json(.b10_canon(x, 17L)))
# the transport decoding of .ds_arg()
.cc_decode <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE)

# Volatile parts of messages: reserved temp-table suffixes, safe-bin keys,
# staging tokens, the session tempdir.
.cc_neutralise <- function(x) {
  if (!length(x)) return(character(0))
  x <- gsub("stg_[0-9a-f]{32}", "<token>", x)
  x <- gsub("bin_[0-9a-f]{24}", "bin_<rand>", x)
  x <- gsub("_[0-9a-f]{12}(?![0-9a-f])", "_<rand>", x, perl = TRUE)
  tmp <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  x <- gsub(tmp, "<tempdir>", x, fixed = TRUE)
  x
}

# ---- the case catalogue -----------------------------------------------------------

.CC_DEFAULT_OPTIONS <- list(nfilter.subset = 0L, dsomop.query_strict = TRUE)

# JSON literal of a string
.cc_q <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))

.cc_exponent <- function(v) {
  if (v == 0) return("0e0")
  e <- floor(log10(abs(v)))
  paste0(format(v / 10^e, digits = 15), "e", e)
}

# The variants of an integer (kind "integer"/"concept") or number (kind
# "number") field, as JSON literals. `alt` is a different valid value: evidence
# that the executor reads the field. A number control that is not an integer
# has no integer-valued-double/fraction variants; it gets `other_fraction`, a
# finite number of the same type that is not the control.
.cc_variants <- function(control, kind = "integer", alt = NULL, other = NULL) {
  q <- .cc_q
  v <- control
  integral <- v == round(v)
  vv <- if (integral) format(v, scientific = FALSE) else format(v, digits = 15)
  out <- list(control = vv,
              alt = if (!is.null(alt)) format(alt, scientific = FALSE, digits = 15))
  out$str <- q(vv)
  out$str_signed <- q(if (v < 0) vv else paste0("+", vv))
  out$str_exponent <- q(.cc_exponent(v))
  out$str_spaces <- q(paste0(" ", vv, " "))
  out$str_hex <- q(if (integral) {
    paste0(if (v < 0) "-" else "", "0x", toupper(format(as.hexmode(abs(v)))))
  } else sprintf("%a", v))
  if (integral) {
    out$double <- paste0(vv, ".0")
    out$fraction <- paste0(vv, ".5")
  } else {
    out$other_fraction <- format(other %||% (v + 0.25), digits = 15)
  }
  out$str_alpha <- q("abc")
  out$boolean <- "true"
  out$empty_array <- "[]"
  Filter(Negate(is.null), out)
}

.CC_VARIANT_TEXT <- c(
  control = "the control value", alt = "a different valid value (reach evidence)",
  str = "integer/number-valued numeric string", str_signed = "numeric string with an explicit sign",
  str_exponent = "numeric string in exponent form", str_spaces = "numeric string with spaces",
  str_hex = "hexadecimal string", double = "integer-valued double written as a double",
  fraction = "fractional number", other_fraction = "another finite number (not the control)",
  str_alpha = "non-numeric string", boolean = "a boolean", empty_array = "an empty array")

.cc_catalogue <- function() {
  cases <- list()
  add <- function(case) {
    case$plan <- as.character(jsonlite::minify(case$plan))
    case$options <- case$options %||% .CC_DEFAULT_OPTIONS
    plan <- .cc_decode(case$plan)
    case$out <- case$out %||% stats::setNames(
      as.list(paste0(names(plan$outputs), "_df")), names(plan$outputs))
    if (case$id %in% vapply(cases, `[[`, "", "id")) stop("duplicate case id ", case$id)
    cases[[length(cases) + 1L]] <<- case
  }
  sub_v <- function(template, value) gsub("@V@", value, template, fixed = TRUE)
  OPT <- '"options":{"translate_concepts":false,"block_sensitive":true}'
  C1 <- '"cohort":{"type":"cohort_table","cohort_definition_id":1}'
  DEMO <- '"outputs":{"demo":{"type":"baseline","columns":"gender_concept_id"}}'
  SS <- function(n_bins = "3", concept_id = "3004410") {
    paste0('{"table":"measurement","column":"value_as_number","concept_id":', concept_id,
           ',"concept_col":null,"n_bins":', n_bins, '}')
  }
  BINS <- list(issue_bins = list(table = "measurement", column = "value_as_number",
                                 concept_id = 3004410L, n_bins = 3L, breaks = c(0, 7.5, 10, 20)))
  tree_plan <- function(tree) paste0('{"cohort":{"filter_tree":', tree, '},', DEMO, ',', OPT, '}')
  el_plan <- function(extra, table = "condition_occurrence",
                      columns = '["person_id","condition_concept_id"]', cohort = NULL) {
    paste0('{', if (!is.null(cohort)) paste0(cohort, ','),
           '"outputs":{"e":{"type":"event_level","table":"', table, '","columns":', columns,
           ',', extra, '}},', OPT, '}')
  }
  rf_plan <- function(lower = "7.5", upper = "10", ss = SS()) {
    el_plan(paste0('"concept_set":[3004410],"filters":{"custom":{"var":"value_as_number",',
                   '"op":"value_bin","value":{"lower":', lower, ',"upper":', upper,
                   '},"safe_scope":', ss, '}}'),
            table = "measurement", columns = '["person_id","value_as_number"]')
  }
  hm_tree <- function(min = "7.5", max = "10", ss = SS()) {
    paste0('{"type":"has_measurement","params":{"concept_id":3004410,"min_value":', min,
           ',"max_value":', max, ',"safe_scope":', ss, '}}')
  }
  iv_plan <- function(extra) {
    paste0('{', C1, ',"outputs":{"iv":{"type":"intervals_long","tables":"condition_occurrence"',
           extra, '}},', OPT, '}')
  }
  tc_plan <- function(type, bw, ws, we) {
    if (identical(type, "temporal_covariates")) {
      paste0('{', C1, ',"outputs":{"tc":{"type":"temporal_covariates","table":"condition_occurrence",',
             '"concept_set":[201820,255573],"bin_width":', bw, ',"window_start":', ws,
             ',"window_end":', we, '}},', OPT, '}')
    } else {
      paste0('{', C1, ',"outputs":{"pp":{"type":"person_period","table":"condition_occurrence",',
             '"concept_set":[201820],"bin_width":', bw, ',"window_start":', ws,
             ',"window_end":', we, ',"grain":"episode","time_origin":"index"}},', OPT, '}')
    }
  }
  surv_plan <- function(tar = '{"start_offset":0,"end_offset":730}', extra = "",
                        concept_set = "4000002") {
    paste0('{', C1, ',"outputs":{"tte":{"type":"survival","outcome":{"table":"condition_occurrence",',
           '"concept_set":', concept_set, '},"tar":', tar, ',"event_order":"first"', extra, '}},',
           OPT, '}')
  }
  ms_plan <- function(state_step = NULL, transitions = '{"index":"mi","mi":[]}') {
    paste0('{"cohort":{"type":"cohort_table","cohort_definition_id":10},"outputs":{"course":',
           '{"type":"survival","outcomes":{"mi":{"table":"condition_occurrence","concept_set":4000002}},',
           '"tar":{"start_offset":0,"end_offset":730},"format":"multi_state","event_order":"all",',
           '"washout_days":0,"tie_policy":"priority","transitions":', transitions,
           ',"initial_state":"index"', if (!is.null(state_step)) paste0(',"state_step":', state_step),
           '}},', OPT, '}')
  }
  inc_plan <- function(count = "1", start = "-365", end = "730", concept = "3004410") {
    paste0('{"cohort":{"type":"spec","spec":{"type":"condition","concept_set":[201820],',
           '"inclusion_criteria":{"c1":{"table":"measurement","concept_set":[', concept, '],',
           '"temporal":{"index_window":{"start":', start, ',"end":', end, '}},',
           '"occurrence":{"type":"at_least","count":', count, '}}}}},', DEMO, ',', OPT, '}')
  }
  svb_plan <- function(lower = "7.5", upper = "10") {
    paste0('{"cohort":{"type":"spec","spec":{"type":"measurement","concept_set":[3004410],',
           '"value_bin":{"lower":', lower, ',"upper":', upper, ',"safe_scope":', SS(), '}}},',
           DEMO, ',', OPT, '}')
  }
  pop_plan <- function(pops, population_id) {
    paste0('{"populations":{"base":{"id":"base","kind":"criteria"},', pops, '},',
           '"outputs":{"demo":{"type":"baseline","columns":"gender_concept_id","population_id":"',
           population_id, '"}},', OPT, '}')
  }
  feat_plan <- function(feature) {
    paste0('{', C1, ',"outputs":{"p":{"type":"person_level","tables":{"condition_occurrence":',
           '{"features":[', feature, ']}}}},', OPT, '}')
  }
  ef_plan <- function(feature) {
    paste0('{', C1, ',"outputs":{"f":{"type":"event_level","table":"condition_occurrence",',
           '"temporal":{"index_window":{"start":-730,"end":730}},"representation":{"format":"features",',
           '"grain":"episode","features":[', feature, ']}}},', OPT, '}')
  }
  oo_plan <- function(options) {
    paste0('{', C1, ',"outputs":{"demo":{"type":"baseline","columns":"gender_concept_id",',
           '"options":', options, '}},', OPT, '}')
  }

  # --- A: integer / number scalars --------------------------------------------------
  # id, JSON path of the field, plan template (@V@ = the variant literal), kind,
  # control, alt, setup, what reads it in 2.10.2
  A <- list(
    list("version", "/version",
         paste0('{"version":@V@,', C1, ',', DEMO, ',', OPT, '}'), "integer", 1, NULL, NULL,
         "not read by the 2.10.2 executor"),
    list("options.min_persons", "/options/min_persons",
         paste0('{', C1, ',', DEMO, ',"options":{"translate_concepts":false,"block_sensitive":true,"min_persons":@V@}}'),
         "integer", 5, NULL, NULL, "removed option; not read"),
    list("harmonization.version", "/harmonization/version",
         paste0('{', C1, ',', DEMO, ',', OPT, ',"harmonization":{"version":@V@}}'), "integer", 1, NULL, NULL,
         "client-side federation evidence; not read"),
    list("cohort.cohort_definition_id", "/cohort/cohort_definition_id",
         paste0('{"cohort":{"type":"cohort_table","cohort_definition_id":@V@},', DEMO, ',', OPT, '}'),
         "integer", 1, 2, NULL, ".planResolveScopeCohort(): as.integer()"),
    list("cohort.cohort_definition_id.spec", "/cohort/cohort_definition_id",
         paste0('{"cohort":{"type":"spec","cohort_definition_id":@V@,"spec":{"type":"condition","concept_set":[201820]}},',
                '"outputs":{"cm":{"type":"cohort_membership"}},', OPT, '}'),
         "integer", 7, 8, NULL, ".cohortCreate(cohort_id) temp name; .extractCohortMembership()"),
    list("populations.cohort_definition_id", "/populations/p/cohort_definition_id",
         pop_plan('"p":{"id":"p","kind":"criteria","cohort_definition_id":@V@}', "p"),
         "integer", 1, 2, NULL, ".planResolvePopulations() -> .resolveCohortTable()"),
    list("scope.cohort", "/scope/cohort",
         paste0('{"scope":{"cohort":@V@},', DEMO, ',', OPT, '}'), "integer", 1, 2, NULL,
         ".planResolveScopeCohort() -> .resolveCohortTable()"),
    list("scope.cohort.item", "/scope/cohort/0",
         paste0('{"scope":{"cohort":[@V@]},', DEMO, ',', OPT, '}'), "integer", 1, 2, NULL,
         ".planResolveScopeCohort() -> .resolveCohortTable()"),
    # population filter-tree leaves (.compileCohortFilterLeaf(): integer_param(),
    # numeric_param(), .windowPredicateSql(), .conceptIdList())
    list("filter_tree.age_range.min", "/cohort/filter_tree/params/min",
         tree_plan('{"type":"age_range","params":{"min":@V@,"max":80,"reference_date":"2020-01-01"}}'),
         "integer", 50, 40, NULL, "integer_param(min, minimum 0)"),
    list("filter_tree.age_range.max", "/cohort/filter_tree/params/max",
         tree_plan('{"type":"age_range","params":{"min":0,"max":@V@,"reference_date":"2020-01-01"}}'),
         "integer", 50, 40, NULL, "integer_param(max, minimum 0)"),
    list("filter_tree.cohort.cohort_definition_id", "/cohort/filter_tree/params/cohort_definition_id",
         tree_plan('{"type":"cohort","params":{"cohort_definition_id":@V@}}'), "integer", 1, 2, NULL,
         "integer_param(cohort_definition_id, minimum 0)"),
    list("filter_tree.has_concept.min_count", "/cohort/filter_tree/params/min_count",
         tree_plan('{"type":"has_concept","params":{"table":"condition_occurrence","concept_ids":[201820,255573],"min_count":@V@}}'),
         "integer", 2, 1, NULL, "integer_param(min_count, minimum 1)"),
    list("filter_tree.concept_count.min_count", "/cohort/filter_tree/params/min_count",
         tree_plan('{"type":"concept_count","params":{"table":"condition_occurrence","concept_ids":[201820,255573],"min_count":@V@}}'),
         "integer", 2, 1, NULL, "integer_param(min_count, minimum 1)"),
    list("filter_tree.visit_count.min_count", "/cohort/filter_tree/params/min_count",
         tree_plan('{"type":"visit_count","params":{"min_count":@V@}}'), "integer", 1, NULL, NULL,
         "integer_param(min_count, minimum 1)"),
    list("filter_tree.prior_observation.min_days", "/cohort/filter_tree/params/min_days",
         tree_plan('{"type":"prior_observation","params":{"min_days":@V@,"reference_date":"2021-06-01"}}'),
         "integer", 365, NULL, NULL, "integer_param(min_days, minimum 0)"),
    list("filter_tree.followup.min_days", "/cohort/filter_tree/params/min_days",
         tree_plan('{"type":"followup","params":{"min_days":@V@,"reference_date":"2021-06-01"}}'),
         "integer", 30, NULL, NULL, "integer_param(min_days, minimum 0)"),
    list("filter_tree.window.start", "/cohort/filter_tree/params/window/start",
         tree_plan('{"type":"has_concept","params":{"table":"condition_occurrence","concept_id":201820,"window":{"start":@V@,"end":0},"reference_date":"2020-01-01"}}'),
         "integer", -365, NULL, NULL, ".windowPredicateSql() coerce_offset()"),
    list("filter_tree.window.end", "/cohort/filter_tree/params/window/end",
         tree_plan('{"type":"has_concept","params":{"table":"condition_occurrence","concept_id":201820,"window":{"start":-365,"end":@V@},"reference_date":"2020-01-01"}}'),
         "integer", 0, NULL, NULL, ".windowPredicateSql() coerce_offset()"),
    list("filter_tree.has_measurement.min_value", "/cohort/filter_tree/params/min_value",
         tree_plan(hm_tree(min = "@V@")), "number", 7.5, 0, BINS,
         "numeric_param(); .assertSafeNumericBinContract()"),
    list("filter_tree.has_measurement.max_value", "/cohort/filter_tree/params/max_value",
         tree_plan(hm_tree(max = "@V@")), "number", 10, 20, BINS,
         "numeric_param(); .assertSafeNumericBinContract()"),
    list("filter_tree.safe_scope.n_bins", "/cohort/filter_tree/params/safe_scope/n_bins",
         tree_plan(hm_tree(ss = SS(n_bins = "@V@"))), "integer", 3, NULL, BINS,
         ".assertSafeNumericBinContract() n_bins"),
    list("filter_tree.safe_scope.concept_id", "/cohort/filter_tree/params/safe_scope/concept_id",
         tree_plan(hm_tree(ss = SS(concept_id = "@V@"))), "integer", 3004410, NULL, BINS,
         ".assertSafeNumericBinContract() concept_id; scope must match the concept"),
    # v1 row filter (custom DSL) value_bin operand and its safe scope
    list("row_filter.value_bin.lower", "/outputs/e/filters/custom/value/lower",
         rf_plan(lower = "@V@"), "number", 7.5, 0, BINS,
         ".assertCustomFilterSafe() / .compileFilter() as.numeric()"),
    list("row_filter.value_bin.upper", "/outputs/e/filters/custom/value/upper",
         rf_plan(upper = "@V@"), "number", 10, 20, BINS,
         ".assertCustomFilterSafe() / .compileFilter() as.numeric()"),
    list("row_filter.safe_scope.n_bins", "/outputs/e/filters/custom/safe_scope/n_bins",
         rf_plan(ss = SS(n_bins = "@V@")), "integer", 3, NULL, BINS,
         ".assertSafeNumericBinContract() n_bins"),
    list("row_filter.safe_scope.concept_id", "/outputs/e/filters/custom/safe_scope/concept_id",
         rf_plan(ss = SS(concept_id = "@V@")), "concept", 3004410, NULL, BINS,
         ".assertSafeNumericBinContract() concept_id"),
    # concept-id lists (one item; `empty_array` is then an array inside the list)
    list("event_level.concept_set.item", "/outputs/e/concept_set/0",
         el_plan('"concept_set":[@V@]'), "concept", 201820, 255573, NULL,
         ".compileSelect() concept_filter"),
    list("event_level.concept_set.scalar", "/outputs/e/concept_set",
         el_plan('"concept_set":@V@'), "concept", 201820, 255573, NULL,
         ".compileSelect() concept_filter (a JSON scalar for a one-item list)"),
    list("event_level.concept_set.concepts.item", "/outputs/e/concept_set/concepts/0",
         el_plan('"concept_set":{"concepts":[@V@]}'), "concept", 201820, 255573, NULL,
         ".vocabExpandConceptSet()"),
    list("event_level.concept_set.exclude.item", "/outputs/e/concept_set/exclude/0",
         el_plan('"concept_set":{"concepts":[201820,255573],"exclude":[@V@]}'), "concept", 255573, 201820, NULL,
         ".vocabExpandConceptSet() exclude"),
    list("event_level.filters.concept_set.ids.item", "/outputs/e/filters/concept_set/ids/0",
         el_plan('"filters":{"concept_set":{"ids":[@V@]}}'), "concept", 201820, 255573, NULL,
         ".compileSelect() concept_filter"),
    list("event_level.filters.visit.concept_ids.item", "/outputs/e/filters/visit/concept_ids/0",
         el_plan('"filters":{"visit":{"concept_ids":[@V@]}}', table = "procedure_occurrence",
                 columns = '["person_id","procedure_concept_id"]'), "concept", 9201, NULL, NULL,
         ".compileSelect() visit_filter"),
    list("event_level.filters.visit.visit_concept_id.item", "/outputs/e/filters/visit/visit_concept_id/0",
         el_plan('"filters":{"visit":{"visit_concept_id":[@V@]}}', table = "procedure_occurrence",
                 columns = '["person_id","procedure_concept_id"]'), "concept", 9201, NULL, NULL,
         ".compileSelect() visit_filter"),
    list("filter_tree.has_concept.concept_id", "/cohort/filter_tree/params/concept_id",
         tree_plan('{"type":"has_concept","params":{"table":"condition_occurrence","concept_id":@V@}}'),
         "concept", 201820, 255573, NULL, ".conceptIdList()"),
    list("filter_tree.has_concept.concept_ids.item", "/cohort/filter_tree/params/concept_ids/0",
         tree_plan('{"type":"has_concept","params":{"table":"condition_occurrence","concept_ids":[@V@]}}'),
         "concept", 201820, 255573, NULL, ".conceptIdList()"),
    list("filter_tree.visit_count.visit_concept_id", "/cohort/filter_tree/params/visit_concept_id",
         tree_plan('{"type":"visit_count","params":{"visit_concept_id":@V@}}'), "concept", 9201, NULL, NULL,
         ".conceptIdList()"),
    list("temporal_covariates.concept_set.item", "/outputs/tc/concept_set/0",
         paste0('{', C1, ',"outputs":{"tc":{"type":"temporal_covariates","table":"condition_occurrence",',
                '"concept_set":[@V@],"bin_width":90,"window_start":-365,"window_end":0}},', OPT, '}'),
         "concept", 201820, 255573, NULL, ".normalizeTemporalSqlConcepts()"),
    list("intervals_long.concept_filter", "/outputs/iv/concept_filter/condition_occurrence",
         iv_plan(',"concept_filter":{"condition_occurrence":@V@}'), "concept", 201820, 255573, NULL,
         ".compileIntervalsLongSql() .resolveConceptSet()"),
    list("survival.outcome.concept_set.item", "/outputs/tte/outcome/concept_set/0",
         surv_plan(concept_set = "[@V@]"), "concept", 4000002, 255573, NULL,
         ".compilePlanSurvivalSql() outcome concept set"),
    list("cohort.spec.concept_set.item", "/cohort/spec/concept_set/0",
         paste0('{"cohort":{"type":"spec","spec":{"type":"condition","concept_set":[@V@]}},', DEMO, ',', OPT, '}'),
         "concept", 201820, 255573, NULL, ".cohortCreate() .resolveConceptSet()"),
    list("inclusion.concept_set.item", "/cohort/spec/inclusion_criteria/c1/concept_set/0",
         inc_plan(concept = "@V@"), "concept", 3004410, NULL, NULL,
         ".applyInclusionCriteria() .resolveConceptSet()"),
    list("index_event.concept_set.item", "/populations/p/index_event/concept_set/0",
         pop_plan('"p":{"id":"p","kind":"criteria","index_event":{"table":"condition_occurrence","concept_set":[@V@],"primary_limit":"first"}}', "p"),
         "concept", 4000002, 317009, NULL, ".materializeIndexEventCohort()"),
    list("feature.concept_set.item", "/outputs/p/tables/condition_occurrence/features/0/concept_set/0",
         feat_plan('{"name":"dm","type":"boolean","concept_set":[@V@]}'), "concept", 201820, 255573, NULL,
         ".resolveFeatureSpecs()"),
    # feature time windows
    list("feature.time_window.start", "/outputs/f/representation/features/0/time_window/start",
         ef_plan('{"name":"dm","type":"boolean","concept_set":[201820],"time_window":{"start":@V@,"end":0}}'),
         "integer", -365, -100, NULL, ".toFeatures() feature time_window normalize_bound()"),
    list("feature.time_window.end", "/outputs/f/representation/features/0/time_window/end",
         ef_plan('{"name":"dm","type":"boolean","concept_set":[201820],"time_window":{"start":-365,"end":@V@}}'),
         "integer", 0, -300, NULL, ".toFeatures() feature time_window normalize_bound()"),
    # event_level temporal (.validateTemporalSpec(), .compileTemporalWhere(), .normalizeMinGap())
    list("temporal.index_window.start", "/outputs/e/temporal/index_window/start",
         el_plan('"temporal":{"index_window":{"start":@V@,"end":0}}', cohort = C1), "integer", -365, -100, NULL,
         ".temporalOffset()"),
    list("temporal.index_window.end", "/outputs/e/temporal/index_window/end",
         el_plan('"temporal":{"index_window":{"start":-365,"end":@V@}}', cohort = C1), "integer", 0, -300, NULL,
         ".temporalOffset()"),
    list("temporal.event_select.n", "/outputs/e/temporal/event_select/n",
         el_plan('"temporal":{"event_select":{"order":"first","by":"concept","n":@V@}}', cohort = C1),
         "integer", 1, 2, NULL, ".validateTemporalSpec() / .wrapEventSelect()"),
    list("temporal.min_gap", "/outputs/e/temporal/min_gap",
         el_plan('"temporal":{"min_gap":@V@}', cohort = C1), "integer", 30, 400, NULL, ".normalizeMinGap()"),
    list("temporal.min_gap.days", "/outputs/e/temporal/min_gap/days",
         el_plan('"temporal":{"min_gap":{"days":@V@,"by":"grain"}}', cohort = C1), "integer", 30, 400, NULL,
         ".normalizeMinGap()"),
    # baseline age grid
    list("baseline.age_breaks.item", "/outputs/demo/age_breaks/5",
         paste0('{', C1, ',"outputs":{"demo":{"type":"baseline","columns":"gender_concept_id",',
                '"derived":"age_at_index","age_breaks":[0,10,20,30,40,@V@,60,70,80]}},', OPT, '}'),
         "integer", 50, NULL, NULL, ".extractBaseline(age_breaks)"),
    # survival
    list("survival.tar.start_offset", "/outputs/tte/tar/start_offset",
         surv_plan(tar = '{"start_offset":@V@,"end_offset":730}'), "integer", 0, 30, NULL,
         ".normalizeSurvivalTar() .survivalExactInteger()"),
    list("survival.tar.end_offset", "/outputs/tte/tar/end_offset",
         surv_plan(tar = '{"start_offset":0,"end_offset":@V@}'), "integer", 730, 365, NULL,
         ".normalizeSurvivalTar() .survivalExactInteger()"),
    list("survival.washout_days", "/outputs/tte/washout_days",
         surv_plan(extra = ',"washout_days":@V@'), "integer", 0, 30, NULL, ".survivalExactInteger()"),
    list("survival.state_step", "/outputs/course/state_step",
         ms_plan(state_step = "@V@"), "number", 0.25, NULL, NULL, ".multistateStateStep()"),
    list("survival.transitions.edges.trans", "/outputs/course/transitions/edges/0/trans",
         ms_plan(transitions = '{"states":["index","mi"],"edges":[{"from":"index","to":"mi","trans":@V@}]}'),
         "integer", 1, NULL, NULL, ".multistateExactPositiveInteger()"),
    # intervals_long (.longitudinalInteger())
    list("intervals_long.window.start", "/outputs/iv/window/start",
         iv_plan(',"window":{"start":@V@,"end":365}'), "integer", -365, 0, NULL, ".longitudinalInteger()"),
    list("intervals_long.window.end", "/outputs/iv/window/end",
         iv_plan(',"window":{"start":-365,"end":@V@}'), "integer", 365, 0, NULL, ".longitudinalInteger()"),
    list("intervals_long.window.at", "/outputs/iv/window/at",
         iv_plan(',"interval_match":"active_at","window":{"at":@V@}'), "integer", 0, 400, NULL,
         ".longitudinalInteger()"),
    list("intervals_long.select_n", "/outputs/iv/select_n",
         iv_plan(',"event_select":"first","select_n":@V@'), "integer", 1, 2, NULL, ".longitudinalInteger()"),
    list("intervals_long.anchor", "/outputs/iv/anchor",
         iv_plan(',"event_select":"nearest","anchor":@V@'), "integer", 9000, 0, NULL, ".longitudinalInteger()"),
    # temporal_covariates / person_period
    list("temporal_covariates.bin_width", "/outputs/tc/bin_width",
         tc_plan("temporal_covariates", "@V@", "-365", "0"), "integer", 90, 30, NULL, "integer_setting()"),
    list("temporal_covariates.window_start", "/outputs/tc/window_start",
         tc_plan("temporal_covariates", "90", "@V@", "0"), "integer", -365, -180, NULL, "integer_setting()"),
    list("temporal_covariates.window_end", "/outputs/tc/window_end",
         tc_plan("temporal_covariates", "90", "-365", "@V@"), "integer", 0, 30, NULL, "integer_setting()"),
    list("person_period.bin_width", "/outputs/pp/bin_width",
         tc_plan("person_period", "@V@", "-90", "0"), "integer", 30, 15, NULL, "integer_setting()"),
    list("person_period.window_start", "/outputs/pp/window_start",
         tc_plan("person_period", "30", "@V@", "0"), "integer", -90, -60, NULL, "integer_setting()"),
    list("person_period.window_end", "/outputs/pp/window_end",
         tc_plan("person_period", "30", "-90", "@V@"), "integer", 0, 30, NULL, "integer_setting()"),
    # inline spec: inclusion criteria and value_bin
    list("inclusion.occurrence.count", "/cohort/spec/inclusion_criteria/c1/occurrence/count",
         inc_plan(count = "@V@"), "integer", 1, 0, NULL, ".applyInclusionCriteria() occ_count"),
    list("inclusion.index_window.start", "/cohort/spec/inclusion_criteria/c1/temporal/index_window/start",
         inc_plan(start = "@V@"), "integer", -365, 400, NULL, ".applyInclusionCriteria() normalize_offset()"),
    list("inclusion.index_window.end", "/cohort/spec/inclusion_criteria/c1/temporal/index_window/end",
         inc_plan(end = "@V@"), "integer", 730, 300, NULL, ".applyInclusionCriteria() normalize_offset()"),
    list("cohort.spec.value_bin.lower", "/cohort/spec/value_bin/lower",
         svb_plan(lower = "@V@"), "number", 7.5, 0, BINS, ".cohortCreate() as.numeric(); bin contract"),
    list("cohort.spec.value_bin.upper", "/cohort/spec/value_bin/upper",
         svb_plan(upper = "@V@"), "number", 10, 20, BINS, ".cohortCreate() as.numeric(); bin contract"),
    # advisory output options (closed shape; no 2.10.2 path reads them)
    list("output.options.window.start", "/outputs/demo/options/window/start",
         oo_plan('{"window":{"start":@V@}}'), "integer", 0, NULL, NULL, "advisory; not read"),
    list("output.options.window.end", "/outputs/demo/options/window/end",
         oo_plan('{"window":{"end":@V@}}'), "integer", 365, NULL, NULL, "advisory; not read"),
    list("output.options.window.at", "/outputs/demo/options/window/at",
         oo_plan('{"window":{"at":@V@}}'), "integer", 0, NULL, NULL, "advisory; not read"),
    list("output.options.tar.start_offset", "/outputs/demo/options/tar/start_offset",
         oo_plan('{"tar":{"start_offset":@V@}}'), "integer", 0, NULL, NULL, "advisory; not read"),
    list("output.options.tar.end_offset", "/outputs/demo/options/tar/end_offset",
         oo_plan('{"tar":{"end_offset":@V@}}'), "integer", 730, NULL, NULL, "advisory; not read"),
    list("output.options.transitions.edges.trans", "/outputs/demo/options/transitions/edges/0/trans",
         oo_plan('{"transitions":{"states":["a","b"],"edges":[{"from":"a","to":"b","trans":@V@}]}}'),
         "integer", 1, NULL, NULL, "advisory; not read")
  )
  # Extra option sets for some fields: the 2.10.2 coercion of the filter-leaf
  # ages and windows also runs through the disclosure classifier when the gate
  # is on (.classifyFilter(): age_range width, filter windows), so they are
  # captured again at nfilter.subset = 3; the baseline age bands are captured
  # again at nfilter.tab = 0, where 2.10.2 suppressed no band (the reviewed F-26
  # correction removes that per-band suppression), so the coercion of
  # age_breaks is compared without the F-26 difference.
  OPTION_SETS <- list(
    default = list(suffix = NULL, title = NULL,
                   options = list(nfilter.subset = 0L, dsomop.query_strict = TRUE)),
    k3 = list(suffix = ".k3", title = ", nfilter.subset = 3",
              options = list(nfilter.subset = 3L, dsomop.query_strict = TRUE)),
    tab0 = list(suffix = ".tab0", title = ", nfilter.tab = 0",
                options = list(nfilter.subset = 0L, nfilter.tab = 0L, dsomop.query_strict = TRUE)))
  EXTRA_SETS <- list("filter_tree.age_range.min" = "k3", "filter_tree.age_range.max" = "k3",
                     "filter_tree.window.start" = "k3", "filter_tree.window.end" = "k3",
                     "baseline.age_breaks.item" = "tab0")
  for (f in A) {
    names(f) <- c("key", "path", "template", "kind", "control", "alt", "setup", "reads")
    variants <- .cc_variants(f$control, f$kind, alt = f$alt)
    for (set in c("default", EXTRA_SETS[[f$key]])) {
      os <- OPTION_SETS[[set]]
      for (vn in names(variants)) {
        lit <- variants[[vn]]
        add(list(id = paste0("A.", f$key, os$suffix, ".", vn), section = "A", field = f$path,
                 field_key = paste0(f$key, os$suffix), kind = f$kind, variant = vn, value = lit,
                 title = paste0(f$key, " = ", lit, " (", .CC_VARIANT_TEXT[[vn]], ")", os$title),
                 reads = f$reads, plan = sub_v(f$template, lit), setup = f$setup,
                 options = os$options, template = f$template, template_key = f$key))
      }
    }
  }

  # --- B: predicate literals of v1 custom filters ------------------------------------
  ops <- c("==", "!=", ">", "in", "not_in", "between")
  op_key <- c("==" = "eq", "!=" = "ne", ">" = "gt", "in" = "in", "not_in" = "not_in",
              "between" = "between")
  b_value <- function(op, lit, second) {
    if (op %in% c("in", "not_in")) paste0("[", lit, "]")
    else if (identical(op, "between")) paste0("[", lit, ",", second, "]")
    else lit
  }
  b_filter <- function(var, op, value) {
    paste0('"filters":{"custom":{"var":"', var, '","op":"', op, '","value":', value, '}}')
  }
  B_concept <- c(int = "201820", str = '"201820"', str_decimal = '"201820.0"', str_exponent = '"2e5"',
                 str_alpha = '"abc"', boolean = "true", fraction = "201820.5")
  for (op in ops) for (ln in names(B_concept)) {
    lit <- B_concept[[ln]]
    add(list(id = paste0("B.condition_concept_id.", op_key[[op]], ".", ln), section = "B",
             field = "/outputs/e/filters/custom/value", variant = ln, value = lit, op = op,
             title = paste0("event_level condition_concept_id ", op, " ", lit),
             plan = el_plan(b_filter("condition_concept_id", op, b_value(op, lit, "255573")))))
  }
  # the executed membership operators again at the default threshold
  for (op in c("in", "not_in")) for (ln in names(B_concept)) {
    lit <- B_concept[[ln]]
    add(list(id = paste0("B.condition_concept_id.", op_key[[op]], ".k3.", ln), section = "B",
             field = "/outputs/e/filters/custom/value", variant = ln, value = lit, op = op,
             title = paste0("event_level condition_concept_id ", op, " ", lit, ", nfilter.subset = 3"),
             plan = el_plan(b_filter("condition_concept_id", op, b_value(op, lit, "255573"))),
             options = list(nfilter.subset = 3L, dsomop.query_strict = TRUE)))
  }
  for (ln in names(B_concept)) {
    lit <- B_concept[[ln]]
    add(list(id = paste0("B.population.filter_tree.has_concept.", ln), section = "B",
             field = "/populations/p/filter_tree/params/concept_id", variant = ln, value = lit,
             title = paste0("population filter_tree has_concept concept_id = ", lit),
             plan = pop_plan(paste0('"p":{"id":"p","kind":"criteria","filter_tree":{"type":"has_concept",',
                                    '"params":{"table":"condition_occurrence","concept_id":', lit, '}}}'), "p")))
    add(list(id = paste0("B.population.filters.in.", ln), section = "B",
             field = "/populations/p/filters/value", variant = ln, value = lit, op = "in",
             title = paste0("population row filter condition_concept_id in [", lit, "]"),
             plan = pop_plan(paste0('"p":{"id":"p","kind":"criteria","filter_tree":{"type":"sex","params":{"value":"F"}},',
                                    '"filters":{"var":"condition_concept_id","op":"in","value":[', lit, ']}}'), "p")))
  }
  B_number <- c(int = "5", str = '"5"', str_decimal = '"5.5"', str_alpha = '"abc"', boolean = "true")
  for (op in ops) for (ln in names(B_number)) {
    lit <- B_number[[ln]]
    add(list(id = paste0("B.value_as_number.", op_key[[op]], ".", ln), section = "B",
             field = "/outputs/e/filters/custom/value", variant = ln, value = lit, op = op,
             title = paste0("event_level measurement value_as_number ", op, " ", lit),
             plan = el_plan(b_filter("value_as_number", op, b_value(op, lit, "100")),
                            table = "measurement", columns = '["person_id","value_as_number"]')))
  }
  # text: the vocabulary table concept (a safe non-person table) has the text
  # column concept_code (values such as "1" and "2") and the approved
  # categorical text column vocabulary_id; person-keyed source values are
  # blocked columns
  B_text <- c(int = "1", str = '"1"')
  for (op in ops) for (ln in names(B_text)) {
    lit <- B_text[[ln]]
    add(list(id = paste0("B.concept_code.", op_key[[op]], ".", ln), section = "B",
             field = "/outputs/e/filters/custom/value", variant = ln, value = lit, op = op,
             title = paste0("event_level concept.concept_code (text) ", op, " ", lit),
             plan = el_plan(b_filter("concept_code", op, b_value(op, lit, '"9"')), table = "concept",
                            columns = '["concept_id","concept_code","vocabulary_id"]')))
  }
  add(list(id = "B.concept_table.no_filter", section = "B", field = "/outputs/e",
           variant = "control", value = NA_character_,
           title = "event_level over the vocabulary table concept without a filter (control of the text cases)",
           plan = el_plan('"representation":{"format":"long"}', table = "concept",
                          columns = '["concept_id","concept_code","vocabulary_id"]')))
  B_vocab <- c(str = '"Race"', int = "1", str_numeric = '"1"', boolean = "true")
  for (op in c("in", "not_in")) for (ln in names(B_vocab)) {
    lit <- B_vocab[[ln]]
    add(list(id = paste0("B.vocabulary_id.", op_key[[op]], ".", ln), section = "B",
             field = "/outputs/e/filters/custom/value", variant = ln, value = lit, op = op,
             title = paste0("event_level concept.vocabulary_id (text) ", op, " [", lit, "]"),
             plan = el_plan(b_filter("vocabulary_id", op, b_value(op, lit, NULL)), table = "concept",
                            columns = '["concept_id","concept_code","vocabulary_id"]')))
  }
  for (op in c("==", "in")) {
    add(list(id = paste0("B.cause_source_value.", op_key[[op]], ".str"), section = "B",
             field = "/outputs/e/filters/custom/value", variant = "str", value = '"I21"', op = op,
             title = paste0("event_level death.cause_source_value (blocked text) ", op, " \"I21\""),
             plan = el_plan(b_filter("cause_source_value", op, b_value(op, '"I21"', NULL)), table = "death",
                            columns = '["person_id","death_date"]')))
  }
  B_date <- c(iso = '"2019-06-01"', slashes = '"2019/06/01"', number = "20190601")
  B_date_range <- c(iso = '["2019-01-01","2019-12-31"]', slashes = '["2019/01/01","2019/12/31"]',
                    number = "[20190101,20191231]")
  for (op in ops) for (ln in names(B_date)) {
    value <- if (identical(op, "between")) B_date_range[[ln]] else b_value(op, B_date[[ln]], NULL)
    add(list(id = paste0("B.condition_start_date.", op_key[[op]], ".", ln), section = "B",
             field = "/outputs/e/filters/custom/value", variant = ln, value = value, op = op,
             title = paste0("event_level condition_start_date ", op, " ", value),
             plan = el_plan(b_filter("condition_start_date", op, value))))
  }
  add(list(id = "B.value_bin.string_bounds.7.5_10", section = "B",
           field = "/outputs/e/filters/custom/value", variant = "str", value = '{"lower":"7.5","upper":"10"}',
           title = "value_bin with string bounds {\"7.5\", \"10\"} (issued edges)",
           plan = rf_plan(lower = '"7.5"', upper = '"10"'), setup = BINS))
  add(list(id = "B.value_bin.string_bounds.0_7.5", section = "B",
           field = "/outputs/e/filters/custom/value", variant = "str", value = '{"lower":"0","upper":"7.5"}',
           title = "value_bin with string bounds {\"0\", \"7.5\"} (issued edges)",
           plan = rf_plan(lower = '"0"', upper = '"7.5"'), setup = BINS))

  # --- C: concept_col override (event_level, long) -----------------------------------
  ids15 <- paste0("[", paste(1:15, collapse = ","), "]")
  c_plan <- function(concept_col, ids, columns, where = "output") {
    cc <- paste0('"concept_col":"', concept_col, '"')
    paste0('{"outputs":{"e":{"type":"event_level","table":"condition_occurrence",',
           if (identical(where, "output")) paste0(cc, ','),
           '"filters":{"concept_set":{"ids":', ids, '}', if (identical(where, "filters")) paste0(',', cc), '},',
           '"columns":', columns, ',"representation":{"format":"long"}}},', OPT, '}')
  }
  C <- list(
    list("person_id.ids1-15.columns_concept", "person_id", ids15, '["condition_concept_id"]', "output",
         "concept_col person_id, ids 1:15, columns [condition_concept_id]"),
    list("person_id.ids1-15.columns_concept_person", "person_id", ids15, '["condition_concept_id","person_id"]', "output",
         "concept_col person_id, ids 1:15, columns [condition_concept_id, person_id]"),
    list("person_id.ids1-15.filters_concept_col", "person_id", ids15, '["condition_concept_id"]', "filters",
         "filters.concept_col person_id, ids 1:15, columns [condition_concept_id]"),
    list("type_concept.44818518.columns_concept", "condition_type_concept_id", "[44818518]",
         '["condition_concept_id"]', "output",
         "concept_col condition_type_concept_id, ids [44818518], columns [condition_concept_id]"),
    list("type_concept.44818518.columns_type", "condition_type_concept_id", "[44818518]",
         '["condition_concept_id","condition_type_concept_id"]', "output",
         "concept_col condition_type_concept_id, ids [44818518], columns [condition_concept_id, condition_type_concept_id]"),
    list("type_concept.201820.columns_type", "condition_type_concept_id", "[201820]",
         '["condition_concept_id","condition_type_concept_id"]', "output",
         "concept_col condition_type_concept_id, ids [201820], columns [condition_concept_id, condition_type_concept_id]"),
    list("no_such_column.ids1-15", "no_such_column", ids15, '["condition_concept_id"]', "output",
         "concept_col no_such_column, ids 1:15, columns [condition_concept_id]")
  )
  for (k in c(3L, 0L)) for (f in C) {
    add(list(id = paste0("C.", f[[1]], ".k", k), section = "C", field = "/outputs/e/concept_col",
             variant = paste0("k", k), value = .cc_q(f[[2]]), title = paste0(f[[6]], ", nfilter.subset = ", k),
             plan = c_plan(f[[2]], f[[3]], f[[4]], f[[5]]), record_sql = TRUE,
             options = list(nfilter.subset = k, dsomop.query_strict = TRUE)))
  }

  # --- D: set operations and private intermediates ----------------------------------
  A_POP <- '"a":{"id":"a","kind":"criteria","filter_tree":{"type":"has_concept","params":{"table":"condition_occurrence","concept_id":201820}}}'
  B_POP <- '"b":{"id":"b","kind":"criteria","filter_tree":{"type":"has_concept","params":{"table":"condition_occurrence","concept_id":255573}}}'
  setop <- function(id, op, members) {
    paste0('"', id, '":{"id":"', id, '","kind":"setop","setop":{"op":"', op, '","members":[',
           paste0('"', members, '"', collapse = ","), ']}}')
  }
  Z_POP <- '"z":{"id":"z","kind":"criteria"}'
  D <- list(
    list("difference.a_b_b", paste(A_POP, B_POP, setop("d", "difference", c("a", "b", "b")), sep = ","), "d",
         "difference [a, b, b] published"),
    list("difference.a_b", paste(A_POP, B_POP, setop("d", "difference", c("a", "b")), sep = ","), "d",
         "difference [a, b] published"),
    list("setdiff.a_b_b", paste(A_POP, B_POP, setop("d", "setdiff", c("a", "b", "b")), sep = ","), "d",
         "setdiff [a, b, b] published"),
    list("difference.a_b_a", paste(A_POP, B_POP, setop("d", "difference", c("a", "b", "a")), sep = ","), "d",
         "difference [a, b, a] (empty) published"),
    list("difference.a_a.unused", paste(A_POP, B_POP, setop("d", "difference", c("a", "a")), sep = ","), "a",
         "self-difference [a, a] as an unused private intermediate; output over a"),
    list("difference.a_a.published", paste(A_POP, setop("d", "difference", c("a", "a")), sep = ","), "d",
         "self-difference [a, a] published"),
    list("bare_criteria.unused", paste(A_POP, Z_POP, sep = ","), "a",
         "bare non-base criteria population z unused; output over a"),
    list("bare_criteria.member", paste(A_POP, Z_POP, setop("d", "union", c("a", "z")), sep = ","), "d",
         "bare criteria population z as a private set-op member: union [a, z] published"),
    list("bare_criteria.published", paste(A_POP, Z_POP, sep = ","), "z",
         "bare non-base criteria population z published")
  )
  for (k in c(0L, 3L)) for (f in D) {
    add(list(id = paste0("D.", f[[1]], ".k", k), section = "D", field = "/populations",
             variant = paste0("k", k), value = NA_character_, title = paste0(f[[4]], ", nfilter.subset = ", k),
             plan = pop_plan(f[[2]], f[[3]]),
             options = list(nfilter.subset = k, dsomop.query_strict = TRUE)))
  }

  # --- E: predicate-cap accounting ----------------------------------------------------
  wrap <- function(leaf, n) {
    for (i in seq_len(n)) leaf <- paste0('{"and":[', leaf, ']}')
    leaf
  }
  VB_LEAF <- paste0('{"var":"value_as_number","op":"value_bin","value":{"lower":7.5,"upper":10},"safe_scope":', SS(), '}')
  e_plan <- function(depth, time_window = FALSE) {
    el_plan(paste0('"concept_set":[3004410],"filters":{"custom":', wrap(VB_LEAF, depth - 1L),
                   if (time_window) ',"time_window":{"date_column":"measurement_date","start_date":"2019-01-01","end_date":"2022-12-31"}',
                   '}'),
            table = "measurement", columns = '["person_id","value_as_number"]')
  }
  for (k in c(0L, 3L)) {
    add(list(id = paste0("E.value_bin.depth32.k", k), section = "E", field = "/outputs/e/filters/custom",
             variant = paste0("k", k), value = "32", setup = BINS,
             title = paste0("issued value_bin under 31 single-child and wrappers (depth 32), nfilter.subset = ", k),
             plan = e_plan(32L), options = list(nfilter.subset = k, dsomop.query_strict = TRUE)))
  }
  add(list(id = "E.value_bin.depth33.k0", section = "E", field = "/outputs/e/filters/custom", variant = "k0",
           value = "33", setup = BINS, title = "issued value_bin under 32 and wrappers (depth 33)",
           plan = e_plan(33L)))
  add(list(id = "E.value_bin.depth32.time_window.k0", section = "E", field = "/outputs/e/filters", variant = "k0",
           value = "32", setup = BINS,
           title = "depth-32 value_bin filter plus filters.time_window (lowering adds a conjunction)",
           plan = e_plan(32L, time_window = TRUE)))
  add(list(id = "E.value_bin.depth31.time_window.k0", section = "E", field = "/outputs/e/filters", variant = "k0",
           value = "31", setup = BINS, title = "depth-31 value_bin filter plus filters.time_window",
           plan = e_plan(31L, time_window = TRUE)))

  # --- F: malformed / inactive shapes (R1) --------------------------------------------
  FT <- '{"type":"sex","params":{"value":"F"}}'
  PEOPLE <- '"outputs":{"people":{"type":"person_level","tables":{"person":["person_id","gender_concept_id"]}}}'
  f_spec <- function(spec) paste0('{"cohort":{"filter_tree":', FT, ',"spec":', spec, '},', PEOPLE, ',', OPT, '}')
  hc_name <- function(name) {
    tree_plan(paste0('{"type":"has_concept","params":{"table":"condition_occurrence","concept_id":201820,',
                     '"concept_name":', name, '}}'))
  }
  F <- list(
    list("cohort.spec.private_true", f_spec('{"private":true}'), "cohort.spec {private: true} next to a filter_tree"),
    list("cohort.spec.private_null", f_spec('{"private":null}'), "cohort.spec {private: null} next to a filter_tree"),
    list("cohort.spec.concept_set_private_null",
         f_spec('{"type":"condition","concept_set":{"concepts":[1],"private":null}}'),
         "cohort.spec {type, concept_set: {concepts: [1], private: null}} next to a filter_tree"),
    list("cohort.spec.legacy_flat_array",
         paste0('{"cohort":{"type":"spec","spec":[{"type":"sex","params":{"value":"F"}},{"type":"age_range",',
                '"params":{"min":18,"max":65,"reference_date":"2024-07-01"}},{"type":"has_concept","params":',
                '{"concept_id":201820,"table":"condition_occurrence","min_count":2}}],"filter_tree":{"or":[',
                '{"type":"sex","params":{"value":"F"}},{"and":[{"type":"age_range","params":{"min":18,"max":65,',
                '"reference_date":"2024-07-01"}},{"type":"has_concept","params":{"concept_id":201820,',
                '"table":"condition_occurrence","min_count":2}}]}]}},"anchor":{"table":"person","id_col":"person_id"},',
                PEOPLE, ',', OPT, '}'),
         "legacy flat-array cohort.spec next to a filter_tree (corpus v1-054)"),
    list("has_concept.concept_name.string", hc_name('"Type 2 diabetes mellitus"'), "has_concept.params.concept_name a string"),
    list("has_concept.concept_name.array", hc_name('["Type 2 diabetes mellitus","T2DM"]'),
         "has_concept.params.concept_name an array of strings"),
    list("has_concept.concept_name.private_null", hc_name('{"private":null}'), "has_concept.params.concept_name {private: null}"),
    list("person_level.filters_true",
         paste0('{', C1, ',"outputs":{"people":{"type":"person_level","tables":{"person":["person_id","gender_concept_id"]},',
                '"filters":true}},', OPT, '}'), "person_level.filters = true"),
    list("event_level.filters_true", el_plan('"filters":true', cohort = C1), "event_level.filters = true"),
    list("population.setop_true",
         pop_plan(paste(A_POP, '"d":{"id":"d","kind":"setop","setop":true}', sep = ","), "a"),
         "population.setop = true (unused population; output over a)"),
    list("population.setop_true.published",
         pop_plan(paste(A_POP, '"d":{"id":"d","kind":"setop","setop":true}', sep = ","), "d"),
         "population.setop = true (published)"),
    list("cohort.spec_true", paste0('{"cohort":{"filter_tree":', FT, ',"spec":true},', PEOPLE, ',', OPT, '}'),
         "cohort.spec = true next to a filter_tree")
  )
  for (f in F) {
    add(list(id = paste0("F.", f[[1]]), section = "F", field = NA_character_, variant = "shape",
             value = NA_character_, title = f[[3]], plan = f[[2]], typed_refusal = TRUE))
  }
  cases
}

# ---- stage of a refusal (reviewed message rules) -----------------------------------

.CC_STAGE_RULES <- list(
  session = c("numeric bin was not issued for this resource session",
              "Named cohort scopes must be temporary cohorts created by this handle"),
  data = c("below disclosure threshold", "reduce population below", "fewer than",
           "insufficient individuals", "insufficient_support", "Publication refused",
           "resolved to no concepts", "resolves to no concepts", "refusing an unfiltered cohort",
           "did not resolve to an executable cohort", "has no materialized person set",
           "Disclosive: cohort", "too small", "suppress"),
  executor = c("no such column: NA", "incomplete input", "syntax error", "replacement has 0 rows"),
  site = c("not found in", "is unavailable", "no such column", "no such table",
           "is not permitted (identifier, blocked, or unknown column)",
           "has no usable", "has no domain concept column", "has no reviewed",
           "is not present in the CDM", "not supported", "lacks a reviewed",
           "cannot apply", "no usable concept column", "cannot scope table"),
  plan = c("must be one", "must be a", "must be an", "must contain", "must not be",
           "must span", "must be TRUE or FALSE", "must be between", "must be first or last",
           "must be grain or concept", "must be strictly", "must match", "must be M/MALE",
           "requires a", "require both", "requires both", "requires exactly", "require exactly",
           "Unknown", "unknown", "not permitted", "is not allowed", "is no longer executable",
           "IN/NOT IN is only permitted", "Numeric BETWEEN", "ISO date", "is not a valid date",
           "SQL literals must be", "Invalid ", "$ operator is invalid", "subscript out of bounds",
           "argument is of length zero", "invalid 'type'", "non-numeric argument",
           "missing value where TRUE/FALSE needed", "cannot be NULL", "nesting", "depth",
           "exceeds the server", "exceed", "only permits", "cannot be compiled",
           "value_bin requires", "Concept ID lists must", "concept_filter must contain",
           "cannot mix", "Cohort set operations accept only", "is not defined before it",
           "has no members", "invalid episode_policy", "non-empty", "uniquely named",
           "compiled to no predicate", "cannot apply event-row", "cannot declare",
           "Plan outputs must", "Unsupported output type", "does not match", "must be finite",
           "requires one or more", "requires temporal")
)

# fixed-string rules, tried in this order; the fixture builder re-derives the
# stage of every baseline refusal from its message with these rules
.cc_stage <- function(message) {
  for (stage in c("session", "data", "executor", "site", "plan")) {
    if (any(vapply(.CC_STAGE_RULES[[stage]], function(p) grepl(p, message, fixed = TRUE), logical(1)))) {
      return(stage)
    }
  }
  "unclassified"
}

# ---- running the catalogue ------------------------------------------------------------

.cc_db_state <- new.env(parent = emptyenv())

.cc_install_db_trace <- function() {
  st <- .cc_db_state
  st$depth <- 0L
  st$on <- FALSE
  st$log <- list()
  assign(".cc_db_enter", function(statement) {
    st$depth <- st$depth + 1L
    if (st$depth == 1L && isTRUE(st$on)) {
      txt <- tryCatch(paste(as.character(statement), collapse = " "), error = function(e) "<unavailable>")
      heads <- vapply(sys.calls(), function(cl) paste(deparse(cl[[1L]], nlines = 1L), collapse = ""), "")
      catalog <- any(grepl("(^|:)\\.buildBlueprint$", heads)) ||
        grepl("sqlite_master|sqlite_temp_master|^\\s*PRAGMA|information_schema|sqlite_version",
              txt, ignore.case = TRUE)
      st$log[[length(st$log) + 1L]] <- list(sql = txt, catalog = catalog)
    }
    invisible(NULL)
  }, envir = globalenv())
  assign(".cc_db_exit", function() {
    st$depth <- max(0L, st$depth - 1L)
    invisible(NULL)
  }, envir = globalenv())
  for (g in c("dbGetQuery", "dbExecute", "dbSendQuery", "dbSendStatement")) {
    suppressMessages(trace(g, where = asNamespace("DBI"), print = FALSE,
                           tracer = quote(get(".cc_db_enter", envir = globalenv())(statement)),
                           exit = quote(get(".cc_db_exit", envir = globalenv())())))
  }
  # Every explicit stop(): its message, the package frames on the stack and the
  # data statements issued so far. The 2.10.2 executor catches an output's error
  # in its own tryCatch and re-raises it, so a calling handler outside
  # .planExecute() would only see the re-raise; the FIRST stop() with the final
  # message is the origin of the refusal.
  st$stops <- list()
  assign(".cc_stop_enter", function(message, calls) {
    if (isTRUE(st$on) && length(st$stops) < 500L) {
      st$stops[[length(st$stops) + 1L]] <- list(message = message, data = .cc_data_statements(),
                                                origin = .cc_origin(calls))
    }
    invisible(NULL)
  }, envir = globalenv())
  suppressMessages(trace("stop", where = baseenv(), print = FALSE, tracer = quote(
    get(".cc_stop_enter", envir = globalenv())(
      tryCatch(if (...length() && inherits(..1, "condition")) conditionMessage(..1)
               else .makeMessage(..., domain = domain), error = function(e) NA_character_),
      sys.calls()))))
  invisible(TRUE)
}

.cc_data_statements <- function() {
  sum(!vapply(.cc_db_state$log, `[[`, logical(1), "catalog"))
}

.cc_option_state <- function() {
  o <- options()
  keep <- sort(grep("^(default\\.)?(nfilter|dsomop|dsstaging|datashield)\\.", names(o), value = TRUE))
  vals <- lapply(o[keep], function(v) .b10_canon(v, 17L))
  env <- Sys.getenv()
  ek <- sort(grep("^DSOMOP_", names(env), value = TRUE))
  list(options = vals, env = as.list(env[ek]))
}

.cc_setup <- function(handle, setup) {
  if (is.null(setup)) return(invisible(NULL))
  b <- setup$issue_bins
  if (!is.null(b)) {
    .test_issue_safe_bins(handle, breaks = as.numeric(unlist(b$breaks)), table = b$table,
                          column = b$column, concept_id = b$concept_id, concept_col = b$concept_col,
                          n_bins = b$n_bins)
  }
  invisible(NULL)
}

.cc_summary <- function(value) {
  if (!is.list(value)) return(NULL)
  lapply(value, function(x) {
    if (is.data.frame(x)) {
      pid <- intersect(c("person_id", "subject_id"), names(x))
      per <- if (length(pid)) table(as.character(x[[pid[[1L]]]])) else NULL
      list(kind = "data.frame", nrow = nrow(x), columns = I(names(x)),
           persons = if (!is.null(per)) length(per),
           rows_per_person = if (!is.null(per)) as.list(stats::setNames(as.integer(per), names(per))))
    } else if (is.list(x)) {
      list(kind = "list", components = I(names(x) %||% character(0)))
    } else list(kind = class(x)[[1L]])
  })
}

.cc_origin <- function(calls) {
  heads <- vapply(calls, function(cl) paste(deparse(cl[[1L]], nlines = 1L), collapse = ""), "")
  heads <- sub("^.*:::?", "", heads)
  heads <- heads[grepl("^\\.[A-Za-z]", heads) &
                   !grepl("^\\.(cc_|b10_|handleSimpleError|signalSimple|doTrace|Call|External|Internal|Primitive)", heads)]
  utils::tail(heads, 4L)
}

.cc_run_case <- function(case) {
  plan <- .cc_decode(case$plan)
  out_symbols <- lapply(case$out, as.character)
  handle <- suppressWarnings(suppressMessages(create_test_handle()))
  on.exit(try(cleanup_handle(handle), silent = TRUE), add = TRUE)
  .cc_setup(handle, case[["setup"]])
  warnings <- character(0)
  signals <- list()
  st <- .cc_db_state
  st$log <- list()
  st$stops <- list()
  st$depth <- 0L
  started <- Sys.time()
  opts <- case$options
  st$on <- TRUE
  outcome <- withr::with_options(opts, tryCatch(
    list(value = withCallingHandlers(
      .planExecute(handle, plan, out_symbols),
      error = function(e) {
        signals[[length(signals) + 1L]] <<- list(cond = e, data = .cc_data_statements(),
                                                 origin = .cc_origin(sys.calls()))
      },
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage"))),
    error = function(e) list(error = e)))
  st$on <- FALSE
  rec <- list(id = case$id, seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 3),
              plan_sha256 = .cc_canon_sha(plan), warnings = I(.cc_neutralise(unique(warnings))))
  if (!is.null(outcome$error)) {
    e <- outcome$error
    raw_msg <- conditionMessage(e)
    hit <- Filter(function(s) identical(s$message, raw_msg), st$stops)
    if (!length(hit)) hit <- Filter(function(s) identical(s$cond, e), signals)
    first <- if (length(hit)) hit[[1L]] else NULL
    msg <- .cc_neutralise(raw_msg)
    call <- conditionCall(e)
    rec$status <- "error"
    rec$error <- list(class = I(class(e)), message = msg,
                      code = if (is.character(e$code)) e$code,
                      call = if (!is.null(call)) paste(deparse(call, nlines = 2L), collapse = " "),
                      data_statements_before = if (!is.null(first)) first$data,
                      refused_before_query = if (!is.null(first)) identical(first$data, 0L),
                      origin = I(if (!is.null(first)) first$origin else character(0)),
                      stage = .cc_stage(msg))
  } else {
    rec$status <- "ok"
    canon <- .cc_json(.b10_canon(outcome$value, 17L))
    rec$result_sha256 <- .cc_sha(canon)
    rec$result <- canon
    rec$summary <- .cc_summary(outcome$value)
  }
  if (isTRUE(case[["record_sql"]])) {
    data <- Filter(function(s) !isTRUE(s$catalog), st$log)
    rec$sql <- I(.cc_neutralise(vapply(data, `[[`, "", "sql")))
  }
  # the plan-only validator of a 2.10.3 build (no handle)
  if (exists(".irPrepare", mode = "function")) {
    ir <- withr::with_options(opts, tryCatch({
      suppressWarnings(suppressMessages(.irPrepare(plan)))
      list(status = "accept")
    }, error = function(e) list(status = "refuse", class = I(class(e)),
                                message = .cc_neutralise(conditionMessage(e)))))
    rec$ir <- ir
  }
  rec
}

.cc_run <- function(src, out_path, label = "run") {
  src <- normalizePath(src, mustWork = TRUE)
  out_path <- file.path(normalizePath(dirname(out_path), mustWork = TRUE), basename(out_path))
  setwd(src)
  suppressWarnings(suppressMessages(
    pkgload::load_all(src, helpers = TRUE, export_all = TRUE, quiet = TRUE)))
  .cc_install_db_trace()
  base_state <- .cc_option_state()
  cases <- .cc_catalogue()
  # development aid: CC_FILTER=<regex on case ids> runs a subset (never for a fixture)
  only <- Sys.getenv("CC_FILTER")
  if (nzchar(only)) cases <- Filter(function(x) grepl(only, x$id), cases)
  results <- vector("list", length(cases))
  for (i in seq_along(cases)) {
    results[[i]] <- .cc_run_case(cases[[i]])
    if (i %% 50L == 0L) cat("[capture_compat]", label, i, "/", length(cases), "\n")
  }
  run <- list(version = 1L, kind = "capture_compat run", label = label, source = src,
              dsomop_version = as.character(utils::packageVersion("dsOMOP")),
              r_version = R.version.string, created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
              probe_sha256 = .cc_probe_sha(), option_state = base_state,
              cases = cases, results = results)
  writeLines(.cc_json(run), out_path, useBytes = TRUE)
  cat("[capture_compat] wrote", length(results), "case outcomes to", out_path, "\n")
  invisible(run)
}

.cc_probe_file <- function() {
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(f)) return(NA_character_)
  normalizePath(sub("^--file=", "", f[[1L]]), mustWork = FALSE)
}
.cc_probe_dir <- function() dirname(.cc_probe_file())
.cc_probe_sha <- function() {
  f <- .cc_probe_file()
  if (is.na(f) || !file.exists(f)) return(NA_character_)
  .cc_sha(paste(readLines(f, warn = FALSE), collapse = "\n"))
}

# ---- the fixture ------------------------------------------------------------------------

.cc_read_run <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)

# The comparable part of an outcome (what two baseline runs must agree on).
.cc_outcome_key <- function(r) {
  if (identical(r$status, "ok")) {
    list(status = "ok", result = r$result_sha256, warnings = r$warnings)
  } else {
    list(status = "error", class = r$error$class, message = r$error$message,
         refused_before_query = r$error$refused_before_query, warnings = r$warnings)
  }
}

.cc_fixture <- function(path_a, path_b, out_path) {
  a <- .cc_read_run(path_a)
  b <- .cc_read_run(path_b)
  # both runs executed exactly this probe's case catalogue (JSON-normalised;
  # the section A templates are fixture metadata, not part of a case)
  strip <- function(cases) lapply(cases, function(x) x[setdiff(names(x), c("template", "template_key"))])
  canon_cases <- function(x) .cc_json(.cc_decode(.cc_json(strip(x))))
  current <- .cc_catalogue()
  catalogue <- canon_cases(current)
  stopifnot(identical(canon_cases(a$cases), catalogue), identical(canon_cases(b$cases), catalogue),
            !identical(a$label, b$label))
  pin <- jsonlite::fromJSON(file.path(.cc_probe_dir(), "..", "ws0", "corpus_baseline.json"))
  line <- function(x) {
    structure(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null",
                                            digits = NA, force = TRUE, json_verbatim = TRUE)),
              class = "json")
  }
  templates <- list()
  setups <- list()
  cases <- list()
  flagged <- character(0)
  for (i in seq_along(current)) {
    case <- current[[i]]
    ra <- a$results[[i]]
    rb <- b$results[[i]]
    stopifnot(identical(ra$id, case$id), identical(rb$id, case$id))
    deterministic <- identical(.cc_outcome_key(ra), .cc_outcome_key(rb))
    if (!deterministic) flagged <- c(flagged, case$id)
    baseline <- list(status = ra$status)
    if (length(unlist(ra$warnings))) baseline$warnings <- I(as.character(unlist(ra$warnings)))
    ir_expectation <- "accept"
    if (identical(ra$status, "ok")) {
      # the lossless canonical result by its sha256; rows and columns per
      # output for diagnostics (sections C and D also persons and rows per
      # person: what the concept_col and set-operation cases restricted)
      baseline$result_sha256 <- ra$result_sha256
      keep <- c("nrow", "columns", "components",
                if (case$section %in% c("C", "D")) c("persons", "rows_per_person"))
      baseline$outputs <- lapply(ra$summary, function(o) o[intersect(names(o), keep)])
    } else {
      e <- ra$error
      stage <- .cc_stage(e$message)
      stopifnot(!identical(stage, "unclassified"))
      baseline$error <- Filter(Negate(is.null), list(
        class = I(as.character(unlist(e$class))), message = e$message, stage = stage,
        refused_before_query = e$refused_before_query, data_statements_before = e$data_statements_before,
        origin = I(as.character(unlist(e$origin))), call = e$call))
      ir_expectation <- if (identical(stage, "plan")) "refuse" else "none"
    }
    if (!is.null(ra[["sql"]])) baseline$sql <- I(as.character(unlist(ra[["sql"]])))
    entry <- list(id = case$id, section = case$section, value = case$value, op = case[["op"]])
    if (!is.null(case[["template_key"]])) {
      key <- case$template_key
      tpl <- list(field = case$field, kind = case$kind, reads = case$reads, template = case$template)
      if (is.null(templates[[key]])) templates[[key]] <- tpl
      stopifnot(identical(templates[[key]], tpl),
                identical(as.character(jsonlite::minify(gsub("@V@", case$value, case$template, fixed = TRUE))),
                          case$plan))
      entry$template <- key
    } else {
      entry$title <- case$title
      entry$field <- case$field
      entry$plan <- structure(case$plan, class = "json")
    }
    entry$plan_sha256 <- ra$plan_sha256
    if (!identical(case$options, .CC_DEFAULT_OPTIONS)) entry$options <- case$options
    if (!is.null(case[["setup"]])) {
      known <- vapply(setups, function(x) identical(x, case$setup), logical(1))
      name <- if (any(known)) names(setups)[known][[1L]] else paste0("safe_bins_", length(setups) + 1L)
      setups[[name]] <- case$setup
      entry$setup <- name
    }
    if (isTRUE(case[["typed_refusal"]]) && !identical(ra$status, "ok")) entry$typed_refusal <- TRUE
    entry$baseline <- baseline
    entry$ir_expectation <- ir_expectation
    entry$disposition <- ""
    cases[[length(cases) + 1L]] <- Filter(Negate(is.null), entry)
  }
  status <- vapply(cases, function(x) x$baseline$status, "")
  fixture <- list(
    version = 2L,
    description = paste(
      "v1 compatibility differential fixture (Astra WS-2 re-review R1/R3, amendment A-19 item 7):",
      "independent BASELINE (dsOMOP 2.10.2) outcomes of v1 plans whose scalar fields or predicate",
      "literals are written in a historically coercible or invalid form. A baseline `ok` must keep",
      "executing with an identical lossless canonical result; a baseline `error` may be refused (typed",
      "where `typed_refusal`). Replayed by tests/testthat/test-ir-v1-compat.R."),
    generated_by = "tools/ws2/capture_compat.R (run x2 at the baseline, then fixture)",
    baseline = list(dsOMOP = pin$dsOMOP, dsOMOPClient = pin$dsOMOPClient,
                    captures = I(c(a$label, b$label)), created = I(c(a$created, b$created)),
                    r_version = a$r_version, dsomop_version = a$dsomop_version,
                    catalogue_sha256 = .cc_sha(catalogue)),
    plan_decoding = paste(
      "a case with `template` has the plan templates[[template]]$template with @V@ replaced by its",
      "`value` (a JSON literal); any other case has `plan`. Either is the transport JSON, decoded exactly",
      "as .ds_arg() decodes it: jsonlite::fromJSON(<plan>, simplifyVector = FALSE). `plan_sha256` is the",
      "sha256 of the decoded plan's 17-digit b10 canonical form."),
    result_encoding = paste(
      "baseline `ok`: `result_sha256` is the sha256 of the b10 canonical form of the .planExecute()",
      "value at 17 significant digits (lossless) as serialised by jsonlite::toJSON(auto_unbox = TRUE,",
      "null = 'null', na = 'null', digits = NA, force = TRUE); `outputs` has each output's rows and",
      "columns for diagnostics. The full canonical forms are recomputed with tools/ws2/capture_compat.R",
      "(run on the baseline and on the current code, then diff)."),
    execution = paste(
      ".planExecute(handle, plan, out_symbols) with every output name mapped to '<name>_df', on a",
      "fresh create_test_handle() after the case's `setup` (setups[[setup]]; issue_bins:",
      ".test_issue_safe_bins()), under `option_state` plus the case's `options` (`default_options`",
      "when absent); every other nfilter.* / dsomop.* / dsstaging.* / datashield.* option unset."),
    fields = list(
      baseline = paste("status ok (result_sha256, outputs) or error (class, message, stage,",
                       "refused_before_query, origin, data_statements_before, call); warnings;",
                       "sql: the data statements of the concept_col cases"),
      stage = paste("plan: a property of the plan alone; site: needs the site catalogue (including its",
                    "blocked columns) or dialect; executor: the database or R failed on the value while",
                    "running; data: needs the data (disclosure gates, vocabulary, empty populations);",
                    "session: server-issued session state (safe-bin contracts, handle-owned temp cohorts)"),
      ir_expectation = paste("accept: .irPrepare(plan) without a handle must accept (baseline ok);",
                             "refuse: it must refuse with a dsomop_plan_rejection (baseline refused at the",
                             "plan stage); none: no plan-level expectation (baseline refused at another",
                             "stage)"),
      typed_refusal = "the current refusal must inherit dsomop_plan_rejection (R1 malformed shapes)",
      disposition = paste("'' or a reviewed deviation {current: 'refused' | 'ok', reference, note};",
                          "the test then asserts `current` instead of the baseline equivalence")),
    default_options = .CC_DEFAULT_OPTIONS,
    option_state = a$option_state,
    setups = setups,
    nondeterministic = I(flagged),
    counts = list(cases = length(cases), ok = sum(status == "ok"), error = sum(status == "error")),
    templates = lapply(templates, line),
    cases = lapply(cases, line))
  txt <- as.character(jsonlite::toJSON(fixture, auto_unbox = TRUE, null = "null", na = "null",
                                       digits = NA, force = TRUE, json_verbatim = TRUE, pretty = TRUE))
  writeLines(txt, out_path, useBytes = TRUE)
  cat("[capture_compat] fixture:", length(cases), "cases,", length(templates), "templates,",
      length(flagged), "nondeterministic ->", out_path, "\n")
  invisible(fixture)
}

# ---- comparing a run of another build with the fixture --------------------------------

.cc_short <- function(r) {
  if (identical(r$status, "ok")) return(paste0("ok ", substr(r$result_sha256 %||% "", 1L, 8L)))
  paste0("error[", r$error$stage %||% "", "] ", substr(gsub("\\s+", " ", r$error$message), 1L, 90L))
}

.cc_compare <- function(fixture_path, run_path) {
  fx <- jsonlite::fromJSON(fixture_path, simplifyVector = FALSE)
  run <- .cc_read_run(run_path)
  by_id <- stats::setNames(run$results, vapply(run$results, `[[`, "", "id"))
  rows <- lapply(fx$cases, function(case) {
    cur <- by_id[[case$id]]
    base <- case$baseline
    same <- if (identical(base$status, "ok")) {
      identical(cur$status, "ok") && identical(cur$result_sha256, base$result_sha256)
    } else identical(cur$status, "error")
    data.frame(id = case$id, baseline = .cc_short(base), current = .cc_short(cur),
               ir = cur$ir$status %||% "", ir_expected = case$ir_expectation,
               differs = !same, stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  utils::write.table(tab, sep = "\t", quote = FALSE, row.names = FALSE)
  invisible(tab)
}

# ---- the full canonical difference between two runs ------------------------------------

.cc_first_difference <- function(x, y, path = "") {
  if (identical(x, y)) return(NULL)
  if (is.list(x) && is.list(y) && length(x) == length(y) && identical(names(x), names(y))) {
    keys <- if (is.null(names(x))) seq_along(x) else names(x)
    for (k in seq_along(x)) {
      d <- .cc_first_difference(x[[k]], y[[k]], paste0(path, "/", if (is.null(names(x))) k - 1L else keys[[k]]))
      if (!is.null(d)) return(d)
    }
  }
  list(path = if (nzchar(path)) path else "/", a = x, b = y)
}

.cc_diff <- function(path_a, path_b, pattern = "") {
  a <- .cc_read_run(path_a)
  b <- .cc_read_run(path_b)
  by_id <- stats::setNames(b$results, vapply(b$results, `[[`, "", "id"))
  for (ra in a$results) {
    if (nzchar(pattern) && !grepl(pattern, ra$id)) next
    rb <- by_id[[ra$id]]
    if (is.null(rb)) next
    if (identical(ra$status, "ok") && identical(rb$status, "ok")) {
      if (identical(ra$result_sha256, rb$result_sha256)) next
      d <- .cc_first_difference(.cc_decode(ra$result), .cc_decode(rb$result))
      cat("==", ra$id, "first difference at", d$path, "\n   ", a$label, ":", .cc_json(d$a),
          "\n   ", b$label, ":", .cc_json(d$b), "\n")
    } else if (!identical(ra$status, rb$status) ||
               !identical(ra$error$message, rb$error$message)) {
      cat("==", ra$id, "\n   ", a$label, ":", .cc_short(ra), "\n   ", b$label, ":", .cc_short(rb), "\n")
    }
  }
  invisible(NULL)
}

# ---- main -----------------------------------------------------------------------------------

if (!interactive() && sys.nframe() == 0L) local({
  args <- commandArgs(TRUE)
  if (!length(args)) stop("usage: capture_compat.R run|fixture|compare|diff ...")
  switch(args[[1L]],
    run = .cc_run(args[[2L]], args[[3L]], if (length(args) >= 4L) args[[4L]] else "run"),
    fixture = .cc_fixture(args[[2L]], args[[3L]], args[[4L]]),
    compare = .cc_compare(args[[2L]], args[[3L]]),
    diff = .cc_diff(args[[2L]], args[[3L]], if (length(args) >= 4L) args[[4L]] else ""),
    stop("unknown mode '", args[[1L]], "'"))
})
