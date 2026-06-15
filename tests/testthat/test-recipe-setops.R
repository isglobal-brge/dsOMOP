# ==============================================================================
# Multi-population + set-op + recipe-level scope EXECUTION (server side)
#
# These tests exercise the server's execution of the multi-population plan
# contract the client's recipe_to_plan() produces (dsOMOPClient/R/recipe.R):
#   - criteria populations resolved through the cohort filter machinery
#     (.buildCohortFromFilters / .compileCohortFilterWhere);
#   - set-op populations (union / intersect / setdiff) folded with .cohortCombine
#     in dependency order, each result fail-closed gated (.assertMinPersons);
#   - a unified recipe-level scope (a cohort ref and/or an omop.table frame),
#     folded by .omopAnalysisResolveScope and INTERSECTED into every population;
#   - per-output execution over its population_id's gated, scope-narrowed set,
#     with pseudonymized identifiers and the per-output person gate intact.
#
# Everything runs on the shared SQLite fixture (create_test_handle). The
# fixture's randomised clinical facts are too tangled for clean female set-math,
# so each setup CLEARS condition_occurrence + measurement and inserts exactly the
# rows the test needs; observation_period (which anchors materialized cohorts)
# and the vocabulary are left intact.
#
# nfilter.subset is pinned to 3 throughout so the disclosure threshold is small,
# explicit, and the fail-closed cases are obvious.
# ==============================================================================

# Females in the default fixture are the EVEN persons (gender_concept_id 8532):
# {2,4,6,8,10,12,14}. The males (odd) carry no inserted facts here, so a sex=F
# leg is what confines every criteria population below.

# Insert one condition_occurrence row.
.so_ins_cond <- function(conn, pid, concept_id, start = "2019-06-01") {
  DBI::dbExecute(conn, sprintf(paste0(
    "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
    "condition_concept_id, condition_start_date, condition_end_date, ",
    "condition_type_concept_id, visit_occurrence_id) ",
    "VALUES (%d, %d, %d, '%s', '%s', 44818518, %d)"),
    sample(100000:999999, 1), pid, concept_id, start, start, pid))
}

# Insert one measurement row (value_as_number drives the high/low legs).
.so_ins_meas <- function(conn, pid, concept_id, value, start = "2019-12-15") {
  DBI::dbExecute(conn, sprintf(paste0(
    "INSERT INTO measurement (measurement_id, person_id, measurement_concept_id, ",
    "measurement_date, measurement_type_concept_id, value_as_number, ",
    "value_as_concept_id, unit_concept_id, range_low, range_high, ",
    "visit_occurrence_id) ",
    "VALUES (%d, %d, %d, '%s', 44818702, %f, 0, 8840, 4.0, 6.0, %d)"),
    sample(100000:999999, 1), pid, concept_id, start, value, pid))
}

# A handle whose clinical tables have been reset to a known state. `keyed` adds a
# person_key so omop.table-token scope sources can be reversed server-side.
.so_handle <- function(n_persons = 15, keyed = FALSE) {
  h <- create_test_handle(n_persons = n_persons)
  if (keyed) h$person_key <- as.raw(1:16)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  h
}

# A token-keyed omop.table frame for a set of ORIGINAL person ids, exactly as a
# pseudonymized plan output would carry them (mirrors test-analysis-catalog.R).
.so_token_frame <- function(handle, ids) {
  toks <- dsOMOP:::.hashPersonKey(as.character(ids), handle$person_key)
  df <- data.frame(person_id = toks, v = seq_along(ids), stringsAsFactors = FALSE)
  attr(df, "dsomop_protected") <- "person_id"
  class(df) <- union("omop.table", class(df))
  df
}

# Criteria/set-op population spec builders (the shape .compile_population_spec
# emits). Keeping them terse makes the per-test plans readable.
.so_sexF <- list(type = "sex", params = list(value = "F"))
.so_has_cond <- function(cid)
  list(type = "has_concept",
       params = list(concept_id = cid, table = "condition_occurrence"))
.so_has_meas <- function(cid, min_value = NULL, max_value = NULL)
  list(type = "has_measurement",
       params = list(concept_id = cid, min_value = min_value,
                     max_value = max_value))
.so_crit <- function(id, ...)
  list(id = id, kind = "criteria",
       filter_tree = list(and = list(...)))
.so_setop <- function(id, op, members)
  list(id = id, kind = "setop", setop = list(op = op, members = members))

.so_plan <- function(populations, outputs, scope = NULL) {
  plan <- list(populations = populations, outputs = outputs,
               options = list(translate_concepts = FALSE,
                              block_sensitive = TRUE))
  if (!is.null(scope)) plan$scope <- scope
  class(plan) <- c("omop_plan", "list")
  plan
}

# Distinct person ids of a resolved population (sorted) for set assertions.
.so_pids <- function(resolved, id) sort(resolved[[id]]$person_ids)

# ---------------------------------------------------------------------------
# (headline) The maintainer's headline query, built DECLARATIVELY with the
# working-tree client's omop_recipe(populations = list(...)) and executed
# end-to-end through the SERVER. This is the load-bearing acceptance test: it
# proves the declarative form COMPILES (recipe_to_plan) and the server EXECUTES
# the compiled contract to the right gated person set (not the old
# "Multiple populations not yet executable" stop).
# ---------------------------------------------------------------------------

test_that("maintainer headline query: declarative recipe compiles AND executes", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("pkgload")
  # Build the recipe with the WORKING-TREE client (the installed one predates
  # multi-population). The sibling checkout sits next to this package; resolve it
  # relative to the test path AND a couple of fallbacks so it is found whether
  # tests run from the package root (test_file) or tests/testthat (R CMD check).
  tp <- tryCatch(testthat::test_path(), error = function(e) ".")
  cand <- c(file.path(tp, "..", "..", "..", "dsOMOPClient"),
            file.path(tp, "..", "..", "dsOMOPClient"),
            "../dsOMOPClient", "../../dsOMOPClient")
  client_path <- NULL
  for (cp in cand) if (dir.exists(cp)) { client_path <- cp; break }
  skip_if(is.null(client_path), "sibling dsOMOPClient checkout not found")
  client_ok <- tryCatch({
    suppressMessages(pkgload::load_all(client_path, quiet = TRUE,
                                       export_all = FALSE))
    exists(".compile_population_spec", envir = asNamespace("dsOMOPClient"))
  }, error = function(e) FALSE)
  skip_if_not(client_ok, "working-tree dsOMOPClient not loadable")

  h <- .so_handle()
  on.exit(cleanup_handle(h))
  # subgroup A = women with conditionX (201820) AND HbA1c high (>=7.0): {2,4,6}
  for (p in c(2, 4, 6)) { .so_ins_cond(h$conn, p, 201820L);
                          .so_ins_meas(h$conn, p, 3004410L, 7.5) }
  # subgroup B = women with conditionY (255573) AND HbA1c low (<=5.0): {8,10,12}
  for (p in c(8, 10, 12)) { .so_ins_cond(h$conn, p, 255573L);
                            .so_ins_meas(h$conn, p, 3004410L, 4.5) }
  # hasMeasZ = women with measurementZ (creatinine 3025315) present: {2,4,6,8,10,14}
  for (p in c(2, 4, 6, 8, 10, 14)) .so_ins_meas(h$conn, p, 3025315L, 1.2)
  .buildBlueprint(h)

  recipe <- dsOMOPClient::omop_recipe(
    populations = list(
      dsOMOPClient::omop_population(
        "A", "women cond201820 + HbA1c high",
        filters = list(
          dsOMOPClient::omop_filter_sex("F"),
          dsOMOPClient::omop_filter_has_concept(201820, "condition_occurrence"),
          dsOMOPClient::omop_filter_has_measurement(3004410, min_value = 7.0))),
      dsOMOPClient::omop_population(
        "B", "women cond255573 + HbA1c low",
        filters = list(
          dsOMOPClient::omop_filter_sex("F"),
          dsOMOPClient::omop_filter_has_concept(255573, "condition_occurrence"),
          dsOMOPClient::omop_filter_has_measurement(3004410, max_value = 5.0))),
      dsOMOPClient::omop_population("union_AB", "A or B",
                                    union = c("A", "B")),
      dsOMOPClient::omop_population(
        "hasMeasZ", "women creatinine present",
        filters = list(
          dsOMOPClient::omop_filter_sex("F"),
          dsOMOPClient::omop_filter_has_measurement(3025315))),
      dsOMOPClient::omop_population("final", "union_AB and hasMeasZ",
                                    intersect = c("union_AB", "hasMeasZ"))),
    variables = dsOMOPClient::omop_variable_age(),
    outputs = dsOMOPClient::omop_output(name = "study", type = "wide",
                                        population_id = "final"))

  # (a) COMPILES: recipe_to_plan serializes every population + stamps the output.
  plan <- dsOMOPClient::recipe_to_plan(recipe)
  expect_s3_class(plan, "omop_plan")
  expect_setequal(names(plan$populations),
                  c("base", "A", "B", "union_AB", "hasMeasZ", "final"))
  expect_identical(plan$populations$final$kind, "setop")
  expect_identical(plan$populations$final$setop$op, "intersect")
  expect_identical(plan$populations$final$setop$members,
                   c("union_AB", "hasMeasZ"))
  expect_identical(plan$outputs[[names(plan$outputs)[1]]]$population_id, "final")

  # (b) EXECUTES: the wide output is produced over `final` =
  # intersect(union(A={2,4,6}, B={8,10,12}) = {2,4,6,8,10,12},
  #           hasMeasZ={2,4,6,8,10,14}) = {2,4,6,8,10} -> 5 distinct persons.
  withr::with_options(list(nfilter.subset = 3), {
    out_names <- stats::setNames("study_sym", names(plan$outputs)[1])
    res <- .planExecute(h, plan, out_names)
    df <- res[[names(plan$outputs)[1]]]
    expect_true(is.data.frame(df))
    expect_true("person_id" %in% names(df))
    expect_equal(length(unique(df$person_id)), 5L)
    # It did NOT collapse onto the base population (all 15) or union_AB (6).
    expect_false(length(unique(df$person_id)) %in% c(6L, 15L))
  })
})

# ---------------------------------------------------------------------------
# (a) union vs intersect vs setdiff of two subgroups give the expected
#     distinct-person sets.
# ---------------------------------------------------------------------------

test_that("set-op union/intersect/setdiff produce the expected person sets", {
  skip_if_not_installed("RSQLite")
  # n=30 gives 15 women {2,4,...,30}, enough room for every set-op result to
  # clear nfilter=3. A = women + diabetes(201820): {2,4,6,8,10,12};
  # B = women + COPD(255573): {8,10,12,14,16,18} (overlap {8,10,12}).
  build <- function() {
    h <- .so_handle(n_persons = 30)
    for (p in c(2, 4, 6, 8, 10, 12)) .so_ins_cond(h$conn, p, 201820L)
    for (p in c(8, 10, 12, 14, 16, 18)) .so_ins_cond(h$conn, p, 255573L)
    .buildBlueprint(h)
    h
  }
  run <- function(op) {
    h <- build(); on.exit(cleanup_handle(h))
    plan <- .so_plan(
      populations = list(
        base = list(id = "base"),
        A = .so_crit("A", .so_sexF, .so_has_cond(201820L)),
        B = .so_crit("B", .so_sexF, .so_has_cond(255573L)),
        res = .so_setop("res", op, c("A", "B"))),
      outputs = list())
    withr::with_options(list(nfilter.subset = 3),
      .so_pids(.planResolvePopulations(h, plan, .buildBlueprint(h)), "res"))
  }
  expect_equal(run("union"), c(2, 4, 6, 8, 10, 12, 14, 16, 18))  # A ∪ B
  expect_equal(run("intersect"), c(8, 10, 12))                   # A ∩ B
  expect_equal(run("setdiff"), c(2, 4, 6))                       # A \ B
})

# ---------------------------------------------------------------------------
# (b) a SET-OP whose result is < nfilter persons FAILS CLOSED — both an empty
#     intersection and a tiny (non-zero) one.
# ---------------------------------------------------------------------------

test_that("set-op result below nfilter fails closed (empty intersection)", {
  skip_if_not_installed("RSQLite")
  # A = {2,4,6}, B = {8,10,12} disjoint -> A ∩ B = {} (< nfilter).
  h <- .so_handle(); on.exit(cleanup_handle(h))
  for (p in c(2, 4, 6)) .so_ins_cond(h$conn, p, 201820L)
  for (p in c(8, 10, 12)) .so_ins_cond(h$conn, p, 255573L)
  .buildBlueprint(h)

  plan <- .so_plan(
    populations = list(
      base = list(id = "base"),
      A = .so_crit("A", .so_sexF, .so_has_cond(201820L)),
      B = .so_crit("B", .so_sexF, .so_has_cond(255573L)),
      res = .so_setop("res", "intersect", c("A", "B"))),
    outputs = list())
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(.planResolvePopulations(h, plan, .buildBlueprint(h)),
                 "Disclosive|disclosure threshold|insufficient")
  })
})

test_that("set-op result below nfilter fails closed (tiny non-zero intersection)", {
  skip_if_not_installed("RSQLite")
  # A = {2,4,6,8}, B = {8,10,12,14} -> A ∩ B = {8} (1 person, > 0 but < nfilter).
  h <- .so_handle(); on.exit(cleanup_handle(h))
  for (p in c(2, 4, 6, 8)) .so_ins_cond(h$conn, p, 201820L)
  for (p in c(8, 10, 12, 14)) .so_ins_cond(h$conn, p, 255573L)
  .buildBlueprint(h)

  plan <- .so_plan(
    populations = list(
      base = list(id = "base"),
      A = .so_crit("A", .so_sexF, .so_has_cond(201820L)),
      B = .so_crit("B", .so_sexF, .so_has_cond(255573L)),
      res = .so_setop("res", "intersect", c("A", "B"))),
    outputs = list())
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(.planResolvePopulations(h, plan, .buildBlueprint(h)),
                 "Disclosive|disclosure threshold|insufficient")
  })
})

# ---------------------------------------------------------------------------
# (c) recipe-level scope (a cohort AND an omop.table symbol, combine=union /
#     intersect) restricts every population + re-gates; a too-small scope fails
#     closed.
# ---------------------------------------------------------------------------

test_that("scope via omop.table frame narrows every population (end-to-end)", {
  skip_if_not_installed("RSQLite")
  register_test_omop <- function(h, symbol = "omop")
    assign(paste0("handle_", symbol), h, envir = dsOMOP:::.dsomop_env)
  unregister_test_omop <- function(symbol = "omop") {
    key <- paste0("handle_", symbol)
    if (exists(key, envir = dsOMOP:::.dsomop_env))
      rm(list = key, envir = dsOMOP:::.dsomop_env)
  }

  # A = women + diabetes: {2,4,6,8}; B = women + COPD: {8,10,12,14};
  # union_AB = {2,4,6,8,10,12,14}. Scope to a frame of {2,4,6,8,10,12} (drops 14)
  # so every population stays >= nfilter after intersection:
  #   A ∩ scope = {2,4,6,8} (4), B ∩ scope = {8,10,12} (3),
  #   union_AB ∩ scope = {2,4,6,8,10,12} (6).
  h <- .so_handle(keyed = TRUE)
  on.exit({ unregister_test_omop(); cleanup_handle(h) })
  for (p in c(2, 4, 6, 8)) .so_ins_cond(h$conn, p, 201820L)
  for (p in c(8, 10, 12, 14)) .so_ins_cond(h$conn, p, 255573L)
  .buildBlueprint(h)
  register_test_omop(h)

  plan <- .so_plan(
    populations = list(
      base = list(id = "base"),
      A = .so_crit("A", .so_sexF, .so_has_cond(201820L)),
      B = .so_crit("B", .so_sexF, .so_has_cond(255573L)),
      union_AB = .so_setop("union_AB", "union", c("A", "B"))),
    outputs = list(study = list(
      type = "person_level", population_id = "union_AB",
      tables = list(person = c("person_id", "gender_concept_id")))))

  frame <- .so_token_frame(h, c(2, 4, 6, 8, 10, 12))
  env <- new.env()
  withr::with_options(list(nfilter.subset = 3), suppressMessages(
    eval(quote(omopPlanExecuteDS("omop", plan, c(study = "study_sym"),
                                 scope = frame, combine = "union")),
         envir = env)))

  expect_true(exists("study_sym", envir = env))
  df <- get("study_sym", envir = env)
  # union_AB narrowed from 7 to 6 by the scope.
  expect_equal(length(unique(df$person_id)), 6L)
  # (e) banded / person-gated: person_id is pseudonymized (a token, not the raw
  # integer id), only the requested columns survive, no raw row-level identifier.
  expect_false(is.numeric(df$person_id))
  expect_setequal(names(df), c("person_id", "gender_concept_id"))
  expect_false(any(grepl("source_value|provider_id|_source_concept_id",
                         names(df))))
})

test_that("scope folds a cohort ref AND a table frame (combine=intersect)", {
  skip_if_not_installed("RSQLite")
  # n=30. A = women + diabetes: {2,4,6,8,10,12}; B = women + COPD:
  # {8,10,12,14,16,18}; union_AB = {2,4,6,8,10,12,14,16,18}. Two scope sources
  # folded by INTERSECT: cohort table {2,4,6,8,10,12,14,16} ∩ frame
  # {4,6,8,10,12,14,16,18} = {4,6,8,10,12,14,16} -> the scope cohort.
  h <- .so_handle(n_persons = 30, keyed = TRUE); on.exit(cleanup_handle(h))
  for (p in c(2, 4, 6, 8, 10, 12)) .so_ins_cond(h$conn, p, 201820L)
  for (p in c(8, 10, 12, 14, 16, 18)) .so_ins_cond(h$conn, p, 255573L)
  bp <- .buildBlueprint(h)

  cohort_ct <- dsOMOP:::.materializeCohortFromIds(
    h, bp, c(2, 4, 6, 8, 10, 12, 14, 16), name = "scope_cohort_src")
  frame <- .so_token_frame(h, c(4, 6, 8, 10, 12, 14, 16, 18))

  plan <- .so_plan(
    populations = list(
      base = list(id = "base"),
      A = .so_crit("A", .so_sexF, .so_has_cond(201820L)),
      B = .so_crit("B", .so_sexF, .so_has_cond(255573L)),
      union_AB = .so_setop("union_AB", "union", c("A", "B"))),
    outputs = list(),
    scope = list(tables_frames = list(cohort_ct, frame),
                 combine = "intersect"))

  withr::with_options(list(nfilter.subset = 3), {
    resolved <- .planResolvePopulations(h, plan, bp)
    scope_ct <- .planResolveScopeCohort(h, plan)
    # The folded scope itself is {4,6,8,10,12,14,16}.
    expect_equal(sort(dsOMOP:::.cohortPersonIds(h, scope_ct)),
                 c(4, 6, 8, 10, 12, 14, 16))
    scoped <- .planScopePopulations(h, resolved, scope_ct, bp)
    expect_equal(.so_pids(scoped, "union_AB"),
                 c(4, 6, 8, 10, 12, 14, 16))          # narrowed from 9 to 7
    expect_equal(.so_pids(scoped, "A"), c(4, 6, 8, 10, 12))     # ∩ scope
    expect_equal(.so_pids(scoped, "B"), c(8, 10, 12, 14, 16))   # ∩ scope
  })
})

test_that("a too-small scope fails closed when intersected into a population", {
  skip_if_not_installed("RSQLite")
  # union_AB = {2,4,6,8,10,12}; scope cohort = {2,8} (2 persons). The scope is
  # itself re-gated by .planResolveScopeCohort -> < nfilter -> fail closed before
  # any population is narrowed.
  h <- .so_handle(keyed = TRUE); on.exit(cleanup_handle(h))
  for (p in c(2, 4, 6)) .so_ins_cond(h$conn, p, 201820L)
  for (p in c(8, 10, 12)) .so_ins_cond(h$conn, p, 255573L)
  bp <- .buildBlueprint(h)
  frame <- .so_token_frame(h, c(2, 8))

  plan <- .so_plan(
    populations = list(
      base = list(id = "base"),
      A = .so_crit("A", .so_sexF, .so_has_cond(201820L)),
      B = .so_crit("B", .so_sexF, .so_has_cond(255573L)),
      union_AB = .so_setop("union_AB", "union", c("A", "B"))),
    outputs = list(),
    scope = list(tables_frames = frame, combine = "union"))

  withr::with_options(list(nfilter.subset = 3), {
    expect_error(.planResolveScopeCohort(h, plan),
                 "Disclosive|disclosure threshold|insufficient")
  })
})

# ---------------------------------------------------------------------------
# (c2) cohort=-as-scope: a SCALAR plan$cohort (type cohort_table) is resolved
#      UNIFORMLY as a recipe-level SCOPE source (a gated person-set intersected
#      into every population), NOT a base population. This is the contract the
#      working-tree client's omop_recipe(cohort=)/recipe_execute(cohort=) emits.
# ---------------------------------------------------------------------------

test_that("a scalar plan cohort id resolves as SCOPE (by id, handle, table)", {
  skip_if_not_installed("RSQLite")
  # The fixture's cohort_definition_id = 1 has members {1,3,5,7,9,11}.
  h <- create_test_handle(); on.exit(cleanup_handle(h))
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)

  withr::with_options(list(nfilter.subset = 3), {
    # (i) scope by scalar cohort_definition_id.
    plan <- .so_plan(populations = NULL, outputs = list())
    plan$cohort <- list(type = "cohort_table", cohort_definition_id = 1)
    sc <- .planResolveScopeCohort(h, plan)
    expect_equal(sort(dsOMOP:::.cohortPersonIds(h, sc)),
                 c(1, 3, 5, 7, 9, 11))

    # (ii) scope by cohort TABLE name (a server-side cohort temp table) folds in
    # the same way through plan$scope$cohort.
    bp <- .buildBlueprint(h)
    tbl <- dsOMOP:::.materializeCohortFromIds(h, bp, c(3, 5, 7, 9),
                                              name = "scope_by_table_src")
    plan_t <- .so_plan(populations = NULL, outputs = list(),
                       scope = list(cohort = tbl, combine = "union"))
    sc_t <- .planResolveScopeCohort(h, plan_t)
    expect_equal(sort(dsOMOP:::.cohortPersonIds(h, sc_t)), c(3, 5, 7, 9))

    # (iii) a scalar plan cohort id and an omop.table FRAME scope are BOTH scope
    # sources and INTERSECT: cohort1 {1,3,5,7,9,11} ∩ frame {1,3,5,99} = {1,3,5}.
    fr <- .so_token_frame(h, c(1, 3, 5, 99))
    plan_f <- .so_plan(populations = NULL, outputs = list(),
                       scope = list(tables_frames = fr, combine = "intersect"))
    plan_f$cohort <- list(type = "cohort_table", cohort_definition_id = 1)
    sc_f <- .planResolveScopeCohort(h, plan_f)
    expect_equal(sort(dsOMOP:::.cohortPersonIds(h, sc_f)), c(1, 3, 5))
  })
})

test_that("a scalar plan cohort id NARROWS populations as scope, not as base", {
  skip_if_not_installed("RSQLite")
  # Build a criteria population over MALE persons (odd, to overlap cohort 1 =
  # {1,3,5,7,9,11}); the scalar cohort id must INTERSECT it (scope), confining the
  # output to the cohort members who also meet the criteria — never replacing it.
  h <- .so_handle(keyed = TRUE); on.exit(cleanup_handle(h))
  # Males with diabetes(201820): {1,3,5,7,9} (5 persons; person 11 omitted).
  for (p in c(1, 3, 5, 7, 9)) .so_ins_cond(h$conn, p, 201820L)
  bp <- .buildBlueprint(h)

  sexM <- list(type = "sex", params = list(value = "M"))
  plan <- .so_plan(
    populations = list(
      base = list(id = "base"),
      M = list(id = "M", kind = "criteria",
               filter_tree = list(and = list(
                 sexM, .so_has_cond(201820L))))),
    outputs = list())
  # The scalar cohort id is the recipe-level scope.
  plan$cohort <- list(type = "cohort_table", cohort_definition_id = 1)

  withr::with_options(list(nfilter.subset = 3), {
    resolved <- .planResolvePopulations(h, plan, bp)
    scope_ct <- .planResolveScopeCohort(h, plan)
    scoped <- .planScopePopulations(h, resolved, scope_ct, bp)
    # M = {1,3,5,7,9}; cohort1 = {1,3,5,7,9,11}; M ∩ cohort1 = {1,3,5,7,9}.
    expect_equal(.so_pids(scoped, "M"), c(1, 3, 5, 7, 9))
    # The base population is ALSO narrowed to the scope cohort members (scope, not
    # base): base ∩ cohort1 = {1,3,5,7,9,11}.
    expect_equal(.so_pids(scoped, "base"), c(1, 3, 5, 7, 9, 11))
  })
})

# ---------------------------------------------------------------------------
# (d) single-population (base-only) recipes are UNCHANGED (regression): the
#     multi-population path is not engaged and the base cohort drives the output
#     exactly as before.
# ---------------------------------------------------------------------------

test_that(".planHasMultiPopulation gates the fast path correctly", {
  mk <- function(pops) {
    p <- list(populations = pops); class(p) <- c("omop_plan", "list"); p
  }
  expect_false(.planHasMultiPopulation(mk(NULL)))
  expect_false(.planHasMultiPopulation(mk(list())))
  expect_false(.planHasMultiPopulation(mk(list(base = list(id = "base")))))
  # base carrying its own criteria, or any non-base population, engages it.
  expect_true(.planHasMultiPopulation(
    mk(list(base = list(id = "base", filters = list(x = 1))))))
  expect_true(.planHasMultiPopulation(
    mk(list(base = list(id = "base"), sub = list(id = "sub")))))
})

test_that("base-only plan executes through the unchanged single-cohort path", {
  skip_if_not_installed("RSQLite")
  # A plan with NO populations key and a cohort_definition_id is the pre-existing
  # fast path; it must be byte-for-byte unaffected by the multi-pop machinery.
  h <- create_test_handle(); on.exit(cleanup_handle(h))
  .buildBlueprint(h)
  plan <- .so_plan(
    populations = NULL,
    outputs = list(demo = list(
      type = "person_level",
      tables = list(person = c("person_id", "gender_concept_id")))))
  plan$cohort <- list(type = "cohort_table", cohort_definition_id = 1)
  plan$populations <- NULL

  withr::with_options(list(nfilter.subset = 3), {
    res <- .planExecute(h, plan, list(demo = "demo_sym"))
    df <- res$demo
    expect_true(is.data.frame(df))
    # Fixture cohort 1 has 6 members {1,3,5,7,9,11}.
    expect_equal(length(unique(df$person_id)), 6L)
  })
})

test_that("base-only populations=list(base=...) matches no-populations result", {
  skip_if_not_installed("RSQLite")
  # An explicit populations=list(base=...) with no extra criteria must resolve to
  # the SAME output as omitting plan$populations entirely (the implicit base).
  run <- function(with_base_pop) {
    h <- create_test_handle(); on.exit(cleanup_handle(h))
    .buildBlueprint(h)
    plan <- .so_plan(
      populations = if (with_base_pop) list(base = list(id = "base")) else NULL,
      outputs = list(demo = list(
        type = "person_level",
        tables = list(person = c("person_id", "gender_concept_id")))))
    plan$cohort <- list(type = "cohort_table", cohort_definition_id = 1)
    withr::with_options(list(nfilter.subset = 3), {
      df <- .planExecute(h, plan, list(demo = "demo_sym"))$demo
      sort(as.character(df$person_id))
    })
  }
  expect_identical(run(TRUE), run(FALSE))
})

# ---------------------------------------------------------------------------
# (e) every output is banded + person-gated — including a multi-population run
#     where the OUTPUT's own per-output gate (inside .extractTable) must also
#     fire when its population is too small.
# ---------------------------------------------------------------------------

test_that("a too-small criteria population fails closed before any output", {
  skip_if_not_installed("RSQLite")
  # A = women + diabetes restricted to just {2,4} (2 persons < nfilter): the
  # population gate in .planResolvePopulations fails closed.
  h <- .so_handle(); on.exit(cleanup_handle(h))
  for (p in c(2, 4)) .so_ins_cond(h$conn, p, 201820L)
  .buildBlueprint(h)
  plan <- .so_plan(
    populations = list(
      base = list(id = "base"),
      A = .so_crit("A", .so_sexF, .so_has_cond(201820L))),
    outputs = list())
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(.planResolvePopulations(h, plan, .buildBlueprint(h)),
                 "Disclosive|disclosure threshold|insufficient")
  })
})

test_that("multi-population output is pseudonymized and column-restricted", {
  skip_if_not_installed("RSQLite")
  register_test_omop <- function(h, symbol = "omop")
    assign(paste0("handle_", symbol), h, envir = dsOMOP:::.dsomop_env)
  unregister_test_omop <- function(symbol = "omop") {
    key <- paste0("handle_", symbol)
    if (exists(key, envir = dsOMOP:::.dsomop_env))
      rm(list = key, envir = dsOMOP:::.dsomop_env)
  }
  # union_AB = {2,4,6,8,10,12}; produce a person_level output over it through the
  # real entrypoint and assert the disclosure shape (tokens, no raw identifiers).
  h <- .so_handle(keyed = TRUE)
  on.exit({ unregister_test_omop(); cleanup_handle(h) })
  for (p in c(2, 4, 6)) .so_ins_cond(h$conn, p, 201820L)
  for (p in c(8, 10, 12)) .so_ins_cond(h$conn, p, 255573L)
  .buildBlueprint(h)
  register_test_omop(h)

  plan <- .so_plan(
    populations = list(
      base = list(id = "base"),
      A = .so_crit("A", .so_sexF, .so_has_cond(201820L)),
      B = .so_crit("B", .so_sexF, .so_has_cond(255573L)),
      union_AB = .so_setop("union_AB", "union", c("A", "B"))),
    outputs = list(study = list(
      type = "person_level", population_id = "union_AB",
      tables = list(person = c("person_id", "gender_concept_id")))))

  env <- new.env()
  withr::with_options(list(nfilter.subset = 3), suppressMessages(
    eval(quote(omopPlanExecuteDS("omop", plan, c(study = "study_sym"))),
         envir = env)))
  expect_true(exists("study_sym", envir = env))
  df <- get("study_sym", envir = env)
  expect_equal(length(unique(df$person_id)), 6L)
  expect_false(is.numeric(df$person_id))             # token, not raw id
  expect_setequal(names(df), c("person_id", "gender_concept_id"))
})
