# A-25: historical handle-owned population cohorts are closed executable IR.
.a25_plan <- function(ref = "Owned.Cohort", index = FALSE, siblings = FALSE) {
  pop <- list(id = "p", kind = "criteria", cohort_definition_id = ref)
  if (index) pop$index_event <- list(table = "condition_occurrence",
                                    concept_set = 201820L, primary_limit = "all")
  plan <- list(populations = list(p = pop), outputs = list(demo = list(
    type = "baseline", columns = "gender_concept_id", population_id = "p")),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
  if (siblings) {
    plan$populations$everyone <- list(id = "everyone", kind = "criteria",
      filter_tree = list(or = list(list(type = "sex", params = list(value = "M")),
                                  list(type = "sex", params = list(value = "F")))))
    plan$outputs$sibling <- list(type = "baseline", columns = "gender_concept_id",
                                population_id = "everyone")
  }
  plan
}
.a25_error <- function(expr) tryCatch(force(expr), error = identity)
.a25_ref <- function(h, ids = c(1L, 3L, 5L, 7L, 9L, 11L)) {
  # Distinctive out-of-observation dates must never become the population index.
  DBI::dbExecute(h$conn, paste0("CREATE TEMP TABLE A25_owned AS SELECT subject_id, ",
    "'2030-01-01' AS cohort_start_date, '2031-01-01' AS cohort_end_date ",
    "FROM cohort WHERE cohort_definition_id = 1 AND subject_id IN (",
    paste(ids, collapse = ","), ")"))
  register_test_temp(h, "A25_owned")
  "A25_owned"
}
.a25_events <- function(h) {
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  rows <- expand.grid(person_id = 1:15, episode = 1:2)
  events <- data.frame(condition_occurrence_id = as.integer(rows$person_id * 10L + rows$episode),
    person_id = as.integer(rows$person_id), condition_concept_id = 201820L,
    condition_start_date = paste0(2020L + rows$episode, "-06-01"),
    condition_end_date = NA_character_, condition_type_concept_id = 44818518L,
    visit_occurrence_id = NA_integer_)
  DBI::dbWriteTable(h$conn, "condition_occurrence", events, append = TRUE)
}
.a25_filter <- function() list(type = "has_concept", params = list(
  concept_id = 201820L, table = "condition_occurrence", min_count = 1L,
  window = list(start = 0L, end = 30L)))

# The historical no-trim numeric dispatch and identifier grammar are different
# from root numeric coercion and from native declaration-name encoding.
test_that("A-25 canonical names enter the closed IR for both permitted kinds", {
  for (index in c(FALSE, TRUE)) for (ref in c("CaseSensitive", "a.b", "_Cohort",
                                              paste(rep("x", 160L), collapse = ""))) {
    plan <- .a25_plan(paste0(" \t", ref, " \n"), index)
    ir <- .irPrepare(plan)
    pop <- ir$populations$p
    expect_identical(pop$legacy_cohort_table, ref)
    expect_identical(pop$kind, if (index) "index" else "criteria")
    expect_null(pop$cohort_definition_id)
    expect_null(pop$legacy_empty)
    if (!index) expect_identical(pop$legacy_episodes, "observation_periods")
    expect_null(ir$scope)
    expect_true(.jsonSchemaValidate(.irSchema("ir-v2"), .irDocument(ir))$valid)
    canonical <- .irPrepare(.a25_plan(ref, index))
    expect_identical(.planSemanticDigest(ir), .planSemanticDigest(canonical))
    different <- .irPrepare(.a25_plan(paste0(ref, "_other"), index))
    expect_false(identical(.planSemanticDigest(ir)$sha256,
                           .planSemanticDigest(different)$sha256))
    explained <- .irExplain(ir)
    expect_identical(explained$plan$populations$p$legacy_cohort_table, ref)
    expect_identical(explained$semantic_digest, .planSemanticDigest(ir)$sha256)
    expect_false(grepl(ref, paste(capture.output(str(attr(ir, "legacy_annex"))), collapse = ""),
                       fixed = TRUE))
  }
})

test_that("A-25 malformed names fail with a typed field path before database work", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  calls <- 0L
  local_mocked_bindings(.executeQuery = function(...) {
    calls <<- calls + 1L
    stop("unexpected result query")
  })
  malformed <- list("", " ", ".cohort", "bad-name", "name;DROP TABLE person", " 1 ",
                    "+1", "1e0", "1.0", "0", 0L, TRUE, c("a", "b"), list("a"), NA_character_)
  for (index in c(FALSE, TRUE)) for (ref in malformed) {
    err <- .a25_error(.irPrepare(.a25_plan(ref, index), h))
    expect_s3_class(err, "dsomop_ir_validation_error", info = paste(ref, collapse = ","))
    expect_identical(err$code, "validation_error")
    expect_match(conditionMessage(err), "/populations/p/cohort_definition_id", fixed = TRUE)
  }
  expect_identical(calls, 0L)
})

test_that("A-25 registry preflight is exact and does not resolve, count or query a table", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  .buildBlueprint(h)
  DBI::dbExecute(h$conn, "CREATE TEMP TABLE physical_unregistered (subject_id INTEGER)")
  h$temp_tables <- c("Registry.Only", "CaseSensitive")
  calls <- 0L
  local_mocked_bindings(.executeQuery = function(...) {
    calls <<- calls + 1L
    stop("unexpected result query")
  }, .resolveCohortTable = function(...) stop("unexpected cohort materialisation"))
  for (index in c(FALSE, TRUE)) {
    for (ref in c("Registry.Only", "CaseSensitive")) {
      expect_s3_class(.irPrepare(.a25_plan(ref, index), h), "dsomop_ir")
    }
    for (ref in c("physical_unregistered", "unowned", "casesensitive", "Registry")) {
      err <- .a25_error(.irPrepare(.a25_plan(ref, index), h))
      expect_s3_class(err, "dsomop_ir_validation_error")
      expect_identical(err$code, "validation_error")
      expect_identical(err$field, "/populations/p/legacy_cohort_table")
      expect_s3_class(.irPrepare(.a25_plan(ref, index)), "dsomop_ir")
    }
  }
  expect_identical(calls, 0L)
})

test_that("A-25 preserves numeric dispatch and native wire types", {
  for (index in c(FALSE, TRUE)) {
    integer <- .irPrepare(.a25_plan(1L, index))
    for (ref in c("1", "01")) {
      ir <- .irPrepare(.a25_plan(ref, index))
      expect_null(ir$populations$p$legacy_cohort_table)
      expect_identical(.planSemanticDigest(ir), .planSemanticDigest(integer))
    }
    named <- .irDocument(.irPrepare(.a25_plan("Owned", index)))
    expect_false(.jsonSchemaValidate(.irSchema("plan-v2"), named)$valid)
    for (mutation in list(
      function(x) { x$populations$p["legacy_cohort_table"] <- list(NULL); x },
      function(x) { x$populations$p$legacy_cohort_table <- " 1 "; x },
      function(x) { x$populations$p$cohort_definition_id <- 1L; x },
      function(x) { x$populations$p$legacy_empty <- TRUE; x })) {
      expect_false(.jsonSchemaValidate(.irSchema("ir-v2"), mutation(named))$valid)
    }
    bad_kind <- named
    bad_kind$populations$p <- list(kind = "setop", legacy_cohort_table = "Owned",
      setop = list(op = "union", members = list("base", "p")))
    expect_false(.jsonSchemaValidate(.irSchema("ir-v2"), bad_kind)$valid)
    if (index) {
      named$populations$p$episode_policy <- "first_episode"
      expect_false(.jsonSchemaValidate(.irSchema("ir-v2"), named)$valid)
    } else {
      named$populations$p$legacy_episodes <- NULL
      expect_false(.jsonSchemaValidate(.irSchema("ir-v2"), named)$valid)
    }
  }
})

test_that("A-25 full execution retains sibling locality and pre-existing table ownership", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 1))
  ref <- .a25_ref(h)
  before <- DBI::dbReadTable(h$conn, ref)
  plan <- .a25_plan(paste0(" ", ref, " "), siblings = TRUE)
  expect_true(.planValidate(h, plan)$valid)
  result <- .planExecute(h, plan, list(demo = "D_demo", sibling = "D_sibling"))
  expect_identical(result$demo$person_id, c(1L, 3L, 5L, 7L, 9L, 11L))
  expect_identical(result$demo$gender_concept_id, rep(8507L, 6L))
  expect_identical(result$sibling$person_id, 1:15)
  expect_identical(DBI::dbReadTable(h$conn, ref), before)
  expect_true(ref %in% h$temp_tables)
  # A name alone supplies no filter index, even though its table has dates.
  plan$populations$p$filter_tree <- .a25_filter()
  err <- .a25_error(.planExecute(h, plan, list(demo = "D_demo")))
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "cohort index|reference_date|anchor")
})

test_that("A-25 criteria filters use the independent recurrent scope anchor and episode policy", {
  for (policy in c("first_episode", "last_episode", "any_episode", "all_episodes")) {
    h <- create_test_handle()
    withr::with_options(list(nfilter.subset = 3, nfilter.tab = 1), {
      .a25_events(h)
      # First three owned persons match only the first anchor, last three only
      # the last; everyone matches one. Own cohort dates remain in 2030.
      DBI::dbExecute(h$conn, paste0("DELETE FROM condition_occurrence WHERE ",
        "(person_id IN (1,3,5) AND condition_start_date = '2022-06-01') OR ",
        "(person_id IN (7,9,11) AND condition_start_date = '2021-06-01')"))
      DBI::dbExecute(h$conn, "UPDATE cohort SET cohort_start_date = '2021-06-01' WHERE cohort_definition_id = 10")
      DBI::dbExecute(h$conn, paste0("INSERT INTO cohort SELECT cohort_definition_id, subject_id, ",
        "'2022-06-01', cohort_end_date FROM cohort WHERE cohort_definition_id = 10"))
      ref <- .a25_ref(h)
      plan <- .a25_plan(ref, siblings = TRUE)
      plan$scope <- list(cohort = 10L)
      plan$populations$p$filter_tree <- .a25_filter()
      plan$populations$p$episode_policy <- policy
      ir <- .irPrepare(plan, h)
      expect_identical(ir$populations$p$legacy_index_anchor, "scope_cohort")
      expect_identical(ir$populations$p$episode_policy, policy)
      if (identical(policy, "all_episodes")) {
        # Empty remains a valid private population; another output still runs.
        result <- .planExecute(h, plan, list(sibling = "D_sibling"))
        expect_identical(result$sibling$person_id, 1:12)
        expect_error(.planExecute(h, plan, list(demo = "D_demo")),
                     "insufficient individuals|Insufficient|insufficient_support")
      } else {
        result <- .planExecute(h, plan, list(demo = "D_demo", sibling = "D_sibling"))
        want <- switch(policy, first_episode = c(1L, 3L, 5L),
                       last_episode = c(7L, 9L, 11L), any_episode = c(1L, 3L, 5L, 7L, 9L, 11L))
        expect_identical(result$demo$person_id, want)
        expect_identical(result$sibling$person_id, 1:12)
      }
    })
    cleanup_handle(h)
  }
})

test_that("A-25 index execution keeps recurrent event identity and its filter anchor", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 1))
  .a25_events(h)
  ref <- .a25_ref(h)
  plan <- .a25_plan(ref, index = TRUE)
  # Each event matches itself; evaluating against the named cohort would yield none.
  plan$populations$p$filter_tree <- .a25_filter()
  result <- .planExecute(h, plan, list(demo = "D_demo"))
  persons <- c(1L, 3L, 5L, 7L, 9L, 11L)
  expect_identical(sort(result$demo$person_id), rep(persons, each = 2L))
  expect_identical(result$demo$gender_concept_id, rep(8507L, 12L))
  # Supplemental episode-level evidence accompanies full execution above.
  pop <- .planResolvePopulations(h, plan, .buildBlueprint(h))$p
  rows <- DBI::dbGetQuery(h$conn, paste0("SELECT subject_id, cohort_start_date, ",
    "cohort_end_date, index_event_id FROM ", pop$cohort_table,
    " ORDER BY subject_id, cohort_start_date"))
  expect_identical(rows$subject_id, rep(persons, each = 2L))
  expect_identical(rows$cohort_start_date, rep(c("2021-06-01", "2022-06-01"), 6L))
  expect_identical(rows$cohort_end_date, rep("2024-12-31", 12L))
  expect_identical(rows$index_event_id, as.integer(rep(persons * 10L, each = 2L) + rep(1:2, 6L)))
})

test_that("A-25 tiny owned cohorts remain private while final support stays enforced", {
  for (index in c(FALSE, TRUE)) {
    h <- create_test_handle()
    withr::with_options(list(nfilter.subset = 3, nfilter.tab = 1), {
      .a25_events(h)
      ref <- .a25_ref(h, 1L)
      plan <- .a25_plan(ref, index, siblings = TRUE)
      expect_s3_class(.irPrepare(plan, h), "dsomop_ir")
      result <- .planExecute(h, plan, list(sibling = "D_sibling"))
      expect_identical(result$sibling$person_id, 1:15)
      expect_error(.planExecute(h, plan, list(demo = "D_demo")),
                   "insufficient individuals|Insufficient|insufficient_support")
      expect_true(ref %in% h$temp_tables)
      expect_identical(DBI::dbReadTable(h$conn, ref)$subject_id, 1L)
    })
    cleanup_handle(h)
  }
})

test_that("A-25 owned names do not prove DP locality through descendants or joins", {
  for (index in c(FALSE, TRUE)) {
    ir <- .irDocument(.irPrepare(.a25_plan("Owned", index, siblings = TRUE)))
    locality <- .dsomopDpIrLocality(ir, list())
    expect_false(locality[["demo"]])
    expect_true(locality[["sibling"]])
    ir$populations$child <- list(kind = "criteria", parent = "p")
    ir$outputs$child <- ir$outputs$demo
    ir$outputs$child$rows <- "child"
    ir$outputs$joined <- list(rows = "everyone", format = "joined",
      options = list(from = list(list(output = "demo"), list(output = "sibling"))))
    expect_true(.jsonSchemaValidate(.irSchema("ir-v2"), ir)$valid)
    expect_true(.dsomopDpIrLocality(ir, list())[["sibling"]])
    expect_false(.dsomopDpIrLocality(ir, list())[["child"]])
    expect_false(.dsomopDpIrLocality(ir, list())[["joined"]])
  }
})


test_that("A-25 public validation and explain reject before metadata discovery", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  ir <- .irPrepare(.a25_plan("unowned"))
  local_mocked_bindings(.buildBlueprint = function(...) stop("unexpected metadata query"),
                        .executeQuery = function(...) stop("unexpected result query"))
  for (ref in c("bad-name", "unowned")) {
    result <- .planValidate(h, .a25_plan(ref))
    expect_false(result$valid)
    expect_match(paste(result$errors, collapse = " "), "/populations/p/")
  }
  err <- .a25_error(.irExplain(ir, h))
  expect_s3_class(err, "dsomop_ir_validation_error")
  expect_identical(err$field, "/populations/p/legacy_cohort_table")
  outputless <- .a25_plan("unowned")
  outputless$outputs <- NULL
  err <- .a25_error(.irPrepare(outputless, h))
  expect_s3_class(err, "dsomop_ir_validation_error")
  expect_identical(err$field, "/populations/p/cohort_definition_id")
})

test_that("A-25 final DP preparation refuses unauthenticated owned names atomically", {
  state <- withr::local_tempdir(pattern = "dsomop-a25-dp-")
  Sys.chmod(state, "0700")
  withr::local_envvar(c(DSOMOP_STATE_DIR = state, DSOMOP_MARKER_DIR = NA_character_,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1", DSOMOP_DP_ENABLED = NA_character_,
    DSOMOP_DP_NOISE_ROOT = NA_character_, DSOMOP_DP_DOMAIN = NA_character_,
    DSOMOP_DP_SNAPSHOT_ID = NA_character_))
  withr::local_options(list(dsomop.state_dir = state, dsomop.dp.enabled = TRUE,
    dsomop.dp.domain = "dsomop-a25-test", dsomop.dp.snapshot_id = "a25-fixture",
    dsomop.dp.noise_root = as.raw(0:31), nfilter.subset = 3, nfilter.tab = 1))
  previous <- list(status = .pkg_state$dp_status, runtime = .pkg_state$dp_runtime,
                   in_progress = .pkg_state$dp_bootstrap_in_progress)
  on.exit({
    .pkg_state$dp_status <- previous$status
    .pkg_state$dp_runtime <- previous$runtime
    .pkg_state$dp_bootstrap_in_progress <- previous$in_progress
  }, add = TRUE)
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
  h <- create_test_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  .setLegacyTestPersonKey(h, "a25-dp", environment())
  .setHandle("a25_dp", h)
  on.exit(.removeHandle("a25_dp"), add = TRUE)
  .a25_events(h)
  ref <- .a25_ref(h)
  session <- new.env(parent = environment())
  for (index in c(FALSE, TRUE)) {
    session$plan <- .a25_plan(ref, index)
    err <- .a25_error(eval(quote(omopDpPlanExecuteDS("a25_dp", plan,
                                                   list(demo = "DP_a25"))), session))
    expect_s3_class(err, "dsomop_dp_rejection")
    expect_identical(err$code, "not_person_local")
    expect_false(exists("DP_a25", session, inherits = FALSE))
    expect_true(ref %in% h$temp_tables)
  }
})
