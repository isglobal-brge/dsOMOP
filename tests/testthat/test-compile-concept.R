# WS-4: concept sets (design-2.10.3 sections 2.1 and 6.5; defects A-E1, A-15)
# on the extended reach fixture, executed on every available backend.

.ws4_cs_backends <- .ws4_backends()
withr::defer(.ws4_cleanup(.ws4_cs_backends), testthat::teardown_env())

# Extra vocabulary rows for the mapping-direction, concept-0 and ingredient
# cases (identical on every backend).
.ws4_cs_seed <- function(h) {
  conn <- h$conn
  q <- function(t) .qualifyTable(h, t, h$cdm_schema)
  DBI::dbExecute(conn, paste0(
    "INSERT INTO ", q("concept_relationship"),
    " (concept_id_1, concept_id_2, relationship_id, valid_start_date, valid_end_date, invalid_reason) VALUES ",
    "(44000001, 201820, 'Maps to', ", .sqlDateLiteral(h, "1970-01-01"), ", ",
    .sqlDateLiteral(h, "2099-12-31"), ", NULL), ",
    "(44000002, 201820, 'Maps to', ", .sqlDateLiteral(h, "1970-01-01"), ", ",
    .sqlDateLiteral(h, "2099-12-31"), ", 'D')"))
  invisible(TRUE)
}
for (.b in .ws4_cs_backends) .ws4_cs_seed(.b$handle)

test_that("concept-set states are all, set and none; a resolved-empty set is never unrestricted", {
  for (b in names(.ws4_cs_backends)) {
    h <- .ws4_cs_backends[[b]]$handle
    expect_identical(.resolveConceptSetNode(h, NULL), list(ids = NULL, state = "all"), info = b)
    expect_identical(.resolveConceptSetNode(h, list(concepts = list(201820))),
                     list(ids = 201820L, state = "set"), info = b)
    # A-E1: {concepts = 123, exclude = 123} resolves to nothing -> none
    expect_identical(.resolveConceptSetNode(h, list(concepts = list(123), exclude = list(123))),
                     list(ids = integer(0), state = "none"), info = b)
    expect_identical(.resolveConceptSetNode(h, list(concepts = list()))$state, "none", info = b)
    expect_identical(.conceptPredicateSql("t.c", list(ids = integer(0), state = "none")), "1 = 0")
    expect_null(.conceptPredicateSql("t.c", list(ids = NULL, state = "all")))
  }
})

test_that("descendants, mapped sources and expanded exclusions resolve in the documented order", {
  for (b in names(.ws4_cs_backends)) {
    h <- .ws4_cs_backends[[b]]$handle
    res <- function(node) .resolveConceptSetNode(h, node)$ids
    # descendants (concept_ancestor): DM 201820 -> 201821, 201822
    expect_identical(res(list(concepts = list(201820), include_descendants = TRUE)),
                     c(201820L, 201821L, 201822L), info = b)
    # mapped = the 'Maps to' SOURCES of the set (invalid mappings ignored), never the targets
    expect_identical(res(list(concepts = list(201820), include_mapped = TRUE)),
                     c(201820L, 44000001L), info = b)
    expect_identical(res(list(concepts = list(44000001), include_mapped = TRUE)), 44000001L, info = b)
    # mapping applies to the set after descendants
    expect_identical(res(list(concepts = list(201820), include_descendants = TRUE, include_mapped = TRUE)),
                     c(201820L, 201821L, 201822L, 44000001L), info = b)
    # a bare exclusion is exact ids (v1 meaning)
    expect_identical(res(list(concepts = list(201820), include_descendants = TRUE, exclude = list(201820))),
                     c(201821L, 201822L), info = b)
    # A-15: an object-valued exclusion is expanded with its OWN flags
    expect_identical(res(list(concepts = list(201820), include_descendants = TRUE,
                              exclude = list(concepts = list(201820), include_descendants = TRUE))),
                     integer(0), info = b)
    expect_identical(res(list(concepts = list(201820), include_descendants = TRUE,
                              exclude = list(concepts = list(201820), include_descendants = FALSE))),
                     c(201821L, 201822L), info = b)
    # a set whose descendants are all excluded keeps only the root
    expect_identical(res(list(concepts = list(201820), include_descendants = TRUE,
                              exclude = list(concepts = list(201821, 201822)))),
                     201820L, info = b)
    # nested object exclusions resolve recursively
    expect_identical(res(list(concepts = list(201820), include_descendants = TRUE,
                              exclude = list(concepts = list(201820), include_descendants = TRUE,
                                             exclude = list(201822)))),
                     201822L, info = b)
    # concept 0 is an ordinary id (unmapped source rows), not "unrestricted"
    expect_identical(res(list(concepts = list(0), include_descendants = TRUE)), 0L, info = b)
  }
})

test_that("malformed concept-set nodes are typed refusals", {
  h <- .ws4_cs_backends$sqlite$handle
  .ws4_expect_rejection(.resolveConceptSetNode(h, list(concepts = list(1), lineage = "x")),
                        pattern = "unknown concept-set field")
  .ws4_expect_rejection(.resolveConceptSetNode(h, list(concepts = list(-1))))
  .ws4_expect_rejection(.resolveConceptSetNode(h, list(include_descendants = TRUE)),
                        pattern = "needs concepts")
  .ws4_expect_rejection(.resolveConceptSetNode(h, list(concepts = list(1), include_mapped = "yes")))
})

test_that("expansion over a missing vocabulary table is a typed unavailable_table refusal", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  DBI::dbExecute(h$conn, "DROP TABLE concept_relationship")
  h$blueprint <- NULL
  .ws4_expect_rejection(.resolveConceptSetNode(h, list(concepts = list(201820), include_mapped = TRUE)),
                        code = "unavailable_table", pattern = "concept_relationship")
  # no expansion requested: no vocabulary table needed
  expect_identical(.resolveConceptSetNode(h, list(concepts = list(201820)))$ids, 201820L)
  withr::local_options(dsomop.max_filter_values = 2L)
  .ws4_expect_rejection(.resolveConceptSetNode(h, list(concepts = list(201820), include_descendants = TRUE)),
                        pattern = "max_filter_values")
})

test_that("concept sets are memoised per plan on the canonical JSON of the node", {
  h <- .ws4_cs_backends$sqlite$handle
  plan <- .ws4_plan(
    events = list(a = list(table = "condition_occurrence", concept_set = "dm"),
                  b = list(table = "condition_occurrence",
                           concept_set = list(concepts = list(201820), include_descendants = TRUE))),
    concept_sets = list(dm = list(concepts = list(201820), include_descendants = TRUE)))
  ctx <- .ws4_ctx(h, plan)
  calls <- 0L
  real <- .vocabExpandConceptSet
  local_mocked_bindings(.vocabExpandConceptSet = function(handle, concept_set) {
    calls <<- calls + 1L
    real(handle, concept_set)
  })
  one <- .compileConceptSet(ctx, "dm")
  two <- .compileConceptSet(ctx, ctx$ir$events$b$concept_set)
  three <- .compileConceptSet(ctx, ctx$ir$concept_sets$dm)
  expect_identical(one, two)
  expect_identical(one, three)
  expect_identical(one$ids, c(201820L, 201821L, 201822L))
  expect_identical(calls, 1L)
  expect_identical(.compileConceptSet(ctx, NULL), list(ids = NULL, state = "all"))
  .ws4_expect_rejection(.compileConceptSet(ctx, "missing_set"), pattern = "dangling concept set")
})

test_that("the empty-set test: a set resolving to nothing yields zero rows and FALSE criteria (A-E1)", {
  for (b in names(.ws4_cs_backends)) {
    h <- .ws4_cs_backends[[b]]$handle
    plan <- .ws4_plan(events = list(
      empty = list(table = "condition_occurrence",
                   concept_set = list(concepts = list(123), exclude = list(123))),
      dm = list(table = "condition_occurrence", concept_set = list(concepts = list(201820)))))
    ctx <- .ws4_ctx(h, plan)
    f <- .compileEventByName(ctx, "empty")
    expect_match(.fragmentGet(ctx, "ev_empty_src")$select_body, "WHERE 1 = 0", fixed = TRUE, info = b)
    expect_identical(nrow(.ws4_rows(ctx, "empty")), 0L, info = b)
    expect_gt(nrow(.ws4_rows(ctx, "dm")), 0L)
    # A criterion over the event (presence per person) is FALSE for everyone;
    # absence is TRUE for everyone: never the unrestricted stream.
    person <- .qualifyTable(h, "person", h$cdm_schema)
    sql <- .buildStatement(ctx, f, select = paste0(
      "SELECT p.person_id, CASE WHEN EXISTS (SELECT 1 FROM ", f$name,
      " e WHERE e.person_id = p.person_id) THEN 1 ELSE 0 END AS has_event FROM ",
      person, " p ORDER BY p.person_id"), dialect = ctx)
    crit <- .executeQuery(h, sql)
    expect_gt(nrow(crit), 100L)
    expect_true(all(crit$has_event == 0L), info = b)
  }
})

test_that("v1 feature specs whose declared set resolves to nothing count no rows (A-E1, corrected)", {
  h <- .ws4_cs_backends$sqlite$handle
  specs <- .resolveFeatureSpecs(h, list(
    dm_excluded = list(type = "count", concept_set = list(concepts = 201820L, exclude = 201820L)),
    dm = list(type = "count", concept_set = list(concepts = 201820L))),
    table = "condition_occurrence")
  expect_identical(specs$dm_excluded$concept_state, "none")
  expect_identical(specs$dm$concept_state, "set")
  # resolving again (as the extraction does) keeps the state
  again <- .resolveFeatureSpecs(h, specs, table = "condition_occurrence")
  expect_identical(again$dm_excluded$concept_state, "none")
  # the shared stream is scoped to the non-empty spec only
  scope <- .featureScopeFilter(specs, "condition_concept_id")
  expect_identical(scope, list(var = "condition_concept_id", op = "in", value = 201820L))
  # every spec empty: a predicate no row satisfies, never NULL (unscoped)
  only_empty <- .featureScopeFilter(specs["dm_excluded"], "condition_concept_id")
  expect_identical(names(only_empty), "and")
  df <- .executeQuery(h, "SELECT person_id, condition_concept_id, condition_start_date FROM condition_occurrence")
  expect_false(any(.evalFilterMask(only_empty, df) %in% TRUE))
  feats <- .toFeatures(df, "condition_occurrence", specs = specs,
                       person_ids = unique(df$person_id),
                       default_concept_col = "condition_concept_id")
  expect_true(all(feats$dm_excluded == 0L))
  expect_true(any(feats$dm > 0L))
})

test_that("the RxNorm ingredient map maps drug concepts to Ingredient ancestors only", {
  for (b in names(.ws4_cs_backends)) {
    h <- .ws4_cs_backends[[b]]$handle
    q <- function(t) .qualifyTable(h, t, h$cdm_schema)
    DBI::dbExecute(h$conn, paste0(
      "INSERT INTO ", q("concept"), " (concept_id, concept_name, domain_id, vocabulary_id, ",
      "concept_class_id, standard_concept, concept_code, valid_start_date, valid_end_date, invalid_reason) VALUES ",
      "(40000001, 'cisplatin 50 MG Injection (fixture)', 'Drug', 'RxNorm', 'Clinical Drug', 'S', '40000001', ",
      .sqlDateLiteral(h, "1970-01-01"), ", ", .sqlDateLiteral(h, "2099-12-31"), ", NULL)"))
    DBI::dbExecute(h$conn, paste0(
      "INSERT INTO ", q("concept_ancestor"), " (ancestor_concept_id, descendant_concept_id, ",
      "min_levels_of_separation, max_levels_of_separation) VALUES (1718850, 40000001, 1, 1), ",
      "(4329847, 40000001, 1, 1)"))
    h$blueprint <- NULL
    ctx <- .ws4_ctx(h, .ws4_plan(events = list(x = list(table = "drug_exposure"))))
    f <- .compileIngredientMap(ctx)
    expect_identical(f$grain, "none")
    expect_identical(.compileIngredientMap(ctx), f)
    map <- .resolveIngredientMap(ctx, c(40000001L, 1718850L, 201820L))
    expect_identical(map$descendant_concept_id, c(1718850L, 40000001L), info = b)
    expect_identical(map$ingredient_concept_id, c(1718850L, 1718850L), info = b)
  }
})

test_that("the reviewed operator classes drive the censored policy", {
  oc <- .operatorClasses()
  expect_identical(.operatorInequalityIds(), c(4171754L, 4171755L, 4171756L, 4172704L))
  expect_identical(oc$class[oc$concept_id == 4172703L], "equality")
})
