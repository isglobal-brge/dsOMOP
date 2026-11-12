# Optional cross-branch component probe, deliberately outside the package suite.
# Copy this file to tests/testthat/test-ir-review-anchor-integration.R in an
# isolated snapshot. Export the following files there using git show from
# WS-5 commit 16860779bc1c02e33dcc02ee20fbc99162638cfb (never its live worktree):
#   tools/ws2/artifacts/ws5-anchor-16860779/compile_anchor.R
#   tools/ws2/artifacts/ws5-anchor-16860779/compile_interval.R
# Then run tools/rtest.sh on that snapshot with filter ir-review-anchor-integration.
# The production IR and event compiler remain this branch's implementations;
# the exact WS-5 modules are loaded into an isolated lexical environment.

test_that("A-20(b) named and nested anchors compile after full IR preparation with pinned WS-5", {
  ws5 <- new.env(parent = asNamespace("dsOMOP"))
  source_dir <- testthat::test_path("..", "..", "tools", "ws2", "artifacts", "ws5-anchor-16860779")
  for (file in c("compile_anchor.R", "compile_interval.R")) {
    sys.source(file.path(source_dir, file), envir = ws5)
  }
  # Register the WS-5 family exactly as that branch's statement builder does,
  # keeping this branch's event allocator and its fail-closed behavior.
  original_generators <- .fragmentIdentityGenerators
  testthat::local_mocked_bindings(.fragmentIdentityGenerators = function() {
    generators <- original_generators()
    generators$anc <- ws5$.anchorFragmentIdentities
    generators
  })
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  first <- list(source = list(event = "conditions"))
  explicit <- list(source = list(event = "conditions"), which = "first", n = 1L,
                   field = "start", scope = "person", search = NULL)
  nested <- list(source = list(event = "conditions"),
                 search = list(start = 1L, end = 36500L, anchor = first))
  plan <- .ws4_plan(events = list(conditions = list(table = "condition_occurrence")))
  plan$populations <- list(
    cases = list(kind = "index", index = list(source = list(event = "conditions"))),
    controls = list(kind = "criteria"),
    matched = list(kind = "match", match = list(cases = "cases", controls = "controls",
      outcome = list(event = "conditions"), match = list(list(strata = list(
        source = list(event = "conditions"), window = list(start = -365L, end = 0L, anchor = nested)))))))
  plan$outputs <- list(dates = list(rows = "matched", format = "long",
    sources = list(list(event = "conditions")), options = list(anchors = list(
      first_omitted = first, first_explicit = explicit, next_date = nested))))
  ir <- .irPrepare(plan, handle)
  ctx <- .planContext(handle, ir)
  on.exit(.planContextClose(ctx), add = TRUE)
  expect_length(.irOrder(ir)$anchors$final$matched, 2L)
  expect_length(.irOrder(ir)$anchors$candidate$controls, 2L)
  roster <- function(phase) {
    key <- if (phase == "final") "cohort_row_id" else "cand_key"
    fields <- list(person_id = .fragmentColumn("integer", FALSE, "person_key"))
    fields[[key]] <- .fragmentColumn("integer", FALSE, "episode_key")
    fields$index_source <- .fragmentColumn("text", FALSE, "origin_identity")
    fields$index_key <- .fragmentColumn("text", FALSE, "origin_identity")
    result <- .fragment(paste0("probe_", phase), if (phase == "final") "roster" else "candidates",
      "episode", paste0("SELECT person_id, person_id AS ", key,
                         ", 'conditions' AS index_source, CAST(person_id AS TEXT) AS index_key FROM person"), fields,
      unique_key = key, person_lineage = list(person_col = "person_id", via_roster = NULL),
      source_identity_columns = c("index_source", "index_key"),
      roster_namespace = if (phase == "final") "matched" else NULL)
    if (phase == "candidate") attr(result, "population") <- "controls"
    .fragmentRegister(ctx, result)
    result
  }
  rows <- function(anchor, roster, phase) {
    fragment <- ws5$.compileAnchor(ctx, anchor, roster, phase)
    .executeQuery(handle, .buildStatement(ctx, fragment,
      select = paste0("SELECT person_id, anchor_date FROM ", fragment$name, " ORDER BY person_id"),
      dialect = ctx))
  }
  expected_first <- DBI::dbGetQuery(handle$conn, paste(
    "SELECT person_id, MIN(condition_start_date) AS anchor_date",
    "FROM condition_occurrence GROUP BY person_id ORDER BY person_id"))
  expected_next <- DBI::dbGetQuery(handle$conn, paste(
    "SELECT c.person_id, MIN(c.condition_start_date) AS anchor_date FROM condition_occurrence c",
    "WHERE c.condition_start_date > (SELECT MIN(p.condition_start_date) FROM condition_occurrence p",
    "WHERE p.person_id = c.person_id) GROUP BY c.person_id ORDER BY c.person_id"))
  for (phase in c("final", "candidate")) {
    current_roster <- roster(phase)
    expect_identical(rows(first, current_roster, phase), expected_first)
    expect_identical(rows(explicit, current_roster, phase), expected_first)
    expect_identical(rows(nested, current_roster, phase), expected_next)
    expect_identical(ws5$.compileAnchor(ctx, first, current_roster, phase)$name,
                     ws5$.compileAnchor(ctx, explicit, current_roster, phase)$name)
    expect_error(ws5$.compileAnchor(ctx, list(source = list(event = "conditions"), which = "last"),
                                   current_roster, phase), "Anchor not declared")
  }
})
