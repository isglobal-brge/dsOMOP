# WS-0 (design-2.10.3 sections 10.7 and 12.1; amendments A-1..A-7; review
# acceptance dispositions): the acceptance corpus and its expected results.

.ws0_corpus_dir <- function() {
  d <- system.file("plans", "acceptance", package = "dsOMOP")
  if (!nzchar(d)) d <- Find(dir.exists, c("inst/plans/acceptance", file.path(testthat::test_path(), "..", "..", "inst", "plans", "acceptance")))
  d
}
.ws0_expected <- function(d, dir) jsonlite::fromJSON(file.path(d, dir, "expected.json"), simplifyVector = FALSE)
.ws0_rows <- function(d, dir, output) {
  e <- jsonlite::fromJSON(file.path(d, dir, "expected.json"), simplifyVector = TRUE)
  e$outputs[[output]]$rows
}

test_that("fifteen recipes with validated plans, recipes, semantics and manifested expected results", {
  d <- .ws0_corpus_dir()
  skip_if(is.null(d) || !nzchar(d), "corpus not found")
  dirs <- sort(list.dirs(d, full.names = FALSE, recursive = FALSE))
  expect_length(dirs, 15L)
  expect_identical(substr(dirs, 1L, 2L), sprintf("%02d", 1:15))
  schema <- dsOMOP:::.jsonSchemaPackaged("plan-v2"); ir <- dsOMOP:::.jsonSchemaPackaged("ir-v2")
  for (dir in dirs) {
    path <- file.path(d, dir)
    for (f in c("recipe.R", "plan.json", "README.md", "expected.json")) expect_true(file.exists(file.path(path, f)), info = paste(dir, f))
    plan <- dsOMOP:::.jsonSchemaLoad(file.path(path, "plan.json"))
    res <- dsOMOP:::.jsonSchemaValidate(schema, plan)
    expect_true(res$valid, info = paste(dir, paste(res$errors, collapse = "; ")))
    expect_true(dsOMOP:::.jsonSchemaValidate(ir, plan)$valid, info = dir)
    expect_silent(parse(file.path(path, "recipe.R")))
    # recipe.R declares the plan's result symbols
    code <- paste(readLines(file.path(path, "recipe.R"), warn = FALSE), collapse = "\n")
    for (o in names(plan$outputs)) {
      sym <- plan$outputs[[o]]$result_symbol
      # the client's default symbol is D_<output>; any other symbol must be declared in recipe.R
      if (!is.null(sym) && !identical(sym, paste0("D_", o))) expect_true(grepl(sym, code, fixed = TRUE), info = paste(dir, o, sym))
    }
    exp <- .ws0_expected(d, dir)
    expect_identical(exp$recipe, dir)
    expect_identical(exp$nfilter_subset, 5L)
    expect_true(all(c("person_id", "index_date") %in% unlist(exp$fixture_keys)), info = dir)
    expect_true(is.list(exp$invariants) && length(exp$invariants) > 0L, info = dir)
    outputs <- names(plan$outputs)
    for (o in names(exp$outputs)) {
      comp <- exp$outputs[[o]]
      base <- sub("\\..*$", "", o)
      expect_true(base %in% outputs, info = paste(dir, o))
      expect_gte(comp$n_persons, 5L)
      released <- unlist(comp$released_columns); cmp <- unlist(comp$comparison_columns); oracle_only <- unlist(comp$oracle_only)
      expect_true(all(c("row_id", "cohort_row_id", "person_id") %in% released), info = paste(dir, o))
      expect_true(all(cmp %in% released), info = paste(dir, o, "comparison columns must be released columns"))
      expect_false(any(c("index_date", "index_key", "n_eligible") %in% released), info = paste(dir, o))
      expect_true(all(oracle_only %in% comp$columns), info = paste(dir, o))
      expect_true(all(setdiff(comp$columns, c(unlist(exp$fixture_keys), oracle_only)) %in% released), info = paste(dir, o))
      expect_true(all(cmp %in% comp$columns), info = paste(dir, o))
      expect_false(any(oracle_only %in% released), info = paste(dir, o))
    }
  }
  # the twelve parametrised variants of item 13
  vdir <- file.path(d, "13-parametrised", "variants")
  manifest <- jsonlite::fromJSON(file.path(vdir, "variants.json"), simplifyVector = FALSE)
  expect_length(manifest, 12L)
  for (m in manifest) {
    expect_true(file.exists(file.path(vdir, m$variant))); expect_silent(parse(file.path(vdir, m$variant)))
    v <- jsonlite::fromJSON(file.path(vdir, m$bound_plan), simplifyVector = FALSE)
    base <- jsonlite::fromJSON(file.path(d, m$equals_plan_of, "plan.json"), simplifyVector = FALSE)
    expect_identical(dsOMOP:::.jcsCanonical(v), dsOMOP:::.jcsCanonical(base), info = m$variant)
    expect_true(file.exists(file.path(d, m$expected_from)))
  }
})

test_that("the design's fixture values and the review's corrections are pinned", {
  d <- .ws0_corpus_dir()
  skip_if(is.null(d) || !nzchar(d), "corpus not found")
  get <- function(dir, output) .ws0_rows(d, dir, output)
  adh <- get("15-costs-adherence", "adherence")
  p1003 <- adh[adh$person_id == 1003, ]
  expect_identical(p1003$pdc_num_eff_10, 4L); expect_identical(p1003$observed_10, 4L); expect_identical(p1003$covered_eff_10, 10L)
  p1004 <- adh[adh$person_id == 1004, ]
  expect_identical(p1004$covered_rec_90, 44L); expect_identical(p1004$covered_eff_90, 60L)
  expect_equal(as.numeric(adh$visit_cost_60[adh$person_id == 1017]), 150); expect_equal(as.numeric(adh$drug_cost_60[adh$person_id == 1017]), 20)
  cov <- get("06-coverage-persistence", "coverage")
  expect_identical(cov$covered_rec[cov$person_id == 1004], 44L); expect_identical(cov$covered_eff[cov$person_id == 1004], 60L)
  expect_identical(cov$covered_rec[cov$person_id == 1005], 1L)
  pers <- get("06-coverage-persistence", "persistence")
  expect_true(all(pers$follow_up_days == pers$exit_days_from_index - pers$entry_days_from_index))
  labs <- get("04-units-censored", "labs")
  p1006 <- labs[labs$person_id == 1006, ]
  expect_equal(p1006$a1c_closest, 0.0915 * 52 + 2.15)                          # same-day tie: first by key
  expect_identical(p1006$a1c_n_ge65, 5L); expect_identical(p1006$a1c_n_uncensored, 5L); expect_identical(p1006$a1c_n_known_unit, 6L)
  days <- get("03-distinct-days", "two_days")
  expect_identical(days$dm_days[days$person_id == 1008], 2L)
  expect_identical(sort(days$person_id), c(1:25, 1008L))
  expect_identical(get("13-parametrised", "two_days"), days)
  ep <- get("05-same-day-same-visit", "episodes")
  expect_identical(sum(ep$person_id == 1007), 2L); expect_true(all(ep$pci_same_visit[ep$person_id == 1007] == 1L))
  t1 <- get("01-characterisation", "table1")
  expect_identical(sort(t1$person_id[t1$cond_covar2 == 1L]), c(1013L, 1014L))
  expect_true(is.na(t1$age[t1$person_id == 1009])); expect_true(is.na(t1$age[t1$person_id == 1010]))
  expect_identical(t1$charlson[t1$person_id == 1], 3L); expect_identical(t1$charlson[t1$person_id == 5], 4L)
  expect_identical(t1$charlson[t1$person_id == 1011], 1L)                       # A-2: MI after the index does not count
  expect_false(1001L %in% t1$person_id)
  # 02: arm-conditioned censoring, own-arm restart, first-ineligible-then-eligible
  base <- get("02-active-comparator", "baseline")
  expect_identical(base$index_date[base$person_id == 1031], "2020-05-01")       # the ineligible first era is skipped
  expect_identical(base$arm[base$person_id == 1032], "acei")
  ot <- get("02-active-comparator", "tte.on_treatment"); itt <- get("02-active-comparator", "tte.itt")
  expect_identical(itt$exit_days_from_index[itt$person_id == 1032], 259L)         # censored by the ARB switch, not by the ACEI restart
  expect_identical(itt$event[itt$person_id == 1032], 0L)
  expect_true(all(itt$follow_up_days == itt$exit_days_from_index - itt$entry_days_from_index))
  # 07: dense panel with the whole-site-empty source and the two-identity source
  pan <- get("07-monthly-panel", "panel")
  expect_true(all(pan$bmi_count == 0L)); expect_true(all(is.na(pan$bmi_last))); expect_true(all(is.na(pan$bmi_carry)))
  expect_true(all(c("vitals_3025315_last", "vitals_3004249_last", "vitals_3025315_count", "vitals_3004249_count") %in% names(pan)))
  expect_identical(sum(pan$person_id == 1002), 2L)                               # January and February only
  expect_identical(sum(pan$person_id == 1), 5L)
  # 08: the width-365 bin boundary adds the last-day period
  m <- get("08-sccs-matrix", "matrix")
  expect_identical(nrow(m), 139L)
  expect_true(any(m$person_id == 1 & m$period_start_days == 365L & m$period_end_days == 365L))
  # 09: the ineligible episode (death before entry) has neither a sentinel nor a risk-set row
  ev <- get("09-recurrent-events", "recurrent.events"); rs <- get("09-recurrent-events", "recurrent.risk_sets")
  expect_false(1011L %in% ev$person_id); expect_false(1011L %in% rs$person_id)
  expect_identical(nrow(ev), 124L); expect_identical(length(unique(ev$person_id)), 121L)
  expect_identical(sum(ev$event == 1L), 5L); expect_identical(sum(ev$event == 0L), 119L)
  expect_true(all(rs$exit_days_from_index >= rs$entry_days_from_index))
  expect_true(all(rs$follow_up_days == rs$exit_days_from_index - rs$entry_days_from_index))
  # 10: zero-control case kept; n_eligible is oracle-only
  cc <- get("10-matching", "cc_table")
  z <- cc[cc$person_id == 1012, ]; expect_identical(nrow(z), 1L); expect_identical(z$is_case, 1L); expect_identical(sum(cc$set_id == z$set_id), 1L)
  e10 <- .ws0_expected(d, "10-matching")
  expect_true("n_eligible" %in% unlist(e10$outputs$cc_table$oracle_only))
  expect_false("n_eligible" %in% unlist(e10$outputs$cc_table$released_columns))
  # 14: first-line duration
  fl <- get("14-oncology-episodes", "first_line")
  expect_identical(fl$line_days[fl$person_id == 1015], 214L); expect_identical(fl$line_days[fl$person_id == 1016], 123L)
  expect_true(all(fl$line_days[fl$person_id %in% 1:8] == 90L))
  onc <- get("14-oncology-episodes", "episodes")
  expect_identical(onc$first_member_days[onc$person_id == 1015 & onc$index_key == "2"], -172L)
})
