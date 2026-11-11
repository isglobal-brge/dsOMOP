# WS-0 (design-2.10.3 sections 10.7 and 12.1): the acceptance corpus.

.ws0_corpus_dir <- function() {
  d <- system.file("plans", "acceptance", package = "dsOMOP")
  if (!nzchar(d)) d <- Find(dir.exists, c("inst/plans/acceptance", file.path(testthat::test_path(), "..", "..", "inst", "plans", "acceptance")))
  d
}

test_that("fifteen recipes with validated plans, recipes, semantics and expected results", {
  d <- .ws0_corpus_dir()
  skip_if(is.null(d) || !nzchar(d), "corpus not found")
  dirs <- sort(list.dirs(d, full.names = FALSE, recursive = FALSE))
  expect_length(dirs, 15L)
  expect_identical(substr(dirs, 1L, 2L), sprintf("%02d", 1:15))
  schema <- dsOMOP:::.jsonSchemaPackaged("plan-v2")
  for (dir in dirs) {
    path <- file.path(d, dir)
    for (f in c("recipe.R", "plan.json", "README.md", "expected.json")) {
      expect_true(file.exists(file.path(path, f)), info = paste(dir, f))
    }
    plan <- dsOMOP:::.jsonSchemaLoad(file.path(path, "plan.json"))
    res <- dsOMOP:::.jsonSchemaValidate(schema, plan)
    expect_true(res$valid, info = paste(dir, paste(res$errors, collapse = "; ")))
    expect_identical(plan$version, 2L, info = dir)
    expect_silent(parse(file.path(path, "recipe.R")))
    exp <- jsonlite::fromJSON(file.path(path, "expected.json"), simplifyVector = FALSE)
    expect_identical(exp$recipe, dir)
    expect_identical(exp$nfilter_subset, 5L)
    # every expected output corresponds to a plan output (survival TARs add .<tar> components)
    outputs <- names(plan$outputs)
    for (o in names(exp$outputs)) {
      base <- sub("\\..*$", "", o)
      expect_true(base %in% outputs, info = paste(dir, o))
      expect_gte(exp$outputs[[o]]$n_persons, 5L)
      expect_true(all(c("person_id", "index_date") %in% exp$outputs[[o]]$columns), info = paste(dir, o))
    }
  }
})

test_that("the design's fixture values are pinned in the expected results", {
  d <- .ws0_corpus_dir()
  skip_if(is.null(d) || !nzchar(d), "corpus not found")
  get <- function(dir, output) {
    e <- jsonlite::fromJSON(file.path(d, dir, "expected.json"), simplifyVector = TRUE)
    e$outputs[[output]]$rows
  }
  adh <- get("15-costs-adherence", "adherence")
  p1003 <- adh[adh$person_id == 1003, ]
  expect_identical(p1003$pdc_num_eff_10, 4L); expect_identical(p1003$observed_10, 4L)   # 4/4, never 10/4
  expect_identical(p1003$covered_eff_10, 10L)
  p1004 <- adh[adh$person_id == 1004, ]
  expect_identical(p1004$covered_rec_90, 44L); expect_identical(p1004$covered_eff_90, 60L)
  cov <- get("06-coverage-persistence", "coverage")
  expect_identical(cov$covered_rec[cov$person_id == 1004], 44L)
  expect_identical(cov$covered_eff[cov$person_id == 1004], 60L)
  expect_identical(cov$covered_rec[cov$person_id == 1005], 1L)                          # NULL supply -> one-day record; end < start dropped
  labs <- get("04-units-censored", "labs")
  p1006 <- labs[labs$person_id == 1006, ]
  expect_equal(p1006$a1c_closest, 0.0915 * 52 + 2.15)
  expect_identical(p1006$a1c_n_ge65, 4L); expect_identical(p1006$a1c_n_uncensored, 4L); expect_identical(p1006$a1c_n_known_unit, 5L)
  days <- get("03-distinct-days", "two_days")
  expect_identical(days$dm_days[days$person_id == 1008], 2L)
  expect_identical(sort(days$person_id), c(1:25, 1008L))
  expect_identical(get("13-parametrised", "two_days"), days)
  ep <- get("05-same-day-same-visit", "episodes")
  expect_identical(sum(ep$person_id == 1007), 2L)
  expect_true(all(ep$pci_same_visit[ep$person_id == 1007] == 1L))
  t1 <- get("01-characterisation", "table1")
  expect_identical(sort(t1$person_id[t1$cond_covar2 == 1L]), c(1013L, 1014L))
  expect_true(is.na(t1$age[t1$person_id == 1009])); expect_true(is.na(t1$age[t1$person_id == 1010]))
  expect_identical(t1$charlson[t1$person_id == 1], 3L)                                   # mi + liver + dm
  expect_identical(t1$charlson[t1$person_id == 5], 4L)                                   # + chf
  cc <- get("10-matching", "cc_table")
  expect_true(all(cc$w[cc$is_case == 1L] == 1))
  z <- cc[cc$person_id == 1012, ]; expect_identical(nrow(z), 1L); expect_identical(z$is_case, 1L)
  expect_identical(sum(cc$set_id == z$set_id), 1L)                                      # zero-control case kept
  rec <- get("09-recurrent-events", "recurrent")
  expect_identical(sum(rec$event == 1L), 3L)
  expect_identical(sum(rec$event == 0L), length(unique(rec$person_id)) - 1L)
})
