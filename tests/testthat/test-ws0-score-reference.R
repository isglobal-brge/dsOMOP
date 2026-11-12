# WS-0 (CR-2 / amendment A-2): paired v1/v2 comorbidity-score expectations.
# The frozen file inst/plans/acceptance/score-reference.json is written by the
# independent oracle (tools/ws0/acceptance_expected.R -> tests/fixtures/
# score_reference.R). These tests (1) recompute it from the fixture, (2) check
# that the v1 engine still produces the `legacy` column (all-time history, age
# at the explicit reference date, no silent default) and (3) pin the documented
# v1/v2 differences the WS-4 native path must reproduce.

.ws0_score_fixture <- function(name) {
  tp <- tryCatch(testthat::test_path(), error = function(e) ".")
  Find(file.exists, c(file.path(tp, "..", "fixtures", name), file.path("tests", "fixtures", name)))
}
.ws0_score_reference_path <- function() {
  Find(file.exists, c(system.file("plans", "acceptance", "score-reference.json", package = "dsOMOP"),
                      file.path(tryCatch(testthat::test_path(), error = function(e) "."), "..", "..", "inst", "plans", "acceptance", "score-reference.json"),
                      "inst/plans/acceptance/score-reference.json"))
}
.ws0_case <- function(ref, cid, reference) {
  Find(function(cs) cs$cohort_definition_id == cid && cs$reference == reference, ref$cases)
}

test_that("the frozen score reference reproduces from the fixture by independent computation", {
  skip_if_not_installed("RSQLite")
  path <- .ws0_score_reference_path()
  skip_if(is.null(path) || !nzchar(path), "score-reference.json not found")
  for (f in c("create_reach_db.R", "extend_reach_db.R", "score_reference.R")) source(.ws0_score_fixture(f), local = TRUE)
  h <- build_extended_reach_handle(100L)
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  frozen <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  fresh <- compute_score_reference(h$conn)
  expect_identical(nrow(frozen$cases), 4L)
  for (i in seq_len(nrow(frozen$cases))) {
    fz <- frozen$cases[i, ]
    cs <- .ws0_case(fresh, fz$cohort_definition_id, fz$reference)
    expect_false(is.null(cs), info = paste(fz$cohort_definition_id, fz$reference))
    expect_identical(as.character(fz$reference_date), as.character(cs$reference_date))
    expect_identical(as.integer(fz$n_persons), as.integer(cs$n_persons))
    for (kind in c("charlson", "chads2", "chadsvasc")) {
      sc <- fz$scores[[kind]]
      expect_identical(as.integer(sc$person_id[[1]]), as.integer(cs$scores[[kind]]$person_id), info = paste(fz$reference, kind))
      expect_identical(as.integer(sc$legacy[[1]]), as.integer(cs$scores[[kind]]$legacy), info = paste(fz$reference, kind, "legacy"))
      expect_identical(as.integer(sc$native[[1]]), as.integer(cs$scores[[kind]]$native), info = paste(fz$reference, kind, "native"))
    }
  }
  # the pins are consistent with the tables they summarise
  c1 <- .ws0_case(fresh, 1L, "fixed_2020")
  expect_identical(as.integer(frozen$pins$charlson_history$legacy), 2L)
  expect_identical(as.integer(frozen$pins$charlson_history$native), 1L)
  expect_identical(c1$scores$charlson$legacy[c1$scores$charlson$person_id == 1011L], 2L)
  expect_identical(c1$scores$charlson$native[c1$scores$charlson$person_id == 1011L], 1L)
  expect_true(length(frozen$pins$age_reference_shift$chadsvasc_persons) > 0L)
  expect_true(length(frozen$pins$index_resolution$chads2_persons) > 0L)
})

test_that("the v1 engine matches the legacy column and still demands an explicit age reference", {
  skip_if_not_installed("RSQLite")
  path <- .ws0_score_reference_path()
  skip_if(is.null(path) || !nzchar(path), "score-reference.json not found")
  for (f in c("create_reach_db.R", "extend_reach_db.R")) source(.ws0_score_fixture(f), local = TRUE)
  h <- build_extended_reach_handle(100L)
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  dsOMOP:::.buildBlueprint(h)
  frozen <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  engine <- function(kind, ids, reference_date = NULL) {
    out <- dsOMOP:::.computeComorbidityScore(h, kind, ids, reference_date = reference_date)
    sc <- out$score[match(ids, out$person_id)]
    sc[is.na(sc)] <- 0L
    as.integer(sc)
  }
  for (i in seq_len(nrow(frozen$cases))) {
    fz <- frozen$cases[i, ]
    ids <- as.integer(fz$scores$charlson$person_id[[1]])
    # Charlson in v1 ignores history dates (all-time): the legacy column, for every reference
    expect_identical(engine("charlson", ids), as.integer(fz$scores$charlson$legacy[[1]]), info = paste("charlson", fz$reference))
    if (identical(fz$resolution, "fixed")) {
      for (kind in c("chads2", "chadsvasc")) {
        expect_identical(engine(kind, ids, reference_date = fz$reference_date), as.integer(fz$scores[[kind]]$legacy[[1]]),
                         info = paste(kind, fz$reference))
      }
    } else {
      # index resolution is a v2 (native) semantics: v1 has no default reference date
      expect_error(engine("chads2", ids), "requires an explicit reference_date")
      expect_error(engine("chadsvasc", ids), "requires an explicit reference_date")
      # the v1 column of this case is what an explicit reference_date equal to the index gives
      for (kind in c("chads2", "chadsvasc")) {
        expect_identical(engine(kind, ids, reference_date = fz$reference_date), as.integer(fz$scores[[kind]]$legacy[[1]]),
                         info = paste(kind, "index as explicit date"))
      }
    }
  }
  # the documented v1/v2 differences (what the WS-4 native path must reproduce)
  c1 <- frozen$cases[frozen$cases$cohort_definition_id == 1 & frozen$cases$reference == "fixed_2020", ]
  expect_identical(engine("charlson", 1011L), 2L)                              # v1: MI dated after the index counts
  expect_identical(as.integer(c1$scores$charlson$native[[1]][c1$scores$charlson$person_id[[1]] == 1011]), 1L)
  c30 <- frozen$cases[frozen$cases$cohort_definition_id == 1 & frozen$cases$reference == "fixed_2030", ]
  shifted <- as.integer(frozen$pins$age_reference_shift$chadsvasc_persons)
  p20 <- as.integer(c1$scores$chadsvasc$person_id[[1]]); p30 <- as.integer(c30$scores$chadsvasc$person_id[[1]])
  for (pid in shifted) {
    expect_true(as.integer(c30$scores$chadsvasc$native[[1]][p30 == pid]) > as.integer(c1$scores$chadsvasc$native[[1]][p20 == pid]), info = pid)
  }
  ix <- frozen$cases[frozen$cases$cohort_definition_id == 1002 & frozen$cases$reference == "index", ]
  pix <- as.integer(ix$scores$chads2$person_id[[1]])
  for (pid in as.integer(frozen$pins$index_resolution$chads2_persons)) {
    expect_true(as.integer(ix$scores$chads2$native[[1]][pix == pid]) < as.integer(ix$scores$chads2$legacy[[1]][pix == pid]), info = pid)
  }
})
