# A-20(b): matching evaluates source/search anchors independently on the
# final cases and on controls instantiated at each case time.

.ws7_match_anchor_fixture <- function(conn) {
  pids <- 9401:9407
  DBI::dbWriteTable(conn, "person", data.frame(
    person_id = pids, gender_concept_id = 8507L, year_of_birth = 1970L,
    month_of_birth = 1L, day_of_birth = 1L, race_concept_id = 8527L,
    ethnicity_concept_id = 38003564L), append = TRUE)
  DBI::dbWriteTable(conn, "observation_period", data.frame(
    observation_period_id = 940000L + pids, person_id = pids,
    observation_period_start_date = "2010-01-01", observation_period_end_date = "2030-12-31",
    period_type_concept_id = 44814724L), append = TRUE)
  event <- function(person, concept, date) data.frame(person_id = person, concept = concept, date = date)
  rows <- rbind(
    event(c(9401L, 9401L, 9402L), 941001L, c("2020-03-01", "2020-06-01", "2020-03-01")),
    event(rep(c(9401L, 9403:9405, 9407L), each = 2L), 941002L,
          rep(c("2020-02-20", "2020-05-20"), 5L)),
    event(rep(c(9401L, 9403:9405), each = 2L), 941003L,
          rep(c("2020-02-21", "2020-05-21"), 4L)),
    event(c(9401L, 9403L, 9404L), 941004L, c("2020-02-21", "2020-02-21", "2020-05-21")),
    event(9403:9407, 941005L, "2010-01-01"))
  DBI::dbWriteTable(conn, "condition_occurrence", data.frame(
    condition_occurrence_id = 940000L + seq_len(nrow(rows)), person_id = rows$person_id,
    condition_concept_id = rows$concept, condition_start_date = rows$date,
    condition_end_date = rows$date, condition_type_concept_id = 44818518L), append = TRUE)
  invisible(conn)
}

.ws7_match_anchor_backends <- .ws6_backends(extend = .ws7_match_anchor_fixture)
withr::defer(.ws4_cleanup(.ws7_match_anchor_backends), testthat::teardown_env())

test_that("matching strata execute nested source anchors on both sides at each case time", {
  event <- function(id) list(table = "condition_occurrence", concept_set = list(concepts = list(id)))
  events <- list(outcome = event(941001L), seed = event(941002L), anchor = event(941003L),
                 stratum = event(941004L), eligible = event(941005L))
  inner <- list(source = list(event = "seed"), which = "last",
                search = list(start = -30L, end = 0L, anchor = "index"))
  outer <- list(source = list(event = "anchor"), which = "first",
                search = list(start = 0L, end = 3L, anchor = inner))
  make_plan <- function(missing) .ws6_plan(events, list(
    cases = list(kind = "index", index = list(source = list(event = "outcome"), select = "all",
                                               end = list(strategy = "fixed", days = 0L))),
    controls = list(kind = "criteria", where = list(has = list(source = list(event = "eligible")))),
    matched = list(kind = "match", match = list(
      cases = "cases", controls = "controls", outcome = list(event = "outcome"),
      controls_per_case = 10L, missing_strata = missing,
      match = list(list(strata = list(source = list(event = "stratum"),
                                      window = list(start = 0L, end = 0L, anchor = outer))))))))
  # Expected membership is independent of the hash order: the requested
  # control count exceeds each complete eligible pool.
  # 9401: TRUE in March, FALSE in June; 9402: missing nested anchor.
  # 9403: TRUE/FALSE; 9404: FALSE/TRUE; 9405: FALSE/FALSE.
  # 9406 lacks the inner anchor; 9407 has the inner but lacks the outer.
  expected <- data.frame(set_id = c(1, 1, 2, 3, 3, 3),
                         person_id = c(9401, 9403, 9402, 9401, 9403, 9405),
                         is_case = c(1, 0, 1, 1, 0, 0))
  sort_members <- function(x) {
    x <- x[order(x$set_id, -x$is_case, x$person_id), c("set_id", "person_id", "is_case")]
    rownames(x) <- NULL
    x
  }
  for (missing in c("ineligible", "match_missing")) {
    plan <- make_plan(missing)
    ir <- .irPrepare(plan)
    inventory <- .irOrder(ir)$anchors
    expect_length(inventory$final$cases, 2L)
    expect_identical(inventory$final$cases, inventory$candidate$controls)
    expect_length(inventory$candidate$cases, 0L)
    expect_length(inventory$final$controls, 0L)
    want <- expected
    if (identical(missing, "match_missing")) {
      want <- rbind(want, data.frame(set_id = 2, person_id = c(9406, 9407), is_case = 0))
    }
    for (b in names(.ws7_match_anchor_backends)) {
      ctx <- .ws4_ctx(.ws7_match_anchor_backends[[b]]$handle, plan)
      got <- tryCatch(.ws6_roster(ctx, "matched"), finally = .planContextClose(ctx))
      expect_equal(sort_members(got), sort_members(want), info = paste(b, missing))
      expect_identical(sort(unique(got$index_date)), c("2020-03-01", "2020-06-01"), info = b)
      expect_equal(got$w, rep(1, nrow(got)), info = paste(b, missing))
    }
  }
})
