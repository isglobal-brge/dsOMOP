# A-20 integration: strata source/search anchors belong to final cases and
# candidate controls. Expected matches below come from the fixture dates,
# independently of the matching sampler and anchor compiler.
.int_a20_ids <- c(idx = 93201001L, pivot = 93201002L, landmark = 93201003L,
                  signal = 93201004L, eligible = 93201005L)

.int_a20_extend <- function(conn) {
  people <- c(9101L, 9102L, 9110:9114)
  DBI::dbWriteTable(conn, "person", data.frame(
    person_id = people, gender_concept_id = 8507L, year_of_birth = 1970L,
    month_of_birth = 1L, day_of_birth = 1L, race_concept_id = 8527L,
    ethnicity_concept_id = 38003564L, location_id = people, provider_id = people,
    care_site_id = people, person_source_value = paste0("A20_", people),
    gender_source_value = "M", stringsAsFactors = FALSE), append = TRUE)
  DBI::dbWriteTable(conn, "observation_period", data.frame(
    observation_period_id = 980000L + people, person_id = people,
    observation_period_start_date = "2010-01-01",
    observation_period_end_date = "2030-12-31", period_type_concept_id = 44814724L,
    stringsAsFactors = FALSE), append = TRUE)
  event <- function(person, kind, date) data.frame(person_id = person,
    condition_concept_id = .int_a20_ids[[kind]], condition_start_date = date,
    condition_end_date = date, stringsAsFactors = FALSE)
  rows <- rbind(
    event(9101L, "idx", c("2020-01-10", "2020-07-10")),
    event(9102L, "idx", "2020-01-10"),
    event(9110:9114, "eligible", "2019-01-01"),
    event(rep(c(9101L, 9110L, 9111L, 9113L, 9114L), each = 2L), "pivot",
          rep(c("2020-01-08", "2020-07-08"), 5L)),
    event(9102L, "pivot", "2020-01-08"),
    event(rep(c(9101L, 9110L, 9111L, 9112L, 9114L), each = 2L), "landmark",
          rep(c("2020-01-05", "2020-07-05"), 5L)),
    event(c(9101L, 9110L, 9112L, 9113L), "signal", "2020-01-05"),
    event(9111L, "signal", "2020-07-05"))
  rows$condition_occurrence_id <- 980000L + seq_len(nrow(rows))
  rows$condition_type_concept_id <- 44818518L
  rows$condition_source_value <- "A20"
  rows$condition_source_concept_id <- 0L
  # Deliberately insert in reverse date/key order; the anchor selectors and
  # matching case order must use declared identities, not physical order.
  DBI::dbWriteTable(conn, "condition_occurrence", rows[rev(seq_len(nrow(rows))), ], append = TRUE)
}

.int_a20_plan <- function(mode = "nested", missing = "ineligible", reverse = FALSE) {
  events <- lapply(.int_a20_ids, function(id) list(table = "condition_occurrence",
    concept_set = list(concepts = list(id), include_descendants = FALSE)))
  inner <- list(source = list(event = "pivot"), which = "last",
                search = list(start = -15L, end = 0L, anchor = "index"))
  outer <- list(source = list(event = "landmark"), which = "last",
                search = list(start = -4L, end = 0L, anchor = inner))
  window <- switch(mode,
    nested = list(start = 0L, end = 0L, anchor = outer),
    builtin = list(start = 0L, end = 0L, anchor = "index"),
    unbounded = list(start = NULL, end = NULL, anchor = "index"))
  pops <- list(
    matched = list(kind = "match", match = list(cases = "cases", controls = "tpl",
      outcome = list(event = "idx"), controls_per_case = 10L,
      missing_strata = missing, replacement = "between_sets", insufficient = "keep_available",
      match = list(list(strata = list(source = list(event = "signal"), window = window))))),
    tpl = list(kind = "criteria", where = list(has = list(source = list(event = "eligible"),
      window = list(start = 0L, end = 0L, anchor = list(source = list(event = "eligible")))))),
    cases = list(kind = "index", index = list(source = list(event = "idx"),
      select = "all", end = list(strategy = "fixed", days = 0L))))
  if (reverse) {
    events <- rev(events)
    pops <- rev(pops)
  }
  .ws6_plan(events = events, populations = pops,
            outputs = list(result = list(rows = "matched", format = "wide")))
}

.int_a20_expected <- function(mode, missing) {
  keys <- function(case, date, controls) paste(case, date, controls)
  if (mode == "builtin") return(c(
    keys(9101L, "2020-01-10", 9110:9114), keys(9101L, "2020-07-10", 9110:9114),
    keys(9102L, "2020-01-10", 9110:9114)))
  if (mode == "unbounded") return(c(
    keys(9101L, "2020-01-10", 9110:9113), keys(9101L, "2020-07-10", 9110:9113),
    keys(9102L, "2020-01-10", 9114L)))
  c(keys(9101L, "2020-01-10", 9110L),
    keys(9101L, "2020-07-10", c(9110L, 9114L)),
    if (missing == "match_missing") keys(9102L, "2020-01-10", c(9112L, 9113L)))
}

.int_a20_backends <- .ws6_backends(extend = .int_a20_extend)
withr::defer(.ws4_cleanup(.int_a20_backends), testthat::teardown_env())

test_that("A-20 integration compiler regression reaches every configured backend", {
  .ws5_expect_backends(.int_a20_backends, "A-20 matching source/search anchors")
})

test_that("matching compiles nested anchors independently on final cases and candidate controls", {
  variants <- list(
    list(mode = "nested", missing = "ineligible", reverse = FALSE),
    list(mode = "nested", missing = "match_missing", reverse = FALSE),
    list(mode = "nested", missing = "ineligible", reverse = TRUE),
    list(mode = "builtin", missing = "ineligible", reverse = FALSE),
    list(mode = "unbounded", missing = "ineligible", reverse = FALSE))
  for (backend in names(.int_a20_backends)) {
    original <- NULL
    for (variant in variants) {
      info <- paste(backend, variant$mode, variant$missing, variant$reverse)
      h <- .int_a20_backends[[backend]]$handle
      plan <- do.call(.int_a20_plan, variant)
      # Full wire/IR/semantic/site preparation, with no validation override.
      ir <- .irPrepare(plan, h)
      ctx <- .planContext(h, ir)
      tryCatch({
        order <- .irOrder(ir)
        expect_lt(match("cases", order$populations), match("matched", order$populations))
        expect_lt(match("tpl", order$populations), match("matched", order$populations))
        anchors <- order$anchors
        expect_length(anchors$candidate$cases, 0L)
        expect_length(anchors$final$tpl, 0L)
        expect_length(anchors$final$matched, 0L)
        nested <- variant$mode == "nested"
        expect_length(anchors$final$cases, if (nested) 2L else 0L)
        expect_length(anchors$candidate$tpl, if (nested) 3L else 1L)
        # The template's eligibility anchor must never enter the cases side.
        expect_false(any(vapply(anchors$final$cases, function(a) {
          identical(a$source$event, "eligible")
        }, logical(1))))

        result <- .compilePopulationByName(ctx, "matched")
        rows <- .ws6_roster(ctx, "matched")
        cases <- .fetchFragment(ctx, .fragmentGet(ctx,
          .fragmentName("mt", "matched", "cases", ctx = ctx)))
        pairs <- .fetchFragment(ctx, result$pairs)
        case_at <- match(as.character(pairs$set_rank), as.character(cases$set_rank))
        actual <- paste(as.character(cases$subject_id[case_at]),
          as.character(cases$t[case_at]), as.character(pairs$subject_id))
        expect_setequal(actual, .int_a20_expected(variant$mode, variant$missing))
        expect_equal(anyDuplicated(actual), 0L, info = info)
        expect_equal(sum(rows$is_case == 1), 3L, info = info)
        expect_equal(sum(rows$is_case == 0), length(actual), info = info)
        expect_true(all(rows$w == 1), info = info)
        selected <- rows[rows$is_case == 0, , drop = FALSE]
        selected_case <- match(as.character(selected$set_id), as.character(cases$set_rank))
        expect_setequal(paste(as.character(cases$subject_id[selected_case]),
          as.character(cases$t[selected_case]), selected$person_id), actual)

        if (nested) {
          final_roster <- .compilePopulationByName(ctx, "cases")$roster
          outer <- ir$populations$matched$match$match[[1L]]$strata$window$anchor
          for (anchor in list(outer, outer$search$anchor)) {
            case_anchor <- .compileAnchor(ctx, anchor, final_roster, "final")
            control_anchor <- .compileAnchor(ctx, anchor, result$candidates, "candidate")
            expect_false(identical(case_anchor$name, control_anchor$name), info = info)
            expect_identical(case_anchor$roster_namespace, "cases")
            expect_null(control_anchor$roster_namespace)
            expect_identical(.rosterSpec(result$candidates, "candidate")$population, "tpl")
            # Fetch the actual anchor SQL, independently checking row-key
            # association and changing dates across the recurrent case.
            for (entry in list(list(f = case_anchor, roster = final_roster, key = "cohort_row_id"),
                               list(f = control_anchor, roster = result$candidates, key = "cand_key"))) {
              got <- .fetchFragment(ctx, entry$f)
              roster <- .fetchFragment(ctx, entry$roster)
              at <- match(as.character(got$row_key), as.character(roster[[entry$key]]))
              expect_false(anyNA(at), info = info)
              expect_identical(as.character(got$person_id), as.character(roster$subject_id[at]))
              expected_day <- if (identical(anchor$source$event, "landmark")) "05" else "08"
              expect_identical(as.character(got$anchor_date),
                paste0(substr(as.character(roster$cohort_start_date[at]), 1L, 8L), expected_day))
            }
          }
          expect_error(.compileAnchor(ctx, list(source = list(event = "eligible")),
            final_roster, "final"), "not declared by the plan's anchor inventory")
        }
        if (variant$mode == "nested" && variant$missing == "ineligible") {
          if (is.null(original)) original <- rows else expect_identical(rows, original, info = info)
        }
      }, finally = .planContextClose(ctx))
      expect_length(h$temp_tables, 0L)
    }
  }
})
