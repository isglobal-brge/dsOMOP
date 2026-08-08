# CohortMethod two-arm overlap normalisation. The expected contract mirrors
# OHDSI CohortMethod commit dd1a2a856ef608547a99d3db2d60d5c872f80dc6:
# keep the first treatment cohort in time, truncate it immediately before the
# second begins, and remove same-day ties from both arms.

cm_overlap_handle <- function(n_persons = 12L) {
  h <- create_test_handle(n_persons = n_persons)
  .buildBlueprint(h)
  h
}

cm_overlap_options <- function(subset = 1L) {
  list(
    nfilter.subset = subset,
    nfilter.tab = subset,
    dsomop.nfilter.dist = subset,
    dsomop.nfilter.band = 1L
  )
}

cm_overlap_write <- function(h, name, rows) {
  DBI::dbWriteTable(h$conn, name, rows, temporary = TRUE)
  register_test_temp(h, name)
  name
}

cm_overlap_fixture <- function(h) {
  a <- data.frame(
    subject_id = c(1L, 2L, 3L, 4L, 1L),
    cohort_start_date = c("2020-01-01", "2020-01-10", "2020-01-05",
                          "2020-02-01", "2021-01-01"),
    cohort_end_date = c("2020-01-31", "2020-02-10", "2020-01-20",
                        "2020-02-28", "2021-01-31"),
    stringsAsFactors = FALSE
  )
  b <- data.frame(
    subject_id = c(1L, 2L, 3L, 5L),
    cohort_start_date = c("2020-01-10", "2020-01-01", "2020-01-05",
                          "2020-03-01"),
    cohort_end_date = c("2020-02-10", "2020-01-31", "2020-01-20",
                        "2020-03-31"),
    stringsAsFactors = FALSE
  )
  c(
    cm_overlap_write(h, "cm_overlap_a", a),
    cm_overlap_write(h, "cm_overlap_b", b)
  )
}

test_that("CohortMethod arms use first-in-time, exclude ties, and truncate", {
  h <- cm_overlap_handle()
  on.exit(cleanup_handle(h))
  raw <- cm_overlap_fixture(h)

  withr::with_options(cm_overlap_options(), {
    effective <- .omopTwoPopCohorts(
      h, list(scoped_cohorts = raw), overlap_policy = "ohdsi_first")
    a <- .executeQuery(h, paste0(
      "SELECT * FROM ", effective$a, " ORDER BY subject_id"))
    b <- .executeQuery(h, paste0(
      "SELECT * FROM ", effective$b, " ORDER BY subject_id"))

    # Person 1 enters target first; person 2 enters comparator first. Person 3
    # starts both arms on the same day and is excluded. Persons 4/5 are unique.
    expect_equal(as.character(a$subject_id), c("1", "4"))
    expect_equal(as.character(b$subject_id), c("2", "5"))
    expect_length(intersect(as.character(a$subject_id),
                            as.character(b$subject_id)), 0L)

    # The winning eras stop the day before the losing arm begins. The later
    # recurrent target episode for person 1 is not a second study entry.
    expect_equal(as.character(a$cohort_end_date[a$subject_id == 1]),
                 "2020-01-09")
    expect_equal(as.character(b$cohort_end_date[b$subject_id == 2]),
                 "2020-01-09")
    expect_equal(nrow(a), 2L)
    expect_equal(nrow(b), 2L)
  })
})

test_that("cohort-overlap diagnostic still observes the protected raw arms", {
  h <- cm_overlap_handle()
  on.exit(cleanup_handle(h))
  raw <- cm_overlap_fixture(h)

  withr::with_options(cm_overlap_options(), {
    entry <- .omopDiagCohortOverlap()
    result <- entry$compute$fn(
      h, list(scoped_cohorts = raw), list())
    result <- .omopAnalysisGate(h, result, entry)

    # Raw shared persons are 1, 2 and 3. A normalised input would report zero,
    # so this also guards the diagnostic's explicit preserve policy.
    expect_equal(result$n[result$category == "both"], 3)
    expect_equal(result$n[result$category == "a_only"], 1)
    expect_equal(result$n[result$category == "b_only"], 1)
  })
})

test_that("effective arms are re-gated without an overlap-specific oracle", {
  h <- cm_overlap_handle()
  on.exit(cleanup_handle(h))
  a <- data.frame(
    subject_id = 1:5,
    cohort_start_date = rep("2020-01-01", 5),
    cohort_end_date = rep("2020-12-31", 5)
  )
  b <- data.frame(
    subject_id = c(1:3, 6:7),
    cohort_start_date = rep("2020-02-01", 5),
    cohort_end_date = rep("2020-12-31", 5)
  )
  raw <- c(
    cm_overlap_write(h, "cm_regate_a", a),
    cm_overlap_write(h, "cm_regate_b", b)
  )

  withr::with_options(cm_overlap_options(subset = 3L), {
    message <- tryCatch({
      .omopTwoPopCohorts(
        h, list(scoped_cohorts = raw), overlap_policy = "ohdsi_first")
      NA_character_
    }, error = conditionMessage)
    ordinary_gate_message <- tryCatch({
      .assertMinPersons(n_persons = 2L)
      NA_character_
    }, error = conditionMessage)

    # Both raw arms contain five people, but the effective comparator contains
    # only persons 6 and 7. It must fail exactly like any ordinary small subset.
    expect_identical(message, ordinary_gate_message)
    expect_false(grepl("overlap|intersect|shared", message,
                       ignore.case = TRUE))
  })
})

test_that("propensity roster consumes disjoint arms without MAX precedence", {
  h <- cm_overlap_handle()
  on.exit(cleanup_handle(h))
  raw <- cm_overlap_fixture(h)

  withr::with_options(cm_overlap_options(), {
    effective <- .omopTwoPopCohorts(
      h, list(scoped_cohorts = raw), overlap_policy = "ohdsi_first")
    design <- .omopCmPropensityDesign(
      h, effective$a, effective$b, integer(0), domain_code = "0")

    expect_identical(anyDuplicated(design$subject_id), 0L)
    expect_setequal(design$subject_id[design$arm == 1L], c("1", "4"))
    expect_setequal(design$subject_id[design$arm == 0L], c("2", "5"))
    expect_false(grepl(
      "MAX(u.arm)", paste(deparse(body(.omopCmPropensityDesign)),
                           collapse = "\n"), fixed = TRUE))
  })
})
