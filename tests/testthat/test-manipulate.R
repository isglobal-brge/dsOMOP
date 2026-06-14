# ==============================================================================
# Tests: Disclosure-Gated Data Manipulation verbs (R/manipulate.R)
#
# omopMergeDS / omopFilterDS / omopSelectDS / omopBindRowsDS operate on the
# token-keyed, `omop.table`-classed frames produced by .pseudonymizeIdentifiers.
# These tests assert each verb:
#   (a) ERRORs on non-omop.table input;
#   (b) returns an omop.table that is still a data.frame;
#   (c) FAILS CLOSED when the result would describe < nfilter.subset distinct
#       person tokens;
#   (d) produces correct results above the threshold;
#   (e) bind_rows of a table with ITSELF is blocked when distinct persons stay
#       below threshold (proving the rbind-doubling bypass is defeated).
#
# Plus regression tests proving the profiling endpoints now suppress on DISTINCT
# person_id (a value backed by many records but few persons is dropped/blocked).
# ==============================================================================

# Build an omop.table exactly as the assign path does: run a plain data.frame
# through .pseudonymizeIdentifiers so it carries the omop.table class +
# dsomop_protected attribute and a per-session token-keyed person_id.
.mk_omop.table <- function(df, salt = as.raw(1:16)) {
  dsOMOP:::.pseudonymizeIdentifiers(df, salt)
}

.mk_persons <- function(n, extra = NULL, salt = as.raw(1:16)) {
  df <- data.frame(person_id = seq_len(n), stringsAsFactors = FALSE)
  if (!is.null(extra)) df <- cbind(df, extra)
  .mk_omop.table(df, salt = salt)
}

# ------------------------------------------------------------------------------
# (a) non-omop.table input is rejected by every verb
# ------------------------------------------------------------------------------

test_that("verbs reject non-omop.table input", {
  plain <- data.frame(person_id = 1:5, age = 1:5)            # no omop.table class
  ot    <- .mk_persons(5, extra = data.frame(age = 1:5))

  withr::with_options(list(nfilter.subset = 3), {
    expect_error(omopMergeDS(plain, ot), "omop.table")
    expect_error(omopMergeDS(ot, plain), "omop.table")
    expect_error(omopFilterDS(plain, "age", ">=", 2), "omop.table")
    expect_error(omopSelectDS(plain, "age"), "omop.table")
    expect_error(omopBindRowsDS(plain, ot), "omop.table")
    expect_error(omopBindRowsDS(ot, plain), "omop.table")
  })
})

# ------------------------------------------------------------------------------
# (b) + (d) happy paths: correct result, still an omop.table data.frame
# ------------------------------------------------------------------------------

test_that("omopMergeDS inner-joins on the person key and stays an omop.table", {
  x <- .mk_persons(5, extra = data.frame(a = 1:5))
  y <- .mk_persons(5, extra = data.frame(b = 6:10))

  withr::with_options(list(nfilter.subset = 3), {
    res <- omopMergeDS(x, y, by = "person_id", type = "inner")
  })

  expect_s3_class(res, "omop.table")
  expect_s3_class(res, "data.frame")
  expect_true(.is_omop.table(res))
  expect_setequal(names(res), c("person_id", "a", "b"))
  expect_equal(nrow(res), 5L)
  # person key carried over and protected attribute restored after merge()
  expect_true("person_id" %in% attr(res, "dsomop_protected"))
})

test_that("omopMergeDS left join keeps all left rows", {
  x <- .mk_persons(5, extra = data.frame(a = 1:5))
  # y covers only persons 1:3 -> left join keeps 5 rows, b is NA for 4,5
  y <- .mk_omop.table(data.frame(person_id = 1:3, b = c(7, 8, 9)))

  withr::with_options(list(nfilter.subset = 3), {
    res <- omopMergeDS(x, y, type = "left")
  })
  expect_equal(nrow(res), 5L)
  expect_equal(sum(is.na(res$b)), 2L)
})

test_that("omopMergeDS rejects a non-person-key join column", {
  x <- .mk_persons(5, extra = data.frame(grp = rep(1L, 5)))
  y <- .mk_persons(5, extra = data.frame(grp = rep(1L, 5)))
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(omopMergeDS(x, y, by = "grp"),
                 "person key|protected|identifier")
  })
})

test_that("omopFilterDS filters rows and stays an omop.table", {
  x <- .mk_persons(10, extra = data.frame(age = c(20:28, 80)))

  withr::with_options(list(nfilter.subset = 3), {
    res <- omopFilterDS(x, var = "age", op = ">=", value = 22)
  })
  expect_s3_class(res, "omop.table")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 8L)             # ages 22..28 and 80
  expect_true(all(res$age >= 22))
  expect_true("person_id" %in% attr(res, "dsomop_protected"))
})

test_that("omopFilterDS supports 'in' on a vector value", {
  x <- .mk_persons(6, extra = data.frame(sex = c("M","F","M","F","M","F")))
  withr::with_options(list(nfilter.subset = 3), {
    res <- omopFilterDS(x, var = "sex", op = "in", value = c("M", "F"))
  })
  expect_equal(nrow(res), 6L)
})

test_that("omopFilterDS rejects filtering on a protected/identifier column", {
  x <- .mk_persons(10, extra = data.frame(age = 1:10))
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(omopFilterDS(x, var = "person_id", op = "==", value = "z"),
                 "protected|identifier")
  })
})

test_that("omopSelectDS projects columns and always retains the person key", {
  x <- .mk_persons(5, extra = data.frame(a = 1:5, b = 6:10, c = 11:15))
  withr::with_options(list(nfilter.subset = 3), {
    res <- omopSelectDS(x, cols = c("a", "b"))
  })
  expect_s3_class(res, "omop.table")
  expect_s3_class(res, "data.frame")
  # person_id retained even though not requested
  expect_setequal(names(res), c("person_id", "a", "b"))
})

test_that("omopBindRowsDS stacks two schema-identical omop.tables", {
  x <- .mk_omop.table(data.frame(person_id = 1:3, a = 1:3))
  y <- .mk_omop.table(data.frame(person_id = 4:6, a = 4:6))
  withr::with_options(list(nfilter.subset = 3), {
    res <- omopBindRowsDS(x, y)
  })
  expect_s3_class(res, "omop.table")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 6L)
  expect_equal(.omopDistinctPersons(res), 6L)
})

test_that("omopBindRowsDS rejects mismatched column names", {
  x <- .mk_omop.table(data.frame(person_id = 1:3, a = 1:3))
  y <- .mk_omop.table(data.frame(person_id = 4:6, b = 4:6))
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(omopBindRowsDS(x, y), "identical column names")
  })
})

# ------------------------------------------------------------------------------
# (c) fail-closed: result below nfilter.subset distinct persons -> error
# ------------------------------------------------------------------------------

test_that("omopFilterDS fails closed when the filter isolates too few persons", {
  x <- .mk_persons(10, extra = data.frame(age = c(20:28, 80)))
  withr::with_options(list(nfilter.subset = 3), {
    # age >= 80 matches exactly one person -> blocked, never returns
    expect_error(omopFilterDS(x, var = "age", op = ">=", value = 80),
                 "Disclosive")
  })
})

test_that("omopMergeDS fails closed when the join yields too few persons", {
  # disjoint person sets -> inner join is empty (0 distinct persons)
  x <- .mk_omop.table(data.frame(person_id = 1:5, a = 1:5))
  y <- .mk_omop.table(data.frame(person_id = 100:104, b = 1:5))
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(omopMergeDS(x, y, type = "inner"), "Disclosive")
  })
})

# ------------------------------------------------------------------------------
# (e) the rbind-doubling bypass is defeated: bind a table with ITSELF
# ------------------------------------------------------------------------------

test_that("omopBindRowsDS of a table with ITSELF is blocked (doubling bypass)", {
  # 2 distinct persons; threshold 3. rbind doubles the ROWS (2 -> 4) but the
  # DISTINCT person count stays 2, so the gate still blocks. A per-row count
  # gate would have been fooled into passing here.
  x2 <- .mk_omop.table(data.frame(person_id = 1:2, a = 1:2))
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(omopBindRowsDS(x2, x2), "Disclosive")
  })
})

test_that("omopBindRowsDS self-bind does not inflate the distinct-person count", {
  # Above threshold the self-bind succeeds, but distinct persons is unchanged
  # while rows double -- the property the gate relies on.
  x <- .mk_omop.table(data.frame(person_id = 1:4, a = 1:4))
  withr::with_options(list(nfilter.subset = 3), {
    res <- omopBindRowsDS(x, x)
  })
  expect_equal(nrow(res), 8L)               # rows doubled
  expect_equal(.omopDistinctPersons(res), 4L)  # distinct persons unchanged
})

# ==============================================================================
# Profiling regression: value.counts / column.stats / missingness must now
# suppress on DISTINCT person_id, not on record counts.
# ==============================================================================

# Replace the measurement table with a controlled shape and rebuild blueprint.
.set_measurement <- function(handle, df) {
  DBI::dbWriteTable(handle$conn, "measurement", df, overwrite = TRUE)
  handle$blueprint <- NULL
  dsOMOP:::.buildBlueprint(handle)
  invisible(handle)
}

.meas_row <- function(person_id, concept_id, value = 5.0, id) {
  data.frame(
    measurement_id = id,
    person_id = person_id,
    measurement_concept_id = concept_id,
    measurement_date = "2019-12-15",
    measurement_type_concept_id = 44818702L,
    value_as_number = value,
    value_as_concept_id = 0L,
    unit_concept_id = 8840L,
    range_low = 4.0, range_high = 6.0,
    measurement_source_value = "X",
    measurement_source_concept_id = 0L,
    stringsAsFactors = FALSE
  )
}

test_that("profileValueCounts drops a value with many records but few persons", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))

  # concept 1111: 10 records, ALL person 1  -> 1 distinct person (disclosive)
  # concept 2222: 10 records across persons 1..10 -> 10 distinct persons (safe)
  disclosive <- do.call(rbind, lapply(1:10, function(i)
    .meas_row(1L, 1111L, id = i)))
  safe <- do.call(rbind, lapply(1:10, function(i)
    .meas_row(i, 2222L, id = 10L + i)))
  .set_measurement(handle, rbind(disclosive, safe))

  withr::with_options(list(
    nfilter.tab = 3, nfilter.subset = 3,
    nfilter.levels.max = 40, nfilter.levels.density = 0.99
  ), {
    res <- .profileValueCounts(handle, "measurement", "measurement_concept_id")
  })

  # n_persons is surfaced; the record count n is retained alongside it
  expect_true("n_persons" %in% names(res))
  expect_true("n" %in% names(res))
  # The many-records / few-persons value is dropped; the safe one survives.
  expect_false("1111" %in% as.character(res$value))
  expect_true("2222" %in% as.character(res$value))
})

test_that("profileColumnStats gates on DISTINCT persons, not record count", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))

  # 10 records but ALL belong to person 1 -> 1 distinct person.
  one_person <- do.call(rbind, lapply(1:10, function(i)
    .meas_row(1L, 3004410L, value = i, id = i)))
  .set_measurement(handle, one_person)

  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    # Record count (10) clears the threshold, but distinct persons (1) does not.
    expect_error(
      .profileColumnStats(handle, "measurement", "value_as_number"),
      "Disclosive|insufficient"
    )
  })
})

test_that("profileColumnStats succeeds when enough distinct persons present", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))

  many <- do.call(rbind, lapply(1:10, function(i)
    .meas_row(i, 3004410L, value = i, id = i)))
  .set_measurement(handle, many)

  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    res <- .profileColumnStats(handle, "measurement", "value_as_number")
  })
  expect_true(!is.null(res$n_persons))
  expect_equal(res$n_persons, 10)
})

test_that("profileMissingness gates on DISTINCT persons, not record count", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))

  one_person <- do.call(rbind, lapply(1:10, function(i)
    .meas_row(1L, 3004410L, value = i, id = i)))
  .set_measurement(handle, one_person)

  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    expect_error(.profileMissingness(handle, "measurement"),
                 "Disclosive|insufficient")
  })
})
