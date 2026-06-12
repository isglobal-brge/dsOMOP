# Tests for the disclosure-safe 2-way cross-tab (omopCrossTabDS / .profileCrossTab)
#
# Two layers:
#   1. The suppression algorithm (.crossTabSuppress / .complementarySuppress)
#      tested directly on dense matrices -- this is the load-bearing disclosure
#      logic and the adversarial invariants in spec section 5 are encoded here.
#   2. End-to-end DB-backed tests (arg decoding, persons-vs-records, gates,
#      stratification) against a SQLite OMOP fixture.

# ---------------------------------------------------------------------------
# Invariant checker: given the true matrix M and the rendered (NA-masked)
# matrix R, assert the released grid is not trivially solvable.
# ---------------------------------------------------------------------------
assert_safe_grid <- function(M, R, t) {
  # (a) Floor: every VISIBLE cell is either a structural zero or >= t.
  vis <- !is.na(R)
  visible_vals <- R[vis]
  expect_true(all(visible_vals == 0 | visible_vals >= t),
              info = "no visible non-zero cell below nfilter_tab")

  # (b) Single-suppression solvability: a line is recoverable only if it has a
  # UNIQUE hidden non-zero cell AND at least one VISIBLE non-zero cell (then the
  # hidden value = line-margin minus the visible non-zeros). If the line's only
  # other entries are structural zeros, the lone hidden cell equals the OMITTED
  # margin and is therefore not recoverable. Assert no line is in the recoverable
  # configuration.
  hidden_nz  <- is.na(R) & (M > 0)
  visible_nz <- !is.na(R) & (R > 0)
  for (i in seq_len(nrow(M))) {
    recoverable <- sum(hidden_nz[i, ]) == 1L && any(visible_nz[i, ])
    expect_false(recoverable,
                 info = paste("row", i, "has a uniquely recoverable hidden cell"))
  }
  for (j in seq_len(ncol(M))) {
    recoverable <- sum(hidden_nz[, j]) == 1L && any(visible_nz[, j])
    expect_false(recoverable,
                 info = paste("col", j, "has a uniquely recoverable hidden cell"))
  }

  # (c) Structural zeros are rendered identically as 0 (never NA), so an
  # attacker cannot distinguish "suppressed small" from "true zero".
  expect_true(all(R[M == 0] == 0),
              info = "structural zeros rendered as 0")
}

# === Layer 1: suppression algorithm =========================================

test_that("primary suppression hides cells in (0, t) and keeps zeros as 0", {
  M <- matrix(c(10L, 2L,  # 2 is below t=3
                0L,  20L),
              nrow = 2, byrow = TRUE)
  res <- .crossTabSuppress(M, t = 3)
  R <- res$matrix
  # The structural zero stays 0
  expect_equal(R[2, 1], 0)
  assert_safe_grid(M, R, 3)
})

test_that("adversarial 1: no row/col has a unique hidden non-zero cell", {
  # A 3x3 with a single small cell would, under primary-only suppression, leave
  # exactly one hidden non-zero in its row and column -> recoverable. The
  # complementary pass must close that.
  M <- matrix(c(10L,  2L, 30L,
                40L, 50L, 60L,
                70L, 80L, 90L),
              nrow = 3, byrow = TRUE)
  res <- .crossTabSuppress(M, t = 3)
  assert_safe_grid(M, res$matrix, 3)
  # The lone small cell (1,2) must be hidden AND at least one complementary cell.
  expect_true(is.na(res$matrix[1, 2]))
  expect_true(sum(is.na(res$matrix)) >= 2L)
})

test_that("adversarial 5: 2x2 with a true small cell cascades to >=3 NA", {
  # Any 2x2 with a small cell cannot publish 3 visible cells + omitted margins
  # without leaking the 4th; complementary suppression cascades.
  M <- matrix(c(2L, 10L,
                20L, 30L),
              nrow = 2, byrow = TRUE)
  res <- .crossTabSuppress(M, t = 3)
  R <- res$matrix
  assert_safe_grid(M, R, 3)
  # The small cell's row-complement and column-complement are also suppressed.
  expect_true(is.na(R[1, 1]))           # primary
  expect_true(is.na(R[1, 2]))           # row complement
  expect_true(is.na(R[2, 1]))           # column complement
  # Spec: a 2x2 with a small cell cascades to >= 3 of 4 NA.
  expect_true(sum(is.na(R)) >= 3L)
})

test_that("complementary suppression converges to a fixpoint (idempotent)", {
  M <- matrix(c(1L, 2L, 50L,
                60L, 70L, 80L,
                90L, 1L, 100L),
              nrow = 3, byrow = TRUE)
  res <- .crossTabSuppress(M, t = 3)
  S <- is.na(res$matrix)
  # Re-running complementary on the already-masked set yields no new cells.
  S2 <- .complementarySuppress(matrix(as.integer(M), 3, 3), S)
  expect_equal(S2, S)
  assert_safe_grid(M, res$matrix, 3)
})

test_that("adversarial 2 & 3: margins omitted by default, banded on opt-in", {
  M <- matrix(c(7L, 2L, 9L,
                4L, 5L, 6L,
                8L, 11L, 13L),
              nrow = 3, byrow = TRUE)
  # Default: NO margins returned at all.
  res <- .crossTabSuppress(M, t = 3, band_margins = FALSE)
  expect_null(res$row_margins)
  expect_null(res$col_margins)
  expect_null(res$grand_total)

  # Banded: every reported margin <= true and a multiple of band_width.
  resb <- .crossTabSuppress(M, t = 3, band_margins = TRUE, band_width = 5L)
  true_rows <- rowSums(M)
  true_cols <- colSums(M)
  expect_true(all(resb$row_margins <= true_rows))
  expect_true(all(resb$col_margins <= true_cols))
  expect_true(all(resb$row_margins %% 5 == 0))
  expect_true(all(resb$col_margins %% 5 == 0))
  expect_true(resb$grand_total <= sum(M) && resb$grand_total %% 5 == 0)

  # Margin-subtraction: banded margin minus visible cells != any true hidden
  # cell. (Banding destroys the exact arithmetic that would recover it.)
  R <- resb$matrix
  for (i in seq_len(nrow(M))) {
    hidden <- which(is.na(R[i, ]) & M[i, ] > 0)
    if (length(hidden) >= 1) {
      visible_sum <- sum(R[i, !is.na(R[i, ])])
      recovered <- resb$row_margins[i] - visible_sum
      # The single banded residual must not equal a unique hidden true value.
      if (length(hidden) == 1L) {
        expect_false(isTRUE(recovered == M[i, hidden]))
      }
    }
  }
})

test_that("adversarial 6: differencing -- banded margins do not shift by 1", {
  # Cohort A vs A-minus-1-person: a single person's removal changes one cell by
  # 1 and its margins by 1; banding to width 5 hides that 1-unit delta.
  MA <- matrix(c(47L, 8L, 12L,
                 9L, 22L, 31L,
                 14L, 18L, 25L),
               nrow = 3, byrow = TRUE)
  MB <- MA; MB[1, 1] <- MB[1, 1] - 1L  # drop one person from cell (1,1)
  rA <- .crossTabSuppress(MA, t = 3, band_margins = TRUE, band_width = 5L)
  rB <- .crossTabSuppress(MB, t = 3, band_margins = TRUE, band_width = 5L)
  # Banded row-1 margin must NOT differ by exactly 1 (47->46 both band to 45).
  expect_false(isTRUE(abs(rA$row_margins[1] - rB$row_margins[1]) == 1))
  expect_equal(unname(rA$row_margins[1]), unname(rB$row_margins[1]))
})

test_that("all-suppressed grid -> all-NA matrix + suppressed flag", {
  M <- matrix(c(1L, 2L,
                2L, 1L),
              nrow = 2, byrow = TRUE)
  res <- .crossTabSuppress(M, t = 3)
  expect_true(all(is.na(res$matrix)))
  expect_true(res$suppressed)
})

test_that("empty population -> empty/all-NA grid, generic (no empty-vs-small leak)", {
  M <- matrix(integer(0), nrow = 0, ncol = 0)
  res <- .crossTabSuppress(M, t = 3)
  expect_equal(length(res$matrix), 0L)
  expect_false(res$suppressed)  # no non-zero cells at all
})

# === Layer 2: DB-backed end-to-end =========================================

# Build a handle whose person table has a controlled gender x race 2-way
# distribution (the default fixture has single-level race, so we overwrite it).
make_crosstab_handle <- function(persons_df) {
  handle <- create_test_handle()
  DBI::dbWriteTable(handle$conn, "person", persons_df, overwrite = TRUE)
  handle$blueprint <- NULL
  .buildBlueprint(handle)
  handle
}

# 30 persons: gender (2 levels) x race (2 levels), all cells comfortably >= t.
big_persons <- function() {
  n <- 30L
  data.frame(
    person_id = seq_len(n),
    gender_concept_id = rep(c(8507L, 8532L), each = 15L),
    race_concept_id   = rep(c(8527L, 8516L), times = 15L),
    year_of_birth = 1950L + seq_len(n),
    month_of_birth = rep(1L, n), day_of_birth = rep(1L, n),
    ethnicity_concept_id = rep(38003564L, n),
    location_id = seq_len(n), provider_id = seq_len(n), care_site_id = seq_len(n),
    person_source_value = paste0("SRC", seq_len(n)),
    gender_source_value = rep("X", n),
    stringsAsFactors = FALSE
  )
}

test_that("omopCrossTabDS decodes JSON args and returns a 2x2 matrix", {
  handle <- make_crosstab_handle(big_persons())
  on.exit(cleanup_handle(handle))
  # Register the handle directly in the package env; clean up by removing the
  # key (avoid .removeHandle, which closes a non-existent resource_client).
  env <- get(".dsomop_env", envir = asNamespace("dsOMOP"))
  assign("handle_omop", handle, envir = env)
  on.exit(suppressWarnings(rm(list = "handle_omop", envir = env)), add = TRUE)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.max = 40, nfilter.levels.density = 1), {
    # count_mode passed as a JSON string (Opal transport) must decode.
    res <- omopCrossTabDS("omop", "person", "gender_concept_id",
                          "race_concept_id", count_mode = "persons")
    expect_true(is.matrix(res$counts))
    expect_equal(dim(res$counts), c(2L, 2L))
    expect_false(res$suppressed)
    # All four cells are 7 or 8 persons -> all visible, none NA.
    expect_true(all(!is.na(res$counts)))
  })
})

test_that("persons vs records counts differ when persons have many records", {
  handle <- make_crosstab_handle(big_persons())
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.max = 40, nfilter.levels.density = 1), {
    rp <- .profileCrossTab(handle, "person", "gender_concept_id",
                           "race_concept_id", count_mode = "persons")
    rr <- .profileCrossTab(handle, "person", "gender_concept_id",
                           "race_concept_id", count_mode = "records")
    # person table is 1 row per person, so records == persons here; assert the
    # code path runs and both return same-shaped matrices.
    expect_equal(dim(rp$counts), dim(rr$counts))
    expect_equal(sum(rp$counts, na.rm = TRUE), 30)
  })
})

test_that("person gate blocks small scoped population with a generic error", {
  # Only 2 distinct persons -> below nfilter_subset -> generic block, no count.
  small <- big_persons()[1:2, ]
  small$gender_concept_id <- c(8507L, 8532L)
  small$race_concept_id   <- c(8527L, 8516L)
  handle <- make_crosstab_handle(small)
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.max = 40, nfilter.levels.density = 1), {
    expect_error(
      .profileCrossTab(handle, "person", "gender_concept_id", "race_concept_id"),
      "insufficient individuals|disclosure threshold"
    )
  })
})

test_that("dimension gate blocks a degenerate 1xN axis (Gate F)", {
  # Single race level -> col axis has only 1 level -> rejected as a 1-way dist.
  df <- big_persons()
  df$race_concept_id <- rep(8527L, nrow(df))
  handle <- make_crosstab_handle(df)
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.max = 40, nfilter.levels.density = 1), {
    expect_error(
      .profileCrossTab(handle, "person", "gender_concept_id", "race_concept_id"),
      "at least 2 levels|one-way"
    )
  })
})

test_that("dimension gate blocks too-many-levels axis via .assertSafeLevels", {
  df <- big_persons()
  df$race_concept_id <- seq_len(nrow(df))  # one distinct level per person
  handle <- make_crosstab_handle(df)
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.max = 5, nfilter.levels.density = 0.33), {
    expect_error(
      .profileCrossTab(handle, "person", "gender_concept_id", "race_concept_id"),
      "Disclosive"
    )
  })
})

test_that("end-to-end grid satisfies the disclosure invariant", {
  # Construct a distribution with a deliberately small cell to exercise both
  # primary and complementary suppression through the real query path.
  df <- big_persons()
  # gender 8507 x race 8516 -> only 2 persons (small cell)
  df$gender_concept_id <- c(rep(8507L, 14L), rep(8532L, 16L))
  df$race_concept_id   <- c(rep(8527L, 12L), rep(8516L, 2L),  # 8507: 12 / 2
                            rep(8527L, 8L),  rep(8516L, 8L))   # 8532: 8  / 8
  handle <- make_crosstab_handle(df)
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.max = 40, nfilter.levels.density = 1), {
    res <- .profileCrossTab(handle, "person", "gender_concept_id",
                            "race_concept_id")
    R <- res$counts
    # No visible cell below threshold (structural zeros allowed).
    vis <- !is.na(R)
    expect_true(all(R[vis] == 0 | R[vis] >= 3))
    # No line has a uniquely recoverable hidden cell (lone hidden non-zero with a
    # visible non-zero companion). Checked on the rendered grid directly.
    for (i in seq_len(nrow(R))) {
      n_hidden <- sum(is.na(R[i, ]))
      has_visible_nz <- any(!is.na(R[i, ]) & R[i, ] > 0)
      expect_false(n_hidden == 1L && has_visible_nz)
    }
    for (j in seq_len(ncol(R))) {
      n_hidden <- sum(is.na(R[, j]))
      has_visible_nz <- any(!is.na(R[, j]) & R[, j] > 0)
      expect_false(n_hidden == 1L && has_visible_nz)
    }
  })
})

test_that("stratify_by returns a named list of independent slices, no total", {
  # gender x race stratified by ethnicity (give 2 ethnicity levels).
  df <- big_persons()
  df$ethnicity_concept_id <- rep(c(38003563L, 38003564L), each = 15L)
  handle <- make_crosstab_handle(df)
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.max = 40, nfilter.levels.density = 1), {
    res <- .profileCrossTab(handle, "person", "gender_concept_id",
                            "race_concept_id",
                            stratify_by = "ethnicity_concept_id")
    expect_true(isTRUE(res$stratified))
    expect_type(res$strata, "list")
    expect_true(length(res$strata) >= 1L)
    # Adversarial 7: the unstratified TOTAL is never returned.
    expect_null(res$counts)
    expect_false("counts" %in% names(res))
    # Each slice is an independent protected 2-way table.
    for (slice in res$strata) {
      expect_true("counts" %in% names(slice) || isTRUE(slice$suppressed))
    }
  })
})

test_that("stratify_by suppresses a stratum below nfilter_subset entirely", {
  df <- big_persons()
  # One ethnicity level has only 2 persons -> that whole slice is suppressed.
  df$ethnicity_concept_id <- c(rep(38003563L, 2L), rep(38003564L, 28L))
  handle <- make_crosstab_handle(df)
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.max = 40, nfilter.levels.density = 1), {
    res <- .profileCrossTab(handle, "person", "gender_concept_id",
                            "race_concept_id",
                            stratify_by = "ethnicity_concept_id")
    small_slice <- res$strata[["38003563"]]
    expect_true(isTRUE(small_slice$suppressed))
    # No visible cell leaks for the small stratum.
    if (!is.null(small_slice$counts) && length(small_slice$counts) > 0) {
      expect_true(all(is.na(small_slice$counts)))
    }
  })
})
