test_that("disclosure settings are read from options", {
  withr::with_options(list(
    nfilter.tab = 5,
    nfilter.subset = 4,
    nfilter.levels.max = 50,
    nfilter.levels.density = 0.5,
    nfilter.string = 100,
    nfilter.stringShort = 30,
    datashield.privacyControlLevel = "banana"
  ), {
    settings <- .omopDisclosureSettings()
    expect_equal(settings$nfilter_tab, 5)
    expect_equal(settings$nfilter_subset, 4)
    expect_equal(settings$nfilter_levels_max, 50)
    expect_equal(settings$nfilter_levels_density, 0.5)
    expect_equal(settings$nfilter_string, 100)
    expect_equal(settings$nfilter_stringShort, 30)
    expect_equal(settings$privacy_level, "banana")
  })
})

test_that("assertPrivacyLevel passes at banana level", {
  withr::with_options(list(
    datashield.privacyControlLevel = "banana"
  ), {
    expect_invisible(.assertPrivacyLevel("banana"))
  })
})

test_that("assertPrivacyLevel is a no-op (always succeeds)", {
  withr::with_options(list(
    datashield.privacyControlLevel = "non-permissive"
  ), {
    # Now a no-op: succeeds regardless of privacy level
    expect_invisible(.assertPrivacyLevel("banana"))
    expect_invisible(.assertPrivacyLevel("permissive"))
  })
})

test_that("assertMinPersons passes with enough persons", {
  withr::with_options(list(nfilter.subset = 3), {
    expect_invisible(.assertMinPersons(n_persons = 10))
  })
})

test_that("assertMinPersons blocks with too few persons", {
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(.assertMinPersons(n_persons = 2), "Disclosive")
  })
})

test_that("suppressSmallCounts replaces small counts with NA", {
  withr::with_options(list(nfilter.tab = 3), {
    df <- data.frame(value = c("a", "b", "c"), n = c(1, 2, 5))
    result <- .suppressSmallCounts(df, "n")
    expect_true(is.na(result$n[1]))  # 1 < 3
    expect_true(is.na(result$n[2]))  # 2 < 3
    expect_equal(result$n[3], 5)      # 5 >= 3
  })
})

test_that("assertSafeLevels passes with valid levels", {
  withr::with_options(list(
    nfilter.levels.max = 40,
    nfilter.levels.density = 0.33
  ), {
    expect_invisible(.assertSafeLevels(10, 100))
  })
})

test_that("assertSafeLevels blocks too many levels", {
  withr::with_options(list(nfilter.levels.max = 5), {
    expect_error(.assertSafeLevels(10, 100), "levels")
  })
})

test_that("assertSafeLevels blocks high density", {
  withr::with_options(list(
    nfilter.levels.max = 100,
    nfilter.levels.density = 0.1
  ), {
    expect_error(.assertSafeLevels(50, 100), "density")
  })
})

test_that("validateString passes short strings", {
  withr::with_options(list(nfilter.string = 80), {
    expect_equal(.validateString("hello"), "hello")
  })
})

test_that("validateString blocks long strings", {
  withr::with_options(list(nfilter.string = 5), {
    expect_error(.validateString("toolongstring"), "too long")
  })
})

test_that("validateIdentifier accepts valid identifiers", {
  expect_equal(.validateIdentifier("person_id", "column"), "person_id")
  expect_equal(.validateIdentifier("condition_occurrence", "table"),
               "condition_occurrence")
})

test_that("validateIdentifier rejects invalid identifiers", {
  expect_error(.validateIdentifier("1bad", "table"), "Invalid")
  expect_error(.validateIdentifier("drop; --", "table"), "Invalid")
  expect_error(.validateIdentifier("a b", "table"), "Invalid")
})

test_that("validateIdentifier returns NULL for NULL input", {
  expect_null(.validateIdentifier(NULL))
})

# ==============================================================================
# Age Safety Tests
# ==============================================================================

test_that("computeAgeGroups bins correctly", {
  withr::with_options(list(nfilter.tab = 1), {
    yob <- c(1990, 1985, 1960, 1940, 1930)
    index <- rep(2024, 5)
    groups <- .computeAgeGroups(yob, index, bin_width = 5L, min_cell = 1L)
    expect_true(all(!is.na(groups)))
    expect_true(all(grepl("-|\\+", groups)))  # All have range format
    expect_false(any(grepl("^[0-9]+$", groups)))  # No exact ages
  })
})

test_that("computeAgeGroups enforces minimum 5-year bins", {
  withr::with_options(list(nfilter.tab = 1), {
    yob <- rep(1990, 10)
    index <- rep(2024, 10)
    # Even if bin_width=1 requested, it floors to 5
    groups <- .computeAgeGroups(yob, index, bin_width = 1L, min_cell = 1L)
    # All should be in same "30-34" bin
    expect_equal(length(unique(groups)), 1)
  })
})

test_that("computeAgeGroups handles NA ages", {
  withr::with_options(list(nfilter.tab = 1), {
    yob <- c(1990, 2030, 1980)  # 2030 will produce negative age -> NA
    index <- rep(2024, 3)
    groups <- .computeAgeGroups(yob, index, bin_width = 5L, min_cell = 1L)
    expect_true(is.na(groups[2]))
    expect_false(is.na(groups[1]))
    expect_false(is.na(groups[3]))
  })
})

test_that("computeAgeGroups merges small bins at extremes", {
  withr::with_options(list(nfilter.tab = 3), {
    # Ages: 94 (1 person), 20-24 (10 persons)
    yob <- c(rep(2000, 10), 1930)
    index <- rep(2024, 11)
    groups <- .computeAgeGroups(yob, index, bin_width = 5L, min_cell = 3L)
    # The single 94-year-old should be merged into a larger group
    expect_true(!is.na(groups[11]))
    # Should end with "+" (merged top bin)
    top_group <- groups[11]
    expect_true(grepl("\\+", top_group))
  })
})

# ==============================================================================
# Filter Safety Tests
# ==============================================================================

test_that("classifyFilter returns correct classification", {
  expect_equal(.classifyFilter("sex"), "allowed")
  expect_equal(.classifyFilter("age_group"), "allowed")
  expect_equal(.classifyFilter("cohort"), "allowed")
  expect_equal(.classifyFilter("concept_set"), "allowed")
  expect_equal(.classifyFilter("value_threshold"), "blocked")
  expect_equal(.classifyFilter("custom"), "blocked")
  expect_equal(.classifyFilter("has_concept"), "constrained")
  expect_equal(.classifyFilter("date_range"), "constrained")
  expect_equal(.classifyFilter("min_count"), "constrained")
})

test_that("classifyFilter blocks narrow age ranges", {
  # 0-year range (single age) -> blocked
  expect_equal(.classifyFilter("age_range", list(min = 18, max = 18)), "blocked")
  # Wide range -> constrained
  expect_equal(.classifyFilter("age_range", list(min = 18, max = 65)), "constrained")
})

test_that("classifyFilter blocks narrow date ranges", {
  # 10-day range -> blocked
  expect_equal(
    .classifyFilter("date_range", list(start = "2024-01-01", end = "2024-01-10")),
    "blocked"
  )
  # 60-day range -> constrained
  expect_equal(
    .classifyFilter("date_range", list(start = "2024-01-01", end = "2024-03-01")),
    "constrained"
  )
})

test_that("classifyFilter blocks unknown filter types", {
  expect_equal(.classifyFilter("unknown_type"), "blocked")
})

test_that("validateFilter stops on blocked filters", {
  expect_error(
    .validateFilter("value_threshold"),
    "not allowed"
  )
  expect_error(
    .validateFilter("custom"),
    "not allowed"
  )
})

test_that("validateFilter passes allowed and constrained filters", {
  expect_invisible(.validateFilter("sex"))
  expect_invisible(.validateFilter("age_group"))
  expect_invisible(.validateFilter("has_concept"))
})

# ==============================================================================
# No-Hint Error Message Test
# ==============================================================================

test_that("assertMinPersons does not reveal count", {
  withr::with_options(list(nfilter.subset = 5), {
    err <- tryCatch(
      .assertMinPersons(n_persons = 1),
      error = function(e) conditionMessage(e)
    )
    # Should not contain the actual count
    expect_false(grepl("\\b1\\b", err))
    expect_true(grepl("blocked|insufficient", err, ignore.case = TRUE))
    # Should not reveal the threshold
    expect_false(grepl("nfilter\\.subset", err))
  })
})
