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
