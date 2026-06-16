test_that("disclosure settings are read from options", {
  withr::with_options(list(
    nfilter.tab = 5,
    nfilter.subset = 4,
    nfilter.levels.max = 50,
    nfilter.levels.density = 0.5,
    nfilter.string = 100,
    nfilter.stringShort = 30
  ), {
    settings <- .omopDisclosureSettings()
    expect_equal(settings$nfilter_tab, 5)
    expect_equal(settings$nfilter_subset, 4)
    expect_equal(settings$nfilter_levels_max, 50)
    expect_equal(settings$nfilter_levels_density, 0.5)
    expect_equal(settings$nfilter_string, 100)
    expect_equal(settings$nfilter_stringShort, 30)
  })
})

test_that("nfilter_band defaults to 5 and follows the option chain", {
  # Default when no option is set.
  withr::with_options(list(dsomop.nfilter.band = NULL,
                           default.dsomop.nfilter.band = NULL), {
    expect_equal(.omopDisclosureSettings()$nfilter_band, 5)
  })
  # Server-side override is honoured and is introspectable via the DS endpoint.
  withr::with_options(list(dsomop.nfilter.band = 10), {
    expect_equal(.omopDisclosureSettings()$nfilter_band, 10)
    expect_equal(omopDisclosureSettingsDS()$nfilter_band, 10)
  })
})

test_that(".bandCount honours a configurable band width and is idempotent", {
  expect_equal(.bandCount(47, band_width = 10), 40)
  expect_equal(.bandCount(47, band_width = 5), 45)
  # Idempotent: re-banding an already-banded value to the same width is a no-op.
  expect_equal(.bandCount(.bandCount(47, 5), 5), 45)
  expect_equal(.bandCount(45, 5), 45)
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

test_that("suppressSmallCounts drops rows with small counts", {
  withr::with_options(list(nfilter.tab = 3), {
    df <- data.frame(value = c("a", "b", "c"), n = c(1, 2, 5))
    result <- .suppressSmallCounts(df, "n")
    # Rows with n < 3 are dropped entirely
    expect_equal(nrow(result), 1)
    expect_equal(result$value[1], "c")
    expect_equal(result$n[1], 5)
  })
})

# --- Regression guards: disclosure leaks closed 2026-06 (audit) --------------

test_that("omopValueCountsDS does not let the client disable small-count suppression", {
  # This aggregate endpoint must always suppress small cells; a caller must not
  # be able to switch disclosure control off through an argument.
  expect_false("suppress_small" %in% names(formals(omopValueCountsDS)))
})

test_that("no query template exposes raw MIN/MAX extreme values (disclosure)", {
  # Extreme individual values (MIN/MAX) are quasi-identifiers and must never be
  # returned, consistent with the Achilles distribution policy.
  templates <- .ql_load_queries()
  skip_if(length(templates) == 0, "query templates not found")
  offenders <- vapply(templates, function(q) {
    isTRUE(grepl("\\b(MIN|MAX)\\s*\\(", q$sql, perl = TRUE, ignore.case = TRUE))
  }, logical(1))
  expect_equal(
    sum(offenders), 0L,
    info = paste("Templates selecting MIN/MAX:",
                 paste(names(templates)[offenders], collapse = ", "))
  )
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
  # age_group is now constrained (5-year minimum band width enforced).
  expect_equal(.classifyFilter("age_group", list(groups = c("0-4", "5-9"))),
               "constrained")
  expect_equal(.classifyFilter("cohort"), "allowed")
  expect_equal(.classifyFilter("concept_set"), "allowed")
  # value_threshold is a population-defining range filter (allowed/size-checked);
  # its disclosive exact-value operators (==, !=) are blocked at the cohort site.
  expect_equal(.classifyFilter("value_threshold"), "constrained")
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
    .validateFilter("custom"),
    "not allowed"
  )
  expect_error(
    .validateFilter("unknown_type"),
    "not allowed"
  )
})

test_that("validateFilter passes allowed and constrained filters", {
  expect_invisible(.validateFilter("sex"))
  expect_invisible(.validateFilter("age_group", list(groups = c("0-4", "5-9"))))
  expect_invisible(.validateFilter("has_concept"))
  expect_invisible(.validateFilter("value_threshold"))
})

test_that("age_group filter enforces the 5-year minimum band width", {
  # Wide / standard 5-year bands are allowed.
  expect_equal(.classifyFilter("age_group", list(groups = c("0-4", "5-9"))),
               "constrained")
  expect_equal(.classifyFilter("age_group", list(groups = c("18-24"))),
               "constrained")
  # Open-ended upper band ("85+") is wide -> allowed.
  expect_equal(.classifyFilter("age_group", list(groups = c("85+"))),
               "constrained")
  # A single-birth-year band must be blocked (would evade the age_range gate).
  expect_equal(.classifyFilter("age_group", list(groups = c("87-87"))),
               "blocked")
  expect_equal(.classifyFilter("age_group", list(groups = c("0-4", "60-62"))),
               "blocked")
  # Empty / unparseable groups fail closed.
  expect_equal(.classifyFilter("age_group", list(groups = character(0))),
               "blocked")
})

test_that("suppressSmallCounts secondary mode hides a lone sub-threshold level", {
  # Binary breakdown with one sub-threshold level (M=2, F=98). Primary alone
  # would leave F visible -> M recoverable from the total. Secondary suppression
  # drops the smallest survivor too, so the whole binary becomes unavailable.
  binary <- data.frame(value = c("F", "M"), n_persons = c(98, 2),
                       stringsAsFactors = FALSE)
  expect_equal(nrow(.suppressSmallCounts(binary, "n_persons", secondary = TRUE)), 0L)
  # Without secondary, F survives (the leak the fix closes).
  expect_equal(nrow(.suppressSmallCounts(binary, "n_persons", secondary = FALSE)), 1L)
  # When >= 2 levels are already suppressed, secondary adds nothing.
  three <- data.frame(value = c("A", "B", "C"), n_persons = c(50, 2, 1),
                      stringsAsFactors = FALSE)
  expect_equal(nrow(.suppressSmallCounts(three, "n_persons", secondary = TRUE)), 1L)
  # When nothing is suppressed, secondary is a no-op.
  safe <- data.frame(value = c("A", "B"), n_persons = c(50, 40),
                     stringsAsFactors = FALSE)
  expect_equal(nrow(.suppressSmallCounts(safe, "n_persons", secondary = TRUE)), 2L)
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

# ==============================================================================
# omopDisclosureSettingsDS: read-only introspection endpoint (Phase E)
# ==============================================================================

test_that("omopDisclosureSettingsDS returns the active settings", {
  res <- omopDisclosureSettingsDS()
  expect_identical(res, .omopDisclosureSettings())
  # carries the standard floors the per-patient gate relies on
  expect_true(all(c("nfilter_subset", "nfilter_tab", "nfilter_levels_max") %in%
                    names(res)))
})

test_that("omopDisclosureSettingsDS reflects a server-side option override at runtime", {
  withr::with_options(list(nfilter.subset = 9), {
    expect_equal(omopDisclosureSettingsDS()$nfilter_subset, 9)
  })
  # and .assertMinPersons honours that same option (the gate is option-driven,
  # so the reported floor is the floor actually enforced)
  withr::with_options(list(nfilter.subset = 9), {
    expect_error(.assertMinPersons(n_persons = 8), "blocked|insufficient")
    expect_true(.assertMinPersons(n_persons = 9))
  })
})

test_that("omopDisclosureSettingsDS is read-only (cannot lower a threshold)", {
  before <- getOption("nfilter.subset")
  invisible(omopDisclosureSettingsDS())
  # the call mutates no option and exposes no setter (it takes no arguments)
  expect_identical(getOption("nfilter.subset"), before)
  expect_length(formals(omopDisclosureSettingsDS), 0L)
})
