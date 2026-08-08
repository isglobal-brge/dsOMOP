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

test_that("reported DataSHIELD noise is not advertised as formal DP", {
  settings <- .omopDisclosureSettings()
  expect_false(settings$sticky_noise_enabled)
  expect_false(settings$privacy_ledger_enabled)
  expect_false("formal_dp_enabled" %in% names(settings))
})

test_that("invalid disclosure settings fail closed instead of weakening gates", {
  withr::with_options(list(nfilter.subset = -1), {
    expect_error(.omopDisclosureSettings(), "nfilter_subset")
  })
  withr::with_options(list(nfilter.levels.density = 1.5), {
    expect_error(.omopDisclosureSettings(), "nfilter_levels_density")
  })
  withr::with_options(list(dsomop.query_strict = NA), {
    expect_error(.omopDisclosureSettings(), "query_strict")
  })
  withr::with_options(list(dsomop.nfilter.date_range = Inf), {
    expect_error(.omopDisclosureSettings(), "nfilter_date_range")
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

test_that("age and date filter widths follow server-side options", {
  withr::with_options(list(
    nfilter.subset = 3,
    dsomop.nfilter.age_range = 10,
    dsomop.nfilter.date_range = 60
  ), {
    settings <- .omopDisclosureSettings()
    expect_equal(settings$nfilter_age_range, 10)
    expect_equal(settings$nfilter_date_range, 60)
    expect_equal(.classifyFilter("age_range", list(min = 20, max = 24)),
                 "blocked")
    expect_equal(.classifyFilter("age_range", list(min = 20, max = 29)),
                 "constrained")
    expect_equal(.classifyFilter("date_range", list(
      start = "2024-01-01", end = "2024-02-28")), "blocked")
    expect_equal(.classifyFilter("date_range", list(
      start = "2024-01-01", end = "2024-02-29")), "constrained")
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

test_that("age contract honestly uses reference year minus year_of_birth", {
  expect_identical(.omopDisclosureSettings()$age_semantics,
                   "reference_year_minus_year_of_birth")
  # Month/day are intentionally unavailable: both ends of calendar 2024 use
  # the same annual-resolution age derived from year_of_birth.
  expect_equal(.computeAgeGroups(2000L, 2024L, min_cell = 1L), "20-24")
})

test_that("date harmonization declares deterministic day conversion", {
  settings <- .omopDisclosureSettings()
  expect_identical(settings$harmonization_contract_version,
                   "dsomop-harmonization-v3")
  expect_identical(settings$date_granularity, "calendar_day")
  expect_identical(settings$datetime_timezone, "UTC")
  expect_identical(settings$week_start, "Monday")
  withr::with_options(list(dsomop.datetime_timezone = "not/a-zone"), {
    expect_error(.omopDisclosureSettings(), "datetime_timezone")
  })
})

test_that("computeAgeGroups suppresses small bins without changing the grid", {
  withr::with_options(list(nfilter.tab = 3), {
    # Ages: 94 (1 person), 20-24 (10 persons)
    yob <- c(rep(2000, 10), 1930)
    index <- rep(2024, 11)
    groups <- .computeAgeGroups(yob, index, bin_width = 5L, min_cell = 3L)
    # The unsupported top bin is suppressed, never merged into a
    # data-dependent label that would differ between servers.
    expect_true(is.na(groups[11]))
    expect_true(all(groups[seq_len(10)] == "20-24"))
  })
})

test_that("computeAgeGroups counts distinct persons, not recurrent episodes", {
  # Person 1 has ten episodes in an otherwise unsupported old-age bin. Those
  # episodes must count as one person, forcing the bin to be suppressed.
  yob <- c(rep(1930, 10), rep(2000, 5))
  index <- rep(2024, length(yob))
  ids <- c(rep(1L, 10), 2:6)
  groups <- .computeAgeGroups(yob, index, bin_width = 5L, min_cell = 3L,
                              person_id = ids)
  expect_true(all(is.na(groups[seq_len(10)])))
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
  # Inclusive five-year epidemiological bands are accepted; four are not.
  expect_equal(.classifyFilter("age_range", list(min = 20, max = 24)),
               "constrained")
  expect_equal(.classifyFilter("age_range", list(min = 20, max = 23)),
               "blocked")
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
  # Shifted client bins are not unions of the public server grid.
  expect_equal(.classifyFilter("age_group", list(groups = c("18-24"))),
               "blocked")
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

test_that("age grid is public, configurable, and only permits coarsening", {
  settings <- .omopDisclosureSettings()
  expect_identical(settings$harmonization_contract_version,
                   "dsomop-harmonization-v3")
  expect_identical(settings$max_feature_specs, 1000)
  expect_identical(settings$max_pivot_concepts, 1000)
  expect_identical(settings$max_output_columns, 5000)
  expect_identical(settings$max_temporal_bins, 10000)
  expect_identical(settings$max_filter_depth, 32)
  expect_identical(settings$max_filter_nodes, 1024)
  expect_identical(settings$max_filter_values, 10000)
  expect_identical(settings$max_plan_outputs, 100)
  expect_identical(settings$max_analysis_scope_tables, 8)
  expect_identical(settings$max_temp_tables_per_handle, 256)
  expect_true(.ageGroupsOnGrid(c("0-9", "10-19", "80+"),
                               settings$age_breaks))
  expect_false(.ageGroupsOnGrid("18-24", settings$age_breaks))

  withr::with_options(list(dsomop.age_breaks = c(0, 10, 20, 30, 40, 50,
                                                 60, 70, 80, 90)), {
    expect_error(.computeAgeGroups(2000, 2024, bin_width = 5L,
                                   min_cell = 1L),
                 "not aligned")
    expect_equal(.computeAgeGroups(2000, 2024, bin_width = 10L,
                                   min_cell = 1L), "20-29")
  })

  expect_equal(
    .computeAgeGroups(
      2000, 2024, age_breaks = seq(0, 80, 10), min_cell = 1L
    ),
    "20-29"
  )
  expect_error(
    .computeAgeGroups(
      2000, 2024, age_breaks = c(0, 18, 40), min_cell = 1L
    ),
    "coarsening"
  )
  expect_error(
    .computeAgeGroups(
      2000, 2024, bin_width = 10L,
      age_breaks = seq(0, 80, 10), min_cell = 1L
    ),
    "not both"
  )
})

test_that("operational expansion caps use controller options and fail closed", {
  withr::with_options(list(
    dsomop.max_feature_specs = 17,
    dsomop.max_pivot_concepts = 23,
    dsomop.max_output_columns = 101,
    dsomop.max_temporal_bins = 211,
    dsomop.max_filter_depth = 7,
    dsomop.max_filter_nodes = 19,
    dsomop.max_filter_values = 31,
    dsomop.max_plan_outputs = 5,
    dsomop.max_analysis_scope_tables = 7,
    dsomop.max_temp_tables_per_handle = 29
  ), {
    settings <- .omopDisclosureSettings()
    expect_identical(settings$max_feature_specs, 17)
    expect_identical(settings$max_pivot_concepts, 23)
    expect_identical(settings$max_output_columns, 101)
    expect_identical(settings$max_temporal_bins, 211)
    expect_identical(settings$max_filter_depth, 7)
    expect_identical(settings$max_filter_nodes, 19)
    expect_identical(settings$max_filter_values, 31)
    expect_identical(settings$max_plan_outputs, 5)
    expect_identical(settings$max_analysis_scope_tables, 7)
    expect_identical(settings$max_temp_tables_per_handle, 29)
  })
  withr::with_options(list(dsomop.max_output_columns = 1.5), {
    expect_error(.omopDisclosureSettings(), "max_output_columns")
  })
  withr::with_options(list(dsomop.max_analysis_scope_tables = 0), {
    expect_error(.omopDisclosureSettings(), "max_analysis_scope_tables")
  })
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
