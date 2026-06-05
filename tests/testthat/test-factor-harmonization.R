# Coordination-layer server methods: omopFactorLevelsDS (aggregate, reports
# disclosure-safe concept-id levels) and omopAsFactorColumnsDS (assign,
# recodes concept-id columns to a harmonized factor from a shared level spec).

# --- omopFactorLevelsDS -------------------------------------------------------

test_that("factorLevels returns empty structure for a non-data.frame", {
  res <- omopFactorLevelsDS(42)
  expect_equal(res$levels, list())
  expect_equal(res$unsafe, character(0))
  expect_true(is.numeric(res$nfilter_levels_max))
})

test_that("factorLevels ignores frames without _concept_id columns", {
  df <- data.frame(person_id = 1:3, value_as_number = c(1.1, 2.2, 3.3))
  res <- omopFactorLevelsDS(df)
  expect_equal(res$levels, list())
  expect_equal(res$unsafe, character(0))
})

test_that("factorLevels reports distinct levels of a safe concept-id column", {
  withr::with_options(list(nfilter.levels.max = 40,
                           nfilter.levels.density = 0.5), {
    df <- data.frame(gender_concept_id = c(8532L, 8507L, 8532L, 8507L))
    res <- omopFactorLevelsDS(df)
    expect_true("gender_concept_id" %in% names(res$levels))
    expect_setequal(res$levels$gender_concept_id, c("8532", "8507"))
    expect_equal(res$unsafe, character(0))
  })
})

test_that("factorLevels excludes NA from the reported levels", {
  withr::with_options(list(nfilter.levels.max = 40,
                           nfilter.levels.density = 1), {
    df <- data.frame(gender_concept_id = c(8532L, NA, 8507L, NA))
    res <- omopFactorLevelsDS(df)
    expect_setequal(res$levels$gender_concept_id, c("8532", "8507"))
  })
})

test_that("factorLevels flags a high-cardinality column as unsafe", {
  withr::with_options(list(nfilter.levels.max = 5,
                           nfilter.levels.density = 1), {
    df <- data.frame(condition_concept_id = 1:20)
    res <- omopFactorLevelsDS(df)
    expect_true("condition_concept_id" %in% res$unsafe)
    expect_false("condition_concept_id" %in% names(res$levels))
  })
})

test_that("factorLevels flags a high-density column as unsafe", {
  withr::with_options(list(nfilter.levels.max = 100,
                           nfilter.levels.density = 0.1), {
    # 8 distinct values over 10 rows => density 0.8 > 0.1
    df <- data.frame(drug_concept_id = c(1:8, 1L, 2L))
    res <- omopFactorLevelsDS(df)
    expect_true("drug_concept_id" %in% res$unsafe)
  })
})

test_that("factorLevels separates safe and unsafe columns in one frame", {
  withr::with_options(list(nfilter.levels.max = 5,
                           nfilter.levels.density = 1), {
    df <- data.frame(
      gender_concept_id = rep(c(8532L, 8507L), 10),
      condition_concept_id = 1:20
    )
    res <- omopFactorLevelsDS(df)
    expect_true("gender_concept_id" %in% names(res$levels))
    expect_true("condition_concept_id" %in% res$unsafe)
  })
})

test_that("factorLevels reports the server level cap", {
  withr::with_options(list(nfilter.levels.max = 17), {
    res <- omopFactorLevelsDS(data.frame(x = 1))
    expect_equal(res$nfilter_levels_max, 17)
  })
})

# --- renamed concept columns: the omop_concept_cols tag -----------------------
# A user may alias a concept column (c(sex = "gender_concept_id")), which lands
# it under a name without the _concept_id suffix. The extractor tags such
# columns by their landed name so harmonization still recognises them; renaming
# never silently opts a column out of federated factor coding.

test_that(".conceptAliases returns the landed name of an aliased concept col", {
  spec <- .colSpec(c(sex = "gender_concept_id", "year_of_birth"))
  expect_equal(.conceptAliases(spec), "sex")
})

test_that(".conceptAliases falls back to the source name when not aliased", {
  spec <- .colSpec(c("gender_concept_id", "year_of_birth"))
  expect_equal(.conceptAliases(spec), "gender_concept_id")
})

test_that(".conceptAliases collects several concept cols with mixed aliasing", {
  spec <- .colSpec(c(sex = "gender_concept_id", "race_concept_id"))
  expect_setequal(.conceptAliases(spec), c("sex", "race_concept_id"))
})

test_that(".conceptAliases ignores non-concept columns and NULL specs", {
  expect_equal(.conceptAliases(.colSpec(c(birth = "year_of_birth"))),
               character(0))
  expect_equal(.conceptAliases(NULL), character(0))
})

test_that("factorLevels harmonizes a renamed concept column via the tag", {
  withr::with_options(list(nfilter.levels.max = 40,
                           nfilter.levels.density = 1), {
    df <- data.frame(sex = c(8507L, 8532L, 8507L, 8532L))
    # Without the tag the renamed column is invisible (no _concept_id suffix).
    expect_equal(omopFactorLevelsDS(df)$levels, list())
    # Tagged (as omopPlanExecuteDS does) it is recognised and reported.
    attr(df, "omop_concept_cols") <- "sex"
    res <- omopFactorLevelsDS(df)
    expect_setequal(res$levels$sex, c("8507", "8532"))
  })
})

test_that("factorLevels ignores a tag that names an absent column", {
  withr::with_options(list(nfilter.levels.max = 40,
                           nfilter.levels.density = 1), {
    df <- data.frame(gender_concept_id = c(8507L, 8532L))
    attr(df, "omop_concept_cols") <- "does_not_exist"
    res <- omopFactorLevelsDS(df)
    expect_false("does_not_exist" %in% names(res$levels))
    expect_true("gender_concept_id" %in% names(res$levels))
  })
})

test_that("factorLevels still gates a tagged renamed column for disclosure", {
  withr::with_options(list(nfilter.levels.max = 5,
                           nfilter.levels.density = 1), {
    df <- data.frame(grp = 1:20)
    attr(df, "omop_concept_cols") <- "grp"
    res <- omopFactorLevelsDS(df)
    expect_true("grp" %in% res$unsafe)
    expect_false("grp" %in% names(res$levels))
  })
})

# --- omopAsFactorColumnsDS ----------------------------------------------------

test_that("asFactorColumns errors on a non-data.frame target", {
  expect_error(omopAsFactorColumnsDS(1:3, list()), "not a data.frame")
})

test_that("asFactorColumns is a no-op for an empty spec", {
  df <- data.frame(gender_concept_id = c(8532L, 8507L))
  expect_identical(omopAsFactorColumnsDS(df, list()), df)
})

test_that("asFactorColumns recodes a column with native-list spec (DSLite path)", {
  df <- data.frame(gender_concept_id = c(8507L, 8532L, 8507L))
  spec <- list(gender_concept_id = c("8532", "8507"))
  out <- omopAsFactorColumnsDS(df, spec)
  expect_s3_class(out$gender_concept_id, "factor")
  # Level ORDER is taken verbatim from the shared spec, not the local data.
  expect_equal(levels(out$gender_concept_id), c("8532", "8507"))
  expect_equal(as.character(out$gender_concept_id),
               c("8507", "8532", "8507"))
})

test_that("asFactorColumns decodes a JSON spec (Opal transport path)", {
  df <- data.frame(gender_concept_id = c(8507L, 8532L))
  out <- omopAsFactorColumnsDS(df, '{"gender_concept_id":["8532","8507"]}')
  expect_s3_class(out$gender_concept_id, "factor")
  expect_equal(levels(out$gender_concept_id), c("8532", "8507"))
})

test_that("asFactorColumns harmonization is lossless when levels cover data", {
  df <- data.frame(gender_concept_id = c(8532L, 8507L, 8532L))
  out <- omopAsFactorColumnsDS(df, list(gender_concept_id = c("8507", "8532")))
  # Round-trip back to the original ids: no value dropped to NA.
  expect_equal(as.integer(as.character(out$gender_concept_id)),
               c(8532L, 8507L, 8532L))
  expect_false(anyNA(out$gender_concept_id))
})

test_that("asFactorColumns keeps a union level absent locally as an empty level", {
  # Missing-value robustness: '9999' exists on another site but not here.
  # It must survive as a valid (empty) level so the factor coding matches
  # across the federation and pooled models line up.
  df <- data.frame(gender_concept_id = c(8532L, 8507L))
  out <- omopAsFactorColumnsDS(df,
    list(gender_concept_id = c("8507", "8532", "9999")))
  expect_equal(levels(out$gender_concept_id), c("8507", "8532", "9999"))
  expect_equal(unname(table(out$gender_concept_id)["9999"]), 0L)
  expect_false(anyNA(out$gender_concept_id))
})

test_that("asFactorColumns skips columns absent from this server's frame", {
  df <- data.frame(gender_concept_id = c(8532L, 8507L))
  out <- omopAsFactorColumnsDS(df,
    list(gender_concept_id = c("8507", "8532"),
         drug_concept_id = c("1", "2")))
  expect_s3_class(out$gender_concept_id, "factor")
  expect_false("drug_concept_id" %in% names(out))
})

test_that("asFactorColumns drops empty and whitespace levels from the spec", {
  df <- data.frame(gender_concept_id = c(8532L, 8507L))
  out <- omopAsFactorColumnsDS(df,
    list(gender_concept_id = c("8507", "", "8532")))
  expect_equal(levels(out$gender_concept_id), c("8507", "8532"))
})

test_that("asFactorColumns re-enforces the level cap independently of client", {
  withr::with_options(list(nfilter.levels.max = 3), {
    df <- data.frame(condition_concept_id = 1:5)
    expect_error(
      omopAsFactorColumnsDS(df,
        list(condition_concept_id = as.character(1:5))),
      "nfilter.levels.max"
    )
  })
})

test_that("asFactorColumns recodes a translated (character) concept column", {
  df <- data.frame(
    gender_concept_id = c("male", "female", "male"),
    stringsAsFactors = FALSE
  )
  out <- omopAsFactorColumnsDS(df,
    list(gender_concept_id = c("female", "male")))
  expect_s3_class(out$gender_concept_id, "factor")
  expect_equal(levels(out$gender_concept_id), c("female", "male"))
  expect_equal(as.character(out$gender_concept_id),
               c("male", "female", "male"))
})
