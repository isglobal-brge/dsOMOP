# A-19 item 1 / F-26: use independently captured, identical-plan tab0 controls
# for the eight default-tab compatibility results whose age_group was suppressed.
# test-ir-results.R independently derives corrected bands from cohort/person
# rows with base cut(); test-ir-v1-compat.R compares each complete result hash.
# This test protects the exact control selection without changing the original
# capture, accepting a broad result waiver, or calling production banding code.
test_that("F-26 compatibility corrections use identical-plan baseline controls", {
  fixture <- jsonlite::fromJSON(testthat::test_path("fixtures", "ir-v1-compat.json"),
                               simplifyVector = FALSE)
  dispositions <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "ir-v1-compat-dispositions.json"),
    simplifyVector = FALSE)$cases
  cases <- stats::setNames(fixture$cases, vapply(fixture$cases, `[[`, "", "id"))
  suffixes <- c("control", "str", "str_signed", "str_exponent", "str_spaces",
                "str_hex", "double", "empty_array")
  prefix <- "A.baseline.age_breaks.item."
  ids <- paste0(prefix, suffixes)
  recorded <- names(Filter(function(d) identical(d$classification, "corrected") &&
                            grepl("F-26", paste(d$reference, d$reason), fixed = TRUE),
                          dispositions))
  expect_setequal(recorded, ids)
  expect_length(fixture$baseline$captures, 2L)
  expect_false(any(c(ids, paste0(prefix, "tab0.", suffixes)) %in%
                     unlist(fixture$nondeterministic)))
  for (suffix in suffixes) {
    id <- paste0(prefix, suffix)
    control_id <- paste0(prefix, "tab0.", suffix)
    original <- cases[[id]]
    control <- cases[[control_id]]
    disposition <- dispositions[[id]]
    expect_identical(disposition$current, "ok", info = id)
    expect_identical(disposition$result_from, control_id, info = id)
    expect_identical(original$baseline$status, "ok", info = id)
    expect_identical(control$baseline$status, "ok", info = control_id)
    expect_identical(original$plan_sha256, control$plan_sha256, info = id)
    expect_identical(original$template, control$template, info = id)
    expect_identical(original$value, control$value, info = id)
    expect_identical(original$baseline$outputs, control$baseline$outputs, info = id)
    expect_false(identical(original$baseline$result_sha256,
                           control$baseline$result_sha256), info = id)
    original_options <- utils::modifyList(fixture$default_options,
                                         if (is.null(original$options)) list() else original$options)
    control_options <- utils::modifyList(fixture$default_options, control$options)
    expect_null(original_options$nfilter.tab, info = id)
    expect_identical(control_options$nfilter.tab, 0L, info = id)
    control_options$nfilter.tab <- NULL
    expect_identical(original_options, control_options, info = id)
  }
})
