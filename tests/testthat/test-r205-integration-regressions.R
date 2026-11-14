test_that("SQL BIGINT selectors retain the signed minimum and character NULLs", {
  values <- c("-9223372036854775808", "-9223372036854775807", NA_character_)
  x <- list(start_date = as.Date("2020-01-01") + 0:2, v = values)
  for (fn in c("mode", "min", "first_value", "nth_value", "closest_value")) {
    expect_identical(.reduceOne(fn, x, list(n = 1L), x$start_date[1], "bigint"), values[1], info = fn)
  }
  expect_identical(.reduceOne("max", x, list(), value_type = "bigint"), values[2])
  expect_identical(.reduceTypedNA("bigint", 2L), rep(NA_character_, 2L))
  expect_identical(.reduceCoerce(list(NA, NA), "bigint"), rep(NA_character_, 2L))
  expect_error(.reduceOne("mean", x, list(), value_type = "bigint"), "cannot retain exact integers")
  x$v <- c(values[1], NA_character_, NA_character_)
  expect_identical(.reduceOne("mean", x, list(), value_type = "bigint"), -2^63)
})

test_that("owned staging roots survive configuration changes but unsafe roots refuse", {
  h <- create_test_handle(); on.exit(cleanup_handle(h), add = TRUE)
  first <- withr::local_tempdir(); second <- withr::local_tempdir()
  Sys.chmod(c(first, second), "0700")
  withr::local_options(list(dsstaging.base_dir = first))
  a <- .planContext(h, list()); on.exit(.planContextClose(a), add = TRUE)
  .publishStagingDir(a)
  options(dsstaging.base_dir = second)
  b <- .planContext(h, list()); on.exit(.planContextClose(b), add = TRUE)
  expect_silent(.publishStagingDir(b))
  expect_true(dir.exists(a$staging_dir)); expect_true(dir.exists(b$staging_dir))
  expect_identical(h$staging_bases[[a$staging_dir]], first)
  if (.Platform$OS.type != "windows") {
    Sys.chmod(first, "0755")
    expect_error(.cleanupHandleStaging(h), "unsafe or invalid owned path")
    expect_true(dir.exists(a$staging_dir))
    expect_true(a$staging_dir %in% h$staging_dirs)
    Sys.chmod(first, "0700")
  }
  expect_silent(.cleanupHandleStaging(h))
  expect_false(dir.exists(a$staging_dir)); expect_false(dir.exists(b$staging_dir))
  expect_length(h$staging_dirs, 0L); expect_length(h$staging_bases, 0L)
  foreign <- file.path(withr::local_tempdir(), .generateStagingToken())
  dir.create(foreign, mode = "0700")
  h$staging_dirs <- foreign
  expect_error(.cleanupHandleStaging(h), "unsafe or invalid owned path")
  expect_true(dir.exists(foreign))
  h$staging_dirs <- character()
})

test_that("factor-only protocol preserves numeric metadata and rejects supplied conflicts", {
  frame <- data.frame(value = 1:3)
  numbers <- list(value = list(unit = "1", input_type = "integer", result_type = "integer"))
  attr(frame, "omop_numbers") <- numbers
  report <- .conceptLevelReport(frame)
  spec <- list(concept_protocol = 1L, columns = list(), evidence = report$evidence)
  result <- .conceptRelevel(frame, spec)
  expect_identical(result$value, frame$value)
  expect_identical(attr(result, "omop_numbers"), numbers)
  spec$numbers <- list(value = list(unit = "days"))
  expect_error(.conceptRelevel(frame, spec), "Numeric column semantics differ")
  expect_error(.numberValidateShared(numbers, list()), "Numeric column semantics differ")
})


test_that("owned-path inspection rejects symlink ancestors even for missing roots", {
  skip_on_os("windows")
  parent <- withr::local_tempdir()
  target <- withr::local_tempdir()
  link <- file.path(parent, "link")
  expect_true(file.symlink(target, link))
  base <- file.path(link, "missing-root")
  path <- file.path(base, .generateStagingToken())
  expect_identical(.inspectOwnedStagingPath(path, base)$state, "unsafe")
})
