test_that("concurrent first DP use logs creation only for the durable writer", {
  skip_on_os("windows")
  .release_state()
  withr::local_options(list(dsomop.dp.noise_root = NULL))
  results <- parallel::mclapply(seq_len(4), function(i) {
    messages <- character()
    root <- withCallingHandlers(.dsomopDpNoiseRoot(), message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
    list(id = root$key_id, messages = messages)
  }, mc.cores = 4L, mc.preschedule = FALSE)
  expect_length(unique(vapply(results, `[[`, "", "id")), 1L)
  expect_equal(sum(grepl("created the DP noise root", unlist(lapply(results, `[[`, "messages")))), 1L)
  path <- .dsomopSecretPath("dp_noise_root")
  expect_equal(file.info(path)$size, 32)
  expect_equal(as.integer(file.info(path)$mode), 384L)
  expect_equal(as.integer(file.info(dirname(path))$mode), 448L)
  expect_message(.dsomopDpNoiseRoot(), NA)
})

test_that("the measurement fixture must request its own concept through the endpoint", {
  .release_state()
  h <- .release_derived_handle(60)
  h$person_key_identity <- "test://e2e-measurement"
  h$resource_client$close <- function() invisible(NULL)
  suppressWarnings(.omopAnalysisRegistry(h))
  .setHandle("e2e_measurement", h)
  withr::defer(.removeHandle("e2e_measurement"))
  DBI::dbExecute(h$conn, "UPDATE measurement SET value_as_number = 1, unit_concept_id = 999001, measurement_concept_id = 3004410")
  run <- function(params) suppressMessages(omopAnalysisRunDS(
    "e2e_measurement", "dsomop:measurement.value_stats", params))
  # GiBleed fixture inserts concept 3004410; an omitted parameter selects the
  # catalogue default instead. Both requests retain the actual final gate.
  expect_error(run(list()), "Disclosive")
  value <- run(list(concept_id = "3004410"))
  expect_identical(value, run(list(concept_id = "3004410")))
  expect_identical(attr(value, "dsomop_protection")$protection, "realised_sticky")
  owners <- attr(value, "dsomop_release")$owners
  expect_true(any(vapply(owners, function(o) any(vapply(o$components,
    function(c) length(c$h) > 0L && all(is.finite(c$h)), logical(1))), logical(1))))
  DBI::dbExecute(h$conn, "UPDATE measurement SET value_as_number = NULL")
  expect_error(run(list(concept_id = "3004410")), "Disclosive")
})


test_that("native string HMAC matches the raw byte contract", {
  key <- as.raw(0:31)
  values <- c("", "ascii", "é\n雪", paste(rep("a", 700000L), collapse = ""))
  for (value in values) {
    expected <- paste(format(as.raw(openssl::sha256(charToRaw(enc2utf8(value)), key = key))), collapse = "")
    expect_identical(.dsomopDpHmac(key, value), expected)
    expect_identical(.dsomopDpHmac(key, charToRaw(enc2utf8(value))), expected)
  }
  expect_error(.dsomopDpHmac(raw(31), "x"), "root is unavailable")
})
