# G-09 (dsOMOP 2.10.3): the typed channel uses canonical protected components.
# Dummy levels, clamp-only outer breaks, harmless cap variants, statistic
# aliases, positive masks and lineage cosmetics reuse the same components;
# the community-tool gate runs before any work and every attempt is audited.
# Helpers in helper-tools-release.R.

# A sealed person-local table of n persons with a few records each.
.typed_table <- function(n = 60, producer = "test/fixture", extra = NULL,
                         reverse = FALSE) {
  set.seed(42)
  person <- rep(seq_len(n), times = rep_len(c(1L, 2L, 3L), n))
  value <- data.frame(
    person_id = person,
    category = c("a", "b", "c")[(seq_along(person) %% 3L) + 1L],
    measurement = round(20 + (seq_along(person) * 7) %% 60, 1),
    age = (seq_along(person) * 11) %% 90,
    event_date = as.Date("2020-01-01") + (seq_along(person) * 13) %% 700,
    stringsAsFactors = FALSE
  )
  if (!is.null(extra)) value <- rbind(value, extra)
  if (reverse) value <- value[rev(seq_len(nrow(value))), , drop = FALSE]
  value <- .testPseudonymize(value)
  .dsomopDpSealPersonLocal(value, producer = producer,
                           dataset_identity = list(
                             resource_identity = "typed-test",
                             target_dialect = "postgresql",
                             cdm_schema = "main", vocabulary_schema = "main",
                             results_schema = "main"))
}

.typed_release <- function(x, ...) {
  suppressMessages(omopDpReleaseDS(x, list(...)))
}

# Signed component of one coordinate.
.typed_signed <- function(payload, coordinate) {
  comps <- payload$components
  comps$signed[match(coordinate, comps$coordinates)]
}

test_that("the community-tool gate runs before any typed work", {
  .release_state()
  x <- .typed_table()
  calls <- 0L
  testthat::local_mocked_bindings(.dsomopTypedRelease = function(...) {
    calls <<- calls + 1L
    stop("unexpected")
  })
  withr::local_options(list(dsomop.tools.enabled = FALSE))
  expect_error(omopDpReleaseDS(x, list(statistic = "count")),
               "community tools .* are disabled")
  expect_identical(calls, 0L)
})

test_that("every typed attempt is audited without private values", {
  .release_state()
  x <- .typed_table()
  logged <- capture_messages(omopDpReleaseDS(x, list(statistic = "count")))
  audit <- grep("method=omopDpReleaseDS", logged, value = TRUE)
  expect_length(audit, 1L)
  expect_match(audit, '"outcome":"released"')
  expect_match(audit, '"protection":"realised_sticky"')
  expect_false(grepl("[0-9a-f]{64}", audit))
  failed <- capture_messages(try(omopDpReleaseDS(
    x, list(statistic = "bounded_mean", variable = "measurement",
            lower = 0, upper = 77)), silent = TRUE))
  audit <- grep("method=omopDpReleaseDS", failed, value = TRUE)
  expect_length(audit, 1L)
  expect_match(audit, '"outcome":"refused"')
  expect_false(grepl("[0-9a-f]{64}", audit))
})

test_that("statistic aliases of one person total share one component", {
  .release_state()
  x <- .typed_table()
  count <- .typed_release(x, statistic = "count")
  records <- .typed_release(x, statistic = "bounded_record_count",
                            reducer = "records", max_contributions = 1)
  rate <- .typed_release(x, statistic = "binary_rate", variable = "category",
                         positive = "a")
  n <- .typed_signed(count, "stratum=root")
  expect_identical(.typed_signed(records, "stratum=root"), n)
  expect_identical(.typed_signed(rate, "stratum=root"), n)
  # The audit's alias attack (2003 / 1999 / 1998 for one true count) no
  # longer draws three independent values.
  expect_equal(records$noisy_count, max(0, n), tolerance = 1e-9)
  expect_equal(rate$noisy_denominator, max(0, n), tolerance = 1e-9)
  expect_identical(count$protection, "realised_sticky")
  expect_true(is.na(count$mechanism_epsilon))
  expect_equal(count$allocation$N, 0.02)
})

# The raw (pre-reconciliation) noisy components of a typed release, by
# coordinate, from the internal pipeline with the actual keys and sampler.
.typed_raw <- function(x, ...) {
  policy <- .dsomopDpPolicy()
  spec <- list(...)
  common <- .dsomopDpParseSpec(x, spec, policy)
  input <- .dsomopDpValidateInput(x, policy)
  common$order_value <- .dsomopDpOrderVector(input$x, common$order_by)
  tokens <- .dsomopReleasePersonTokens(
    .dsomopReleaseKeys(policy), policy,
    .dsomopTypedPersonIds(x, input$token))
  capsule <- attr(x, "dsomop_dp_provenance", exact = TRUE)
  analysis <- .dsomopTypedAnalysis(input, common, policy, tokens,
                                   capsule$lineage_id)
  stats::setNames(
    .dsomopTypedDraw(policy, "realised_sticky", analysis$components),
    vapply(analysis$components, function(c) c$coordinate, character(1L)))
}

test_that("dummy categorical levels never redraw the real levels", {
  .release_state()
  x <- .typed_table()
  spec <- list(statistic = "categorical_histogram", variable = "category",
               levels = c("a", "b", "c"), reducer = "first",
               order_by = "event_date")
  base <- do.call(.typed_release, c(list(x), spec))
  coords <- paste0("stratum=root|cell=", c("a", "b", "c"))
  raw <- do.call(.typed_raw, c(list(x), spec))[c("stratum=root", coords)]
  shown <- vapply(coords, function(k) .typed_signed(base, k), numeric(1L))
  for (i in c(1L, 7L, 400L)) {
    dummies <- sprintf("zz_dummy_%04d", seq_len(i))
    padded_spec <- utils::modifyList(spec, list(
      levels = c(dummies, "c", "a", "b")))
    padded <- do.call(.typed_release, c(list(x), padded_spec))
    # The same draws for the person total and every real cell...
    expect_identical(do.call(.typed_raw, c(list(x), padded_spec))[
      c("stratum=root", coords)], raw)
    expect_identical(.typed_signed(padded, "stratum=root"),
                     .typed_signed(base, "stratum=root"))
    # ...so the exported real cells differ only by the reconciliation shift
    # (their contrasts are unchanged) and still sum with the dummies to N.
    real <- vapply(coords, function(k) .typed_signed(padded, k), numeric(1L))
    expect_equal(real - real[[1L]], shown - shown[[1L]], tolerance = 1e-9)
    expect_equal(sum(padded$components$signed[-1L]),
                 .typed_signed(padded, "stratum=root"), tolerance = 1e-6)
  }
})

test_that("clamp-only outer breaks and snapped breaks share one basis", {
  .release_state()
  x <- .typed_table()
  release <- function(breaks) .typed_release(
    x, statistic = "numeric_histogram", variable = "measurement",
    breaks = breaks, reducer = "mean")
  a <- release(c(0, 30, 50, 80))
  b <- release(c(-1e6, 30, 50, 1e6))
  c <- release(c(-5, 30.2, 49.9, 99))
  expect_identical(a$components$signed, b$components$signed)
  expect_identical(a$components$signed, c$components$signed)
  expect_identical(a$breaks, c(0, 30, 50, 80))
  expect_identical(c$requested_breaks, c(-5, 30.2, 49.9, 99))
  expect_identical(c$breaks, c(-5, 30, 50, 99))
  expect_identical(a$interval_contract,
                   "left_closed_right_open_first_last_open")
})

test_that("record-cap variants are views of one multiplicity basis", {
  .release_state()
  x <- .typed_table()
  payloads <- lapply(1:5, function(k) .typed_release(
    x, statistic = "bounded_record_count", reducer = "records",
    max_contributions = k))
  for (p in payloads[-1L]) {
    expect_identical(p$components$signed, payloads[[1L]]$components$signed)
  }
  counts <- vapply(payloads, `[[`, numeric(1L), "noisy_count")
  expect_false(is.unsorted(counts))
})

test_that("positive masks and reordered or duplicated public lists replay", {
  .release_state()
  x <- .typed_table()
  base <- .typed_release(x, statistic = "binary_rate", variable = "category",
                         positive = c("a", "b"))
  same <- .typed_release(x, statistic = "binary_rate", variable = "category",
                         positive = c("b", "zz_dummy", "a", "a"))
  expect_identical(same$components$signed, base$components$signed)
  expect_identical(same$value, base$value)
})

test_that("bounded_distinct ignores dummy domains and k variants of equal maps", {
  .release_state()
  x <- .typed_table()
  base <- .typed_release(x, statistic = "bounded_distinct",
                         variable = "category", levels = c("a", "b", "c"),
                         reducer = "distinct", max_contributions = 3)
  padded <- .typed_release(x, statistic = "bounded_distinct",
                           variable = "category",
                           levels = c("c", "zz1", "a", "zz2", "b"),
                           reducer = "distinct", max_contributions = 3)
  wider <- .typed_release(x, statistic = "bounded_distinct",
                          variable = "category", levels = c("a", "b", "c"),
                          reducer = "distinct", max_contributions = 7)
  expect_identical(padded$components$signed, base$components$signed)
  # Every person has at most three labels: k = 3 and k = 7 select the same
  # map, and the fixed sensitivity K makes them one draw.
  expect_identical(wider$components$signed, base$components$signed)
  expect_equal(base$sensitivity$D, 10)
  expect_true(base$noisy_count >= 0 && base$noisy_count <= 3)
})

test_that("bounded means need a registered public scale", {
  .release_state()
  x <- .typed_table()
  ok <- .typed_release(x, statistic = "bounded_mean", variable = "age",
                       lower = 0, upper = 110)
  expect_identical(ok$scale, "age_years")
  expect_true(ok$value >= 0 && ok$value <= 110)
  expect_error(.typed_release(x, statistic = "bounded_mean",
                              variable = "age", lower = 0, upper = 120),
               "public_scale_unavailable")
  # The refusal is static: it does not depend on the data.
  expect_error(.typed_release(.typed_table(5), statistic = "bounded_mean",
                              variable = "age", lower = 0, upper = 120),
               "public_scale_unavailable")
})

test_that("row order, row duplication and renamed columns replay exactly", {
  .release_state()
  x <- .typed_table()
  y <- .typed_table(reverse = TRUE)
  spec <- list(statistic = "categorical_histogram", variable = "category",
               levels = c("a", "b", "c"), reducer = "presence",
               max_contributions = 2)
  a <- suppressMessages(omopDpReleaseDS(x, spec))
  b <- suppressMessages(omopDpReleaseDS(y, spec))
  expect_identical(a$components$signed, b$components$signed)
  renamed <- .typed_table()
  names(renamed)[names(renamed) == "category"] <- "kind"
  renamed <- .dsomopDpSealPersonLocal(
    renamed, producer = "test/fixture", dataset_identity = list(
      resource_identity = "typed-test", target_dialect = "postgresql",
      cdm_schema = "main", vocabulary_schema = "main",
      results_schema = "main"))
  spec$variable <- "kind"
  c <- suppressMessages(omopDpReleaseDS(renamed, spec))
  expect_identical(c$components$signed, a$components$signed)
})

test_that("a changed contribution redraws only its own components", {
  .release_state()
  x <- .typed_table()
  extra <- data.frame(person_id = 999L, category = "b", measurement = 50,
                      age = 40, event_date = as.Date("2020-05-05"))
  y <- .typed_table(extra = extra)
  spec <- list(statistic = "categorical_histogram", variable = "category",
               levels = c("a", "b", "c"), reducer = "first",
               order_by = "event_date")
  ra <- do.call(.typed_raw, c(list(x), spec))
  rb <- do.call(.typed_raw, c(list(y), spec))
  # Cells a and c keep their draws; the new person's cell b and the person
  # total are redrawn.
  expect_identical(ra[["stratum=root|cell=a"]], rb[["stratum=root|cell=a"]])
  expect_identical(ra[["stratum=root|cell=c"]], rb[["stratum=root|cell=c"]])
  expect_false(identical(ra[["stratum=root|cell=b"]],
                         rb[["stratum=root|cell=b"]]))
  expect_false(identical(ra[["stratum=root"]], rb[["stratum=root"]]))
  # In the exported components the unchanged contrast a - c is preserved.
  a <- suppressMessages(omopDpReleaseDS(x, spec))
  b <- suppressMessages(omopDpReleaseDS(y, spec))
  expect_equal(.typed_signed(a, "stratum=root|cell=a") -
                 .typed_signed(a, "stratum=root|cell=c"),
               .typed_signed(b, "stratum=root|cell=a") -
                 .typed_signed(b, "stratum=root|cell=c"), tolerance = 1e-9)
})

test_that("DP-only mode accepts only fixed-synopsis typed releases", {
  .release_state(mode = "dp")
  plan <- .typed_table()
  expect_error(.typed_release(plan, statistic = "count"),
               "no fixed-synopsis protection")
  loader <- .typed_table(producer = "query/measurement.load/0123456789abcdef")
  out <- .typed_release(loader, statistic = "count")
  expect_identical(out$protection, "fixed_synopsis")
  expect_equal(out$mechanism_epsilon, 0.02)
  expect_identical(out$guarantee_scope,
                   "fixed_vector_before_exact_support_gate")
  expect_error(.typed_release(loader, statistic = "bounded_distinct",
                              variable = "category", levels = c("a", "b"),
                              reducer = "distinct", max_contributions = 2),
               "no fixed-synopsis protection")
  # Without a custodial generation even a loader table is not a fixed basis.
  .release_state(mode = "dp", snapshot_id = "")
  expect_error(.typed_release(loader, statistic = "count"),
               "no fixed-synopsis protection|identity")
})

test_that("typed releases are history-free and carry no secrets", {
  .release_state()
  x <- .typed_table()
  first <- .typed_release(x, statistic = "numeric_histogram",
                          variable = "measurement", breaks = c(0, 30, 60),
                          reducer = "mean")
  for (i in seq_len(30L)) {
    .typed_release(x, statistic = "count")
    .typed_release(x, statistic = "numeric_histogram",
                   variable = "measurement",
                   breaks = c(0, 30 + i %% 3, 60), reducer = "mean")
  }
  .release_restart()
  again <- .typed_release(x, statistic = "numeric_histogram",
                          variable = "measurement", breaks = c(0, 30, 60),
                          reducer = "mean")
  expect_identical(again, first)
  text <- paste(capture.output(str(first)), collapse = "\n")
  expect_false(grepl(.release_test_root, text, fixed = TRUE))
  expect_identical(first$privacy_call_quota, "none")
  expect_false(first$history_based_admission)
  expect_false(any(c("seed", "fingerprint", "token", "remaining_epsilon",
                     "spent_epsilon") %in% names(first)))
})

test_that("after 0, 1 and 10,000 earlier calls the answer and status are unchanged", {
  .release_state()
  x <- .typed_table(n = 9)
  first <- .typed_release(x, statistic = "count")
  status <- omopDpStatusDS()
  expect_identical(.typed_release(x, statistic = "count"), first)
  # 10,000 repeated questions (about 10 ms each), then distinct ones.
  elapsed <- system.time({
    for (i in seq_len(10000L)) .typed_release(x, statistic = "count")
    for (k in rep(1:3, 20L)) {
      .typed_release(x, statistic = "bounded_record_count",
                     reducer = "records", max_contributions = k)
    }
  })[["elapsed"]]
  message("[typed-history] 10060 calls in ", round(elapsed, 1), " s")
  expect_identical(.typed_release(x, statistic = "count"), first)
  # A worker restart replays as well, and the status is the same.
  .release_restart()
  expect_identical(.typed_release(x, statistic = "count"), first)
  expect_identical(omopDpStatusDS(), status)
})
