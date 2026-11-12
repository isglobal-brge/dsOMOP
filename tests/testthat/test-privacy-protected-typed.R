# G-09 (dsOMOP 2.10.3): the typed channel uses canonical protected components
# of registered complete public bases. Statistic aliases share one person
# total, requested levels and breaks are views, harmless cap variants and
# lineage cosmetics reuse the same components, contributor identity is sealed
# with the table, compound identities are injective, the community-tool gate
# runs before any work and every attempt is audited. Helpers in
# helper-tools-release.R.

.typed_dataset <- list(resource_identity = "typed-test",
                       target_dialect = "postgresql", cdm_schema = "main",
                       vocabulary_schema = "main", results_schema = "main")

# A sealed person-local table of n persons with a few records each; the
# categorical column has the declared public domain {a, b, c}.
.typed_table <- function(n = 60, producer = "test/fixture", extra = NULL,
                         reverse = FALSE, key = NULL,
                         domains = list(category = c("a", "b", "c")),
                         dataset_identity = .typed_dataset,
                         data_generation = NULL) {
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
                           dataset_identity = dataset_identity,
                           person_key = key, domains = domains,
                           data_generation = data_generation)
}

.typed_release <- function(x, ...) {
  suppressMessages(omopDpReleaseDS(x, list(...)))
}

# Signed (exported view) component of one coordinate.
.typed_signed <- function(payload, coordinate) {
  comps <- payload$components
  comps$signed[match(coordinate, comps$coordinates)]
}

# The raw (pre-reconciliation) noisy components of a typed release, by
# coordinate, from the internal pipeline with the actual keys and sampler.
.typed_raw <- function(x, ...) {
  policy <- .dsomopDpPolicy()
  spec <- list(...)
  common <- .dsomopDpParseSpec(x, spec, policy)
  input <- .dsomopDpValidateInput(x, policy)
  common$order_value <- .dsomopDpOrderVector(input$x, common$order_by)
  tokens <- .dsomopDpSealedReleaseTokens(x, input$token)
  capsule <- attr(x, "dsomop_dp_provenance", exact = TRUE)
  analysis <- .dsomopTypedAnalysis(input, common, policy, tokens, capsule)
  stats::setNames(
    .dsomopTypedDraw(policy, "realised_sticky", analysis$components),
    vapply(analysis$components, function(c) c$coordinate, character(1L)))
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
  logged <- capture_messages(expect_error(
    omopDpReleaseDS(x, list(statistic = "count")),
    "community tools .* are disabled"))
  expect_identical(calls, 0L)
  # The refused attempt is audited too, without any decoded argument.
  audit <- grep("method=omopDpReleaseDS", logged, value = TRUE)
  expect_length(audit, 1L)
  expect_match(audit, '"outcome":"refused"')
  expect_match(audit, '"statistic":null|"statistic":"NA"|"statistic":NA')
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
  rec <- capture_messages(omopDpReleaseDS(x, list(
    statistic = "bounded_record_count", reducer = "records",
    max_contributions = 2)))
  expect_match(grep("method=omopDpReleaseDS", rec, value = TRUE),
               '"settings":\\{"reducer":"records","max_contributions":2')
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
  first <- .typed_release(x, statistic = "binary_rate", variable = "category",
                          positive = "a", reducer = "first",
                          order_by = "event_date")
  hist <- .typed_release(x, statistic = "categorical_histogram",
                         variable = "category", levels = "a",
                         reducer = "first", order_by = "event_date")
  numeric <- .typed_release(x, statistic = "numeric_histogram",
                            variable = "measurement", breaks = c(0, 50, 80),
                            reducer = "mean")
  n <- .typed_signed(count, "stratum=root")
  for (p in list(records, rate, first, hist, numeric)) {
    expect_identical(.typed_signed(p, "stratum=root"), n)
  }
  # The audit's alias attack (2003 / 1999 / 1998 for one true count) no
  # longer draws independent values.
  expect_equal(records$noisy_count, max(0, n), tolerance = 1e-9)
  expect_equal(rate$noisy_denominator, max(0, n), tolerance = 1e-9)
  expect_identical(count$protection, "realised_sticky")
  expect_true(is.na(count$mechanism_epsilon))
  expect_equal(count$allocation$N, 0.02)
})

test_that("requested levels are views: no view redraws or reshifts a cell", {
  .release_state()
  x <- .typed_table()
  spec <- list(statistic = "categorical_histogram", variable = "category",
               reducer = "first", order_by = "event_date")
  views <- list(c("a", "b", "c"), "a", c("c", "a"), c("b", "a", "a", "c"))
  out <- lapply(views, function(l) do.call(.typed_release,
                                           c(list(x), spec, list(levels = l))))
  raw <- lapply(views, function(l) do.call(.typed_raw,
                                           c(list(x), spec, list(levels = l))))
  for (i in seq_along(views)[-1L]) {
    # The same complete basis and the same draws, whatever is displayed...
    expect_identical(raw[[i]], raw[[1L]])
    expect_identical(out[[i]]$components$basis, out[[1L]]$components$basis)
    # ...and every displayed cell is the same signed value.
    for (l in out[[i]]$levels) {
      coordinate <- paste0("stratum=root|cell=", l)
      expect_identical(.typed_signed(out[[i]], coordinate),
                       .typed_signed(out[[1L]], coordinate))
    }
  }
  expect_identical(out[[4L]], out[[1L]])
  # The basis is the complete domain plus Other and Unknown, reconciled to
  # the one person total.
  basis <- out[[1L]]$components$basis
  expect_identical(basis$coordinates, c(
    "stratum=root", paste0("stratum=root|cell=", c("a", "b", "c")),
    "stratum=root|other", "stratum=root|unknown"))
  expect_equal(sum(basis$signed[-1L]), basis$signed[[1L]], tolerance = 1e-6)
})

test_that("labels outside the registered domain are refused publicly (B05)", {
  .release_state()
  x <- .typed_table()
  calls <- 0L
  testthat::local_mocked_bindings(.dsomopDpValidateInput = function(...) {
    calls <<- calls + 1L
    stop("private input reached")
  })
  for (levels in list(c("a", "b", "c", "unregistered_invalid_level"),
                      sprintf("zz_dummy_%04d", 1:400))) {
    expect_error(.typed_release(x, statistic = "categorical_histogram",
                                variable = "category", levels = levels,
                                reducer = "first", order_by = "event_date"),
                 "registered public domain")
  }
  expect_error(.typed_release(x, statistic = "binary_rate",
                              variable = "category",
                              positive = c("a", "zz_dummy")),
               "registered public domain")
  expect_error(.typed_release(x, statistic = "categorical_histogram",
                              variable = "measurement", levels = "1",
                              reducer = "first", order_by = "event_date"),
               "public_domain_unavailable")
  expect_identical(calls, 0L)
})

test_that("selection runs on the complete domain, never on the requested view", {
  .release_state()
  raw <- .testPseudonymize(data.frame(
    person_id = rep(1:40, each = 2), category = rep(c("c", "a"), 40),
    day = rep(c(1, 2), 40), stringsAsFactors = FALSE))
  x <- .dsomopDpSealPersonLocal(raw, producer = "test/selection",
                                dataset_identity = .typed_dataset,
                                domains = list(category = c("a", "b", "c")))
  # Everyone's first record is "c": a view without "c" must not move them to
  # their first displayed label "a".
  internal <- function(levels) {
    policy <- .dsomopDpPolicy()
    common <- .dsomopDpParseSpec(x, list(
      statistic = "categorical_histogram", variable = "category",
      levels = levels, reducer = "first", order_by = "day"), policy)
    input <- .dsomopDpValidateInput(x, policy)
    common$order_value <- .dsomopDpOrderVector(input$x, common$order_by)
    a <- .dsomopTypedAnalysis(input, common, policy,
                              .dsomopDpSealedReleaseTokens(x, input$token),
                              attr(x, "dsomop_dp_provenance"))
    stats::setNames(vapply(a$components, `[[`, numeric(1L), "value"),
                    vapply(a$components, `[[`, character(1L), "coordinate"))
  }
  narrow <- internal(c("a", "b"))
  expect_identical(narrow, internal(c("a", "b", "c")))
  expect_identical(unname(narrow[c("stratum=root|cell=a",
                                   "stratum=root|cell=c")]), c(0, 40))
})

test_that("numeric breaks are views of one fixed atom basis (B05)", {
  .release_state()
  x <- .typed_table()
  release <- function(breaks, reducer = "mean") .typed_release(
    x, statistic = "numeric_histogram", variable = "measurement",
    breaks = breaks, reducer = reducer)
  a <- release(c(0, 30, 50, 80))
  b <- release(c(-1e6, 30, 50, 1e6))
  c <- release(c(-5, 30.2, 49.9, 99))
  expect_identical(a$components$signed, b$components$signed)
  expect_identical(a$components$signed, c$components$signed)
  expect_identical(a$breaks, c(0, 30, 50, 80))
  expect_identical(c$requested_breaks, c(-5, 30.2, 49.9, 99))
  expect_identical(c$breaks, c(-5, 30, 50, 100))
  expect_identical(a$interval_contract,
                   "left_closed_right_open_first_last_open")
  expect_identical(a$basis_id, "typed-number-signed-nice-v1")
  # Coarse bins are exact sums of the same signed atoms as fine bins.
  fine <- release(c(0, 20, 30, 50, 80))
  expect_equal(sum(fine$components$signed[2:3]), a$components$signed[[2L]],
               tolerance = 1e-9)
  expect_identical(fine$components$basis, a$components$basis)
  for (reducer in c("min", "max", "median")) {
    p <- release(c(0, 30, 50, 80), reducer)
    expect_identical(.typed_signed(p, "stratum=root"),
                     .typed_signed(a, "stratum=root"))
  }
  # A declared public scale replaces the universal grid.
  y <- .dsomopDpSealPersonLocal(
    .testPseudonymize(data.frame(person_id = 1:30, age = 1:30 * 2.5)),
    producer = "test/scale", dataset_identity = .typed_dataset,
    scales = list(age = "age_years"))
  s <- .typed_release(y, statistic = "numeric_histogram", variable = "age",
                      breaks = c(0, 17.6, 40, 110), reducer = "mean")
  expect_identical(s$basis_id, "typed-number-scale=age_years-v1")
  expect_identical(s$breaks, c(0, 18, 40, 110))
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

test_that("irrelevant caps are normalized; presence is one multi-label owner", {
  .release_state()
  x <- .typed_table()
  first <- function(k) .typed_release(
    x, statistic = "categorical_histogram", variable = "category",
    levels = c("a", "b", "c"), reducer = "first", order_by = "event_date",
    max_contributions = k)
  expect_identical(first(4), first(1))
  expect_identical(first(4)$max_contributions, 1L)
  presence <- function(k) .typed_release(
    x, statistic = "categorical_histogram", variable = "category",
    levels = c("a", "b", "c"), reducer = "presence", max_contributions = k)
  p1 <- presence(1)
  p2 <- presence(2)
  # One owner, family and calibration for every k (no H/W switch at k = 1).
  expect_identical(p1$components$owner, p2$components$owner)
  expect_identical(p1$components$family, "W")
  expect_equal(p1$sensitivity$W, 10)
  expect_equal(p2$sensitivity$W, 10)
})

test_that("positive masks are views of one label-set basis", {
  .release_state()
  x <- .typed_table()
  base <- .typed_release(x, statistic = "binary_rate", variable = "category",
                         positive = c("a", "b"))
  same <- .typed_release(x, statistic = "binary_rate", variable = "category",
                         positive = c("b", "a", "a"))
  narrow <- .typed_release(x, statistic = "binary_rate",
                           variable = "category", positive = "a")
  every <- .typed_release(x, statistic = "binary_rate", variable = "category",
                          positive = c("a", "b"), reducer = "all")
  expect_identical(same, base)
  # Different masks and reducers are post-processing of the same components.
  expect_identical(narrow$components$basis, base$components$basis)
  expect_identical(every$components$basis, base$components$basis)
  expect_lte(narrow$noisy_numerator, base$noisy_numerator + 1e-9)
})

test_that("bounded_distinct is over the fixed domain U with fixed K (B05)", {
  .release_state()
  x <- .typed_table()
  base <- .typed_release(x, statistic = "bounded_distinct",
                         variable = "category", reducer = "distinct",
                         max_contributions = 3)
  restated <- .typed_release(x, statistic = "bounded_distinct",
                             variable = "category",
                             levels = c("c", "a", "b", "a"),
                             reducer = "distinct", max_contributions = 3)
  wider <- .typed_release(x, statistic = "bounded_distinct",
                          variable = "category", reducer = "distinct",
                          max_contributions = 7)
  expect_identical(restated, base)
  # Every person has at most three labels: k = 3 and k = 7 select the same
  # map, and the fixed sensitivity K makes them one draw.
  expect_identical(wider$components$signed, base$components$signed)
  expect_equal(base$sensitivity$D, 10)
  expect_identical(base$domain_size, 3L)
  expect_true(base$noisy_count >= 0 && base$noisy_count <= 3)
  expect_error(.typed_release(x, statistic = "bounded_distinct",
                              variable = "category", levels = c("a", "b"),
                              reducer = "distinct"),
               "complete registered public domain")
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
  renamed <- .typed_table(domains = NULL)
  names(renamed)[names(renamed) == "category"] <- "kind"
  renamed <- .dsomopDpSealPersonLocal(
    renamed, producer = "test/fixture", dataset_identity = .typed_dataset,
    domains = list(kind = c("a", "b", "c")))
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
  # Cells a, c, Other and Unknown keep their draws; the new person's cell b
  # and the person total are redrawn.
  for (k in c("stratum=root|cell=a", "stratum=root|cell=c",
              "stratum=root|other", "stratum=root|unknown")) {
    expect_identical(ra[[k]], rb[[k]], info = k)
  }
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
  dataset <- .release_session_dataset()
  plan <- .typed_table(data_generation = .release_loader_generation())
  expect_error(.typed_release(plan, statistic = "count"),
               "no fixed-synopsis protection")
  loader <- .typed_table(producer = "query/measurement.load/0123456789abcdef",
                         dataset_identity = dataset,
                         data_generation = .release_loader_generation())
  out <- .typed_release(loader, statistic = "count")
  expect_identical(out$protection, "fixed_synopsis")
  expect_equal(out$mechanism_epsilon, 0.02)
  expect_identical(out$delta, 0)
  expect_identical(out$guarantee_scope,
                   "fixed_vector_before_exact_support_gate")
  expect_error(.typed_release(loader, statistic = "bounded_distinct",
                              variable = "category", reducer = "distinct",
                              max_contributions = 2),
               "no fixed-synopsis protection")
  # A loader table sealed without a generation (its loader request could not
  # read it, or an older capsule) is not a fixed basis; no custodial setting
  # is needed or consulted.
  unsealed <- .typed_table(producer = "query/measurement.load/0123456789abcdef")
  expect_error(.typed_release(unsealed, statistic = "count"),
               "no fixed-synopsis protection")
  .release_state(mode = "dp", snapshot_id = "")
  dataset <- .release_session_dataset()
  loader <- .typed_table(producer = "query/measurement.load/0123456789abcdef",
                         dataset_identity = dataset,
                         data_generation = .release_loader_generation())
  expect_identical(.typed_release(loader, statistic = "count")$protection,
                   "fixed_synopsis")
})

test_that("heuristic payloads claim no delta; fixed ones claim delta 0 (B14)", {
  .release_state()
  dataset <- .release_session_dataset()
  x <- .typed_table(dataset_identity = dataset)
  realised <- .typed_release(x, statistic = "count")
  expect_false("delta" %in% names(realised))
  expect_true(is.na(realised$mechanism_epsilon))
  expect_true(is.na(realised$api_transcript_epsilon))
  loader <- .typed_table(producer = "query/measurement.load/0123456789abcdef",
                         dataset_identity = dataset,
                         data_generation = .release_loader_generation())
  fixed <- .typed_release(loader, statistic = "count")
  expect_identical(fixed$delta, 0)
  expect_true(is.finite(fixed$mechanism_epsilon))
  # One public logical-source id per node for client deduplication (B12).
  expect_match(realised$source$logical_source_id, "^dsl_[0-9a-f]{40}$")
  expect_identical(realised$source, fixed$source)
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
