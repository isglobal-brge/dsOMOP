# Astra review of dsOMOP 2.10.3 (astra-p0-review.md), typed-channel blocking
# findings: B01 sealed contributor identity, B02 canonical eligible
# populations of fixed aliases, B04 injective framing, B05 registered numeric
# bases. Each test is the review's reproduction (or its closest valid form
# under B05) and failed on 174f521. Helpers in helper-tools-release.R.

.ti_dataset <- list(resource_identity = "typed-review",
                    target_dialect = "postgresql", cdm_schema = "main",
                    vocabulary_schema = "main", results_schema = "main")

.ti_release <- function(x, ...) suppressMessages(omopDpReleaseDS(x, list(...)))

# --- B01 ----------------------------------------------------------------------------

test_that("B01: a typed answer is invariant to earlier handle key resolution", {
  # Port of astra-scratch/typed-review/dsOMOP-probes/test-astra-handle-history.R
  .release_state()
  key <- .testPseudonymKey()
  identity <- "test://astra-review/pseudonym-key-cache"
  rid <- substr(as.character(openssl::sha256(charToRaw(identity))), 1L, 32L)
  scoped <- paste(format(key), collapse = "")
  names(scoped) <- paste0("DSOMOP_PSEUDONYM_KEY_", rid)
  withr::local_envvar(c(scoped, DSOMOP_PSEUDONYM_PROVIDER = "scoped",
                        DSOMOP_PSEUDONYM_EPOCH = "1",
                        DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false",
                        DSOMOP_PSEUDONYM_ROOT = "", DSOMOP_PSEUDONYM_KEY = ""))
  h <- new.env(parent = emptyenv())
  h$person_key_identity <- identity
  state <- .dsomopDpState()
  old <- state$dp_handle
  withr::defer(state$dp_handle <- old)
  state$dp_handle <- h
  x <- .testPseudonymize(data.frame(person_id = seq_len(2000L),
                                    category = "a"))
  x <- .dsomopDpSealPersonLocal(x, producer = "test/astra-review",
                                dataset_identity = .ti_dataset)
  expect_null(h$person_key_id)
  a <- .ti_release(x, statistic = "count")
  expect_null(h$person_key_id)
  # The production key resolver, not a mocked key or sampler.
  expect_identical(.personKey(h), key)
  expect_identical(.omopTablePseudonymization(x),
                   .canonicalPseudonymizationContract(
                     .personKeyPublicContract(h)))
  b <- .ti_release(x, statistic = "count")
  expect_identical(a, b, info = paste(
    "Resolving the handle's existing key must not redraw unchanged",
    "contributions:", a$components$signed[1L], b$components$signed[1L]))
  # Another handle, a worker restart and a restored (serialized) table keep
  # the same sealed identity.
  state$dp_handle <- new.env(parent = emptyenv())
  expect_identical(.ti_release(x, statistic = "count"), a)
  .release_restart()
  expect_identical(.ti_release(x, statistic = "count"), a)
  restored <- unserialize(serialize(x, NULL))
  expect_identical(.ti_release(restored, statistic = "count"), a)
})

test_that("B01: missing, tampered or pre-v3 identity is one stable integrity error", {
  .release_state()
  x <- .dsomopDpSealPersonLocal(
    .testPseudonymize(data.frame(person_id = 1:30, v = 1)),
    producer = "test/identity", dataset_identity = .ti_dataset)
  message_of <- function(y) tryCatch({
    .ti_release(y, statistic = "count")
    NA_character_
  }, error = conditionMessage)
  missing <- x
  attr(missing, "dsomop_dp_release_persons") <- NULL
  tampered <- x
  map <- attr(tampered, "dsomop_dp_release_persons")
  map[[1L]] <- map[[2L]]
  attr(tampered, "dsomop_dp_release_persons") <- map
  old <- x
  capsule <- attr(old, "dsomop_dp_provenance")
  capsule$protocol <- "dsomop-dp-person-local-provenance-v2"
  attr(old, "dsomop_dp_provenance") <- capsule
  for (y in list(missing, tampered)) {
    # The frame digest covers the map, so either the MAC or the identity
    # check refuses; neither falls back to another person namespace.
    expect_match(message_of(y), "MAC does not match|person identity")
  }
  expect_match(message_of(old), "protected-release person identity")
})

test_that("B01: a CDM-namespace table shares the automatic person total", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  ids <- 1:40
  cohort <- .release_cohort(h, "ti_twin", ids)
  auto <- .release_run(h, "dsomop:achilles.103", scope = cohort)
  comps <- unlist(lapply(attr(auto, "dsomop_release")$owners, `[[`,
                         "components"), recursive = FALSE)
  root <- Filter(function(c) identical(c$stratum, "root"), comps)
  expect_length(root, 1L)
  # The same people sealed with the key that produced their tokens: the
  # typed count is the same component as the automatic root total.
  key <- .testPseudonymKey()
  x <- .dsomopDpSealPersonLocal(
    .testPseudonymize(data.frame(person_id = ids, v = 1)),
    producer = "test/twin", dataset_identity = .ti_dataset, person_key = key)
  expect_identical(attr(x, "dsomop_dp_provenance")$release_person_namespace,
                   "cdm_person_id_v1")
  typed <- .ti_release(x, statistic = "count")
  expect_identical(typed$components$signed[[1L]], root[[1L]]$n)
  # Double ids pseudonymized as "1e+05" map to the integer text that the
  # automatic routes use; other identifier text is kept as it is.
  policy <- .dsomopDpPolicy()
  z <- .testPseudonymize(data.frame(person_id = c(1e5, 2e5, 3e5), v = 1))
  expect_identical(
    sort(unname(.dsomopDpReleasePersonMap(z, "person_id", key, policy)$map)),
    sort(.dsomopReleasePersonTokens(.dsomopReleaseKeys(policy), policy,
                                    c(100000, 200000, 300000))))
  s <- .testPseudonymize(data.frame(person_id = c("007", "A1"), v = 1))
  expect_identical(
    sort(unname(.dsomopDpReleasePersonMap(s, "person_id", key, policy)$map)),
    sort(.dsomopReleasePersonTokens(.dsomopReleaseKeys(policy), policy,
                                    c("007", "A1"))))
  # A keyless fixture of the same people is a different (pseudonym)
  # namespace, fixed at sealing.
  y <- .dsomopDpSealPersonLocal(
    .testPseudonymize(data.frame(person_id = ids, v = 1)),
    producer = "test/twin", dataset_identity = .ti_dataset)
  expect_identical(attr(y, "dsomop_dp_provenance")$release_person_namespace,
                   "pseudonym_v1")
  expect_error(.dsomopDpSealPersonLocal(
    .testPseudonymize(data.frame(person_id = ids, v = 1)),
    producer = "test/twin", dataset_identity = .ti_dataset,
    person_key = as.raw(1:32)), "does not match")
})

# --- B02 ----------------------------------------------------------------------------

.ti_loader_table <- function(n = 2000L, dataset_identity = .ti_dataset) {
  d <- data.frame(person_id = seq_len(n), age = rep(50, n), day = seq_len(n))
  x <- .testPseudonymize(d)
  q <- .ql_load_queries()[["measurement.load"]]
  .dsomopDpSealAnalysisAssign(x, list(
    meta = list(adapter = "query", query_id = "measurement.load"),
    compute = list(sql = q$sql)), dataset_identity = dataset_identity)
}

test_that("B02: fixed loader aliases share their person total", {
  # Port of test-astra-typed-review.R "fixed loader aliases must share their
  # person total". Adapted per the review text: count and record-count(k=1)
  # are one eligible population and must be one N; the mean's denominator is
  # the declared nonmissing population of its variable, a different public
  # eligibility, so it shares N only on the realised route (equal maps).
  .release_state()
  x <- .ti_loader_table(dataset_identity = .release_session_dataset())
  a <- .ti_release(x, statistic = "count")
  b <- .ti_release(x, statistic = "bounded_record_count", reducer = "records",
                   max_contributions = 1)
  c <- .ti_release(x, statistic = "bounded_mean", variable = "age",
                   lower = 0, upper = 110)
  expect_identical(a$protection, "fixed_synopsis")
  expect_identical(b$protection, "fixed_synopsis")
  ns <- c(count = a$components$signed[[1L]],
          records = b$components$signed[[1L]],
          mean = c$components$signed[[1L]])
  message("[typed-identity] fixed loader alias N=", paste(ns, collapse = ","))
  expect_identical(ns[["records"]], ns[["count"]])
  # Every record-count cap is a view of the same fixed basis.
  for (k in 2:4) {
    expect_identical(.ti_release(x, statistic = "bounded_record_count",
                                 reducer = "records",
                                 max_contributions = k)$components$signed,
                     b$components$signed)
  }
  # Realised route: the same people give the same N for all three.
  y <- .dsomopDpSealPersonLocal(.testPseudonymize(data.frame(
    person_id = seq_len(2000L), age = rep(50, 2000L))),
    producer = "test/astra-review", dataset_identity = .ti_dataset)
  r <- vapply(list(
    .ti_release(y, statistic = "count"),
    .ti_release(y, statistic = "bounded_record_count", reducer = "records",
                max_contributions = 1),
    .ti_release(y, statistic = "bounded_mean", variable = "age", lower = 0,
                upper = 110)), function(p) p$components$signed[[1L]],
    numeric(1L))
  expect_identical(r, rep(r[[1L]], 3L))
})

# --- B04 ----------------------------------------------------------------------------

test_that("B04: distinct-label maps are injective under delimiters and controls", {
  # Closest valid form of the review's U+001F probe under B05: the labels of
  # both maps belong to one registered domain, and per person {a, b} must not
  # share noise with {"a<U+001F>b"} (and likewise for newline, colon and the
  # coordinate separator).
  .release_state()
  pairs <- list(
    list(c("a", "b"), "a\u001fb"),
    list(c("x", "y"), "x\ny"),
    list(c("c", "d"), "c:d"),
    list(c("e", "f"), "e|cell=f"),
    list(c("g", "h"), "g\th"))
  domain <- unique(unlist(pairs))
  # The custodian registers the complete domain of the label variable.
  withr::local_options(list(dsomop.release.typed_domains = list(
    label = domain)))
  table_of <- function(labels) {
    d <- data.frame(person_id = rep(1:60, each = length(labels)),
                    label = rep(labels, 60), stringsAsFactors = FALSE)
    .dsomopDpSealPersonLocal(.testPseudonymize(d), producer = "test/framing",
                             dataset_identity = .ti_dataset)
  }
  noise <- function(labels) {
    p <- .ti_release(table_of(labels), statistic = "bounded_distinct",
                     variable = "label", levels = domain,
                     reducer = "distinct", max_contributions = 3)
    p$components$signed[[1L]] - length(labels)
  }
  for (pair in pairs) {
    two <- noise(pair[[1L]])
    one <- noise(pair[[2L]])
    expect_false(identical(two, one), info = paste(
      "ambiguous map:", paste(pair[[1L]], collapse = "+"), "vs",
      encodeString(pair[[2L]])))
  }
  # Populations and identities are framed: no join of their parts collides.
  expect_false(identical(.dsomopFrame("a|b", "c"), .dsomopFrame("a", "b|c")))
  expect_false(identical(.dsomopFrame(c("a", "b")), .dsomopFrame("a\u001fb")))
  expect_false(identical(.dsomopFrame(list(x = "1")), .dsomopFrame(list(x = 1))))
})

# --- B05 ----------------------------------------------------------------------------

test_that("B05: coarse bins are public sums of one typed numeric atom basis", {
  # Port of test-astra-public-basis.R (first test); its exploratory sweep of
  # empty interior cuts is reduced from 400 to 40 releases.
  .release_state()
  x <- .testPseudonymize(data.frame(person_id = seq_len(2000L),
    value = c(rep(0, 500L), rep(100, 1500L))))
  x <- .dsomopDpSealPersonLocal(x, producer = "test/astra-empty-cut",
                                dataset_identity = .ti_dataset)
  release <- function(breaks) .ti_release(x, statistic = "numeric_histogram",
                                          variable = "value", breaks = breaks,
                                          reducer = "mean")
  cuts <- as.numeric(outer(seq(1, 9.9, by = .1), 10^(-6:-2)))[
    seq(1L, 400L, by = 10L)]
  contrasts <- vapply(cuts, function(cut) {
    a <- release(c(-1, cut, 200))
    a$components$signed[2L] - a$components$signed[3L]
  }, numeric(1L))
  message("[typed-identity] 40 empty interior cuts: unique signed contrasts=",
          length(unique(contrasts)))
  fine <- release(c(-1, .01, .05, 200))
  coarse <- release(c(-1, .05, 200))
  expect_equal(sum(fine$components$signed[2:3]), coarse$components$signed[2L],
               info = paste("Coarse bins must be public sums of the same",
                            "released atom basis"))
  expect_identical(fine$components$basis, coarse$components$basis)
})
