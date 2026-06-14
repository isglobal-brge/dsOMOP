# Phase D: persistent, per-resource, REVERSIBLE person-key pseudonymization.
#
# Verifies the token transform in R/interface.R (.hashPersonKey /
# .unhashPersonKey / .deriveAesParams / .pseudonymizeIdentifiers) and the
# persistent key resolution in R/blueprint.R (.resolvePersonKey), which together
# guarantee: tokens are STABLE for a resource across reconnect / workspace
# save-load (no token->id map stored), REVERSIBLE server-side only, NON-NUMERIC
# (so ds.asNumeric cannot recover an id), distinct-preserving (cohort person
# counts stay correct), and per-resource (a person is not linkable across sites).

# A minimal fake resource_client exposing only what .resolvePersonKey touches:
# getResource()$url (the resource identity) and getParsed()$server (fallback).
fake_resource <- function(url = "datashield://siteA/omop") {
  list(
    getResource = function() list(url = url, name = NULL),
    getParsed   = function() list(server = url)
  )
}

# --- (a) DETERMINISTIC --------------------------------------------------------

test_that("(a) same ids + same key -> identical tokens across two calls", {
  key <- as.raw(1:16)
  ids <- c("1", "9007199254740992", "9007199254740993", "42")
  expect_identical(
    dsOMOP:::.hashPersonKey(ids, key),
    dsOMOP:::.hashPersonKey(ids, key)
  )
})

test_that("(a) two fresh handles re-resolving the SAME option key -> same tokens", {
  # Simulate reconnect/workspace reload: a brand-new handle re-resolves the key
  # from the persistent source (here the R option) and must reproduce identical
  # tokens for the same ids, with NO token->id table stored anywhere.
  withr::local_options(list(dsomop.pseudonym_key = "00112233445566778899aabbccddeeff"))
  withr::local_envvar(c(DSOMOP_PSEUDONYM_KEY = ""))
  rc <- fake_resource()

  k1 <- dsOMOP:::.resolvePersonKey(rc)   # "handle 1"
  k2 <- dsOMOP:::.resolvePersonKey(rc)   # "handle 2" (fresh resolve)
  expect_identical(k1, k2)

  ids <- c("100", "8805478484003283429", "7")
  expect_identical(
    dsOMOP:::.hashPersonKey(ids, k1),
    dsOMOP:::.hashPersonKey(ids, k2)
  )
})

test_that("(a) two fresh handles re-resolving the SAME persisted file key -> same tokens", {
  # No env var, no option: the key must come from (and persist to) the 0600
  # ~/.dsomop/keys/<rid>.key file. Point HOME at a temp dir so the first resolve
  # GENERATES + persists, and the second resolve READS BACK the same bytes.
  home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = home, DSOMOP_PSEUDONYM_KEY = ""))
  withr::local_options(list(dsomop.pseudonym_key = NULL))
  rc <- fake_resource("datashield://siteA/persisted")

  k1 <- dsOMOP:::.resolvePersonKey(rc)   # generates + persists
  key_files <- list.files(file.path(home, ".dsomop", "keys"), full.names = TRUE)
  expect_length(key_files, 1L)           # exactly one per-resource key file
  k2 <- dsOMOP:::.resolvePersonKey(rc)   # reads the persisted file back
  expect_identical(k1, k2)               # stable across "reconnect"

  ids <- c("5", "9007199254740993")
  expect_identical(
    dsOMOP:::.hashPersonKey(ids, k1),
    dsOMOP:::.hashPersonKey(ids, k2)
  )
})

# --- (b) REVERSIBLE -----------------------------------------------------------

test_that("(b) .unhashPersonKey(.hashPersonKey(ids)) == as.character(ids)", {
  key <- as.raw(1:16)
  ids <- c("10", "8805478484003283429", "0", "9223372036854775000", "42")
  toks <- dsOMOP:::.hashPersonKey(ids, key)
  expect_equal(dsOMOP:::.unhashPersonKey(toks, key), as.character(ids))
})

test_that("(b) round-trip preserves NA element-wise", {
  key <- as.raw(11:26)
  ids <- c("1", NA, "8805478484003283429", NA, "2")
  toks <- dsOMOP:::.hashPersonKey(ids, key)
  expect_true(is.na(toks[2]) && is.na(toks[4]))
  expect_equal(dsOMOP:::.unhashPersonKey(toks, key), as.character(ids))
})

# --- (c) NON-NUMERIC (the "p" prefix) -----------------------------------------

test_that("(c) every token is non-numeric: as.numeric(token) is NA", {
  key <- as.raw(1:16)
  # Include ids whose ciphertext hex could plausibly be all-digits; the "p"
  # prefix must still force NA so ds.asNumeric cannot resurrect an id.
  ids <- as.character(c(1:50, 9007199254740992, 9007199254740993))
  toks <- dsOMOP:::.hashPersonKey(ids, key)
  expect_true(all(grepl("^p[0-9a-f]+$", toks)))
  expect_true(all(is.na(suppressWarnings(as.numeric(toks)))))
})

# --- (d) COLLISION GUARD ------------------------------------------------------

test_that("(d) distinct ids -> distinct tokens", {
  key <- as.raw(1:16)
  ids <- as.character(1:500)
  toks <- dsOMOP:::.hashPersonKey(ids, key)
  expect_equal(length(unique(toks)), length(ids))
})

test_that("(d) .pseudonymizeIdentifiers aborts when cardinality drops", {
  key <- as.raw(1:16)
  df <- data.frame(person_id = c("a", "b", "c"), v = 1:3, stringsAsFactors = FALSE)
  expect_silent(dsOMOP:::.pseudonymizeIdentifiers(df, key))
  # Force a hash collision (constant token) -> must fail closed, never silently
  # merge two real identities into one pseudonym.
  local_mocked_bindings(
    .hashPersonKey = function(ids, key) rep("pXX", length(ids)),
    .package = "dsOMOP"
  )
  expect_error(dsOMOP:::.pseudonymizeIdentifiers(df, key), "collision")
})

# --- (e) DISTINCTNESS PRESERVED (person-count / distinct-person gates) --------

test_that("(e) token column preserves the distinct-person count", {
  key <- as.raw(1:16)
  # 4 distinct persons across 7 rows -> the omop.table verbs and distinct-person
  # gates must still see exactly 4 distinct keys after tokenization.
  df <- data.frame(
    person_id = c(1, 1, 2, 3, 3, 3, 4),
    v = 1:7
  )
  out <- dsOMOP:::.pseudonymizeIdentifiers(df, key)
  expect_equal(length(unique(out$person_id)), length(unique(df$person_id)))
  expect_equal(nrow(out), nrow(df))
  # tokens land under the ORIGINAL column name, tagged + classed for the verbs.
  expect_true("person_id" %in% names(out))
  expect_true("person_id" %in% attr(out, "dsomop_protected"))
  expect_true(inherits(out, "omop.table"))
})

test_that("(e) subject_id is also tokenized + protected; non-key id cols dropped", {
  key <- as.raw(1:16)
  df <- data.frame(
    subject_id = c(10, 10, 20),
    person_id = c(1, 1, 2),
    visit_occurrence_id = c(7, 8, 9),  # row-level identifier -> must be DROPPED
    v = 1:3
  )
  out <- dsOMOP:::.pseudonymizeIdentifiers(df, key)
  expect_false("visit_occurrence_id" %in% names(out))
  expect_true(all(grepl("^p[0-9a-f]+$", out$subject_id)))
  expect_true(all(grepl("^p[0-9a-f]+$", out$person_id)))
  expect_setequal(attr(out, "dsomop_protected"), c("person_id", "subject_id"))
})

# --- (f) PER-RESOURCE: different keys -> different tokens ----------------------

test_that("(f) a different key yields different tokens (key is actually used)", {
  ids <- c("1", "2", "3", "8805478484003283429")
  t1 <- dsOMOP:::.hashPersonKey(ids, as.raw(1:16))
  t2 <- dsOMOP:::.hashPersonKey(ids, as.raw(rev(1:16)))
  expect_false(any(t1 == t2))
  # A token minted under key1 must NOT decrypt back to the id under key2.
  wrong <- tryCatch(dsOMOP:::.unhashPersonKey(t1[1], as.raw(rev(1:16))),
                    error = function(e) NA_character_)
  expect_false(identical(wrong, "1"))
})

test_that("(f) two different resources resolve different keys -> not linkable", {
  # Same secret source for both would be linkable; the per-resource scoping must
  # make them diverge. Use distinct per-resource env vars keyed by <rid>.
  withr::local_options(list(dsomop.pseudonym_key = NULL))
  rcA <- fake_resource("datashield://siteA/omop")
  rcB <- fake_resource("datashield://siteB/omop")
  ridA <- substr(as.character(openssl::sha256(charToRaw("datashield://siteA/omop"))), 1L, 32L)
  ridB <- substr(as.character(openssl::sha256(charToRaw("datashield://siteB/omop"))), 1L, 32L)
  envs <- c("11111111111111111111111111111111", "22222222222222222222222222222222")
  names(envs) <- c(paste0("DSOMOP_PSEUDONYM_KEY_", ridA),
                   paste0("DSOMOP_PSEUDONYM_KEY_", ridB))
  envs <- c(envs, DSOMOP_PSEUDONYM_KEY = "")
  withr::local_envvar(envs)

  kA <- dsOMOP:::.resolvePersonKey(rcA)
  kB <- dsOMOP:::.resolvePersonKey(rcB)
  expect_false(identical(kA, kB))
  ids <- c("1", "2", "3")
  expect_false(any(dsOMOP:::.hashPersonKey(ids, kA) ==
                   dsOMOP:::.hashPersonKey(ids, kB)))
})

# --- key resolution precedence (env > option) --------------------------------

test_that(".resolvePersonKey honors env var over option and hex-decodes it", {
  hex <- "0011223344556677889900aabbccddee"
  withr::local_envvar(c(DSOMOP_PSEUDONYM_KEY = hex))
  withr::local_options(list(dsomop.pseudonym_key = "a-different-string-key"))
  k <- dsOMOP:::.resolvePersonKey(fake_resource())
  # even-length all-hex -> decoded to the 16 raw bytes it represents.
  expect_identical(k, as.raw(strtoi(substring(hex, seq(1, nchar(hex), 2),
                                              seq(2, nchar(hex), 2)), 16L)))
})
