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
  withr::local_options(list(dsomop.pseudonym_key = paste0(
    "00112233445566778899aabbccddeeff", "00112233445566778899aabbccddeeff"),
    dsomop.allow_legacy_global_pseudonyms = TRUE))
  withr::local_envvar(c(
    DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = ""
  ))
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

test_that("(a) two fresh handles re-derive from the SAME persisted node root", {
  # No env var, no option: bootstrap one private node root, then derive the same
  # resource-separated key on every fresh handle without a token->id table.
  state <- withr::local_tempdir()
  Sys.chmod(state, mode = "0700")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = ""
  ))
  withr::local_options(list(dsomop.pseudonym_key = NULL))
  rc <- fake_resource("datashield://siteA/persisted")

  k1 <- dsOMOP:::.resolvePersonKey(rc)   # generates + persists the root
  key_files <- list.files(file.path(state, "secrets"), full.names = TRUE)
  expect_true(any(basename(key_files) == "pseudonym_root"))
  k2 <- dsOMOP:::.resolvePersonKey(rc)   # re-derives from the persisted root
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

# --- (c) NON-NUMERIC + AUTHENTICATED TOKEN -----------------------------------

test_that("(c) every token is non-numeric: as.numeric(token) is NA", {
  key <- as.raw(1:16)
  # Include ids whose ciphertext hex could plausibly be all-digits; the "p"
  # prefix must still force NA so ds.asNumeric cannot resurrect an id.
  ids <- as.character(c(1:50, 9007199254740992, 9007199254740993))
  toks <- dsOMOP:::.hashPersonKey(ids, key)
  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$", toks)))
  expect_true(all(is.na(suppressWarnings(as.numeric(toks)))))
})

test_that("(c) modified and legacy unauthenticated tokens fail closed", {
  key <- as.raw(1:16)
  token <- dsOMOP:::.hashPersonKey("42", key)
  last <- substr(token, nchar(token), nchar(token))
  replacement <- if (last == "0") "1" else "0"
  tampered <- paste0(substr(token, 1L, nchar(token) - 1L), replacement)

  expect_error(
    dsOMOP:::.unhashPersonKey(tampered, key),
    "Invalid or unauthenticated"
  )
  expect_error(
    dsOMOP:::.unhashPersonKey("p0011223344556677", key),
    "Invalid or unauthenticated"
  )
})

# --- (d) COLLISION GUARD ------------------------------------------------------

test_that("(d) distinct ids -> distinct tokens", {
  key <- as.raw(1:16)
  ids <- as.character(1:500)
  toks <- dsOMOP:::.hashPersonKey(ids, key)
  expect_equal(length(unique(toks)), length(ids))
})

test_that("(d) .pseudonymizeIdentifiers aborts when cardinality drops", {
  key <- .testPseudonymKey("collision")
  contract <- .testPublicPseudonymization(key)
  df <- data.frame(person_id = c("a", "b", "c"), v = 1:3, stringsAsFactors = FALSE)
  expect_silent(dsOMOP:::.pseudonymizeIdentifiers(df, key, contract))
  # Force a hash collision (constant token) -> must fail closed, never silently
  # merge two real identities into one pseudonym.
  local_mocked_bindings(
    .hashPersonKey = function(ids, key) rep("pXX", length(ids)),
    .package = "dsOMOP"
  )
  expect_error(dsOMOP:::.pseudonymizeIdentifiers(df, key, contract),
               "collision")
})

# --- (e) DISTINCTNESS PRESERVED (person-count / distinct-person gates) --------

test_that("(e) token column preserves the distinct-person count", {
  key <- .testPseudonymKey("distinct")
  # 4 distinct persons across 7 rows -> the omop.table verbs and distinct-person
  # gates must still see exactly 4 distinct keys after tokenization.
  df <- data.frame(
    person_id = c(1, 1, 2, 3, 3, 3, 4),
    v = 1:7
  )
  out <- dsOMOP:::.pseudonymizeIdentifiers(
    df, key, .testPublicPseudonymization(key)
  )
  expect_equal(length(unique(out$person_id)), length(unique(df$person_id)))
  expect_equal(nrow(out), nrow(df))
  # tokens land under the ORIGINAL column name, tagged + classed for the verbs.
  expect_true("person_id" %in% names(out))
  expect_true("person_id" %in% attr(out, "dsomop_protected"))
  expect_true(inherits(out, "omop.table"))
})

test_that("(e) subject_id is also tokenized + protected; non-key id cols dropped", {
  key <- .testPseudonymKey("subject")
  df <- data.frame(
    subject_id = c(10, 10, 20),
    person_id = c(1, 1, 2),
    visit_occurrence_id = c(7, 8, 9),  # row-level identifier -> must be DROPPED
    v = 1:3
  )
  out <- dsOMOP:::.pseudonymizeIdentifiers(
    df, key, .testPublicPseudonymization(key)
  )
  expect_false("visit_occurrence_id" %in% names(out))
  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$", out$subject_id)))
  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$", out$person_id)))
  expect_setequal(attr(out, "dsomop_protected"), c("person_id", "subject_id"))
})

test_that("(e) OMOP 5.3/5.4 event and polymorphic row identifiers are dropped", {
  key <- .testPseudonymKey("identifiers")
  df <- data.frame(
    person_id = c(1, 2, 3),
    episode_id = c(101, 102, 103),
    observation_period_id = c(201, 202, 203),
    cost_event_id = c(301, 302, 303),
    fact_id_1 = c(401, 402, 403),
    production_id = c("serial-a", "serial-b", "serial-c"),
    condition_concept_id = c(201820L, 201820L, 201820L)
  )

  out <- dsOMOP:::.pseudonymizeIdentifiers(
    df, key, .testPublicPseudonymization(key)
  )

  expect_false(any(c(
    "episode_id", "observation_period_id", "cost_event_id", "fact_id_1",
    "production_id"
  ) %in% names(out)))
  expect_true("condition_concept_id" %in% names(out))
  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$", out$person_id)))
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
  envs <- c(strrep("1", 64), strrep("2", 64))
  names(envs) <- c(paste0("DSOMOP_PSEUDONYM_KEY_", ridA),
                   paste0("DSOMOP_PSEUDONYM_KEY_", ridB))
  envs <- c(envs, DSOMOP_PSEUDONYM_ROOT = "",
            DSOMOP_PSEUDONYM_KEY = "")
  withr::local_envvar(envs)

  kA <- dsOMOP:::.resolvePersonKey(rcA)
  kB <- dsOMOP:::.resolvePersonKey(rcB)
  expect_false(identical(kA, kB))
  ids <- c("1", "2", "3")
  expect_false(any(dsOMOP:::.hashPersonKey(ids, kA) ==
                   dsOMOP:::.hashPersonKey(ids, kB)))
})

# --- key resolution precedence (env > option) --------------------------------

test_that(".resolvePersonKey honors the explicit root and separates resources", {
  hex <- paste0("0011223344556677889900aabbccddee",
                "0011223344556677889900aabbccddee")
  withr::local_envvar(c(
    DSOMOP_PSEUDONYM_ROOT = hex,
    DSOMOP_PSEUDONYM_KEY = ""
  ))
  withr::local_options(list(
    dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = "a-different-string-key",
    dsomop.allow_legacy_global_pseudonyms = TRUE
  ))
  k <- dsOMOP:::.resolvePersonKey(fake_resource())
  root <- as.raw(strtoi(substring(hex, seq(1, nchar(hex), 2),
                                      seq(2, nchar(hex), 2)), 16L))
  expect_identical(
    k,
    dsOMOP:::.deriveDsomopResourceKey(root, "datashield://siteA/omop")
  )
  expect_false(identical(
    k,
    dsOMOP:::.resolvePersonKey(fake_resource("datashield://siteB/omop"))
  ))
})

test_that("legacy global key is default-deny and requires explicit opt-in", {
  hex <- paste0("0011223344556677889900aabbccddee",
                "0011223344556677889900aabbccddee")
  expected <- .coerceDsomopSecret(hex, "legacy key")
  withr::local_envvar(c(
    DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = hex
  ))
  withr::local_options(list(
    dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = NULL,
    dsomop.allow_legacy_global_pseudonyms = NULL,
    default.dsomop.allow_legacy_global_pseudonyms = NULL
  ))

  expect_error(.resolvePersonKey(fake_resource()), "disabled by default")

  withr::local_options(list(dsomop.allow_legacy_global_pseudonyms = TRUE))
  expect_identical(.resolvePersonKey(fake_resource()), expected)
  expect_identical(.resolvePersonKey(
    fake_resource("datashield://siteB/omop")
  ), expected)

  handle <- new.env(parent = emptyenv())
  handle$person_key_identity <- "datashield://siteA/omop"
  public <- .personKeyPublicContract(handle)
  expect_identical(public$provider, "legacy")
  expect_true(public$legacy_global_opt_in)
})

test_that("workspace handles contain a locator, never raw injected key bytes", {
  hex <- paste0("0011223344556677889900aabbccddee",
                "0011223344556677889900aabbccddee")
  withr::local_envvar(c(
    DSOMOP_PSEUDONYM_ROOT = hex,
    DSOMOP_PSEUDONYM_KEY = ""
  ))
  withr::local_options(list(
    dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = NULL
  ))
  handle <- new.env(parent = emptyenv())
  handle$person_key_identity <- "datashield://siteA/omop"
  expected <- .personKey(handle)

  serialized <- serialize(handle, NULL)
  expect_false(any(vapply(
    seq_len(length(serialized) - length(expected) + 1L),
    function(i) identical(serialized[i:(i + length(expected) - 1L)], expected),
    logical(1)
  )))
  expect_identical(.personKey(unserialize(serialized)), expected)
})

test_that("legacy raw-key handles fail closed unless explicitly opted in", {
  handle <- new.env(parent = emptyenv())
  handle$person_key <- as.raw(1:16)
  withr::local_options(list(
    dsomop.allow_legacy_global_pseudonyms = NULL,
    default.dsomop.allow_legacy_global_pseudonyms = NULL
  ))
  withr::local_envvar(c(
    DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS = NA_character_
  ))

  expect_error(.personKey(handle), "exactly 32 raw bytes")
  handle$person_key <- .testPseudonymKey("legacy-handle")
  expect_error(.personKey(handle), "disabled by default")
  expect_error(.personKeyPublicContract(handle), "disabled by default")

  withr::local_options(list(dsomop.allow_legacy_global_pseudonyms = TRUE))
  expect_identical(.personKey(handle), handle$person_key)
  public <- .personKeyPublicContract(handle)
  expect_identical(public$provider, "legacy_handle")
  expect_identical(public$key_id, .personKeyId(handle$person_key))
  expect_true(public$legacy_global_opt_in)
})

test_that("person-bearing frames require a matching public key contract", {
  key <- .testPseudonymKey("public-contract")
  frame <- data.frame(person_id = 1:3)
  expect_error(.pseudonymizeIdentifiers(frame, key), "explicit public")
  expect_error(
    .pseudonymizeIdentifiers(
      frame, as.raw(1:16), .testPublicPseudonymization(key)
    ),
    "exactly 32 raw bytes"
  )
  expect_error(
    .pseudonymizeIdentifiers(
      frame, key,
      .testPublicPseudonymization(.testPseudonymKey("different-key"))
    ),
    "does not identify"
  )
})

test_that("a handle fails closed when its key provider drifts", {
  root_a <- paste(rep("ab", 32L), collapse = "")
  root_b <- paste(rep("cd", 32L), collapse = "")
  withr::local_envvar(c(
    DSOMOP_PSEUDONYM_ROOT = root_a,
    DSOMOP_PSEUDONYM_KEY = ""
  ))
  withr::local_options(list(
    dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = NULL
  ))

  handle <- new.env(parent = emptyenv())
  handle$person_key_identity <- "datashield://siteA/omop"
  first <- .personKey(handle)
  expect_identical(handle$person_key_id, .personKeyId(first))

  Sys.setenv(DSOMOP_PSEUDONYM_ROOT = root_b)
  expect_error(.personKey(handle), "changed after this handle was created")
})

test_that("public key contract is non-secret and epoch drift fails closed", {
  root <- paste(rep("ab", 32L), collapse = "")
  withr::local_envvar(c(
    DSOMOP_PSEUDONYM_PROVIDER = "injected",
    DSOMOP_PSEUDONYM_EPOCH = "4",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "true",
    DSOMOP_PSEUDONYM_ROOT = root,
    DSOMOP_PSEUDONYM_KEY = NA_character_
  ))
  withr::local_options(list(
    dsomop.pseudonym_provider = NULL,
    default.dsomop.pseudonym_provider = NULL,
    dsomop.pseudonym_epoch = NULL,
    default.dsomop.pseudonym_epoch = NULL,
    dsomop.pseudonym_require_existing = NULL,
    default.dsomop.pseudonym_require_existing = NULL,
    dsomop.pseudonym_root = NULL,
    default.dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = NULL,
    default.dsomop.pseudonym_key = NULL
  ))

  handle <- new.env(parent = emptyenv())
  handle$person_key_identity <- "datashield://siteA/omop"
  public <- .personKeyPublicContract(handle)

  expect_identical(public$provider, "injected")
  expect_identical(public$epoch, 4L)
  expect_true(public$require_existing)
  expect_false(public$legacy_global_opt_in)
  expect_match(public$key_id, "^dsomop-person-key-v1:[0-9a-f]{64}$")
  expect_false(any(c("key", "identity", "root") %in% names(public)))
  expect_false(root %in% unlist(public, use.names = FALSE))

  handle$person_key_contract_version <- 2L
  expect_error(.personKey(handle), "contract changed after this handle was created")
  handle$person_key_contract_version <- 1L
  Sys.setenv(DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false")
  expect_error(.personKey(handle), "contract changed after this handle was created")
  Sys.setenv(DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "true")
  Sys.setenv(DSOMOP_PSEUDONYM_EPOCH = "5")
  expect_error(.personKey(handle), "contract changed after this handle was created")
})
