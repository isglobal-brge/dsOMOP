test_that(".sqlIdList formats ids exactly without int32 overflow or NA", {
  skip_if_not_installed("bit64")
  big <- bit64::as.integer64("8805478484003283429")
  expect_equal(dsOMOP:::.sqlIdList(c(big, bit64::as.integer64("123"))),
               "8805478484003283429, 123")
  expect_equal(dsOMOP:::.sqlIdList(c(5e9, 123)), "5000000000, 123")  # no NA, no sci
  expect_equal(dsOMOP:::.sqlIdList(c("42", "7")), "42, 7")
  expect_equal(dsOMOP:::.sqlIdList(c(1L, NA, 3L)), "1, 3")
  expect_equal(dsOMOP:::.sqlIdList(integer(0)), "")
})

test_that(".coerce_integer64 preserves precision (never a lossy double)", {
  skip_if_not_installed("bit64")
  small <- data.frame(a = 1:3)
  small$pid <- bit64::as.integer64(c("10", "20", "30"))
  cs <- dsOMOP:::.coerce_integer64(small)
  expect_true(is.integer(cs$pid))
  expect_equal(cs$pid, c(10L, 20L, 30L))

  huge <- data.frame(a = 1:2)
  huge$pid <- bit64::as.integer64(c("8805478484003283429", "9223372036854775000"))
  ch <- dsOMOP:::.coerce_integer64(huge)
  expect_true(is.character(ch$pid))
  expect_equal(ch$pid[1], "8805478484003283429")

  # 2^53 and 2^53+1 collapse to one double under as.numeric() but must stay distinct
  pair <- data.frame(a = 1:2)
  pair$pid <- bit64::as.integer64(c("9007199254740992", "9007199254740993"))
  expect_equal(suppressWarnings(as.numeric(pair$pid[1])),
               suppressWarnings(as.numeric(pair$pid[2])))          # the old hazard
  expect_equal(length(unique(dsOMOP:::.coerce_integer64(pair)$pid)), 2L)
})

test_that(".hashPersonKey yields deterministic, non-numeric, injective tokens", {
  key <- as.raw(1:16)
  ids <- c("9007199254740992", "9007199254740993")
  toks <- dsOMOP:::.hashPersonKey(ids, key)
  expect_true(all(grepl("^p[0-9a-f]+$", toks)))   # "p" prefix => non-numeric token
  # ds.asNumeric / as.numeric cannot recover or infer the id (forced to NA).
  expect_true(all(is.na(suppressWarnings(as.numeric(toks)))))
  expect_equal(length(unique(toks)), 2L)          # distinct ids -> distinct tokens
  # Deterministic: same id + same key -> same token (stable across reconnect).
  expect_identical(toks, dsOMOP:::.hashPersonKey(ids, key))
})

test_that(".unhashPersonKey reverses .hashPersonKey server-side (NA preserved)", {
  key <- as.raw(1:16)
  ids <- c("10", "8805478484003283429", NA, "30")
  toks <- dsOMOP:::.hashPersonKey(ids, key)
  back <- dsOMOP:::.unhashPersonKey(toks, key)
  expect_equal(back, as.character(ids))
  # A different key cannot recover the id (no shared secret -> no inversion):
  # decryption either errors (bad padding) or yields garbage, never "10".
  wrong <- tryCatch(dsOMOP:::.unhashPersonKey(toks[1], as.raw(rev(1:16))),
                    error = function(e) NA_character_)
  expect_false(identical(wrong, "10"))
})

test_that(".pseudonymizeIdentifiers aborts on a cardinality collision", {
  key <- as.raw(1:16)
  df <- data.frame(person_id = c("a", "b", "c"), v = 1:3, stringsAsFactors = FALSE)
  expect_silent(dsOMOP:::.pseudonymizeIdentifiers(df, key))
  # Force a collision by stubbing the hash to a constant -> must fail closed.
  local_mocked_bindings(.hashPersonKey = function(ids, key) rep("X", length(ids)),
                        .package = "dsOMOP")
  expect_error(dsOMOP:::.pseudonymizeIdentifiers(df, key), "collision")
})
