test_that("bulk entropy consumption preserves scalar bits across block boundaries", {
  key <- as.raw(0:31)
  context <- list(test = "e2e-fix3", unicode = "雪")
  baseline <- new.env(parent = environment(.dsomopDpBitStream))
  sys.source(test_path("../../tools/e2e-fix3/baseline-sampler.R"), baseline)
  for (start in c("0", "18446744073709551617")) {
    old <- baseline$.dsomopDpBitStream(key, context, 3, start)
    new <- .dsomopDpBitStream(key, context, 3, start)
    take <- attr(new, "take", exact = TRUE)
    for (n in c(0L, 1L, 254L, 1L, 257L, 513L, 7L)) {
      expect_identical(take(n), vapply(seq_len(n), function(i) old(), integer(1)))
      expect_identical(new(), old())
    }
  }
})

test_that("bulk uniform sampling preserves rejection and the subsequent cursor", {
  key <- as.raw(31:0)
  baseline <- new.env(parent = environment(.dsomopDpBitStream))
  sys.source(test_path("../../tools/e2e-fix3/baseline-sampler.R"), baseline)
  old <- baseline$.dsomopDpBitStream(key, list(test = "uniform"), 1)
  new <- .dsomopDpBitStream(key, list(test = "uniform"), 1)
  for (denominator in c("1", "3", "257", "18446744073709551617", paste0("1", strrep("0", 100)))) {
    for (i in seq_len(10)) {
      expect_identical(.dsomopDpUniformInteger(denominator, new),
                       baseline$.dsomopDpUniformInteger(denominator, old))
      expect_identical(new(), old())
    }
  }
})

test_that("canonical person token fast path preserves the framed HMAC contract", {
  .release_state()
  .dsomopDpEnsureRuntime()
  policy <- .dsomopDpPolicy()
  keys <- .dsomopReleaseKeys(policy)
  for (ids in list(c(" 1 ", "2", "é", "雪", "a|1:b"),
                   bit64::as.integer64(c("9007199254740993", "9223372036854775807")))) {
    text <- .dsomopReleasePersonIdText(ids)
    domain <- enc2utf8(policy$domain)
    expected <- vapply(text, function(id) .dsomopDpHmac(keys$person,
      paste0("dsomop-release-person-v2|", nchar(domain, type = "bytes"), ":", domain,
             "|", nchar(id, type = "bytes"), ":", id)), "", USE.NAMES = FALSE)
    expect_identical(.dsomopReleaseCanonicalPersonTokens(keys, policy, text), expected)
    expect_identical(.dsomopReleasePersonTokens(keys, policy, ids), expected)
  }
  expect_error(.dsomopReleaseCanonicalPersonTokens(keys, policy, ""), "identifier")
  expect_error(.dsomopReleaseCanonicalPersonTokens(keys, policy, NA_character_), "identifier")
})

test_that("two-digit rational coins match arbitrary precision including bit consumption", {
  baseline <- new.env(parent = environment(.dsomopDpBitStream))
  sys.source(test_path("../../tools/e2e-fix3/baseline-sampler.R"), baseline)
  key <- as.raw(0:31)
  denominators <- c("1", "2", "3", "4294967295", "4294967296", "4294967297",
                    "9007199254740993", "144115188075855872", "18446744073709551615",
                    "18446744073709551616", paste0("1", strrep("0", 100)))
  for (d in denominators) {
    numerators <- list(.dsomopBigInteger(0), .dsomopBigInteger(1),
                       .dsomopBigDivide(d, 2), .dsomopBigSubtract(d, 1),
                       .dsomopBigInteger(d), .dsomopBigAdd(d, 1))
    for (n in numerators) {
      for (i in seq_len(12)) {
        old <- baseline$.dsomopDpBitStream(key, list(d = d, n = as.character(n)), i)
        new <- .dsomopDpBitStream(key, list(d = d, n = as.character(n)), i)
        expect_identical(.dsomopDpBernoulli(n, d, new), baseline$.dsomopDpBernoulli(n, d, old))
        expect_identical(attr(new, "take")(16), vapply(seq_len(16), function(j) old(), integer(1)))
      }
    }
  }
})
