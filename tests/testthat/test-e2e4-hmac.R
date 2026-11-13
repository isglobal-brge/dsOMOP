test_that("request-local person hashing matches the portable OpenSSL oracle", {
  for (root in list(as.raw(0:31), as.raw(255:224))) {
    keys <- list(person = root)
    for (domain in c("", "ordinary", "a|b:é\n漢字")) {
      policy <- list(domain = domain)
      ids <- c("1", "-2", "9223372036854775807", "a|b:1", "é", "漢字", "1")
      framed <- paste0("dsomop-release-person-v2|", nchar(enc2utf8(domain), "bytes"),
                       ":", enc2utf8(domain), "|", nchar(enc2utf8(ids), "bytes"), ":", enc2utf8(ids))
      expect_identical(.dsomopReleaseCanonicalPersonTokens(keys, policy, ids),
                       as.character(openssl::sha256(framed, key = root)))
      # Repeated calls, changed key/domain, empty vectors and >1000 inputs
      # exercise context reset and the interrupt-check boundary.
      many <- rep(framed, length.out = 2001L)
      native <- .Call(C_dsomop_person_hmac, many, root)
      if (!is.null(native)) expect_identical(native, as.character(openssl::sha256(many, key = root)))
      empty <- .Call(C_dsomop_person_hmac, character(), root)
      if (!is.null(empty)) expect_identical(empty, character())
    }
  }
  expect_error(.Call(C_dsomop_person_hmac, "a", raw(31)), "Invalid protected")
  expect_error(.Call(C_dsomop_person_hmac, 1, raw(32)), "Invalid protected")
  if (!is.null(.Call(C_dsomop_person_hmac, "a", raw(32)))) {
    expect_error(.Call(C_dsomop_person_hmac, c("a", NA_character_), raw(32)), "complete text")
    # A failed call frees its context; a following independent request works.
    expect_identical(.Call(C_dsomop_person_hmac, "a", raw(32)),
                     as.character(openssl::sha256("a", key = raw(32))))
  }
})

test_that("streamed contribution chunks preserve framing and chunk boundaries", {
  root <- as.raw(0:31)
  for (n in c(0L, 1L, 9999L, 10000L, 10001L, 20001L)) {
    records <- rep(c("64:abc:1", "", "é\n漢字|:2"), length.out = n)
    native <- .Call(C_dsomop_fingerprint_chunks, records, root)
    expected <- if (n == 0L) character() else vapply(seq.int(1L, n, by = 10000L), function(start) {
      chunk <- records[seq.int(start, min(n, start + 9999L))]
      as.character(openssl::sha256(paste(c("dsomop-contribution-chunk-v2", length(chunk), chunk),
                                        collapse = "\n"), key = root))
    }, "")
    if (!is.null(native)) expect_identical(native, expected)
  }
})
