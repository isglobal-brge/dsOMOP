# WS-0 (design-2.10.3 section 6.10): the matching-order encoding test vectors.
# This reference implementation of the encoding is the contract WS-6 must
# reproduce byte for byte.

.ws0_match_field <- function(tag, text = NULL) {
  bytes <- if (is.null(text)) raw(0) else charToRaw(enc2utf8(as.character(text)))
  n <- length(bytes)
  len <- as.raw(c(bitwAnd(bitwShiftR(n, 24L), 255L), bitwAnd(bitwShiftR(n, 16L), 255L),
                  bitwAnd(bitwShiftR(n, 8L), 255L), bitwAnd(n, 255L)))
  c(as.raw(tag), len, bytes)
}
.ws0_match_hash <- function(seed, namespace, subject_id, start, end, source, key, control) {
  E <- c(.ws0_match_field(0x02, "dsomop-match-v1"),
         .ws0_match_field(0x01, format(seed, scientific = FALSE)),
         .ws0_match_field(0x02, namespace),
         .ws0_match_field(0x01, format(subject_id, scientific = FALSE)),
         .ws0_match_field(0x03, start),
         if (is.null(end)) .ws0_match_field(0x00) else .ws0_match_field(0x03, end),
         .ws0_match_field(0x02, source),
         if (is.null(key)) .ws0_match_field(0x00) else .ws0_match_field(0x02, key),
         .ws0_match_field(0x01, format(control, scientific = FALSE)))
  paste(as.character(openssl::sha256(E)))
}

test_that("the published matching hash vectors reproduce", {
  v <- jsonlite::fromJSON(testthat::test_path("fixtures", "match-vectors.json"), simplifyVector = FALSE)
  expect_length(v$vectors, 3L)
  for (x in v$vectors) {
    got <- .ws0_match_hash(x$seed, x$namespace, x$subject_id, x$cohort_start_date,
                           x$cohort_end_date, x$index_source, x$index_key, x$control)
    expect_identical(got, x$sha256, info = paste("seed", x$seed, x$index_source))
  }
  # seeds 1 and 2 order differently; the encoding is length-prefixed so a
  # namespace/key boundary cannot shift ("ca" + "ses" != "cas" + "es")
  expect_false(identical(v$vectors[[1]]$sha256, v$vectors[[2]]$sha256))
  expect_false(identical(.ws0_match_hash(1, "ca", 17, "2019-03-04", NULL, "ses", NULL, 1),
                         .ws0_match_hash(1, "cas", 17, "2019-03-04", NULL, "es", NULL, 1)))
})
