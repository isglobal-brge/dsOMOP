# WS-0 (design-2.10.3 section 4.4): the semantic digest and its vectors.

test_that("the three digest vectors reproduce", {
  d <- system.file("schema", package = "dsOMOP")
  if (!nzchar(d)) d <- "inst/schema"
  vec <- jsonlite::fromJSON(file.path(d, "digest-vectors.json"), simplifyVector = FALSE)
  expect_length(vec$vectors, 3L)
  got <- lapply(vec$vectors, function(v) dsOMOP:::.planSemanticDigest(v$instance))
  for (i in seq_along(got)) {
    expect_identical(got[[i]]$payload, vec$vectors[[i]]$payload, info = vec$vectors[[i]]$label)
    expect_identical(got[[i]]$sha256, vec$vectors[[i]]$payload_sha256, info = vec$vectors[[i]]$label)
  }
  expect_false(identical(got[[1]]$sha256, got[[2]]$sha256))   # semantic version changes the digest
  expect_identical(got[[1]]$sha256, got[[3]]$sha256)          # result symbol does not
})

test_that("the payload keeps the version tuple and drops advisory fields only", {
  ir <- list(version = 2L,
             meta = list(language_version = "2.0", semantic_version = "2.0.0",
                         generated_by = "x", client_version = "2.10.3", recipe_hash = strrep("a", 64)),
             outputs = list(o = list(rows = "p", format = "wide", result_symbol = "D_o",
                                     label = "L", options = structure(list(), names = character(0)))))
  payload <- dsOMOP:::.planDigestPayload(ir)
  expect_identical(names(payload$meta), c("language_version", "semantic_version"))
  expect_false("result_symbol" %in% names(payload$outputs$o))
  expect_false("label" %in% names(payload$outputs$o))
  expect_identical(payload$version, 2L)
  canon <- dsOMOP:::.jcsCanonical(payload)
  expect_identical(canon, '{"meta":{"language_version":"2.0","semantic_version":"2.0.0"},"outputs":{"o":{"format":"wide","options":{},"rows":"p"}},"version":2}')
})

test_that("canonical JSON follows RFC 8785 for numbers, strings and key order", {
  n <- dsOMOP:::.jcsNumber
  expect_identical(n(0), "0"); expect_identical(n(-0), "0")
  expect_identical(n(365L), "365"); expect_identical(n(365), "365")
  expect_identical(n(0.0915), "0.0915"); expect_identical(n(2.15), "2.15")
  expect_identical(n(1e21), "1e+21"); expect_identical(n(1e-7), "0.0000001")
  expect_identical(n(2.5e-8), "2.5e-8"); expect_identical(n(1/3), "0.3333333333333333")
  expect_identical(n(100000000000000000000), "100000000000000000000")
  expect_identical(n(5e-324), "5e-324")
  expect_identical(n(1.7976931348623157e308), "1.7976931348623157e+308")
  expect_error(n(Inf), "Non-finite")
  s <- dsOMOP:::.jcsString
  expect_identical(s("a\"b\\c\n\té\u0001"), "\"a\\\"b\\\\c\\n\\té\\u0001\"")
  expect_identical(dsOMOP:::.jcsCanonical(list(b = 1L, a = list(2.15, NULL, TRUE, "x"),
                                              c = structure(list(), names = character(0)), d = list())),
                   '{"a":[2.15,null,true,"x"],"b":1,"c":{},"d":[]}')
  # UTF-16 code-unit order: a supplementary character sorts before U+FF5E
  ord <- dsOMOP:::.jcsKeyOrder(c("～", "\U0001f600", "a"))
  expect_identical(ord, c(3L, 2L, 1L))
})
