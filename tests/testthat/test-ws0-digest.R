# WS-0 (design-2.10.3 section 4.4; amendments A-1..A-7; review B1): the
# semantic digest, its vectors and the RFC 8785 number/string/key rules.

.ws0_digest_dir <- function() {
  d <- system.file("schema", package = "dsOMOP")
  if (!nzchar(d)) d <- "inst/schema"
  d
}
.ws0_dbl <- function(hex) {
  readBin(as.raw(strtoi(substring(hex, seq(1, 15, 2), seq(2, 16, 2)), 16L)), "double", size = 8, endian = "big")
}

test_that("every digest vector reproduces and the documented relations hold", {
  vec <- jsonlite::fromJSON(file.path(.ws0_digest_dir(), "digest-vectors.json"), simplifyVector = FALSE)
  expect_gte(length(vec$vectors), 11L)
  got <- list()
  for (v in vec$vectors) {
    d <- dsOMOP:::.planSemanticDigest(v$instance)
    expect_identical(d$payload, v$payload, info = v$label)
    expect_identical(d$sha256, v$payload_sha256, info = v$label)
    got[[v$label]] <- d$sha256
  }
  expect_false(identical(got$base, got$semantic_version_change))          # the version tuple is bound
  expect_identical(got$base, got$result_symbol_change)                     # result symbols are advisory
  expect_identical(got$base, got$population_label_text_change)             # population labels are advisory
  for (nm in c("description", "label", "result_symbol")) {                 # definition NAMES are clinical content
    expect_false(identical(got[[paste0("event_named_", nm, "_mi")]], got[[paste0("event_named_", nm, "_t2dm")]]), info = nm)
  }
  num <- vec$vectors[[which(vapply(vec$vectors, function(v) v$label == "numeric_canonicalisation", logical(1)))]]
  for (needle in c('"factor":1e-7', '"offset":1000000000000000100', '"factor":1234567890123456800',
                   '"offset":-4.051847215643804e+161', '"factor":0.000001', '"offset":1e+21')) {
    expect_true(grepl(needle, num$payload, fixed = TRUE), info = needle)
  }
})

test_that("advisory properties are stripped at their owning paths only", {
  ir <- list(version = 2L,
             meta = list(language_version = "2.0", semantic_version = "2.0.0",
                         generated_by = "x", client_version = "2.10.3", recipe_hash = strrep("a", 64)),
             events = list(description = list(table = "death"), label = list(table = "death")),
             populations = list(p = list(kind = "criteria", label = "L", where = NULL)),
             outputs = list(o = list(rows = "p", format = "wide", result_symbol = "D_o",
                                     label = "an output label is not advisory", options = structure(list(), names = character(0)))))
  payload <- dsOMOP:::.planDigestPayload(ir)
  expect_identical(names(payload$meta), c("language_version", "semantic_version"))
  expect_identical(names(payload$events), c("description", "label"))           # names kept
  expect_false("label" %in% names(payload$populations$p))
  expect_false("result_symbol" %in% names(payload$outputs$o))
  expect_true("label" %in% names(payload$outputs$o))                         # not an owning path for outputs
  expect_identical(payload$version, 2L)
  canon <- dsOMOP:::.jcsCanonical(payload)
  expect_identical(canon, '{"events":{"description":{"table":"death"},"label":{"table":"death"}},"meta":{"language_version":"2.0","semantic_version":"2.0.0"},"outputs":{"o":{"format":"wide","label":"an output label is not advisory","options":{},"rows":"p"}},"populations":{"p":{"kind":"criteria","where":null}},"version":2}')
})

test_that("numbers follow RFC 8785 Appendix B and the review's counterexamples", {
  n <- dsOMOP:::.jcsNumber
  corpus <- list(
    c("0000000000000000", "0"), c("8000000000000000", "0"), c("0000000000000001", "5e-324"),
    c("8000000000000001", "-5e-324"), c("7fefffffffffffff", "1.7976931348623157e+308"),
    c("ffefffffffffffff", "-1.7976931348623157e+308"), c("4340000000000000", "9007199254740992"),
    c("c340000000000000", "-9007199254740992"), c("4430000000000000", "295147905179352830000"),
    c("44b52d02c7e14af5", "9.999999999999997e+22"), c("44b52d02c7e14af6", "1e+23"),
    c("44b52d02c7e14af7", "1.0000000000000001e+23"), c("444b1ae4d6e2ef4e", "999999999999999700000"),
    c("444b1ae4d6e2ef4f", "999999999999999900000"), c("444b1ae4d6e2ef50", "1e+21"),
    c("3eb0c6f7a0b5ed8c", "9.999999999999997e-7"), c("3eb0c6f7a0b5ed8d", "0.000001"),
    c("41b3de4355555553", "333333333.3333332"), c("41b3de4355555554", "333333333.33333325"),
    c("41b3de4355555555", "333333333.3333333"), c("41b3de4355555556", "333333333.3333334"),
    c("41b3de4355555557", "333333333.33333343"), c("becbf647612f3696", "-0.0000033333333333333333"),
    c("43143ff3c1cb0959", "1424953923781206.2"), c("e17cd1f192002619", "-4.051847215643804e+161"))
  for (cs in corpus) expect_identical(n(.ws0_dbl(cs[1])), cs[2], info = cs[1])
  expect_identical(n(1e-7), "1e-7"); expect_identical(n(1000000000000000100), "1000000000000000100")
  expect_identical(n(1.2345678901234568e18), "1234567890123456800")
  expect_identical(n(0.0915), "0.0915"); expect_identical(n(2.15), "2.15"); expect_identical(n(365L), "365")
  expect_identical(n(365), "365"); expect_identical(n(1e21), "1e+21"); expect_identical(n(2.5e-8), "2.5e-8")
  expect_identical(n(1/3), "0.3333333333333333"); expect_identical(n(100), "100"); expect_identical(n(-0), "0")
  expect_error(n(Inf), "Non-finite"); expect_error(n(NaN), "Non-finite"); expect_error(n(NA_real_), "Non-finite")
  s <- dsOMOP:::.jcsString
  expect_identical(s("a\"b\\c\n\té\u0001"), "\"a\\\"b\\\\c\\n\\té\\u0001\"")
  expect_identical(dsOMOP:::.jcsCanonical(list(b = 1L, a = list(2.15, NULL, TRUE, "x"),
                                              c = structure(list(), names = character(0)), d = list())),
                   '{"a":[2.15,null,true,"x"],"b":1,"c":{},"d":[]}')
  ord <- dsOMOP:::.jcsKeyOrder(c("～", "\U0001f600", "a"))
  expect_identical(ord, c(3L, 2L, 1L))
})

test_that("the differential number corpus agrees with the Python reference", {
  path <- testthat::test_path("fixtures", "es6-number-corpus.csv")
  skip_if(!file.exists(path), "differential corpus not generated")
  corpus <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  for (i in seq_len(nrow(corpus))) {
    expect_identical(dsOMOP:::.jcsNumber(.ws0_dbl(corpus$hex[i])), corpus$expected[i], info = corpus$hex[i])
  }
})
