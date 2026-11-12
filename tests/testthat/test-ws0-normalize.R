# WS-0 (amendment A-11): the normal form of a version-2 plan. R/plan_normalize_v2.R is
# vendored byte-identically in both packages; inst/schema/normalized-vectors.json pins
# the normal forms produced by the Python reference (tools/ws0/normalize_reference.py).

.ws0_norm_dir <- function() {
  d <- system.file("schema", package = "dsOMOP")
  if (!nzchar(d)) d <- Find(dir.exists, c("inst/schema", file.path(testthat::test_path(), "..", "..", "inst", "schema")))
  d
}
.ws0_norm_sha <- function(x) paste(as.character(openssl::sha256(charToRaw(enc2utf8(dsOMOP:::.jcsCanonical(x))))))
.ws0_norm_source <- function(d, rel) {
  path <- normalizePath(file.path(d, rel), mustWork = FALSE)
  if (file.exists(path)) return(path)
  # acceptance plans live in the server package only
  alt <- sub("^\\.\\./plans/", "", rel)
  cand <- c(system.file("plans", alt, package = "dsOMOP"), file.path(testthat::test_path(), "..", "..", "inst", "plans", alt), file.path("inst", "plans", alt))
  Find(function(p) nzchar(p) && file.exists(p), cand)
}

test_that("the vendored normaliser reproduces every pinned normal form and is a fixed point", {
  d <- .ws0_norm_dir(); skip_if(is.null(d) || !nzchar(d), "schema directory not found")
  vec <- dsOMOP:::.jsonSchemaLoad(file.path(d, "normalized-vectors.json"))
  expect_gte(length(vec$vectors), 17L)
  schema <- dsOMOP:::.jsonSchemaPackaged("plan-v2"); ir <- dsOMOP:::.jsonSchemaPackaged("ir-v2")
  for (v in vec$vectors) {
    stored <- dsOMOP:::.jsonSchemaLoad(file.path(d, "normalized", paste0(v$label, ".json")))
    expect_identical(.ws0_norm_sha(stored), v$normalized_sha256, info = v$label)
    expect_identical(dsOMOP:::.planSemanticDigest(stored)$sha256, v$digest_sha256, info = v$label)
    again <- dsOMOP:::.planNormalizeV2(stored)
    expect_identical(.ws0_norm_sha(again), v$normalized_sha256, info = paste(v$label, "fixed point"))
    expect_true(dsOMOP:::.jsonSchemaValidate(schema, stored)$valid, info = v$label)
    expect_true(dsOMOP:::.jsonSchemaValidate(ir, stored)$valid, info = v$label)
    src <- .ws0_norm_source(d, v$source)
    if (!is.null(src)) {
      n <- dsOMOP:::.planNormalizeV2(dsOMOP:::.jsonSchemaLoad(src))
      expect_identical(.ws0_norm_sha(n), v$normalized_sha256, info = paste(v$label, "from source"))
      expect_identical(dsOMOP:::.planNormalizedDigest(dsOMOP:::.jsonSchemaLoad(src))$sha256, v$digest_sha256, info = v$label)
    }
  }
})

test_that("the normal form applies exactly the A-11 defaults", {
  d <- .ws0_norm_dir(); skip_if(is.null(d) || !nzchar(d), "schema directory not found")
  a <- dsOMOP:::.jsonSchemaLoad(file.path(d, "examples", "plan-v2-example-A.json"))
  minimal <- list(version = 2L, meta = a$meta, concept_sets = a$concept_sets, events = a$events,
                  populations = list(p = list(kind = "index", index = list(source = list(event = "mi")))),
                  variables = list(age = list(derived = list(kind = "age"))),
                  outputs = list(w = list(rows = "p", format = "wide"),
                                 s = list(rows = "p", format = "survival",
                                          options = list(outcomes = list(mi = list(event = "mi")), tar = list(start = 0L, end = 365L), censor = list("observation_end")))))
  n <- dsOMOP:::.planNormalizeV2(minimal)
  expect_identical(n$publish, list("w", "s"))
  expect_identical(names(n$options)[1:3], c("translate_concepts", "block_sensitive", "factor_concepts"))
  expect_identical(n$intervals, structure(list(), names = character(0)))
  expect_identical(n$populations$p$coverage, list(policy = "unique", on_ambiguous = "drop", on_missing = "drop", gap = 0L))
  expect_identical(n$populations$p$index$select, "first")
  expect_identical(n$outputs$w$grain, "episode")
  expect_identical(n$outputs$w$columns, list("age"))
  expect_identical(n$outputs$s$options$event_order, "first")
  expect_identical(n$outputs$s$options$outcome_mode, "composite")
  expect_identical(dsOMOP:::.planNormalizeV2(n), n)
  # restrict_to_observation = FALSE keeps uncovered episodes
  minimal$populations$p$index$restrict_to_observation <- FALSE
  expect_identical(dsOMOP:::.planNormalizeV2(minimal)$populations$p$coverage$on_missing, "keep")
  # a plan-level coverage is inherited by populations without one
  minimal$options <- list(coverage = list(policy = "earliest"))
  cov <- dsOMOP:::.planNormalizeV2(minimal)$populations$p$coverage
  expect_identical(cov$policy, "earliest"); expect_identical(cov$on_missing, "keep")
  # explicit values are never overwritten
  minimal$outputs$w$grain <- "person"; minimal$outputs$w$columns <- list()
  m <- dsOMOP:::.planNormalizeV2(minimal)
  expect_identical(m$outputs$w$grain, "person"); expect_identical(m$outputs$w$columns, list())
  # recurrent events default to event_order all and named outcomes
  minimal$outputs$s$options$format <- "recurrent_events"
  r <- dsOMOP:::.planNormalizeV2(minimal)$outputs$s$options
  expect_identical(r$event_order, "all"); expect_identical(r$outcome_mode, "named")
  expect_error(dsOMOP:::.planNormalizeV2(list(version = 1L, outputs = list())), "version 2")
})

test_that("population indexedness follows design 6.9 (union: any member; intersect/setdiff: the left member)", {
  pops <- list(
    persons = list(kind = "criteria"),
    idx = list(kind = "index", index = list(source = list(event = "mi"))),
    child = list(kind = "criteria", parent = "idx"),
    grandchild = list(kind = "criteria", parent = "child"),
    u = list(kind = "setop", setop = list(op = "union", members = list("persons", "idx"))),
    i_left_persons = list(kind = "setop", setop = list(op = "intersect", members = list("persons", "idx"))),
    i_left_idx = list(kind = "setop", setop = list(op = "intersect", members = list("idx", "persons"))),
    d_left_persons = list(kind = "setop", setop = list(op = "setdiff", members = list("persons", "idx"))),
    d_left_idx = list(kind = "setop", setop = list(op = "setdiff", members = list("idx", "persons"))),
    cc = list(kind = "match", match = list(cases = "idx", controls = "persons")),
    loop_a = list(kind = "criteria", parent = "loop_b"), loop_b = list(kind = "criteria", parent = "loop_a"))
  is_idx <- function(id) dsOMOP:::.pnv2PopulationIndexed(pops, id)
  expect_false(is_idx("persons")); expect_true(is_idx("idx")); expect_true(is_idx("child")); expect_true(is_idx("grandchild"))
  expect_true(is_idx("u")); expect_false(is_idx("i_left_persons")); expect_true(is_idx("i_left_idx"))
  expect_false(is_idx("d_left_persons")); expect_true(is_idx("d_left_idx")); expect_true(is_idx("cc"))
  expect_false(is_idx("loop_a")); expect_false(is_idx("missing"))
})
