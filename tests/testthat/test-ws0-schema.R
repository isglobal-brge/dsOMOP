# WS-0 (design-2.10.3 sections 4 and 12.1): the frozen schema artefacts.
# plan-v2.json (wire), ir-v2.json (compatibility IR), mutate-v1.json, the two
# canonical examples, the probe corpus and the checksum list.

.ws0_schema_dir <- function() {
  d <- system.file("schema", package = "dsOMOP")
  if (!nzchar(d)) {
    d <- Find(dir.exists, c(file.path(testthat::test_path(), "..", "..", "inst", "schema"),
                            "inst/schema"))
  }
  d
}
.ws0_sha256_file <- function(path) {
  paste(as.character(openssl::sha256(readBin(path, "raw", file.info(path)$size))))
}

test_that("the schema artefacts exist, parse and match SHA256SUMS", {
  d <- .ws0_schema_dir()
  skip_if(is.null(d) || !nzchar(d), "schema directory not found")
  sums <- read.table(file.path(d, "SHA256SUMS"), header = FALSE,
                     col.names = c("sha256", "path"), stringsAsFactors = FALSE)
  expect_true(all(c("plan-v2.json", "ir-v2.json", "mutate-v1.json",
                    "examples/plan-v2-example-A.json",
                    "examples/plan-v2-example-B.json",
                    "probes/plan-v2-probes.json", "probes/mutate-v1-probes.json",
                    "digest-vectors.json") %in% sums$path))
  for (i in seq_len(nrow(sums))) {
    path <- file.path(d, sums$path[i])
    expect_true(file.exists(path), info = sums$path[i])
    expect_identical(.ws0_sha256_file(path), sums$sha256[i], info = sums$path[i])
    expect_silent(jsonlite::fromJSON(path, simplifyVector = FALSE))
  }
})

test_that("plan-v2.json is the design's closed wire schema", {
  schema <- dsOMOP:::.jsonSchemaPackaged("plan-v2")
  expect_identical(schema[["$id"]], "https://isglobal-brge.github.io/dsOMOP/schema/plan-v2.json")
  expect_identical(schema$properties$version$const, 2L)
  expect_false(isTRUE(schema$additionalProperties))
  # no compatibility variant on the wire
  expect_false("legacy_leaf" %in% names(schema[["$defs"]]))
  expect_false("legacy_spec" %in% names(schema[["$defs"]]))
  expect_false("legacy_end_strategy" %in% names(schema[["$defs"]]$index$properties))
  expect_false("legacy_cohort_end_cap" %in% names(schema[["$defs"]]$options_survival$properties))
  # the reducer enumeration of the capability contract (design 4.4) is the schema's
  fns <- unlist(schema[["$defs"]]$reducer$properties$fn$enum)
  expect_true(all(c("covered_days", "supply_days", "observed_only") %in%
                    c(fns, names(schema[["$defs"]]$reducer$properties))))
})

test_that("ir-v2.json is the wire schema plus the closed compatibility variants", {
  ir <- dsOMOP:::.jsonSchemaPackaged("ir-v2")
  defs <- ir[["$defs"]]
  expect_true(all(c("legacy_leaf", "legacy_spec", "legacy_end_strategy",
                    "legacy_inclusion_criterion") %in%
                    c(names(defs), names(defs$index$properties))))
  types <- vapply(defs$legacy_leaf$oneOf, function(v) v$properties$type$const, "")
  expect_setequal(types, c("sex", "age_range", "age_group", "cohort", "has_concept",
                           "not_has_concept", "concept_count", "prior_observation",
                           "followup", "visit_count", "has_measurement",
                           "missing_measurement"))
  expect_true("legacy_cohort_end_cap" %in% names(defs$options_survival$properties))
  expect_true("legacy_composite" %in% names(defs$options_person_period$properties))
  expect_true("legacy_split" %in% names(defs$output$properties))
})

test_that("both canonical examples validate against plan-v2 and ir-v2", {
  d <- .ws0_schema_dir()
  plan <- dsOMOP:::.jsonSchemaPackaged("plan-v2")
  ir <- dsOMOP:::.jsonSchemaPackaged("ir-v2")
  for (ex in c("plan-v2-example-A.json", "plan-v2-example-B.json")) {
    inst <- dsOMOP:::.jsonSchemaLoad(file.path(d, "examples", ex))
    res <- dsOMOP:::.jsonSchemaValidate(plan, inst)
    expect_true(res$valid, info = paste(ex, paste(res$errors, collapse = "; ")))
    expect_true(dsOMOP:::.jsonSchemaValidate(ir, inst)$valid, info = ex)
  }
})

test_that("every probe of the corpus has its documented verdict", {
  d <- .ws0_schema_dir()
  schemas <- list(`plan-v2` = dsOMOP:::.jsonSchemaPackaged("plan-v2"),
                  `ir-v2` = dsOMOP:::.jsonSchemaPackaged("ir-v2"),
                  `mutate-v1` = dsOMOP:::.jsonSchemaPackaged("mutate-v1"))
  probes <- c(dsOMOP:::.jsonSchemaLoad(file.path(d, "probes", "plan-v2-probes.json")),
              dsOMOP:::.jsonSchemaLoad(file.path(d, "probes", "mutate-v1-probes.json")))
  expect_gte(length(probes), 56L)
  for (p in probes) {
    res <- dsOMOP:::.jsonSchemaValidate(schemas[[p$schema]], p$instance)
    expect_identical(res$valid, identical(p$expect, "valid"),
                     info = paste0(p$id, " ", p$label, ": ", paste(res$errors, collapse = "; ")))
  }
  # the wire schema refuses every IR-only probe that the IR accepts
  ir_ok <- Filter(function(p) p$schema == "ir-v2" && p$expect == "valid", probes)
  for (p in ir_ok) {
    expect_false(dsOMOP:::.jsonSchemaValidate(schemas[["plan-v2"]], p$instance)$valid, info = p$id)
  }
})

test_that("the validator agrees with jsonvalidate when it is installed", {
  skip_if_not_installed("jsonvalidate")
  d <- .ws0_schema_dir()
  probes <- dsOMOP:::.jsonSchemaLoad(file.path(d, "probes", "plan-v2-probes.json"))
  for (name in c("plan-v2", "ir-v2")) {
    v <- jsonvalidate::json_validator(file.path(d, paste0(name, ".json")), engine = "ajv")
    for (p in Filter(function(p) p$schema == name, probes)) {
      expect_identical(unname(v(jsonlite::toJSON(p$instance, auto_unbox = TRUE, null = "null"))),
                       identical(p$expect, "valid"), info = p$id)
    }
  }
})

test_that("the validator reports what fails, with instance pointers", {
  plan <- dsOMOP:::.jsonSchemaPackaged("plan-v2")
  d <- .ws0_schema_dir()
  inst <- dsOMOP:::.jsonSchemaLoad(file.path(d, "examples", "plan-v2-example-A.json"))
  inst$outputs$baseline$roster <- "new_users"
  res <- dsOMOP:::.jsonSchemaValidate(plan, inst)
  expect_false(res$valid)
  expect_true(any(grepl("^/outputs/baseline: unknown property roster", res$errors)))
  expect_error(dsOMOP:::.jsonSchemaAssert(inst, "plan-v2"), "Invalid plan")
  # an unknown applicator keyword in a schema is an error, never ignored
  expect_error(dsOMOP:::.jsonSchemaValidate(list(type = "string", patternProperties = list()), "x"),
               "Unsupported JSON Schema keyword")
})

test_that("the vendored client copies are byte-identical", {
  d <- .ws0_schema_dir()
  client_dir <- system.file("schema", package = "dsOMOPClient")
  if (!nzchar(client_dir)) {
    tp <- testthat::test_path()
    client_dir <- Find(dir.exists, file.path(tp, "..", "..", "..",
      c("dsOMOPClient", "wt_ws0_dsOMOPClient", "dsOMOPClient_ws0"), "inst", "schema"))
  }
  skip_if(is.null(client_dir) || !nzchar(client_dir), "dsOMOPClient schema directory not reachable")
  for (rel in c("plan-v2.json", "mutate-v1.json", "examples/plan-v2-example-A.json",
                "examples/plan-v2-example-B.json", "probes/plan-v2-probes.json",
                "probes/mutate-v1-probes.json", "digest-vectors.json")) {
    expect_identical(.ws0_sha256_file(file.path(client_dir, rel)),
                     .ws0_sha256_file(file.path(d, rel)), info = rel)
  }
  tp <- testthat::test_path()
  client_src <- Find(dir.exists, file.path(tp, "..", "..", "..",
    c("dsOMOPClient", "wt_ws0_dsOMOPClient", "dsOMOPClient_ws0"), "R"))
  server_src <- Find(dir.exists, file.path(tp, "..", "..", "R"))
  skip_if(is.null(client_src) || is.null(server_src), "source trees not reachable")
  for (f in c("json_schema.R", "semantic_digest.R")) {
    expect_identical(.ws0_sha256_file(file.path(client_src, f)),
                     .ws0_sha256_file(file.path(server_src, f)), info = f)
  }
})
