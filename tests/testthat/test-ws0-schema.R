# WS-0 (design-2.10.3 sections 4 and 12.1; amendments A-6/A-7; review B2/B3):
# the frozen schema artefacts and the packaged validator.

.ws0_schema_dir <- function() {
  d <- system.file("schema", package = "dsOMOP")
  if (!nzchar(d)) {
    d <- Find(dir.exists, c(file.path(testthat::test_path(), "..", "..", "inst", "schema"), "inst/schema"))
  }
  d
}
.ws0_sha256_file <- function(path) {
  paste(as.character(openssl::sha256(readBin(path, "raw", file.info(path)$size))))
}
.ws0_probe_instance <- function(p) {
  if (!is.null(p$instance)) return(p$instance)
  jsonlite::fromJSON(p$instance_text, simplifyVector = FALSE)
}

test_that("the schema artefacts exist, parse, lint and match SHA256SUMS", {
  d <- .ws0_schema_dir()
  skip_if(is.null(d) || !nzchar(d), "schema directory not found")
  sums <- read.table(file.path(d, "SHA256SUMS"), header = FALSE,
                     col.names = c("sha256", "path"), stringsAsFactors = FALSE)
  expect_true(all(c("plan-v2.json", "ir-v2.json", "mutate-v1.json",
                    "examples/plan-v2-example-A.json", "examples/plan-v2-example-B.json",
                    "probes/plan-v2-probes.json", "probes/mutate-v1-probes.json",
                    "probes/plan-v1-probes.json", "digest-vectors.json", "normalized-vectors.json") %in% sums$path))
  for (i in seq_len(nrow(sums))) {
    path <- file.path(d, sums$path[i])
    expect_true(file.exists(path), info = sums$path[i])
    expect_identical(.ws0_sha256_file(path), sums$sha256[i], info = sums$path[i])
    expect_silent(jsonlite::fromJSON(path, simplifyVector = FALSE))
  }
  for (name in c("plan-v2", "ir-v2", "mutate-v1")) {
    expect_silent(dsOMOP:::.jsonSchemaLint(dsOMOP:::.jsonSchemaLoad(file.path(d, paste0(name, ".json")))))
  }
  expect_error(dsOMOP:::.jsonSchemaLint(list(type = "object", patternProperties = list())), "Unsupported JSON Schema keyword")
  expect_error(dsOMOP:::.jsonSchemaLint(list(`$defs` = list(x = list(dependentRequired = list())))), "at #/\\$defs/x")
})

test_that("plan-v2.json is the design's closed wire schema with the accepted amendments", {
  schema <- dsOMOP:::.jsonSchemaPackaged("plan-v2")
  expect_identical(schema[["$id"]], "https://isglobal-brge.github.io/dsOMOP/schema/plan-v2.json")
  expect_identical(schema$properties$version$const, 2L)
  expect_false("legacy_leaf" %in% names(schema[["$defs"]]))
  expect_false("legacy_spec" %in% names(schema[["$defs"]]))
  expect_false("legacy_end_strategy" %in% names(schema[["$defs"]]$index$properties))
  expect_false("legacy_cohort_end_cap" %in% names(schema[["$defs"]]$options_survival$properties))
  censor_obj <- Filter(function(v) !is.null(v$properties$censor), schema[["$defs"]]$censor_item$oneOf)[[1]]
  expect_true("when_index_source" %in% names(censor_obj$properties$censor$properties))      # A-7
  fns <- unlist(schema[["$defs"]]$reducer$properties$fn$enum)
  expect_true(all(c("covered_days", "supply_days") %in% fns))
  expect_true(all(c("observed_only", "coverage", "fills") %in% names(schema[["$defs"]]$reducer$properties)))
})

test_that("ir-v2.json adds the closed compatibility variants and the shared safe scope", {
  ir <- dsOMOP:::.jsonSchemaPackaged("ir-v2")
  defs <- ir[["$defs"]]
  expect_true(all(c("legacy_leaf", "legacy_spec", "legacy_inclusion_criterion", "legacy_safe_scope") %in% names(defs)))
  expect_setequal(unlist(defs$legacy_safe_scope$required), c("table", "column", "n_bins"))
  expect_false(isTRUE(defs$legacy_safe_scope$additionalProperties))
  hm <- Filter(function(v) v$properties$type$const == "has_measurement", defs$legacy_leaf$oneOf)[[1]]
  expect_identical(hm$properties$params$properties$safe_scope[["$ref"]], "#/$defs/legacy_safe_scope")
  expect_identical(defs$legacy_spec$properties$value_bin$properties$safe_scope[["$ref"]], "#/$defs/legacy_safe_scope")
  types <- vapply(defs$legacy_leaf$oneOf, function(v) v$properties$type$const, "")
  expect_setequal(types, c("sex", "age_range", "age_group", "cohort", "has_concept", "not_has_concept", "concept_count",
                           "prior_observation", "followup", "visit_count", "has_measurement", "missing_measurement"))
  expect_true("legacy_end_strategy" %in% names(defs$index$properties))
  expect_true("legacy_cohort_end_cap" %in% names(defs$options_survival$properties))
  score <- Filter(function(v) "charlson" %in% unlist(v$properties$kind$enum), defs$variable$oneOf[[2]]$properties$derived$oneOf)[[1]]
  expect_true("legacy_all_time_conditions" %in% names(score$properties))                    # A-2
})

test_that("mutate-v1.json uses one identifier grammar for assigned names and references (A-6)", {
  mut <- dsOMOP:::.jsonSchemaPackaged("mutate-v1")
  col <- Filter(function(v) "col" %in% names(v$properties), mut[["$defs"]]$expr$oneOf)[[1]]
  expect_identical(col$properties$col$pattern, "^[A-Za-z][A-Za-z0-9_]{0,63}$")
  expect_identical(mut$properties$assign$items$properties$name$pattern, "^[A-Za-z][A-Za-z0-9_]{0,63}$")
  for (leaf in mut[["$defs"]]$predicate_leaf$oneOf) expect_identical(leaf$properties$var$pattern, "^[A-Za-z][A-Za-z0-9_]{0,63}$")
})

test_that("both canonical examples validate against plan-v2 and ir-v2", {
  d <- .ws0_schema_dir()
  plan <- dsOMOP:::.jsonSchemaPackaged("plan-v2"); ir <- dsOMOP:::.jsonSchemaPackaged("ir-v2")
  for (ex in c("plan-v2-example-A.json", "plan-v2-example-B.json")) {
    inst <- dsOMOP:::.jsonSchemaLoad(file.path(d, "examples", ex))
    res <- dsOMOP:::.jsonSchemaValidate(plan, inst)
    expect_true(res$valid, info = paste(ex, paste(res$errors, collapse = "; ")))
    expect_true(dsOMOP:::.jsonSchemaValidate(ir, inst)$valid, info = ex)
  }
})

test_that("every probe of the corpus has its documented verdict", {
  d <- .ws0_schema_dir()
  schemas <- list(`plan-v2` = dsOMOP:::.jsonSchemaPackaged("plan-v2"), `ir-v2` = dsOMOP:::.jsonSchemaPackaged("ir-v2"),
                  `mutate-v1` = dsOMOP:::.jsonSchemaPackaged("mutate-v1"))
  probes <- c(dsOMOP:::.jsonSchemaLoad(file.path(d, "probes", "plan-v2-probes.json")),
              dsOMOP:::.jsonSchemaLoad(file.path(d, "probes", "mutate-v1-probes.json")))
  ids <- vapply(probes, function(p) p$id, "")
  for (needed in c("P37", "P38", "P43", "P44", "P45", "P46", "P47", "P48", "P49", "I10", "I11", "I12", "I13", "I14", "I15", "I16", "M06", "M07", "M08")) {
    expect_true(needed %in% ids, info = needed)
  }
  for (p in probes) {
    inst <- .ws0_probe_instance(p)
    res <- dsOMOP:::.jsonSchemaValidate(schemas[[p$schema]], inst)
    expect_identical(res$valid, identical(p$expect, "valid"),
                     info = paste0(p$id, " ", p$label, ": ", paste(res$errors, collapse = "; ")))
  }
  ir_ok <- Filter(function(p) p$schema == "ir-v2" && p$expect == "valid", probes)
  for (p in ir_ok) expect_false(dsOMOP:::.jsonSchemaValidate(schemas[["plan-v2"]], p$instance)$valid, info = p$id)
  # the input-level probes fail at input validation, with the offending pointer
  p47 <- probes[[which(ids == "P47")]]; r47 <- dsOMOP:::.jsonSchemaValidate(schemas[["plan-v2"]], .ws0_probe_instance(p47))
  expect_true(any(grepl("^/events/hba1c/unit/convert/8753/factor: number is not finite", r47$errors)))
  p48 <- probes[[which(ids == "P48")]]; r48 <- dsOMOP:::.jsonSchemaValidate(schemas[["plan-v2"]], .ws0_probe_instance(p48))
  expect_true(any(grepl("^/: duplicate object member\\(s\\) version", r48$errors)))
  p49 <- probes[[which(ids == "P49")]]; r49 <- dsOMOP:::.jsonSchemaValidate(schemas[["plan-v2"]], .ws0_probe_instance(p49))
  expect_true(any(grepl("^/events/hba1c: duplicate object member\\(s\\) table", r49$errors)))
})

test_that("the actual v1 inputs of section 4.5 are materialised with verdicts for WS-2", {
  d <- .ws0_schema_dir()
  v1 <- dsOMOP:::.jsonSchemaLoad(file.path(d, "probes", "plan-v1-probes.json"))
  expect_length(v1, 3L)
  expect_identical(vapply(v1, function(p) p$id, ""), c("V01", "V02", "V03"))
  expect_true(all(vapply(v1, function(p) p$schema == "v1-validator" && p$owner == "WS-2", logical(1))))
  expect_identical(vapply(v1, function(p) p$expect, ""), c("invalid", "invalid", "valid"))
  expect_true(isTRUE(v1[[1]]$instance$already_gated))
  expect_true(isTRUE(v1[[2]]$instance$outputs$dict$already_gated))
  expect_identical(unlist(v1[[3]]$instance$outputs$demo$age_breaks), c(0L, 18L, 19L, 65L))
  # these are v1 plans: the wire schema refuses them (no version 2 envelope)
  for (p in v1) expect_false(dsOMOP:::.jsonSchemaValidate(dsOMOP:::.jsonSchemaPackaged("plan-v2"), p$instance)$valid, info = p$id)
})

test_that("the validator rejects non-finite numbers and duplicate members from real JSON", {
  plan <- dsOMOP:::.jsonSchemaPackaged("plan-v2")
  d <- .ws0_schema_dir()
  text <- paste(readLines(file.path(d, "examples", "plan-v2-example-A.json"), warn = FALSE), collapse = "\n")
  inf_text <- sub('"factor": 0.0915', '"factor": 1e400', text, fixed = TRUE)
  inst <- jsonlite::fromJSON(inf_text, simplifyVector = FALSE)
  expect_true(is.infinite(inst$events$hba1c$unit$convert[["8753"]]$factor))
  res <- dsOMOP:::.jsonSchemaValidate(plan, inst)
  expect_false(res$valid); expect_true(any(grepl("not finite", res$errors)))
  dup <- jsonlite::fromJSON('{"x": 1, "x": "wrong"}', simplifyVector = FALSE)
  closed <- list(type = "object", additionalProperties = FALSE, required = list("x"), properties = list(x = list(type = "integer")))
  res <- dsOMOP:::.jsonSchemaValidate(closed, dup)
  expect_false(res$valid); expect_true(any(grepl("duplicate object member", res$errors)))
  expect_true(dsOMOP:::.jsonSchemaValidate(closed, jsonlite::fromJSON('{"x": 1}', simplifyVector = FALSE))$valid)
  nested <- jsonlite::fromJSON('{"version": 2, "meta": {"language_version": "2.0", "language_version": "9.9", "semantic_version": "2.0.0", "generated_by": "x"}, "outputs": {"o": {"rows": "p", "format": "wide"}}}', simplifyVector = FALSE)
  res <- dsOMOP:::.jsonSchemaValidate(plan, nested)
  expect_false(res$valid); expect_true(any(grepl("^/meta: duplicate object member", res$errors)))
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
  expect_error(dsOMOP:::.jsonSchemaValidate(list(type = "string", patternProperties = list()), "x"), "Unsupported JSON Schema keyword")
  # RFC 6901 escape order: ~1 then ~0
  root <- list(`$defs` = list(`a/b` = list(type = "string"), `a~b` = list(type = "integer")))
  expect_identical(dsOMOP:::.jsonSchemaResolveRef("#/$defs/a~1b", root)$type, "string")
  expect_identical(dsOMOP:::.jsonSchemaResolveRef("#/$defs/a~0b", root)$type, "integer")
})

test_that("the independent Python oracle agrees when it is available", {
  py <- Sys.which("python3")
  skip_if(!nzchar(py), "python3 not available")
  has <- system2(py, c("-c", shQuote("import jsonschema")), stdout = FALSE, stderr = FALSE) == 0L
  skip_if(!has, "python jsonschema not available")
  pkg <- Find(dir.exists, c(file.path(testthat::test_path(), "..", ".."), "."))
  out <- system2(py, c(file.path(pkg, "tools", "ws0", "check_contracts.py"), pkg), stdout = TRUE, stderr = TRUE)
  expect_true(any(grepl("all contract checks passed", out)), info = paste(out, collapse = "\n"))
})

test_that("the vendored client copies are byte-identical", {
  d <- .ws0_schema_dir()
  client_dir <- system.file("schema", package = "dsOMOPClient")
  tp <- testthat::test_path()
  siblings <- file.path(tp, "..", "..", "..", c("dsOMOPClient", "wt_ws0_dsOMOPClient", "dsOMOPClient_ws0"))
  if (!nzchar(client_dir)) client_dir <- Find(dir.exists, file.path(siblings, "inst", "schema"))
  skip_if(is.null(client_dir) || !nzchar(client_dir), "dsOMOPClient schema directory not reachable")
  for (rel in c("plan-v2.json", "mutate-v1.json", "examples/plan-v2-example-A.json", "examples/plan-v2-example-B.json",
                "probes/plan-v2-probes.json", "probes/mutate-v1-probes.json", "probes/plan-v1-probes.json", "digest-vectors.json",
                "normalized-vectors.json", "normalized/plan-v2-example-A.json", "normalized/plan-v2-example-B.json")) {
    expect_identical(.ws0_sha256_file(file.path(client_dir, rel)), .ws0_sha256_file(file.path(d, rel)), info = rel)
  }
  client_root <- Find(dir.exists, siblings)
  server_root <- Find(dir.exists, file.path(tp, "..", ".."))
  skip_if(is.null(client_root) || is.null(server_root), "source trees not reachable")
  for (f in c("R/json_schema.R", "R/semantic_digest.R", "R/plan_normalize_v2.R", "inst/column_classes.csv")) {
    expect_identical(.ws0_sha256_file(file.path(client_root, f)), .ws0_sha256_file(file.path(server_root, f)), info = f)
  }
})
