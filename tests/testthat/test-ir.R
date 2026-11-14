# WS-2: the plan IR (design-2.10.3 sections 4.4, 4.5, 5 and 6.1).

.ir_schema_file <- function(...) {
  path <- system.file("schema", ..., package = "dsOMOP")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "schema", ...)
  path
}
.ir_json <- function(...) jsonlite::fromJSON(.ir_schema_file(...), simplifyVector = FALSE)
.ir_probes <- function(name) {
  probes <- .ir_json("probes", name)
  stats::setNames(probes, vapply(probes, `[[`, "", "id"))
}
.ir_rejection <- function(expr) {
  tryCatch({ force(expr); NULL }, dsomop_plan_rejection = function(e) e)
}
.ir_v1 <- function(...) {
  plan <- list(...)
  plan
}

# --- step 2: normalisation of a language 2.0 plan ---------------------------------

test_that("normalising a v2 plan expands every schema and semantic default", {
  b <- .ir_json("examples", "plan-v2-example-B.json")
  ir <- .irNormalize(b)
  expect_s3_class(ir, "dsomop_ir")
  expect_identical(attr(ir, "source_version"), 2L)
  # schema defaults
  expect_identical(ir$populations$cases$index$n, 1L)
  expect_identical(ir$populations$cases$index$restrict_to_observation, TRUE)
  expect_identical(ir$populations$cases$where$observed$gap, NULL)
  expect_true("gap" %in% names(ir$populations$cases$where$observed))
  expect_identical(ir$populations$cc$match$max_candidates, 1000000L)
  expect_identical(ir$variables$statin_365$reducer$missing, "skip")
  expect_identical(ir$variables$statin_365$reducer$coverage, "recorded")
  expect_identical(ir$events$mi$on_missing_table, "error")
  expect_identical(ir$concept_sets$mi$include_mapped, FALSE)
  # semantic defaults
  expect_identical(ir$options$translate_concepts, TRUE)
  expect_identical(ir$populations$cases$coverage,
                   list(policy = "unique", on_ambiguous = "drop",
                        on_missing = "drop", gap = 0L))
  expect_identical(ir$populations$eligible$coverage$policy, "unique")
  expect_identical(unlist(ir$publish), "cc_table")
  # a field with neither a schema nor a semantic default stays absent (CR-11)
  expect_false("scope" %in% names(ir))
  expect_false("temporal" %in% names(ir$outputs$cc_table$options))
  expect_identical(ir$outputs$cc_table$options$date_handling$reference, "index")
  # the normalised plan still validates and is a fixed point
  expect_true(.jsonSchemaValidate(.irSchema("ir-v2"), unclass(.irStripPrivate(ir)))$valid)
  again <- .irNormalize(unclass(.irStripPrivate(ir)))
  expect_identical(.jcsCanonical(.irStripPrivate(again)), .jcsCanonical(.irStripPrivate(ir)))
})

test_that("grain and coverage defaults follow the population kind", {
  a <- .ir_json("examples", "plan-v2-example-A.json")
  a$outputs$baseline$grain <- NULL
  a$populations$new_users$coverage <- NULL
  a$populations$people <- list(kind = "criteria")
  a$outputs$demo <- list(rows = "people", format = "wide", columns = list("sex"))
  a$options$coverage <- list(policy = "earliest")
  ir <- .irNormalize(a)
  expect_identical(ir$outputs$baseline$grain, "episode")
  expect_identical(ir$outputs$demo$grain, "person")
  expect_identical(ir$populations$new_users$coverage$policy, "earliest")
  expect_identical(ir$populations$people$coverage$on_missing, "drop")
  # restrict_to_observation = FALSE lowers to on_missing = keep
  a$populations$new_users$index$restrict_to_observation <- FALSE
  ir <- .irNormalize(a)
  expect_identical(ir$populations$new_users$coverage$on_missing, "keep")
  # an explicitly conflicting coverage is a validation error
  a$populations$new_users$coverage <- list(on_missing = "drop")
  err <- .ir_rejection(.irValidate(.irNormalize(a)))
  expect_match(conditionMessage(err), "restrict_to_observation")
})

test_that("absent and explicit defaults normalise identically (one digest)", {
  b <- .ir_json("examples", "plan-v2-example-B.json")
  ir <- .irNormalize(b)
  explicit <- .irNormalize(unclass(.irStripPrivate(ir)))
  expect_identical(.planSemanticDigest(.irDocument(ir))$sha256, .planSemanticDigest(.irDocument(explicit))$sha256)
  expect_identical(.planSemanticDigest(.irDocument(ir))$sha256,
                   .planSemanticDigest(unclass(.irStripPrivate(ir)))$sha256)
  # an absent publish is every output in declaration order; a given one is kept
  b$outputs <- c(list(zeta = b$outputs$cc_table), b$outputs)
  b$publish <- NULL
  expect_identical(unlist(.irNormalize(b)$publish), c("zeta", "cc_table"))
  b$publish <- list("cc_table", "zeta")
  expect_identical(unlist(.irNormalize(b)$publish), c("cc_table", "zeta"))
})

test_that("the semantic defaults of the canonical example A are the normaliser's (CR-11)", {
  # WS-0 ships example A with the semantic defaults its R recipe leaves
  # implicit written out (wide columns, publish, coverage, grain, event_order);
  # leaving them implicit gives the same normal form
  a <- .ir_json("examples", "plan-v2-example-A.json")
  implicit <- a
  implicit$outputs$baseline$columns <- NULL
  implicit$publish <- NULL
  implicit$populations$new_users$coverage <- NULL
  implicit$outputs$baseline$grain <- NULL
  implicit$outputs$tte$grain <- NULL
  implicit$outputs$tte$options$event_order <- NULL
  expect_identical(.planSemanticDigest(.irDocument(.irNormalize(implicit)))$sha256,
                   .planSemanticDigest(.irDocument(.irNormalize(a)))$sha256)
  ir <- .irNormalize(implicit)
  expect_identical(unlist(ir$outputs$baseline$columns), c("age", "sex", "hba1c_base"))
  expect_identical(unlist(ir$publish), c("baseline", "tte"))
  expect_identical(ir$outputs$tte$options$outcome_mode, "composite")
  # normalising twice changes nothing
  again <- .irNormalize(unclass(.irStripPrivate(ir)))
  expect_identical(.jcsCanonical(.irStripPrivate(again)), .jcsCanonical(.irStripPrivate(ir)))
})

test_that("survival format defaults follow the format matrix", {
  a <- .ir_json("examples", "plan-v2-example-A.json")
  a$outputs$tte$options$event_order <- NULL
  ir <- .irNormalize(a)
  expect_identical(ir$outputs$tte$options$event_order, "first")
  expect_identical(ir$outputs$tte$options$outcome_mode, "composite")
  a$outputs$tte$options$format <- "recurrent_events"
  a$outputs$tte$options$tie_policy <- "all"
  ir <- .irNormalize(a)
  expect_identical(ir$outputs$tte$options$event_order, "all")
  expect_identical(ir$outputs$tte$options$outcome_mode, "named")
})

test_that("A-11: the v2 normal form is the frozen .planNormalizeV2(), pinned by WS-0", {
  sha <- function(s) paste(as.character(openssl::sha256(charToRaw(enc2utf8(s)))), collapse = "")
  vectors <- .ir_json("normalized-vectors.json")$vectors
  expect_gte(length(vectors), 17L)
  for (v in vectors) {
    plan <- jsonlite::fromJSON(.ir_schema_file(v$source), simplifyVector = FALSE)
    ir <- .irNormalize(plan)
    doc <- .irDocument(ir)
    expect_identical(.jcsCanonical(doc), .jcsCanonical(.planNormalizeV2(plan)), info = v$label)
    expect_identical(sha(.jcsCanonical(doc)), v$normalized_sha256, info = v$label)
    expect_identical(.planSemanticDigest(doc)$sha256, v$digest_sha256, info = v$label)
    pinned <- .ir_json("normalized", paste0(v$label, ".json"))
    expect_identical(.jcsCanonical(doc), .jcsCanonical(pinned), info = v$label)
    # idempotent, and never an IR-only legacy field in a native plan
    expect_identical(.jcsCanonical(.irDocument(.irNormalize(doc))), .jcsCanonical(doc), info = v$label)
    expect_false(grepl("\"legacy_", .jcsCanonical(doc), fixed = TRUE), info = v$label)
  }
})

test_that("A-11: server and client give every valid wire probe one normal form and digest", {
  probes <- Filter(function(p) identical(p$expect, "valid") && identical(p$schema, "plan-v2"),
                   .ir_probes("plan-v2-probes.json"))
  expect_gt(length(probes), 10L)
  client <- if (requireNamespace("dsOMOPClient", quietly = TRUE)) {
    get0(".planNormalizeV2", envir = asNamespace("dsOMOPClient"), inherits = FALSE)
  }
  for (p in probes) {
    doc <- .irDocument(.irNormalize(p$instance))
    expect_identical(.jcsCanonical(doc), .jcsCanonical(.planNormalizeV2(p$instance)), info = p$id)
    if (is.function(client)) {
      expect_identical(.planSemanticDigest(doc)$sha256,
                       .planSemanticDigest(client(p$instance))$sha256, info = p$id)
    }
  }
  if (!is.function(client)) skip("dsOMOPClient with .planNormalizeV2() is not installed")
})

# --- the frozen sequence -----------------------------------------------------------

test_that("the v2 probe corpus behaves as documented at the wire step", {
  probes <- .ir_probes("plan-v2-probes.json")
  for (p in probes) {
    if (!identical(p$schema, "plan-v2")) next
    err <- .ir_rejection(.irValidateWire(p$instance))
    if (identical(p$expect, "valid")) {
      expect_null(err, info = p$id)
    } else {
      expect_s3_class(err, "dsomop_plan_rejection")
      expect_identical(err$code, "validation_error", info = p$id)
    }
  }
})

test_that("valid v2 probes pass the whole sequence without a site", {
  probes <- .ir_probes("plan-v2-probes.json")
  # Schema-level probes that the section 4.1 semantic rules refuse: effective
  # supply on a non-stockpiled era (P32, P34) and an index_source tag the
  # population cannot carry (P44, amendment A-7).
  semantic <- c(P32 = "effective_supply needs a stockpiled interval source",
                P34 = "effective_supply needs a stockpiled interval source",
                P44 = "impossible index_source tag")
  for (p in probes) {
    if (!identical(p$schema, "plan-v2") || !identical(p$expect, "valid")) next
    ir <- .ir_rejection(.irPrepare(p$instance))
    if (p$id %in% names(semantic)) {
      expect_s3_class(ir, "dsomop_plan_rejection")
      expect_match(conditionMessage(ir), semantic[[p$id]], fixed = TRUE, info = p$id)
    } else {
      expect_false(inherits(ir, "dsomop_plan_rejection"),
                   info = paste(p$id, if (inherits(ir, "condition")) conditionMessage(ir)))
    }
  }
})

test_that("IR probes: compatibility variants validate only in the IR schema", {
  probes <- .ir_probes("plan-v2-probes.json")
  for (p in probes) {
    if (!identical(p$schema, "ir-v2")) next
    ok <- .jsonSchemaValidate(.irSchema("ir-v2"), p$instance)$valid
    expect_identical(ok, identical(p$expect, "valid"), info = p$id)
  }
  for (id in c("P09", "P10", "P11", "P12", "P42", "P43")) {
    expect_s3_class(.ir_rejection(.irPrepare(probes[[id]]$instance)),
                    "dsomop_plan_rejection")
  }
})

test_that("the v1 route probes V01-V03 behave as pinned (section 4.5)", {
  probes <- .ir_probes("plan-v1-probes.json")
  v01 <- .ir_rejection(.irPrepare(probes$V01$instance))
  expect_s3_class(v01, "dsomop_ir_validation_error")
  expect_match(conditionMessage(v01), "already_gated")
  v02 <- .ir_rejection(.irPrepare(probes$V02$instance))
  expect_s3_class(v02, "dsomop_ir_validation_error")
  expect_match(conditionMessage(v02), "/outputs/dict.*already_gated")
  ir <- .irPrepare(probes$V03$instance)
  expect_identical(unlist(ir$variables$age_group$derived$age_breaks), c(0L, 18L, 19L, 65L))
  expect_identical(ir$variables$age_group$derived$bands, TRUE)
})

test_that("normalisation never discards an unknown field", {
  v1 <- list(already_gated = TRUE,
             outputs = list(p = list(type = "person_level",
                                     tables = list(person = list("gender_concept_id")),
                                     lineage = "forged")))
  ir <- .irNormalize(.irJsonForm(v1))
  expect_identical(ir$already_gated, TRUE)
  expect_identical(ir$outputs$p$lineage, "forged")
  expect_false(.jsonSchemaValidate(.irSchema("ir-v2"), unclass(.irStripPrivate(ir)))$valid)
  # a nested unknown field invalidates the IR as well
  v1 <- list(outputs = list(e = list(type = "event_level", table = "measurement",
                                     temporal = list(index_window = list(start = -30, end = 0),
                                                     forged = TRUE))))
  ir <- .irNormalize(.irJsonForm(v1))
  expect_false(.jsonSchemaValidate(.irSchema("ir-v2"), unclass(.irStripPrivate(ir)))$valid)
  # and a v2 plan keeps its unknown fields
  b <- .ir_json("examples", "plan-v2-example-B.json")
  b$outputs$cc_table$roster <- "forged"
  expect_identical(.irNormalize(b)$outputs$cc_table$roster, "forged")
})

test_that("forged plans are refused before any query", {
  b <- .ir_json("examples", "plan-v2-example-B.json")
  forged <- list(
    unknown_kind = function(x) { x$populations$cases$kind <- "magic"; x },
    mixed_shape = function(x) { x$populations$cases$setop <- list(op = "union", members = list("a", "b")); x },
    cycle = function(x) { x$populations$eligible$parent <- "eligible"; x },
    dangling = function(x) { x$outputs$cc_table$rows <- "nobody"; x },
    lineage = function(x) { x$outputs$cc_table$lineage <- list(mode = "direct"); x },
    private = function(x) { x$populations$cases$private <- TRUE; x },
    negative_seed = function(x) { x$populations$cc$match$seed <- -1L; x },
    compat_on_wire = function(x) { x$outputs$cc_table$legacy_split <- TRUE; x }
  )
  for (nm in names(forged)) {
    err <- .ir_rejection(.irPrepare(forged[[nm]](b)))
    expect_s3_class(err, "dsomop_plan_rejection")
    expect_identical(err$code, "validation_error", info = nm)
  }
})

test_that("rejections are typed and name fields, never values or counts", {
  err <- .ir_rejection(.irPrepare(list(version = 3, outputs = list())))
  expect_s3_class(err, c("dsomop_ir_validation_error", "dsomop_plan_rejection", "error", "condition"))
  expect_identical(err$code, "validation_error")
  expect_identical(err$field, "/version")
  expect_match(conditionMessage(err), "^validation_error: ")
})

# --- semantic rules ------------------------------------------------------------------

test_that("semantic rules of section 4.1 are enforced", {
  a <- .ir_json("examples", "plan-v2-example-A.json")
  check <- function(mutate, pattern) {
    x <- mutate(a)
    err <- .ir_rejection(.irValidate(.irNormalize(x)))
    expect_s3_class(err, "dsomop_plan_rejection")
    if (!is.null(err)) expect_match(conditionMessage(err), pattern)
  }
  check(function(x) { x$outputs$tte$options$tars <- list(itt = list(start = 1L)); x$outputs$tte$options$tar <- list(start = 1L); x }, "exactly one of tar or tars")
  check(function(x) { x$outputs$tte$options$tie_policy <- "error"; x }, "tie_policy")
  check(function(x) { x$outputs$tte$options$event_order <- "all"; x }, "event_order")
  check(function(x) { x$populations$new_users$index$n <- 2L; x }, "nth")
  check(function(x) { x$populations$new_users$index$end <- list(strategy = "fixed"); x }, "fixed end strategy")
  check(function(x) { x$publish <- list("nope"); x }, "dangling output 'nope'")
  check(function(x) { x$events$mi$concept_set <- "missing_set"; x }, "dangling concept set")
  check(function(x) { x$variables$hba1c_base$source <- list(event = "nothing"); x }, "dangling event reference")
  check(function(x) { x$variables$age$derived$age_breaks <- list(5L, 10L); x }, "age_breaks")
  check(function(x) { x$outputs$baseline$grain <- "person"; x }, "collapse")
  check(function(x) {
    x$outputs$tte$options$censor <- list(list(censor = list(source = list(interval = "metf_era"),
                                                            when_index_source = list("ghost"))))
    x
  }, "impossible index_source")
})

test_that("arm-conditioned censoring accepts the possible tags (A-7)", {
  a <- .ir_json("examples", "plan-v2-example-A.json")
  a$outputs$tte$options$censor <- list(
    "observation_end",
    list(censor = list(source = list(interval = "metf_era"),
                       when_index_source = list("metf_era"))))
  expect_silent(.irValidate(.irNormalize(a)))
})

test_that("set-op shape rules: setdiff has two members, members share coverage", {
  b <- .ir_json("examples", "plan-v2-example-B.json")
  b$populations$both <- list(kind = "setop", setop = list(op = "setdiff",
                                                          members = list("cases", "eligible", "cc")))
  expect_match(conditionMessage(.ir_rejection(.irValidate(.irNormalize(b)))), "setdiff takes exactly two")
  b$populations$both$setop$members <- list("cases", "eligible")
  b$populations$eligible$coverage <- list(policy = "union")
  expect_match(conditionMessage(.ir_rejection(.irValidate(.irNormalize(b)))), "share one coverage")
})

test_that("predicates are typed against their columns", {
  a <- .ir_json("examples", "plan-v2-example-A.json")
  a$events$mi$where <- list(var = "condition_start_date", op = ">=", value = 5)
  expect_match(conditionMessage(.ir_rejection(.irValidate(.irNormalize(a)))), "ISO dates")
  a$events$mi$where <- list(var = "condition_type_concept_id", op = "in", value = list("x"))
  expect_match(conditionMessage(.ir_rejection(.irValidate(.irNormalize(a)))), "integer concept ids")
  a$events$mi$where <- list(var = "value_as_number", op = "between",
                            value = list(lower = 9, upper = 1))
  expect_match(conditionMessage(.ir_rejection(.irValidate(.irNormalize(a)))), "ordered")
})

test_that("deployment-disabled operations are refused", {
  a <- .ir_json("examples", "plan-v2-example-A.json")
  withr::local_options(list(dsomop.recipe_operations_disabled = "output.survival"))
  err <- .ir_rejection(.irValidate(.irNormalize(a)))
  expect_match(conditionMessage(err), "disabled by this deployment: output.survival")
})

# --- order and nodes --------------------------------------------------------------------

test_that(".irOrder orders populations topologically and joined outputs last", {
  b <- .ir_json("examples", "plan-v2-example-B.json")
  b$outputs$extra <- list(rows = "cc", format = "wide", columns = list("age"))
  b$outputs$both <- list(rows = "cc", format = "joined",
                         options = list(from = list(list(output = "cc_table"), list(output = "extra"))))
  ir <- .irNormalize(b)
  ord <- .irOrder(ir)
  expect_lt(match("cases", ord$populations), match("cc", ord$populations))
  expect_lt(match("eligible", ord$populations), match("cc", ord$populations))
  expect_identical(utils::tail(ord$outputs, 1L), "both")
  expect_length(ord$anchors$candidate$cases, 0L)
  # a population cycle is refused
  b$populations$eligible$parent <- "cc"
  expect_s3_class(.ir_rejection(.irOrder(.irNormalize(b))), "dsomop_plan_rejection")
})

test_that(".irOrder separates candidate and final anchors", {
  a <- .ir_json("examples", "plan-v2-example-A.json")
  anchor <- list(source = list(event = "mi"), which = "first", field = "start")
  a$populations$new_users$where$and[[2]]$has$window$anchor <- anchor
  a$variables$hba1c_base$reducer$window$anchor <- anchor
  ord <- .irOrder(.irNormalize(a))
  expect_length(ord$anchors$candidate$new_users, 1L)
  expect_length(ord$anchors$final$new_users, 1L)
  expect_identical(ord$anchors$candidate$new_users[[1]]$source$event, "mi")
})

test_that(".irNode resolves typed references and refuses dangling ones", {
  ir <- .irNormalize(.ir_json("examples", "plan-v2-example-A.json"))
  expect_identical(.irNode(ir, list(event = "mi"))$table, "condition_occurrence")
  expect_identical(.irNode(ir, "population", "new_users")$kind, "index")
  err <- .ir_rejection(.irNode(ir, list(interval = "nope")))
  expect_identical(err$field, "/intervals/nope")
})

.ir_shipped_v2_plans <- function() {
  acc <- system.file("plans", "acceptance", package = "dsOMOP")
  if (!nzchar(acc)) acc <- testthat::test_path("..", "..", "inst", "plans", "acceptance")
  files <- c(.ir_schema_file("examples", "plan-v2-example-A.json"),
             .ir_schema_file("examples", "plan-v2-example-B.json"),
             Sys.glob(file.path(acc, "*", "plan.json")))
  stats::setNames(lapply(files, jsonlite::fromJSON, simplifyVector = FALSE),
                  ifelse(basename(files) == "plan.json", basename(dirname(files)),
                         sub("[.]json$", "", basename(files))))
}

test_that("an output's closure keeps every node it references", {
  plans <- .ir_shipped_v2_plans()
  expect_gte(length(plans), 17L)
  for (p in names(plans)) {
    ir <- .irPrepare(plans[[p]])
    covered <- character(0)
    for (o in names(ir$outputs)) {
      sub <- .irOutputClosure(ir, o)
      # the closure passes the reference rules on its own: nothing it
      # depends on was left out
      expect_identical(.irSemanticErrors(sub), character(0), info = paste(p, o))
      expect_true(o %in% names(sub$outputs), info = paste(p, o))
      ops <- .irOperations(sub)
      expect_true(all(ops %in% .irOperations(ir)), info = paste(p, o))
      covered <- union(covered, ops)
    }
    # in the shipped plans every node serves an output: the outputs account
    # for every operation but the plan-level publish roots
    expect_setequal(setdiff(covered, "publish.roots"),
                    setdiff(.irOperations(ir), "publish.roots"))
  }
  tte <- .irOutputClosure(.irPrepare(plans[["plan-v2-example-A"]]), "tte")
  expect_named(tte$outputs, "tte")
  expect_length(tte$variables, 0L)
  expect_named(tte$populations, "new_users")
})

test_that("an output projects its columns, or every variable only when wide", {
  acc <- .ir_shipped_v2_plans()[["02-active-comparator"]]
  expect_null(acc$outputs$baseline$columns)
  ir <- .irPrepare(acc)
  # the normaliser writes a wide output's implicit columns (CR-11) ...
  expect_identical(unlist(ir$outputs$baseline$columns), names(ir$variables))
  expect_identical(.irOutputVariables(ir, "baseline"), names(ir$variables))
  # ... and an un-normalised null reads the same way; survival projects none
  ir$outputs$baseline$columns <- NULL
  expect_identical(.irOutputVariables(ir, "baseline"), names(ir$variables))
  expect_null(ir$outputs$tte$columns)
  expect_identical(.irOutputVariables(ir, "tte"), character(0))
  a <- .irPrepare(.ir_json("examples", "plan-v2-example-A.json"))
  expect_identical(.irOutputVariables(a, "baseline"), c("age", "sex", "hba1c_base"))
})

# --- section 5: lowering, one test per row ------------------------------------------

test_that("v1 cohort_table lowers to a scope and the cohort's rows", {
  ir <- .irPrepare(list(cohort = list(type = "cohort_table", cohort_definition_id = 3L),
                        outputs = list(demo = list(type = "baseline"))))
  expect_identical(attr(ir, "source_version"), 1L)
  expect_identical(ir$scope$cohort, 3L)
  base <- ir$populations$base
  expect_identical(base$kind, "index")
  expect_identical(base$index$source, list(cohort_definition_id = 3L))
  expect_identical(base$index$select, "all")
  expect_identical(base$index$end$strategy, "event_end")
  expect_identical(base$coverage$on_missing, "keep")
  # baseline: wide, episode grain, raw person variables, age bands
  out <- ir$outputs$demo
  expect_identical(c(out$format, out$grain), c("wide", "episode"))
  expect_identical(unlist(out$columns), c("gender_concept_id", "race_concept_id"))
  expect_identical(ir$variables$gender_concept_id, list(raw = list(table = "person", column = "gender_concept_id"),
                        labels = setNames(list(), character()),
                        column_names = setNames(list(), character())))
})

test_that("v1 inline cohort spec lowers to the closed legacy_spec variant", {
  spec <- list(type = "condition", concept_set = list(concepts = list(201826L), include_descendants = TRUE),
               inclusion_criteria = list(prior = list(table = "drug_exposure", concept_set = list(1503297L),
                                                      temporal = list(index_window = list(start = -365L, end = -1L)))))
  ir <- .irPrepare(list(cohort = list(type = "spec", spec = spec),
                        outputs = list(m = list(type = "cohort_membership"))))
  base <- ir$populations$base
  expect_identical(base$kind, "index")
  expect_identical(names(base$index$source), "legacy_spec")
  expect_identical(base$index$source$legacy_spec$concept_set$include_descendants, TRUE)
  expect_identical(base$index$restrict_to_observation, FALSE)
  expect_identical(base$coverage$on_missing, "keep")
  expect_identical(ir$outputs$m$format, "membership")
  expect_identical(ir$outputs$m$options$date_handling$mode, "remove")
  # value_threshold stays non-executable (refused with the legacy message)
  spec$value_threshold <- list(op = ">", value = 7)
  expect_match(conditionMessage(.ir_rejection(.irPrepare(list(
    cohort = list(type = "spec", spec = spec), outputs = list(m = list(type = "cohort_membership")))))),
    "value_threshold is no longer executable")
})

test_that("v1 filter trees lower to legacy criteria leaves", {
  tree <- list(and = list(list(type = "sex", params = list(value = "F")),
                          list(type = "has_concept", params = list(concept_id = list(201820L, 201826L),
                                                                   table = "condition_occurrence",
                                                                   concept_name = "Diabetes",
                                                                   window = list(start = -365L, end = 0L),
                                                                   reference_date = "2020-01-01"))))
  ir <- .irPrepare(list(cohort = list(type = "spec", filter_tree = tree, episode_policy = "any_episode"),
                        outputs = list(p = list(type = "person_level", tables = list(person = list("gender_concept_id"))))))
  where <- ir$populations$base$where
  expect_identical(where$and[[1]], list(legacy = list(type = "sex", params = list(value = "F"))))
  leaf <- where$and[[2]]$legacy
  expect_identical(leaf$type, "has_concept")
  expect_identical(unlist(leaf$params$concept_ids), c(201820L, 201826L))
  expect_null(leaf$params$concept_name)
  expect_identical(ir$populations$base$episode_policy, "any_episode")
  # the concept_name label is advisory: the annex, never the document
  annex <- attr(ir, "legacy_annex")
  expect_identical(unlist(annex[["populations/base"]]$concept_names), "Diabetes")
  expect_identical(ir$populations$base$legacy_episodes, "observation_periods")
  # a flat legacy array is an implicit AND
  ir <- .irPrepare(list(cohort = list(type = "spec", filter_tree = list(list(type = "sex", params = list(value = "M")))),
                        outputs = list(p = list(type = "person_level", tables = list(person = list("gender_concept_id"))))))
  expect_named(ir$populations$base$where, "and")
})

test_that("v1 populations keep their ids: criteria, set-ops and index events", {
  pops <- list(
    base = list(id = "base", kind = "criteria", label = "All"),
    a = list(id = "a", kind = "criteria", filter_tree = list(type = "sex", params = list(value = "F")),
             cohort_definition_id = 4L, episode_policy = "first_episode"),
    idx = list(id = "idx", index_event = list(table = "condition_occurrence",
                                             concept_set = list(concepts = list(201820L), exclude = list(201826L)),
                                             primary_limit = "last",
                                             end_strategy = list(DateOffset = list(DateField = "EndDate", Offset = 30L)))),
    u = list(id = "u", kind = "setop", setop = list(op = "difference", members = list("a", "idx")))
  )
  ir <- .irPrepare(list(populations = pops,
                        outputs = list(x = list(type = "person_level", population_id = "u",
                                                tables = list(person = list("gender_concept_id"))))))
  expect_identical(ir$populations$a$kind, "criteria")
  expect_identical(ir$populations$a$cohort_definition_id, 4L)
  expect_identical(ir$populations$a$episode_policy, "first_episode")
  # v1 criteria populations are their persons' observation periods; with no
  # plan cohort and no scope their windowed leaves have no index to anchor on
  expect_identical(ir$populations$a$legacy_episodes, "observation_periods")
  expect_null(ir$populations$a$legacy_index_anchor)
  idx <- ir$populations$idx
  expect_identical(idx$kind, "index")
  expect_identical(idx$index$select, "last")
  expect_identical(idx$index$legacy_end_strategy, list(DateOffset = list(DateField = "EndDate", Offset = 30L)))
  ev <- ir$events[[idx$index$source$event]]
  expect_identical(unlist(ev$concept_set$exclude), 201826L)
  expect_identical(ir$populations$u$setop, list(op = "setdiff", members = list("a", "idx")))
  expect_identical(ir$outputs$x$rows, "u")
})

test_that("A-E2: set-op shape exclusivity and unknown kinds are refused", {
  setop <- list(id = "s", kind = "setop", setop = list(op = "union", members = list("a", "b")))
  base <- list(a = list(id = "a", kind = "criteria", filter_tree = list(type = "sex", params = list(value = "F"))),
               b = list(id = "b", kind = "criteria", cohort_definition_id = 2L))
  outputs <- list(x = list(type = "person_level", tables = list(person = list("gender_concept_id"))))
  for (field in c("filter_tree", "cohort_definition_id")) {
    bad <- setop
    bad[[field]] <- if (field == "filter_tree") list(type = "sex", params = list(value = "F")) else 1L
    err <- .ir_rejection(.irPrepare(list(populations = c(base, list(s = bad)), outputs = outputs)))
    expect_match(conditionMessage(err), paste0("cannot declare ", field), info = field)
  }
  err <- .ir_rejection(.irPrepare(list(populations = c(base, list(s = list(id = "s", kind = "cohort"))),
                                       outputs = outputs)))
  expect_match(conditionMessage(err), "/populations/s/kind: must be one of criteria, setop", fixed = TRUE)
  # an absent kind is a criteria population, as in v1
  outputs$x$population_id <- "k"
  ir <- .irPrepare(list(populations = c(base, list(k = list(id = "k", filter_tree = list(type = "sex", params = list(value = "F"))))),
                        outputs = outputs))
  expect_identical(ir$populations$k$kind, "criteria")
})

test_that("v1 person_level lowers to raw, feature and derived variables", {
  out <- list(type = "person_level",
              tables = list(person = list(sex = "gender_concept_id"),
                            measurement = list(features = list(
                              list(name = "a1c_mean", type = "mean_value", concept_set = list(3004410L),
                                   value_column = "value_as_number",
                                   filter = list(var = "unit_concept_id", op = "in", value = list(8554L)))),
                              concept_set = list(3004410L))),
              derived_columns = list(list(kind = "age", name = "age", reference = "index"),
                                     list(kind = "charlson", name = "cci")))
  ir <- .irPrepare(list(outputs = list(p = out)))
  expect_identical(ir$outputs$p$grain, "person")
  expect_identical(unlist(ir$outputs$p$columns), c("sex", "a1c_mean", "age", "cci"))
  expect_identical(ir$variables$sex$raw, list(table = "person", column = "gender_concept_id"))
  v <- ir$variables$a1c_mean
  expect_identical(v$reducer$fn, "mean")
  expect_identical(v$reducer$value, "value_as_number")
  ev <- ir$events[[v$source$event]]
  expect_identical(ev$table, "measurement")
  expect_identical(ev$where, list(var = "unit_concept_id", op = "in", value = list(8554L)))
  # the table's concept_set restricts the stream the spec selects within:
  # their intersection (A-10 legacy_concept_scope, interned and resolvable)
  scope <- ev$legacy_concept_scope$concept_set_ref
  expect_identical(unlist(ir$concept_sets[[scope]]$concepts), 3004410L)
  expect_identical(ir$variables$age$derived$reference, "index")
  expect_identical(ir$variables$cci$derived$legacy_all_time_conditions, TRUE)
})

test_that("v1 event_level outputs lower to long, wide and sparse", {
  long <- list(type = "event_level", table = "condition_occurrence", concept_set = list(201820L),
               columns = list("condition_concept_id", "condition_start_date"),
               temporal = list(index_window = list(start = -30L, end = 0L),
                               event_select = list(order = "first", n = 1L)),
               filters = list(custom = list(var = "condition_type_concept_id", op = "eq", value = 32020L)),
               date_handling = "relative_to_index", representation = list(format = "long"))
  ir <- .irPrepare(list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
                        outputs = list(e = long)))
  o <- ir$outputs$e
  expect_identical(o$format, "long")
  expect_identical(o$options$date_handling$mode, "relative")
  ev <- ir$events[[o$sources[[1]]$event]]
  expect_identical(ev$where, list(var = "condition_type_concept_id", op = "==", value = 32020L))
  # A-10: the projection and the temporal policy are in the IR document
  expect_identical(unlist(o$legacy_columns), c("condition_concept_id", "condition_start_date"))
  expect_identical(o$legacy_temporal, list(index_window = list(start = -30L, end = 0L),
                                           event_select = list(order = "first", n = 1L, by = "grain")))
  expect_null(o$options$temporal)
  # sparse: the legacy automatic sparse builder
  ir <- .irPrepare(list(outputs = list(s = list(type = "event_level", table = "drug_exposure",
                                                representation = list(format = "sparse")))))
  expect_identical(ir$outputs$s$format, "sparse")
  expect_identical(ir$outputs$s$legacy_automatic_features$format, "sparse")
  # value_bin row filters lower to one half-open between leaf (lower <= x < upper)
  vb <- list(var = "value_as_number", op = "value_bin", value = list(lower = 5, upper = 7),
             safe_scope = list(table = "measurement", column = "value_as_number", n_bins = 4L))
  ir <- .irPrepare(list(outputs = list(e = list(type = "event_level", table = "measurement",
                                                filters = list(custom = vb)))))
  ev <- ir$events[[ir$outputs$e$sources[[1]]$event]]
  expect_identical(ev$where, list(var = "value_as_number", op = "between",
                                  value = list(lower = 5L, upper = 7L, upper_inclusive = FALSE)))
})

test_that("v1 survival lowers with the legacy cohort-end cap", {
  surv <- list(type = "survival", outcome = list(table = "condition_occurrence", concept_set = list(4000002L)),
               tar = list(start_offset = 0L, end_offset = 730L))
  ir <- .irPrepare(list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
                        outputs = list(tte = surv)))
  opts <- ir$outputs$tte$options
  expect_identical(opts$legacy_cohort_end_cap, TRUE)
  expect_identical(unlist(opts$censor), "observation_end")
  expect_identical(opts$tar, list(start = 0L, end = 730L, anchor = "index"))
  expect_named(opts$outcomes, "outcome")
  expect_identical(opts$legacy_single_outcome, TRUE)
  # named outcomes keep death by default and every censoring flag
  surv <- list(type = "survival", format = "competing_risk",
               outcomes = list(mi = list(table = "condition_occurrence", concept_set = list(1L)),
                               death_any = list(table = "death", concept_set = list(2L))),
               censoring = list(admin_date = "2024-12-31"))
  ir <- .irPrepare(list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
                        outputs = list(cr = surv)))
  censor <- ir$outputs$cr$options$censor
  expect_identical(censor[[1]], "observation_end")
  expect_identical(censor[[2]], "death")
  expect_identical(censor[[3]], list(admin_date = "2024-12-31"))
  # multi-state transitions in the normalised edge form
  surv <- list(type = "survival", format = "multi_state", event_order = "all",
               outcomes = list(ill = list(table = "condition_occurrence", concept_set = list(1L)),
                               dead = list(table = "death", concept_set = list(2L))),
               transitions = list(states = list("index", "ill", "dead"),
                                  edges = list(list(from = "index", to = "ill", trans = 1L),
                                               list(from = "ill", to = "dead", trans = 2L))),
               state_step = 0.01)
  ir <- .irPrepare(list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
                        outputs = list(ms = surv)))
  opts <- ir$outputs$ms$options
  expect_identical(opts$transitions, list(list(from = "index", to = "ill"), list(from = "ill", to = "dead")))
  expect_null(opts$state_step)
  expect_identical(opts$legacy_state_step, 0.01)
  expect_identical(unlist(opts$legacy_states), c("index", "ill", "dead"))
  expect_identical(unlist(opts$state_hierarchy), c("index", "ill", "dead"))
})

test_that("v1 intervals, temporal covariates and person periods lower to their formats", {
  ir <- .irPrepare(list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(
      iv = list(type = "intervals_long", tables = list("drug_exposure", "visit_occurrence"),
                concept_filter = list(drug_exposure = list(1503297L)), window = list(start = 0L, end = 30L),
                interval_match = "overlaps", event_select = "all", select_n = 1L,
                select_by = "episode_source", anchor = 0L),
      tc = list(type = "temporal_covariates", table = "condition_occurrence", bin_width = 90L,
                window_start = -365L, window_end = 0L, analyses = list("binary", "count")),
      pp = list(type = "person_period", table = "condition_occurrence", bin_width = 30L,
                window_start = 0L, window_end = 365L, grain = "episode", time_origin = "index"))))
  iv <- ir$outputs$iv$options
  expect_length(iv$sources, 2L)
  expect_identical(iv$window, list(start = 0L, end = 30L, anchor = "index"))
  tc <- ir$outputs$tc$options
  expect_true(tc$legacy_composite && tc$legacy_covariate_ids)
  expect_identical(tc$window$start, -365L)
  pp <- ir$outputs$pp$options
  expect_identical(pp$representation, "sparse")
  expect_identical(pp$bins, list(width = 30L))
  expect_identical(ir$outputs$pp$grain, "episode")
})

test_that("v1 dictionaries list their source outputs explicitly", {
  ir <- .irPrepare(list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
                        outputs = list(demo = list(type = "baseline"),
                                       dict = list(type = "concept_dictionary"))))
  expect_identical(unlist(ir$outputs$dict$options$source_outputs), "demo")
  expect_identical(attr(ir, "legacy_annex")[["outputs/dict"]]$source_outputs, "all")
})

test_that("v1 names that are not 2.0 names are re-encoded injectively", {
  ir <- .irPrepare(list(outputs = list(`my.output` = list(type = "person_level",
                                                         tables = list(person = list("gender_concept_id"))))))
  key <- names(ir$outputs)
  expect_match(key, "^my_output_[0-9a-f]{10}$")
  expect_identical(ir$legacy_names$outputs[[key]], "my.output")
})

test_that("a v1 plan without outputs has no IR and is still accepted", {
  expect_null(.irPrepare(list(populations = list(base = list(id = "base")), outputs = list())))
  expect_s3_class(.ir_rejection(.irPrepare(list(outputs = list(), already_gated = TRUE))),
                  "dsomop_plan_rejection")
})

test_that("a lowered v1 plan names no publication roots: they are the mapped outputs", {
  ir <- .irPrepare(list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(b = list(type = "baseline", columns = list("gender_concept_id")),
                   a = list(type = "baseline", columns = list("year_of_birth")))))
  expect_false("publish" %in% names(ir))
  expect_false("publish.roots" %in% .irOperations(ir))
  expect_true(.jsonSchemaValidate(.irSchema("ir-v2"), unclass(.irStripPrivate(ir)))$valid)
})

test_that("A-10: v1 semantics are in the document the one digest binds; the annex is not", {
  digest <- function(p) .planSemanticDigest(.irDocument(.irPrepare(p)))$sha256
  p1 <- list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
             outputs = list(e = list(type = "event_level", table = "measurement",
                                     columns = list("measurement_concept_id"))))
  p2 <- p1
  p2$outputs$e$columns <- list("measurement_date")
  expect_false(identical(digest(p1), digest(p2)))
  # an advisory field changes the annex only
  p3 <- p1
  p3$anchor <- list(table = "person", id_col = "person_id")
  expect_identical(digest(p3), digest(p1))
  expect_false(is.null(attr(.irPrepare(p3), "legacy_annex")$root$anchor))
  # omitted and explicitly supplied compatibility defaults normalise alike
  t1 <- list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
             outputs = list(e = list(type = "event_level", table = "measurement",
                                     temporal = list(index_window = list(start = -30L, end = 0L),
                                                     event_select = list(order = "last"),
                                                     min_gap = 7L),
                                     representation = list(format = "long"))))
  t2 <- t1
  t2$outputs$e$temporal$event_select <- list(order = "last", n = 1L, by = "grain")
  t2$outputs$e$temporal$min_gap <- list(days = 7L, by = "concept", keep = "first")
  expect_identical(digest(t1), digest(t2))
  expect_identical(.irPrepare(t1)$outputs$e$legacy_temporal$min_gap,
                   list(days = 7L, by = "concept", keep = "first"))
  t2$outputs$e$temporal$min_gap$keep <- "last"
  expect_false(identical(digest(t1), digest(t2)))
})

test_that("plans built as R lists and plans parsed from JSON give one IR", {
  r_plan <- list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
                 outputs = list(demo = list(type = "baseline", columns = c(sex = "gender_concept_id"),
                                            derived = "age_at_index")))
  json_plan <- jsonlite::fromJSON(as.character(jsonlite::toJSON(
    list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
         outputs = list(demo = list(type = "baseline", columns = list(sex = "gender_concept_id"),
                                    derived = "age_at_index"))), auto_unbox = TRUE)), simplifyVector = FALSE)
  expect_identical(.planSemanticDigest(.irDocument(.irPrepare(r_plan)))$sha256,
                   .planSemanticDigest(.irDocument(.irPrepare(json_plan)))$sha256)
  # baseline reads column values only: the name of an alias map is ignored
  expect_identical(.irPrepare(r_plan)$variables$gender_concept_id$raw$column, "gender_concept_id")
})

test_that("the role tables are vendored byte-identically", {
  tp <- testthat::test_path()
  client_root <- Find(dir.exists, file.path(tp, "..", "..", "..", "dsOMOPClient"))
  server_root <- file.path(tp, "..", "..")
  skip_if(is.null(client_root) || !file.exists(file.path(client_root, "inst", "ohdsi", "roles-5.4.csv")),
          "sibling dsOMOPClient source tree not reachable")
  sha <- function(f) paste(as.character(openssl::sha256(file(f))), collapse = "")
  for (f in c("inst/ohdsi/roles-5.3.csv", "inst/ohdsi/roles-5.4.csv")) {
    expect_identical(sha(file.path(client_root, f)), sha(file.path(server_root, f)), info = f)
  }
  # A-11: the server's normaliser is the vendored WS-0 artefact itself
  expect_false(file.exists(file.path(server_root, "R", "ir_defaults.R")))
})

test_that("the legacy-spec adapter wraps the unchanged .cohortCreate rows as a roster", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  id <- DBI::dbGetQuery(handle$conn,
    "SELECT condition_concept_id AS id FROM condition_occurrence LIMIT 1")$id
  ir <- .irPrepare(list(cohort = list(type = "spec", spec = list(type = "condition",
                                                                 concept_set = list(id))),
                        outputs = list(m = list(type = "cohort_membership"))))
  spec <- ir$populations$base$index$source$legacy_spec
  fr <- withr::with_options(list(nfilter.subset = 0), .adaptLegacySpec(handle, spec))
  expect_s3_class(fr, "dsomop_fragment")
  expect_identical(fr$kind, "roster")
  expect_identical(fr$roster_namespace, "base")
  rows <- DBI::dbGetQuery(handle$conn, paste0("SELECT * FROM (", fr$select_body,
                                              ") r ORDER BY cohort_row_id"))
  legacy <- DBI::dbGetQuery(handle$conn, paste0(
    "SELECT * FROM ", attr(fr, "temp_table"),
    " ORDER BY subject_id, cohort_start_date, cohort_end_date"))
  expect_gt(nrow(rows), 0L)
  expect_identical(nrow(rows), nrow(legacy))
  expect_identical(as.integer(rows$cohort_row_id), seq_len(nrow(rows)))
  expect_identical(as.integer(rows$subject_id), as.integer(legacy$subject_id))
  expect_identical(as.character(rows$cohort_start_date), as.character(legacy$cohort_start_date))
  expect_true(all(rows$index_source == "legacy_spec"))
  expect_true(all(is.na(rows$index_key)) && all(is.na(rows$op_start)))
})

test_that("the legacy-spec adapter forwards a private plan context to .cohortCreate", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  expect_true("ctx" %in% names(formals(.cohortCreate)))
  id <- DBI::dbGetQuery(handle$conn,
    "SELECT condition_concept_id AS id FROM condition_occurrence LIMIT 1")$id
  ir <- .irPrepare(list(cohort = list(type = "spec", spec = list(type = "condition",
                                                                 concept_set = list(id))),
                        outputs = list(m = list(type = "cohort_membership"))))
  spec <- ir$populations$base$index$source$legacy_spec
  ctx <- .planContext(handle, ir)
  on.exit(.planContextClose(ctx), add = TRUE, after = FALSE)
  rows <- function(fr) {
    DBI::dbGetQuery(handle$conn, paste0("SELECT * FROM (", fr$select_body,
                                        ") r ORDER BY cohort_row_id"))
  }
  first <- withr::with_options(list(nfilter.subset = 0), .adaptLegacySpec(handle, spec))
  direct <- rows(first)
  # .cohortCreate() names its temporary cohort dsomop_cohort_<id> (v1, one
  # inline spec per plan): release it before the second materialisation
  .dropTempTable(handle, attr(first, "temp_table"))
  private <- rows(withr::with_options(list(nfilter.subset = 0), .adaptLegacySpec(ctx, spec)))
  expect_gt(nrow(private), 0L)
  expect_identical(private, direct)
  # a forged context is refused by the cohort primitive, never ignored
  forged <- new.env()
  forged$handle <- handle
  expect_error(.adaptLegacySpec(forged, spec), "Invalid private execution context")
})
