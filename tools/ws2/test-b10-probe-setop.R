# WS-2 tooling (dsOMOP 2.10.3), review B10, A-19 open item (i): EVIDENCE ONLY.
#
# Not part of the package tests. `compare_results.py capture --add-tests
# tools/ws2/test-b10-probe-setop.R` copies it into a snapshot's tests/testthat/
# so that the result capture records these calls at the baseline and at the
# current code. It asserts nothing about the outcome: for the two blocks
# `dsOMOP:test-recipe-setops.R:set-op result below nfilter fails closed (empty
# intersection)` / `(tiny non-zero intersection)` (class newly_permitted), it
# prints one `[b10-probe]` line per call to the suite log naming the outcome,
# the refusing call chain and the execution context the refusing
# .cohortCombine() saw (no ctx parameter = 2.10.2, ctx NULL, or a private plan
# context):
#   1. .planResolvePopulations(handle, plan, bp), as the two blocks call it;
#   2. (2.10.3 only) the same with a private execution context (.planContext());
#   3. .planExecute() of a plan with an output over the non-empty population A
#      next to the empty / tiny private intersection;
#   4. omopPlanExecuteDS() of that plan.
# The fixtures are the blocks' own (women with diabetes / COPD inserted after
# clearing condition_occurrence and measurement), with deterministic row ids.

.probe_line <- function(...) cat("[b10-probe]", ..., "\n")

.probe_trace <- function(expr) {
  chain <- NULL
  value <- tryCatch(withCallingHandlers(expr, error = function(e) {
    if (!is.null(chain)) return(invisible(NULL))
    calls <- sys.calls()
    frames <- sys.frames()
    nm <- vapply(calls, function(cl) {
      f <- cl[[1L]]
      if (is.name(f)) as.character(f) else if (is.call(f)) paste(deparse(f), collapse = "") else "?"
    }, "")
    keep <- which(nm %in% c("omopPlanExecuteDS", ".omopPlanExecuteDS", ".planExecute",
                            ".planResolvePopulations", ".cohortCombine", ".assertMinPersons",
                            ".publishPrepare", ".publishReject"))
    ctx <- vapply(keep[nm[keep] == ".cohortCombine"], function(i) {
      fr <- frames[[i]]
      if (!exists("ctx", envir = fr, inherits = FALSE)) return("no ctx parameter")
      cx <- get("ctx", envir = fr, inherits = FALSE)
      if (is.null(cx)) "ctx NULL" else if (isTRUE(cx$private)) "private ctx" else "other ctx"
    }, "")
    chain <<- paste0(paste(nm[keep], collapse = " > "),
                     if (length(ctx)) paste0(" [.cohortCombine: ", paste(unique(ctx), collapse = ", "), "]") else "")
  }), error = function(e) e)
  list(value = value, chain = chain)
}

.probe_report <- function(case, call, res) {
  v <- res$value
  if (inherits(v, "error")) {
    .probe_line(case, "|", call, "| REFUSED |", conditionMessage(v), "| refusing chain:", res$chain)
  } else {
    status <- if (inherits(v, "dsomop_plan_status")) paste("status", v$status) else "ok"
    .probe_line(case, "|", call, "| OK |", status)
  }
}

.probe_handle <- function(a, b) {
  h <- create_test_handle()
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  id <- 700000L
  for (p in a) {
    id <- id + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, condition_concept_id, ",
      "condition_start_date, condition_end_date, condition_type_concept_id, visit_occurrence_id) ",
      "VALUES (%d, %d, 201820, '2019-06-01', '2019-06-01', 44818518, %d)"), id, p, p))
  }
  for (p in b) {
    id <- id + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, condition_concept_id, ",
      "condition_start_date, condition_end_date, condition_type_concept_id, visit_occurrence_id) ",
      "VALUES (%d, %d, 255573, '2019-06-01', '2019-06-01', 44818518, %d)"), id, p, p))
  }
  .buildBlueprint(h)
  h
}

.probe_key <- function(handle, .local_envir = parent.frame()) {
  key <- .testPseudonymKey("b10-probe")
  identity <- "test://resource-scoped/b10-probe"
  rid <- substr(as.character(openssl::sha256(charToRaw(identity))), 1L, 32L)
  environment <- paste(format(key), collapse = "")
  names(environment) <- paste0("DSOMOP_PSEUDONYM_KEY_", rid)
  withr::local_envvar(c(environment, DSOMOP_PSEUDONYM_PROVIDER = "scoped", DSOMOP_PSEUDONYM_EPOCH = "1",
                        DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false", DSOMOP_PSEUDONYM_ROOT = "",
                        DSOMOP_PSEUDONYM_KEY = ""), .local_envir = .local_envir)
  handle$person_key <- key
  handle$person_key_identity <- identity
  handle$person_key_id <- .personKeyId(key)
  handle$person_key_provider <- "scoped"
  handle$person_key_epoch <- 1L
  handle$person_key_require_existing <- FALSE
  handle$person_key_contract_version <- 1L
}

.probe_plan <- function(with_output) {
  sex_f <- list(type = "sex", params = list(value = "F"))
  has <- function(cid) list(type = "has_concept", params = list(concept_id = cid, table = "condition_occurrence"))
  plan <- list(
    populations = list(
      base = list(id = "base"),
      A = list(id = "A", kind = "criteria", filter_tree = list(and = list(sex_f, has(201820L)))),
      B = list(id = "B", kind = "criteria", filter_tree = list(and = list(sex_f, has(255573L)))),
      res = list(id = "res", kind = "setop", setop = list(op = "intersect", members = c("A", "B")))),
    outputs = if (with_output) list(a_out = list(
      type = "person_level", population_id = "A",
      tables = list(person = c("person_id", "gender_concept_id")))) else list(),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
  class(plan) <- c("omop_plan", "list")
  plan
}

.probe_case <- function(case, a, b) {
  version <- if (exists(".irPrepare", mode = "function")) "2.10.3" else "2.10.2"
  case <- paste(version, case)
  withr::local_options(list(nfilter.subset = 3))
  h <- .probe_handle(a, b)
  .probe_report(case, ".planResolvePopulations(handle, plan, bp)",
                .probe_trace(.planResolvePopulations(h, .probe_plan(FALSE), .buildBlueprint(h))))
  cleanup_handle(h)
  if (exists(".planContext", mode = "function")) {
    h <- .probe_handle(a, b)
    ctx <- .planContext(h, list())
    .probe_report(case, ".planResolvePopulations(handle, plan, bp, ctx = <private>)",
                  .probe_trace(.planResolvePopulations(h, .probe_plan(FALSE), .buildBlueprint(h), ctx = ctx)))
    cleanup_handle(h)
  }
  h <- .probe_handle(a, b)
  .probe_report(case, ".planExecute(plan with an output over A)",
                .probe_trace(.planExecute(h, .probe_plan(TRUE), list(a_out = "a_sym"))))
  cleanup_handle(h)
  h <- .probe_handle(a, b)
  .probe_key(h)
  .setHandle("b10_probe_setop", h)
  target <- new.env()
  res <- .probe_trace(eval(quote(omopPlanExecuteDS("b10_probe_setop", .probe_plan(TRUE),
                                                     list(a_out = "a_sym"))), envir = target))
  .probe_report(case, "omopPlanExecuteDS(plan with an output over A)", res)
  .probe_line(case, "|", "omopPlanExecuteDS bindings:", paste(ls(target), collapse = ","))
  .removeHandle("b10_probe_setop")
  cleanup_handle(h)
}

test_that("B10 probe: set-op result below nfilter, empty intersection", {
  # A = {2,4,6}, B = {8,10,12} (women): A and B disjoint, A n B empty
  .probe_case("empty intersection", c(2, 4, 6), c(8, 10, 12))
  succeed()
})

test_that("B10 probe: set-op result below nfilter, tiny non-zero intersection", {
  # A = {2,4,6,8}, B = {8,10,12,14}: A n B = {8}
  .probe_case("tiny intersection", c(2, 4, 6, 8), c(8, 10, 12, 14))
  succeed()
})
