# Independent, policy-adjusted 2.10.2 oracle for the exact 70 predicate cases
# in predicate-policy-case-ids.txt. These are NEWLY PERMITTED by endorsed
# section 7.4, not unchanged baseline results. The immutable original refusal
# remains in ir-v1-compat.json. This tool never loads the current executor.
#
# Run from a testthat probe in a pristine baseline source archive:
#   source("tools/ws2/capture_predicate_policy_oracle.R")
#   .policyOracleCapture(<current tools directory>, <capture.json>, <label>)
# Repeat in a second fresh R process; the merger requires identical outcomes.
# Only .assertCustomFilterSafe() is locally adjusted. Its incoming object,
# operator, column-allowlist, complexity and membership-value checks remain;
# its date operands remain ISO-validated. The old SQL generator, DBI queries,
# result shaping, support gate and pseudonymization execute unchanged.

.policyOracleGate <- function(original) {
  changed <- character(0)
  record <- function(name, replacement) {
    changed <<- c(changed, name)
    replacement
  }
  walk <- function(node) {
    if (!is.call(node)) return(node)
    if (identical(node[[1L]], as.name("if")) &&
        identical(node[[2L]], as.name("is_date"))) {
      return(record("date-operator-width-policy", quote(if (is_date) {
        values <- unlist(value, use.names = FALSE)
        if (!length(values)) stop("Date filters require ISO date operands.", call. = FALSE)
        if (identical(op, "between")) {
          if (length(values) != 2L) stop("Date BETWEEN filters require exactly two ISO dates.", call. = FALSE)
          .validateDateBounds(values[[1L]], values[[2L]], "date filter")
        } else {
          for (date_value in values) .isoDate(date_value, "date filter value")
        }
        return(invisible(TRUE))
      })))
    }
    if (identical(node[[1L]], as.name("stop")) && is.character(node[[2L]])) {
      text <- node[[2L]]
      policies <- c(
        "Disclosive: client-authored ordered thresholds are not permitted; " = "ordered-threshold-policy",
        "Disclosive: IN/NOT IN is only permitted for concept IDs or " = "categorical-membership-policy",
        "Numeric BETWEEN filters are not permitted; use a validated " = "numeric-between-policy"
      )
      if (text %in% names(policies)) return(record(unname(policies[[text]]), quote(invisible(TRUE))))
    }
    if (identical(node, quote(.validateFilter(.filterOpClass(op), list())))) {
      return(record("custom-filter-family-policy", quote(invisible(TRUE))))
    }
    for (i in seq_along(node)[-1L]) node[[i]] <- walk(node[[i]])
    node
  }
  patched <- original
  body(patched) <- walk(body(original))
  expected <- c("date-operator-width-policy", "ordered-threshold-policy",
                "categorical-membership-policy", "numeric-between-policy",
                "custom-filter-family-policy")
  stopifnot(length(changed) == 5L, setequal(changed, expected))
  list(fn = patched, changed = changed)
}

.policyOracleCapture <- function(tool_dir, output, label) {
  stopifnot(identical(as.character(utils::packageVersion("dsOMOP")), "2.10.2"))
  ns <- asNamespace("dsOMOP")
  stopifnot(!exists(".irPrepare", envir = ns, inherits = FALSE))
  env <- new.env(parent = parent.frame())
  sys.source(file.path(tool_dir, "capture_compat.R"), envir = env)
  ids <- readLines(file.path(tool_dir, "predicate-policy-case-ids.txt"), warn = FALSE)
  stopifnot(length(ids) == 70L, !anyDuplicated(ids), all(startsWith(ids, "B.")))
  cases <- Filter(function(x) x$id %in% ids, env$.cc_catalogue())
  stopifnot(length(cases) == length(ids))
  original <- get(".assertCustomFilterSafe", envir = ns)
  patch <- .policyOracleGate(original)
  # Smoke the adjusted gate before any capture; the date policy remains
  # conditional, and structural/identifier checks remain mandatory.
  stopifnot(isTRUE(patch$fn(list(var = "value_as_number", op = "==", value = 1),
                             "value_as_number")))
  stopifnot(isTRUE(patch$fn(list(var = "condition_start_date", op = "==", value = "2020-01-01"),
                             "condition_start_date")))
  refuses <- function(filter, columns) inherits(tryCatch({patch$fn(filter, columns); NULL},
                                                         error = identity), "error")
  stopifnot(refuses(list(var = "person_id", op = "==", value = 1), "value_as_number"),
            refuses(list(var = "value_as_number", op = "made_up", value = 1), "value_as_number"),
            refuses(list(var = "condition_start_date", op = "==", value = "bad-date"), "condition_start_date"))
  option_state <- env$.cc_option_state()
  unlockBinding(".assertCustomFilterSafe", ns)
  assign(".assertCustomFilterSafe", patch$fn, envir = ns)
  lockBinding(".assertCustomFilterSafe", ns)
  on.exit({
    unlockBinding(".assertCustomFilterSafe", ns)
    assign(".assertCustomFilterSafe", original, envir = ns)
    lockBinding(".assertCustomFilterSafe", ns)
  }, add = TRUE)
  # Do not instrument stop()/DBI: this oracle needs only full results or exact
  # execution refusals, not the original capture's rejection-stage heuristic.
  env$.cc_db_state$log <- list()
  results <- lapply(cases, function(case) {
    cat("[policy oracle]", label, case$id, "\n")
    rec <- env$.cc_run_case(case)
    rec$seconds <- NULL
    if (!is.null(rec$error)) {
      rec$error$origin <- NULL
      rec$error$data_statements_before <- NULL
      rec$error$refused_before_query <- NULL
    }
    rec
  })
  result <- list(
    version = 1L,
    kind = "policy-adjusted baseline predicate oracle; not original baseline equivalence",
    baseline = "30b7905617dbe3de5d06699c324095635c68011f",
    reference = "Endorsed section 7.4; A-19 item 7 historical literal semantics",
    label = label,
    option_state = option_state,
    tool_sha256 = env$.cc_sha(paste(readLines(file.path(tool_dir, "capture_predicate_policy_oracle.R")), collapse = "\n")),
    gate_changes = patch$changed,
    original_gate_sha256 = env$.cc_sha(paste(deparse(original), collapse = "\n")),
    adjusted_gate_sha256 = env$.cc_sha(paste(deparse(patch$fn), collapse = "\n")),
    adjusted_gate = paste(deparse(patch$fn), collapse = "\n"),
    results = results
  )
  writeLines(env$.cc_json(result), output, useBytes = TRUE)
  invisible(result)
}
