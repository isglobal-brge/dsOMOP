# WS-2 tooling (dsOMOP 2.10.3): capture every v1 plan the 2.10.x test suites
# build, for the IR golden corpus (tests/testthat/fixtures/ir-v1-corpus.json).
#
# Not part of the package tests. Copy this file into tests/testthat/ of a
# scratch copy of dsOMOP or dsOMOPClient, run the suite with
# DSOMOP_PLAN_CAPTURE_DIR=<dir> (and DSOMOP_PLAN_CAPTURE_LABEL=<label>), then
# build the corpus with tools/ws2/build_ir_corpus.py. It records every plan
# that reaches the v1 server entry points (.planExecute, .planPreview,
# .planValidate, .planResolvePopulations; with the call's success) and every
# plan the client builds or sends (ds.omop.plan.* builders, recipe_to_plan,
# .ds_encode), with the test file, line and title. Output: JSON lines.
if (nzchar(Sys.getenv("DSOMOP_PLAN_CAPTURE_DIR")) &&
    requireNamespace("jsonlite", quietly = TRUE)) local({
  out_dir <- Sys.getenv("DSOMOP_PLAN_CAPTURE_DIR")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  pkg_label <- Sys.getenv("DSOMOP_PLAN_CAPTURE_LABEL", "pkg")
  out_file <- file.path(out_dir, paste0(pkg_label, "-", Sys.getpid(), ".jsonl"))
  state <- new.env(parent = emptyenv())
  state$n <- 0L
  state$stack <- integer(0)
  assign(".dsomop_plan_capture_state", state, envir = globalenv())

  sanitize <- function(x, depth = 0L) {
    if (depth > 60L) return(list(`__truncated__` = TRUE))
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) {
      return(list(`__frame__` = list(ncol = ncol(x), nrow_class = "hidden",
                                     names = as.list(names(x)),
                                     class = as.list(class(x)))))
    }
    if (is.environment(x)) return(list(`__environment__` = as.list(class(x))))
    if (is.function(x)) return(list(`__function__` = TRUE))
    if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
    if (is.factor(x)) return(as.character(x))
    if (is.list(x)) {
      cls <- setdiff(class(x), "list")
      out <- lapply(x, sanitize, depth = depth + 1L)
      if (!is.null(names(x))) names(out) <- names(x)
      attributes(out) <- attributes(out)[intersect(names(attributes(out)), "names")]
      if (length(cls) > 0L && depth > 0L) {
        # keep S3 class tags of nested objects visible to the inventory
        out <- c(out, list(`__class__` = as.list(cls)))
      }
      return(out)
    }
    if (is.atomic(x)) {
      a <- attributes(x)
      keep <- x
      attributes(keep) <- NULL
      if (!is.null(a$names)) {
        keep <- as.list(keep); names(keep) <- a$names
      }
      return(keep)
    }
    list(`__unsupported__` = as.list(class(x)))
  }

  context <- function() {
    calls <- sys.calls()
    test <- NA_character_; file <- NA_character_; line <- NA_integer_
    tc <- tryCatch(get("test_code", envir = asNamespace("testthat")),
                   error = function(e) NULL)
    sf <- tryCatch(get("source_file", envir = asNamespace("testthat")),
                   error = function(e) NULL)
    for (i in rev(seq_along(calls))) {
      f <- tryCatch(sys.function(i), error = function(e) NULL)
      if (is.null(f)) next
      if (is.na(test) && !is.null(tc) && identical(f, tc)) {
        test <- tryCatch(as.character(get("test", envir = sys.frame(i)))[1],
                         error = function(e) NA_character_)
        # the caller is test_that(); its srcref carries the line
        if (i > 1L) {
          sr <- attr(calls[[i - 1L]], "srcref")
          if (!is.null(sr)) line <- as.integer(sr[[1L]])
        }
      }
      if (is.na(file) && !is.null(sf) && identical(f, sf)) {
        file <- tryCatch(basename(get("path", envir = sys.frame(i))),
                         error = function(e) NA_character_)
      }
    }
    if (is.na(line)) {
      # fall back: first call carrying a srcref from a test file
      for (i in seq_along(calls)) {
        sr <- attr(calls[[i]], "srcref")
        if (!is.null(sr)) {
          sf2 <- attr(sr, "srcfile")
          fname <- tryCatch(basename(sf2$filename), error = function(e) "")
          if (grepl("^test-", fname)) { line <- as.integer(sr[[1L]]); if (is.na(file)) file <- fname; break }
        }
      }
    }
    list(file = file, test = test, line = line)
  }

  write_record <- function(rec) {
    txt <- tryCatch(
      as.character(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null",
                                    na = "null", digits = NA, force = TRUE)),
      error = function(e) as.character(jsonlite::toJSON(list(
        id = rec$id, kind = rec$kind, serialization_error = conditionMessage(e)),
        auto_unbox = TRUE)))
    cat(txt, "\n", sep = "", file = out_file, append = TRUE)
  }

  record_entry <- function(fn, plan) {
    state$n <- state$n + 1L
    id <- state$n
    state$stack <- c(state$stack, id)
    ctx <- context()
    write_record(list(id = id, kind = "entry", pkg = pkg_label, fn = fn,
                      file = ctx$file, test = ctx$test, line = ctx$line,
                      plan = sanitize(plan)))
    id
  }
  record_exit <- function(fn, ok) {
    if (length(state$stack) == 0L) return(invisible(NULL))
    id <- state$stack[[length(state$stack)]]
    state$stack <- state$stack[-length(state$stack)]
    write_record(list(id = id, kind = "exit", fn = fn, ok = ok))
  }
  assign(".dsomop_plan_capture_entry", record_entry, envir = globalenv())
  assign(".dsomop_plan_capture_exit", record_exit, envir = globalenv())
  assign(".dsomop_plan_capture_value", function(fn, value) {
    if (is.list(value) && "outputs" %in% names(value)) {
      state$n <- state$n + 1L
      ctx <- context()
      write_record(list(id = state$n, kind = "value", pkg = pkg_label, fn = fn,
                        file = ctx$file, test = ctx$test, line = ctx$line,
                        ok = TRUE, plan = sanitize(value)))
    }
    invisible(NULL)
  }, envir = globalenv())

  sentinel <- quote(structure(list(), class = "dsomop_capture_error_sentinel"))
  trace_entry_exit <- function(ns_name, fn) {
    ns <- tryCatch(asNamespace(ns_name), error = function(e) NULL)
    if (is.null(ns) || !exists(fn, envir = ns, inherits = FALSE)) return(FALSE)
    tracer <- bquote(get(".dsomop_plan_capture_entry", envir = globalenv())(.(fn), plan))
    exit <- bquote(get(".dsomop_plan_capture_exit", envir = globalenv())(
      .(fn), !inherits(returnValue(default = .(sentinel)), "dsomop_capture_error_sentinel")))
    suppressMessages(trace(fn, tracer = tracer, exit = exit, where = ns, print = FALSE))
    TRUE
  }
  traced <- character(0)
  for (fn in c(".planExecute", ".planPreview", ".planValidate",
               ".planResolvePopulations", ".planRequiredCohortErrors")) {
    if (trace_entry_exit("dsOMOP", fn)) traced <- c(traced, paste0("dsOMOP:::", fn))
  }
  cns <- tryCatch(asNamespace("dsOMOPClient"), error = function(e) NULL)
  if (!is.null(cns)) {
    if (exists(".ds_encode", envir = cns, inherits = FALSE)) {
      suppressMessages(trace(".ds_encode", tracer = quote(
        get(".dsomop_plan_capture_value", envir = globalenv())(".ds_encode", x)),
        where = cns, print = FALSE))
      traced <- c(traced, "dsOMOPClient:::.ds_encode")
    }
    dsl <- grep("^ds\\.omop\\.plan(\\.|$)", ls(cns, all.names = TRUE), value = TRUE)
    dsl <- setdiff(dsl, c("ds.omop.plan.execute", "ds.omop.plan.validate",
                          "ds.omop.plan.preview", "ds.omop.plan.save"))
    for (fn in c("recipe_to_plan", ".plan_from_plain", dsl)) {
      if (!exists(fn, envir = cns, inherits = FALSE)) next
      if (!is.function(get(fn, envir = cns))) next
      ex <- bquote(get(".dsomop_plan_capture_value", envir = globalenv())(
        .(fn), returnValue(default = NULL)))
      ok <- tryCatch({
        suppressMessages(trace(fn, exit = ex, where = cns, print = FALSE)); TRUE
      }, error = function(e) FALSE)
      if (ok) traced <- c(traced, paste0("dsOMOPClient:::", fn))
    }
  }
  cat("[plan-capture] tracing:", paste(traced, collapse = ", "), "->", out_file, "\n")
})
