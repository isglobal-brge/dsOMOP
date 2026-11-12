# WS-2 acceptance, Astra's WS-2 re-review R1/R3 and amendment A-19 item 7:
# v1 COMPATIBILITY differential replay.
#
# fixtures/ir-v1-compat.json holds independent BASELINE (dsOMOP 2.10.2,
# 30b7905) outcomes of v1 plans whose scalar fields or predicate literals are
# written in a historically coercible or invalid form (sections A-F of
# tools/ws2/capture_compat.R, which captured them twice in fresh R processes
# and built the fixture). The coordinator's decision (A-19 item 7): v1 plans
# preserve 2.10.2 behaviour -- what 2.10.2 executed (deterministically) keeps
# executing with an identical result through validated compatibility
# normalisation in the v1 lowering; what 2.10.2 refused may be refused.
#
# Every case is replayed on the CURRENT code, on a fresh create_test_handle()
# after the case's setup (issued safe bins), under the baseline option state
# plus the case's options (other nfilter.* / dsomop.* / dsstaging.* /
# datashield.* options unset), with the plan decoded exactly as .ds_arg()
# decodes the transport JSON (section A: the field's template with the
# variant literal substituted):
#   - baseline `ok`: .planExecute() succeeds and the sha256 of the b10
#     canonical form of its value at 17 significant digits equals
#     the baseline's; on a mismatch the failure lists the rows and columns of
#     both, and tools/ws2/capture_compat.R (run on both builds, then diff)
#     recomputes the full canonical difference. Date/POSIX objects are refused
#     before hashing until an independent numeric-payload capture pins them;
#   - baseline `error`: the current code refuses the plan; where the case
#     carries `typed_refusal` (the R1 malformed shapes 2.10.2 refused with an R
#     error) the refusal inherits dsomop_plan_rejection;
#   - `ir_expectation`: .irPrepare(plan) WITHOUT a handle accepts (baseline
#     ok) or refuses with a dsomop_plan_rejection (baseline refused at the plan
#     stage); `none` when 2.10.2 refused only at the site, executor, data or
#     session stage;
#   - ir-v1-compat-dispositions.json records each reviewed contract change
#     separately from the immutable baseline capture. A new refusal checks
#     its typed code and field, a newly permitted case compares its exact
#     result to an admissible independently captured baseline control, and
#     a deferred legacy declaration checks its reviewed context refusal text;
#   - a case flagged nondeterministic at the baseline is not replayed.
# Cases fail where the current code has not (yet) restored the 2.10.2
# behaviour; that is the purpose of the fixture.

# >>> b10-canonical: the .b10_canon() family copied verbatim from the
# b10-canonical block of tests/testthat/test-ir-results.R (at cb5907c), as in
# tools/ws2/capture_compat.R; always called here with 17 significant digits.
.b10_num <- function(v, digits) {
  v <- as.vector(unclass(v))
  s <- sprintf(paste0("%.", digits, "g"), v)
  s[is.na(v)] <- NA_character_
  s[is.nan(v)] <- "NaN"
  s
}
.b10_attrs <- function(x, drop, digits) {
  a <- attributes(x)
  a <- a[setdiff(names(a), drop)]
  if (!length(a)) return(NULL)
  a <- a[order(names(a))]
  lapply(a, .b10_canon, digits = digits)
}
.b10_canon <- function(x, digits = 15L) {
  if (is.null(x)) return(list(kind = "null"))
  if (is.environment(x)) return(list(kind = "environment", class = I(class(x))))
  if (is.function(x)) return(list(kind = "function"))
  if (is.data.frame(x)) {
    rn <- if (.row_names_info(x, type = 1L) < 0L) NULL else
      I(as.character(attr(x, "row.names")))
    return(list(kind = "data.frame", class = I(class(x)),
                names = I(as.character(names(x))), nrow = nrow(x),
                row_names = rn,
                columns = lapply(seq_along(x), function(j) .b10_canon(x[[j]], digits)),
                attrs = .b10_attrs(x, c("names", "row.names", "class"), digits)))
  }
  if (is.list(x)) {
    return(list(kind = "list", class = I(class(x)),
                names = if (is.null(names(x))) NULL else I(as.character(names(x))),
                items = lapply(seq_along(x), function(i) .b10_canon(x[[i]], digits)),
                attrs = .b10_attrs(x, c("names", "class"), digits)))
  }
  if (is.atomic(x)) {
    lev <- NULL
    if (is.factor(x)) {
      lev <- I(as.character(levels(x)))
      vals <- as.character(x)
    } else if (inherits(x, "Date")) {
      vals <- format(x, "%Y-%m-%d")
    } else if (inherits(x, "POSIXt")) {
      vals <- format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
    } else if (inherits(x, "integer64")) {
      vals <- if (requireNamespace("bit64", quietly = TRUE)) {
        bit64::as.character.integer64(x)
      } else paste0("integer64-bits:", .b10_num(unclass(x), 17L))
    } else if (is.double(x)) {
      vals <- .b10_num(x, digits)
    } else if (is.complex(x)) {
      vals <- paste0(.b10_num(Re(x), digits), "|", .b10_num(Im(x), digits))
      vals[is.na(x)] <- NA_character_
    } else if (is.raw(x)) {
      vals <- as.character(x)
    } else if (is.character(x)) {
      vals <- enc2utf8(as.vector(x))
    } else {
      vals <- as.vector(unclass(x))
    }
    vals <- as.vector(vals)
    attributes(vals) <- NULL
    return(list(kind = "atomic", class = I(class(x)), type = typeof(x),
                length = length(x), levels = lev, values = I(vals),
                names = if (is.null(names(x))) NULL else I(as.character(names(x))),
                attrs = .b10_attrs(x, c("names", "class", "levels"), digits)))
  }
  list(kind = "other", class = I(class(x)), type = typeof(x))
}
# <<< b10-canonical

.COMPAT_OPTION_PATTERN <- "^(default\\.)?(nfilter|dsomop|dsstaging|datashield)\\."

.compat_or <- function(a, b) if (is.null(a)) b else a

# the serialisation of tools/ws2/capture_compat.R (.cc_json) and its sha256
.compat_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null",
                                digits = NA, force = TRUE))
}
.compat_has_temporal_object <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(TRUE)
  if (is.list(x) && any(vapply(x, .compat_has_temporal_object, logical(1)))) return(TRUE)
  a <- attributes(x)
  !is.null(a) && any(vapply(a, .compat_has_temporal_object, logical(1)))
}
.compat_sha <- function(x) {
  # This immutable capture used format-only temporal encoding. Its 17-digit
  # numeric comparison is exact only for the types actually represented: a
  # Date/POSIX result requires an independent numeric-payload recapture.
  if (.compat_has_temporal_object(x)) {
    stop("Compatibility result contains Date/POSIX objects; exact numeric-payload baseline recapture required.",
         call. = FALSE)
  }
  unclass(as.character(openssl::sha256(charToRaw(enc2utf8(.compat_json(.b10_canon(x, 17L)))))))
}

test_that("compatibility hashes never silently discard temporal numeric payloads", {
  expect_error(.compat_sha(as.Date("2020-01-01")), "exact numeric-payload baseline recapture", fixed = TRUE)
  expect_error(.compat_sha(list(t = as.POSIXct("2020-01-01", tz = "UTC"))),
               "exact numeric-payload baseline recapture", fixed = TRUE)
  expect_silent(.compat_sha(list(date_text = "2020-01-01")))
})

# Rebuild an R value from its canonical form (the option state).
.compat_decode <- function(n) {
  if (is.null(n) || identical(n$kind, "null")) return(NULL)
  cls <- as.character(unlist(n$class))
  finish <- function(x) {
    if (!is.null(n$names)) names(x) <- vapply(n$names, as.character, "")
    if (!identical(class(x), cls)) class(x) <- cls
    x
  }
  if (identical(n$kind, "atomic")) {
    v <- vapply(n$values, function(v) if (is.null(v)) NA_character_ else as.character(v), "")
    x <- switch(n$type, logical = as.logical(v), integer = as.integer(v),
                double = as.numeric(v), character = v,
                stop("cannot decode a canonical ", n$type, " option"))
    return(finish(x))
  }
  if (identical(n$kind, "list")) return(finish(lapply(n$items, .compat_decode)))
  stop("cannot decode a canonical '", n$kind, "' option")
}

# The case's plan in its transport form, decoded as .ds_arg() decodes it.
.compat_plan <- function(fx, case) {
  if (is.null(case[["template"]])) return(case$plan)
  template <- fx$templates[[case$template]]$template
  jsonlite::fromJSON(gsub("@V@", case$value, template, fixed = TRUE), simplifyVector = FALSE)
}
# every output name -> "<name>_df"
.compat_out <- function(plan) {
  nm <- names(plan$outputs)
  stats::setNames(as.list(paste0(nm, "_df")), nm)
}

# the baseline option state, the case's options over it, every other server
# option of the pattern unset
.compat_options <- function(fx, case) {
  opts <- lapply(fx$option_state$options, .compat_decode)
  own <- .compat_or(case[["options"]], fx$default_options)
  for (nm in names(own)) opts[nm] <- list(own[[nm]])
  stale <- setdiff(grep(.COMPAT_OPTION_PATTERN, names(options()), value = TRUE), names(opts))
  c(opts, stats::setNames(vector("list", length(stale)), stale))
}
.compat_env <- function(fx) {
  env <- unlist(lapply(fx$option_state$env, as.character))
  stale <- setdiff(grep("^DSOMOP_", names(Sys.getenv()), value = TRUE),
                   c(names(env), "DSOMOP_RESULT_CAPTURE_DIR", "DSOMOP_RESULT_CAPTURE_LABEL",
                     "DSOMOP_RESULT_CAPTURE_PKG"))
  c(env, stats::setNames(rep(NA_character_, length(stale)), stale))
}

.compat_setup <- function(fx, handle, case) {
  setup <- if (!is.null(case[["setup"]])) fx$setups[[case$setup]]
  bins <- setup$issue_bins
  if (!is.null(bins)) {
    .test_issue_safe_bins(handle, breaks = as.numeric(unlist(bins$breaks)), table = bins$table,
                          column = bins$column, concept_id = bins$concept_id,
                          concept_col = bins$concept_col, n_bins = bins$n_bins)
  }
  invisible(NULL)
}

# Run fn() under the case's options and DSOMOP_* environment.
.compat_with <- function(fx, case, fn) {
  env <- .compat_env(fx)
  withr::with_options(.compat_options(fx, case), {
    if (length(env)) withr::with_envvar(env, fn()) else fn()
  })
}

.compat_execute <- function(fx, case, plan) {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .compat_setup(fx, handle, case)
  .compat_with(fx, case, function() tryCatch(
    list(value = suppressWarnings(suppressMessages(.planExecute(handle, plan, .compat_out(plan))))),
    error = function(e) list(error = e)))
}

.compat_prepare <- function(fx, case, plan) {
  .compat_with(fx, case, function() tryCatch({
    suppressWarnings(suppressMessages(.irPrepare(plan)))
    NULL
  }, error = function(e) e))
}

# rows and columns of each returned output (the fixture's `outputs`)
.compat_outputs <- function(value) {
  lapply(value, function(x) {
    if (is.data.frame(x)) list(nrow = nrow(x), columns = names(x))
    else if (is.list(x)) list(components = .compat_or(names(x), character(0)))
    else list(kind = class(x)[[1L]])
  })
}

.compat_baseline_text <- function(case) {
  b <- case$baseline
  if (identical(b$status, "ok")) return("2.10.2 executed it")
  paste0("2.10.2 refused it at the ", b$error$stage, " stage",
         if (isTRUE(b$error$refused_before_query)) " before any query" else " while running",
         ": ", b$error$message)
}

.compat_fx <- jsonlite::fromJSON(testthat::test_path("fixtures", "ir-v1-compat.json"),
                                 simplifyVector = FALSE)
.compat_dispositions <- jsonlite::fromJSON(
  testthat::test_path("fixtures", "ir-v1-compat-dispositions.json"), simplifyVector = FALSE)
.compat_fx$cases <- lapply(.compat_fx$cases, function(case) {
  case$disposition <- .compat_dispositions$cases[[case$id]]
  case
})

test_that("v1 compatibility dispositions name exact cases and reviewed contracts", {
  ids <- vapply(.compat_fx$cases, `[[`, "", "id")
  expect_identical(.compat_dispositions$baseline, .compat_fx$baseline$dsOMOP)
  expect_true(all(names(.compat_dispositions$cases) %in% ids))
  for (d in .compat_dispositions$cases) {
    expect_true(nzchar(d$reference) && nzchar(d$reason))
    expect_true(d$current %in% c("refused", "ok", "unchanged_refusal"))
    if (identical(d$current, "refused")) expect_true(nzchar(d$field))
    if (identical(d$current, "ok")) expect_true(d$result_from %in% ids)
    if (!is.null(d$refusal_from)) {
      expect_true(d$refusal_from %in% ids)
      control <- .compat_fx$cases[[match(d$refusal_from, ids)]]
      expect_identical(d$message, control$baseline$error$message)
    }
  }
})

test_that("the v1 compatibility fixture is a reviewed 2.10.2 capture", {
  fx <- .compat_fx
  expect_identical(fx$version, 2L)
  expect_identical(fx$baseline$dsOMOP, "30b7905617dbe3de5d06699c324095635c68011f")
  expect_identical(fx$baseline$dsOMOPClient, "7eb2f6c8c2b49e5492e1f778916b84319626341d")
  expect_length(fx$baseline$captures, 2L)
  ids <- vapply(fx$cases, `[[`, "", "id")
  expect_false(anyDuplicated(ids) > 0L)
  expect_setequal(unique(vapply(fx$cases, `[[`, "", "section")), c("A", "B", "C", "D", "E", "F"))
  expect_identical(length(fx$cases), fx$counts$cases)
  for (case in fx$cases) {
    info <- case$id
    expect_true(case$baseline$status %in% c("ok", "error"), info = info)
    expect_true(case$ir_expectation %in% c("accept", "refuse", "none"), info = info)
    if (identical(case$baseline$status, "ok")) {
      expect_identical(case$ir_expectation, "accept", info = info)
      expect_true(grepl("^[0-9a-f]{64}$", case$baseline$result_sha256), info = info)
    } else {
      expect_true(case$baseline$error$stage %in% c("plan", "site", "executor", "data", "session"),
                  info = info)
      expect_identical(case$ir_expectation,
                       if (identical(case$baseline$error$stage, "plan")) "refuse" else "none",
                       info = info)
    }
    if (!is.null(case[["setup"]])) expect_false(is.null(fx$setups[[case$setup]]), info = info)
    # the plan object is reconstructed exactly from the transport JSON
    expect_identical(.compat_sha(.compat_plan(fx, case)), case$plan_sha256,
                     info = paste(info, "plan reconstruction"))
  }
})

for (.compat_case in .compat_fx$cases) local({
  case <- .compat_case
  name <- if (is.null(case[["title"]])) paste0(case$id, " = ", case$value) else paste0(case$id, ": ", case$title)
  test_that(paste("v1 compat", name), {
    fx <- .compat_fx
    if (case$id %in% unlist(fx$nondeterministic)) skip("nondeterministic at the 2.10.2 baseline")
    plan <- .compat_plan(fx, case)
    disposition <- case[["disposition"]]
    run <- .compat_execute(fx, case, plan)
    info <- paste0(case$id, " (", .compat_baseline_text(case), ")")
    if (is.list(disposition)) {
      # Each changed case has an explicit reviewed structural/publication
      # contract and an outcome assertion; no blanket compatibility waiver.
      if (identical(disposition$current, "refused")) {
        expect_s3_class(run$error, "dsomop_plan_rejection")
        if (!is.null(run$error)) {
          expect_identical(run$error$code, "validation_error")
          location <- c(run$error$field, run$error$errors, conditionMessage(run$error))
          expect_true(any(grepl(disposition$field, location, fixed = TRUE)),
                      info = paste(case$id, "expected refusal field", disposition$field))
        }
      } else if (identical(disposition$current, "unchanged_refusal")) {
        expect_s3_class(run$error, "error")
        if (!is.null(run$error)) {
          expect_match(conditionMessage(run$error), disposition$message, fixed = TRUE)
        }
      } else {
        expect_null(run$error, info = paste(info, "disposition", disposition$reference))
        if (is.null(run$error)) {
          control <- Filter(function(c) identical(c$id, disposition$result_from), fx$cases)[[1L]]
          expect_identical(.compat_sha(run$value), control$baseline$result_sha256,
                           info = paste(info, "independent control", control$id))
        }
        expect_null(.compat_prepare(fx, case, plan))
      }
      return(invisible())
    }
    if (identical(case$baseline$status, "ok")) {
      if (!is.null(run$error)) {
        fail(paste0(info, "; the current code refuses it: ", conditionMessage(run$error)))
      } else {
        got <- .compat_sha(run$value)
        if (identical(got, case$baseline$result_sha256)) {
          succeed()
        } else {
          fail(paste0(info, "; the lossless canonical result differs (sha256 ", substr(got, 1L, 12L),
                      ", baseline ", substr(case$baseline$result_sha256, 1L, 12L), "); outputs ",
                      .compat_json(.compat_outputs(run$value)), ", baseline ",
                      .compat_json(case$baseline$outputs),
                      "; tools/ws2/capture_compat.R run + diff recomputes the full canonical forms"))
        }
      }
    } else {
      expect_false(is.null(run$error),
                   info = paste0(info, "; the current code executes it"))
      if (isTRUE(case[["typed_refusal"]]) && !is.null(run$error)) {
        expect_s3_class(run$error, "dsomop_plan_rejection")
      }
    }
    # the plan-level expectation (no handle)
    if (!identical(case$ir_expectation, "none")) {
      ir <- .compat_prepare(fx, case, plan)
      if (identical(case$ir_expectation, "accept")) {
        expect_null(ir, info = paste0(info, "; .irPrepare(plan) refuses it: ",
                                      if (!is.null(ir)) conditionMessage(ir)))
      } else {
        expect_s3_class(ir, "dsomop_plan_rejection")
      }
    }
  })
})
rm(.compat_case)
