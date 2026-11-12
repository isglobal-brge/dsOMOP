# WS-2 acceptance, review B10: the A-19 evidence (CR-19, approved 12 Nov 2026).
#
# Each test block below runs one or more v1 plan calls on a DECLARED,
# deterministic fixture (a fresh create_test_handle() plus the rows and handle
# fields this file declares; never a test's in-place insert) and compares them
# with fixtures/ir-v1-results-a19.json: the canonical records of the same blocks
# captured at the 2.10.2 baseline (30b7905) in two independent runs that agreed
# at 17 significant digits (`tools/ws2/compare_results.py capture --side baseline
# --add-tests tests/testthat/test-ir-results-a19.R`, then `pin-a19`). The
# canonical form and its volatile-field rules are the shared `b10-canonical`
# block (lossless: 17-digit doubles; staged descriptors compared field by field
# except the staging token, the tempdir/tempfile parts of `metadata$file` and
# `expires_at`; staged files by content and byte hash; person tokens by
# first-appearance linkage within each output or binding and the hash of their
# raw values). Every call also checks that the fixture content equals the
# baseline call's (`db_sha256`).
#
# On the 2.10.2 build (compare_results.py validate --side baseline) every call
# must reproduce its pin. On the current code each item asserts its reviewed
# A-19 disposition:
#   item 2  recurrent survival plan on an admissible-support fixture (every
#           final component >= nfilter.subset persons), memory and staged:
#           identical results (the `#admissible` safe row);
#   item 3  DP preparation through omopDpPlanExecuteDS() (a fixed noise root,
#           so that the capsule is deterministic): the prepared table's values
#           are identical and sealed in both versions, only
#           `dsomop_dp_provenance` differs, and only in its producer-bound
#           fields (2.10.3 seals the publication candidate under its component
#           name, producer ".../ages" where 2.10.2 wrote ".../primary"; the
#           lineage_id and mac derive from it); plan loading stays DP-free and
#           identical; a baseline age_at_index is refused in both versions, now
#           with the typed reason not_person_local and without leaving a binding
#           (2.10.2 left the unsealed table assigned);
#   item 4  streamed event_select, staged: with a requested root identical
#           descriptor and file content (`#requested_roots`, safe); without
#           one 2.10.3 publishes nothing (`#no_requested_roots`, corrected);
#   item 5  A-E14: an output over the unrestricted `base` of a multi-population
#           plan, memory and staged, at nfilter.subset 0 (2.10.2: no person)
#           and 3 (2.10.2: refused): every person in scope, equal to the
#           single-population plan's result; the output over the other
#           population is unchanged; population resolution nobody -> every
#           person;
#   item 8  a custom filter on person_id: non-strict 2.10.2 warned and
#           returned no `tte`; 2.10.3 refuses with the typed validation_error
#           naming /outputs/tte/filters/custom (through omopPlanExecuteDS() as
#           the bounded status object: refused, rejected$tte$code
#           validation_error, nothing assigned); strict: refused in both.
# The last block checks that v1-corpus-classes.csv carries the A-19
# dispositions of the rows these fixtures evidence.

# >>> b10-canonical: verbatim copy of the block in tools/ws2/capture_results.R
# (compare_results.py checks the copies agree)
.b10_or <- function(a, b) if (is.null(a)) b else a
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
# 17 significant digits round-trip every IEEE-754 double: lossless.
.b10_canon <- function(x, digits = 17L) {
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
    out <- list(kind = "atomic", class = I(class(x)), type = typeof(x),
                length = length(x), levels = lev, values = I(vals),
                names = if (is.null(names(x))) NULL else I(as.character(names(x))),
                attrs = .b10_attrs(x, c("names", "class", "levels"), digits))
    # ISO formatting discards fractional Date days and submicrosecond POSIXct
    # values. Keep their complete numeric payload alongside readable text.
    if (inherits(x, "Date") || inherits(x, "POSIXct")) {
      out$numeric_payload <- I(.b10_num(x, 17L))
    }
    return(out)
  }
  list(kind = "other", class = I(class(x)), type = typeof(x))
}
.b10_sha <- function(txt) {
  unclass(as.character(openssl::sha256(charToRaw(enc2utf8(paste(txt, collapse = "\n"))))))
}
# Content fingerprint of an SQLite fixture: every main-schema table, its rows
# as text (doubles to 17 significant digits) sorted, with the column classes.
.b10_col_text <- function(v) {
  if (inherits(v, "blob") || is.list(v)) {
    return(vapply(v, function(b) if (is.null(b)) "\001NA" else paste(as.character(b), collapse = ""), ""))
  }
  s <- if (is.double(v) && !inherits(v, "Date")) sprintf("%.17g", v) else as.character(v)
  s[is.na(v)] <- "\001NA"
  s
}
.b10_table_sha <- function(conn, schema, tbl) {
  q <- sprintf('SELECT * FROM "%s"."%s"', schema, gsub('"', '""', tbl))
  df <- DBI::dbGetQuery(conn, q)
  hdr <- paste(names(df), vapply(df, function(v) paste(class(v), collapse = "/"), ""), sep = ":", collapse = "|")
  rows <- if (ncol(df)) do.call(paste, c(lapply(df, .b10_col_text), sep = "\037")) else character(0)
  rows <- sort(rows, method = "radix")
  list(n = nrow(df), sha = substr(.b10_sha(c(hdr, rows)), 1L, 24L))
}
.b10_db_tables <- function(conn) {
  main <- DBI::dbGetQuery(conn, paste("SELECT name FROM sqlite_master WHERE type = 'table'",
                                      "AND name NOT LIKE 'sqlite_%' ORDER BY name"))$name
  tabs <- lapply(main, function(t) .b10_table_sha(conn, "main", t))
  names(tabs) <- main
  list(sha256 = .b10_sha(paste(main, vapply(tabs, `[[`, "", "sha"),
                               vapply(tabs, function(x) as.character(x$n), ""))),
       tables = tabs)
}
# A staged descriptor's file(s): content (CSV as text columns, Parquet through
# arrow) and the sha256 of the bytes.
.b10_read_staged <- function(desc) {
  f <- tryCatch(desc$metadata$file, error = function(e) NULL)
  if (!is.character(f) || length(f) != 1L || is.na(f)) return(list(state = "no-file"))
  files <- if (dir.exists(f)) sort(list.files(f, recursive = TRUE, full.names = TRUE), method = "radix") else f
  files <- files[file.exists(files)]
  if (!length(files)) return(list(state = "missing"))
  fmt <- tolower(.b10_or(desc$metadata$format, tools::file_ext(files[[1L]])))
  bytes <- .b10_sha(vapply(files, function(p) unclass(as.character(openssl::sha256(file(p)))), ""))
  frames <- lapply(files, function(p) tryCatch({
    if (grepl("csv", fmt) || grepl("\\.csv$", p)) {
      utils::read.csv(p, colClasses = "character", check.names = FALSE,
                      na.strings = character(0), stringsAsFactors = FALSE)
    } else if (requireNamespace("arrow", quietly = TRUE)) {
      as.data.frame(arrow::read_parquet(p))
    } else NULL
  }, error = function(e) NULL))
  if (any(vapply(frames, is.null, TRUE))) {
    return(list(state = "unreadable", format = fmt, n_files = length(files), bytes_sha256 = bytes))
  }
  list(state = "read", format = fmt, n_files = length(files), bytes_sha256 = bytes,
       content = if (length(frames) == 1L) frames[[1L]] else do.call(rbind, frames))
}
# Every staged descriptor -> list(descriptor, staged_file)
.b10_attach_staged <- function(x) {
  if (inherits(x, "FlowerDatasetDescriptor")) {
    return(structure(list(descriptor = unclass(x), staged_file = .b10_read_staged(x)),
                     class = c("b10_staged", class(x))))
  }
  if (is.list(x) && !is.data.frame(x)) {
    a <- attributes(x)
    out <- lapply(x, .b10_attach_staged)
    attributes(out) <- a
    return(out)
  }
  x
}
# The bindings `names` of an environment (staged descriptors attached), in C order.
.b10_bindings <- function(env, names) {
  names <- sort(as.character(names), method = "radix")
  vals <- lapply(names, function(n) .b10_attach_staged(get(n, envir = env, inherits = FALSE)))
  names(vals) <- names
  vals
}
# The canonical form of a returned or assigned value with its volatile parts
# neutralised (see the header of tools/ws2/capture_results.R): list(tree, volatile).
.b10_value <- function(x) {
  tmp_prefix <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  pid_rx <- paste0("(?<=_)", Sys.getpid(), "(?![0-9A-Za-z])")
  token_rx <- "stg_[0-9a-f]{32}"
  time_rx <- "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:?[0-9]{2})?"
  log <- new.env(parent = emptyenv())
  tokens <- new.env(parent = emptyenv())
  tokens$map <- character(0)
  tokens$raw <- list()
  count <- function(what, n = 1L) assign(what, .b10_or(log[[what]], 0L) + n, envir = log)
  fix_names <- function(node) {
    if (is.null(node$names)) return(node)
    nn <- as.character(node$names)
    nn2 <- gsub(pid_rx, "<pid>", gsub(token_rx, "<token>", nn), perl = TRUE)
    if (!identical(nn, nn2)) { node$names <- I(nn2); count("names") }
    node
  }
  walk <- function(node, in_data) {
    if (!is.list(node)) return(node)
    if (identical(node$kind, "data.frame")) {
      nm <- as.character(node$names)
      for (j in seq_along(node$columns)) {
        col <- node$columns[[j]]
        if (tolower(nm[[j]]) %in% c("person_id", "subject_id") &&
            identical(col$kind, "atomic") && identical(col$type, "character")) {
          v <- as.character(col$values)
          tokens$raw[[length(tokens$raw) + 1L]] <- v
          new <- setdiff(unique(v[!is.na(v)]), names(tokens$map))
          if (length(new)) {
            tokens$map <- c(tokens$map, stats::setNames(paste0("<p", length(tokens$map) + seq_along(new), ">"), new))
          }
          out <- unname(tokens$map[v])
          out[is.na(v)] <- NA_character_
          col$values <- I(out)
          count("person_tokens")
          node$columns[[j]] <- col
        } else {
          node$columns[[j]] <- walk(col, TRUE)
        }
      }
      if (!is.null(node$attrs)) node$attrs <- walk(node$attrs, FALSE)
      return(node)
    }
    if (identical(node$kind, "atomic") && identical(node$type, "character")) {
      v <- as.character(node$values)
      if (!length(v)) return(node)
      orig <- v
      v <- gsub(token_rx, "<token>", v)
      v <- gsub(tmp_prefix, "<tempdir>", v, fixed = TRUE)
      under_tmp <- !is.na(v) & startsWith(v, "<tempdir>")
      if (any(under_tmp)) {
        v[under_tmp] <- gsub("(/[^/<>]*?)[0-9a-f]{10,}(?=/|$)", "\\1<rand>", v[under_tmp], perl = TRUE)
      }
      if (!in_data) {
        v <- gsub(time_rx, "<timestamp>", v)
        v <- gsub(pid_rx, "<pid>", v, perl = TRUE)
      }
      ch <- !is.na(orig) & orig != v
      if (any(ch)) {
        count("strings", sum(ch))
        node$values <- I(v)
      }
      return(fix_names(node))
    }
    if (identical(node$kind, "list")) node <- fix_names(node)
    for (k in names(node)) {
      if (k %in% c("kind", "type", "length", "nrow")) next
      if (is.list(node[[k]])) node[[k]] <- walk(node[[k]], in_data)
    }
    if (is.null(names(node)) && length(node)) {
      for (i in seq_along(node)) if (is.list(node[[i]])) node[[i]] <- walk(node[[i]], in_data)
    }
    node
  }
  tree <- .b10_canon(x, 17L)
  if (identical(tree$kind, "list")) {
    # a list of outputs / bindings / populations: person tokens are numbered
    # within each element, so a change confined to one output never renumbers
    # another (the raw token hash below still covers every token, in order)
    tree <- fix_names(tree)
    for (i in seq_along(tree$items)) {
      tokens$map <- character(0)
      tree$items[[i]] <- walk(tree$items[[i]], FALSE)
    }
    if (!is.null(tree$attrs)) {
      tokens$map <- character(0)
      tree$attrs <- walk(tree$attrs, FALSE)
    }
  } else {
    tree <- walk(tree, FALSE)
  }
  list(tree = tree,
       volatile = c(as.list(log, sorted = TRUE), list(
         raw_token_sha256 = if (length(tokens$raw)) .b10_sha(unlist(tokens$raw)) else NULL)))
}
# A canonical form as it reads back from a JSON fixture.
.b10_roundtrip <- function(x) {
  jsonlite::fromJSON(as.character(jsonlite::toJSON(
    x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, force = TRUE)),
    simplifyVector = FALSE)
}
# Rebuild an R object from its canonical form (exact: 17-digit doubles).
.b10_decode <- function(n) {
  if (is.null(n) || identical(n$kind, "null")) return(NULL)
  cls <- as.character(unlist(n$class))
  text <- function(v) vapply(v, function(e) if (is.null(e)) NA_character_ else as.character(e), "")
  finish <- function(x) {
    if (!is.null(n$names)) names(x) <- text(n$names)
    for (a in names(n$attrs)) attr(x, a) <- .b10_decode(n$attrs[[a]])
    if (!identical(class(x), cls)) class(x) <- cls
    x
  }
  if (identical(n$kind, "atomic")) {
    v <- text(n$values)
    x <- if ("factor" %in% cls) {
      factor(v, levels = text(n$levels))
    } else if ("Date" %in% cls || "POSIXct" %in% cls) {
      if (is.null(n$numeric_payload)) stop("date pin lacks its lossless numeric payload")
      as.numeric(text(n$numeric_payload))
    } else switch(n$type,
      logical = as.logical(v), integer = as.integer(v), double = as.numeric(v),
      character = v, raw = as.raw(strtoi(v, 16L)),
      stop("cannot decode a canonical ", n$type, " vector"))
    return(finish(x))
  }
  if (identical(n$kind, "list")) return(finish(lapply(n$items, .b10_decode)))
  if (identical(n$kind, "data.frame")) {
    x <- lapply(n$columns, .b10_decode)
    names(x) <- text(n$names)
    attr(x, "row.names") <- if (is.null(n$row_names)) .set_row_names(as.integer(n$nrow)) else text(n$row_names)
    for (a in names(n$attrs)) attr(x, a) <- .b10_decode(n$attrs[[a]])
    class(x) <- cls
    return(x)
  }
  stop("cannot decode a canonical '", n$kind, "' node")
}
# Staged files written by a replayed call (live, not neutralised descriptors).
.b10_staged_dirs <- function(x) {
  if (inherits(x, "FlowerDatasetDescriptor")) {
    f <- tryCatch(x$metadata$file, error = function(e) NULL)
    return(if (is.character(f) && length(f) == 1L && !is.na(f)) dirname(f) else character(0))
  }
  if (is.list(x) && !is.data.frame(x)) return(unique(unlist(lapply(x, .b10_staged_dirs))))
  character(0)
}
# Replay tests: output by output, then the complete canonical forms (testthat).
.b10_expect_result <- function(got, want, info) {
  expect_identical(got$names, want$names, info = paste(info, "output names and order"))
  got_items <- stats::setNames(got$items, vapply(.b10_or(got$names, list()), as.character, ""))
  for (i in seq_along(want$items)) {
    nm <- as.character(want$names[[i]])
    w <- want$items[[i]]
    g <- got_items[[nm]]
    at <- paste(info, "output", nm)
    if (is.null(g)) {
      fail(paste(at, "is missing"))
      next
    }
    expect_identical(g$kind, w$kind, info = paste(at, "structure"))
    expect_identical(g$class, w$class, info = paste(at, "class"))
    if (identical(w$kind, "data.frame")) {
      by_column <- function(x, f) {
        stats::setNames(lapply(x$columns, f), vapply(x$names, as.character, ""))
      }
      expect_identical(g$names, w$names, info = paste(at, "column names"))
      expect_identical(by_column(g, function(col) col[c("class", "type", "levels")]),
                       by_column(w, function(col) col[c("class", "type", "levels")]),
                       info = paste(at, "column types"))
      expect_identical(g$nrow, w$nrow, info = paste(at, "row count"))
      expect_identical(by_column(g, function(col) col$values), by_column(w, function(col) col$values),
                       info = paste(at, "values in returned row order"))
    } else if (identical(w$kind, "list")) {
      expect_identical(g$names, w$names, info = paste(at, "component inventory"))
      if ("b10_staged" %in% unlist(w$class)) {
        gi <- stats::setNames(g$items, vapply(g$names, as.character, ""))
        wi <- stats::setNames(w$items, vapply(w$names, as.character, ""))
        expect_identical(gi$descriptor, wi$descriptor,
                         info = paste(at, "staged descriptor (volatile fields neutralised)"))
        expect_identical(gi$staged_file, wi$staged_file, info = paste(at, "staged file content and bytes"))
      }
    }
    expect_identical(g, w, info = paste(at, "complete canonical form"))
  }
  expect_identical(got, want, info = paste(info, "complete canonical result"))
}
# <<< b10-canonical

.a19_file <- "test-ir-results-a19.R"
.a19_baseline_build <- function() !exists(".irPrepare", mode = "function")
.a19_pin <- function(desc) {
  path <- testthat::test_path("fixtures", "ir-v1-results-a19.json")
  if (!file.exists(path)) return(NULL)
  fx <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fx$pins[[paste0("dsOMOP:", .a19_file, ":", desc)]]
}

# ---- declared fixtures ------------------------------------------------------

# A resource-scoped person key, as the staging, survival and DP tests declare
# it (set_key: also the raw handle key the staging helper sets).
.a19_scoped_key <- function(handle, label, identity, set_key = FALSE,
                            .local_envir = parent.frame()) {
  key <- .testPseudonymKey(label)
  resource_id <- substr(as.character(openssl::sha256(charToRaw(identity))), 1L, 32L)
  environment <- paste(format(key), collapse = "")
  names(environment) <- paste0("DSOMOP_PSEUDONYM_KEY_", resource_id)
  withr::local_envvar(c(
    environment, DSOMOP_PSEUDONYM_PROVIDER = "scoped", DSOMOP_PSEUDONYM_EPOCH = "1",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false", DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = ""
  ), .local_envir = .local_envir)
  withr::local_options(list(
    dsomop.pseudonym_provider = NULL, dsomop.pseudonym_epoch = NULL,
    dsomop.pseudonym_require_existing = NULL, dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = NULL
  ), .local_envir = .local_envir)
  if (set_key) handle$person_key <- key
  handle$person_key_identity <- identity
  handle$person_key_id <- .personKeyId(key)
  handle$person_key_provider <- "scoped"
  handle$person_key_epoch <- 1L
  handle$person_key_require_existing <- FALSE
  handle$person_key_contract_version <- 1L
  invisible(key)
}

# The DP service of test-privacy-dp.R's .dp_local_state(), with a FIXED noise
# root so that the provenance capsule is deterministic and comparable.
.a19_dp_state <- function(.local_envir = parent.frame()) {
  state <- withr::local_tempdir(pattern = "dsomop-dp-", .local_envir = .local_envir)
  Sys.chmod(state, mode = "0700")
  state <- normalizePath(state, winslash = "/", mustWork = TRUE)
  names <- c("DSOMOP_DP_ENABLED", "DSOMOP_DP_DOMAIN", "DSOMOP_DP_SNAPSHOT_ID",
             "DSOMOP_DP_RELEASE_EPSILON", "DSOMOP_DP_PRIVACY_EPOCH",
             "DSOMOP_DP_MAX_LEVELS", "DSOMOP_DP_MAX_CONTRIBUTIONS",
             "DSOMOP_DP_NUMERIC_GRID", "DSOMOP_DP_NOISE_ROOT")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state, DSOMOP_MARKER_DIR = NA_character_,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    stats::setNames(rep.int(NA_character_, length(names)), names)
  ), .local_envir = .local_envir)
  withr::local_options(list(
    dsomop.state_dir = state, dsomop.dp.enabled = TRUE,
    dsomop.dp.domain = "dsomop-dp-test", dsomop.dp.snapshot_id = "etl-2026-08-01",
    dsomop.dp.release_epsilon = 0.1, dsomop.dp.privacy_epoch = 1L,
    dsomop.dp.noise_root = as.raw(0:31), dsomop.dp.max_levels = 1000L,
    dsomop.dp.max_contributions = 10L, dsomop.dp.numeric_grid = 65535L
  ), .local_envir = .local_envir)
  previous <- list(status = .pkg_state$dp_status, runtime = .pkg_state$dp_runtime,
                   in_progress = .pkg_state$dp_bootstrap_in_progress)
  withr::defer({
    .pkg_state$dp_status <- previous$status
    .pkg_state$dp_runtime <- previous$runtime
    .pkg_state$dp_bootstrap_in_progress <- previous$in_progress
  }, envir = .local_envir)
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
  invisible(state)
}

# Item 2: the default fixture (n = 15) plus three declared myocardial
# infarction events before death (cohort 10 = persons 1..12, death 2021-03-15):
# a recurrent second event for person 1 and first events for persons 3 and 9.
# The legacy sparse `events` component then holds persons 1, 3, 5 and 9 and the
# risk sets twelve persons: every final component >= nfilter.subset = 3.
.a19_recurrent_events <- data.frame(
  condition_occurrence_id = c(990001L, 990002L, 990003L),
  person_id = c(1L, 3L, 9L),
  condition_concept_id = rep(4000002L, 3L),
  condition_start_date = c("2021-01-10", "2020-11-02", "2021-02-01"),
  condition_end_date = c("2021-01-10", "2020-11-02", "2021-02-01"),
  condition_type_concept_id = rep(44818518L, 3L),
  visit_occurrence_id = rep(NA_integer_, 3L),
  stringsAsFactors = FALSE
)
.a19_recurrent_plan <- function() structure(list(
  cohort = list(type = "cohort_table", cohort_definition_id = 10L),
  outputs = list(recurrent = list(
    type = "survival",
    outcomes = list(mi = list(table = "condition_occurrence", concept_set = 4000002L)),
    tar = list(start_offset = 0L, end_offset = 730L),
    format = "recurrent_events", event_order = "all", washout_days = 0L,
    tie_policy = "priority"
  )),
  options = list(translate_concepts = FALSE, block_sensitive = TRUE)
), class = c("omop_plan", "list"))

# Item 3: the plans of test-privacy-dp.R ("DP preparation seals plan outputs
# while plan loading stays DP-free").
.a19_dp_ages_plan <- function() list(
  outputs = list(ages = list(
    type = "person_level", tables = list(), representation = "features",
    derived_columns = list(age_at_2025 = list(
      kind = "age", name = "age_at_2025", reference = "today",
      reference_date = "2025-07-01"
    )),
    population_id = "base", options = list()
  )),
  populations = list(base = list(id = "base", label = "All Persons", kind = "criteria")),
  options = list(translate_concepts = TRUE, block_sensitive = TRUE, factor_concepts = TRUE),
  cohort = NULL, scope = NULL
)
.a19_dp_baseline_plan <- function() list(
  cohort = list(type = "spec", spec = list(type = "condition", concept_set = 201820)),
  anchor = list(table = "person", id_col = "person_id"),
  outputs = list(baseline = list(
    type = "baseline", columns = "gender_concept_id", derived = "age_at_index"
  )),
  options = list(translate_concepts = FALSE, block_sensitive = TRUE)
)

# Item 4: the plan of test-plan.R ("streamed event_select keeps an omitted
# ordering date private").
.a19_event_select_plan <- function() list(outputs = list(first_condition = list(
  type = "event_level", table = "condition_occurrence", columns = "condition_concept_id",
  representation = list(format = "long"),
  temporal = list(event_select = list(order = "first", n = 1L)), filters = list()
)), options = list(translate_concepts = FALSE, block_sensitive = TRUE))

# Item 5: an output over the unrestricted base next to a declared population;
# the same base output in a single-population plan; the declared population's
# output alone.
.a19_person_level <- function(population = NULL) {
  out <- list(type = "person_level", tables = list(person = c("person_id", "gender_concept_id")))
  if (!is.null(population)) out$population_id <- population
  out
}
.a19_populations <- function() list(
  base = list(id = "base"),
  women = list(id = "women", kind = "criteria",
               filter_tree = list(and = list(list(type = "sex", params = list(value = "F")))))
)
.a19_multi_plan <- function() structure(list(
  populations = .a19_populations(),
  outputs = list(everyone = .a19_person_level("base"), women_out = .a19_person_level("women")),
  options = list(translate_concepts = FALSE, block_sensitive = TRUE)
), class = c("omop_plan", "list"))
.a19_single_plan <- function() structure(list(
  outputs = list(everyone = .a19_person_level()),
  options = list(translate_concepts = FALSE, block_sensitive = TRUE)
), class = c("omop_plan", "list"))
.a19_women_plan <- function() structure(list(
  populations = .a19_populations(),
  outputs = list(women_out = .a19_person_level("women")),
  options = list(translate_concepts = FALSE, block_sensitive = TRUE)
), class = c("omop_plan", "list"))

# Item 8: the plan of test-output-contracts.R ("survival custom filter on an
# identifier column is rejected").
.a19_identifier_plan <- function() structure(list(
  cohort = list(type = "cohort_table", cohort_definition_id = 1),
  outputs = list(tte = list(
    type = "survival",
    outcome = list(table = "condition_occurrence", concept_set = c(4000002)),
    tar = list(start_offset = 0, end_offset = 730),
    filters = list(custom = list(var = "person_id", op = "in", value = list(1, 5)))
  )),
  options = list(translate_concepts = FALSE, block_sensitive = TRUE)
), class = c("omop_plan", "list"))

# ---- one call, recorded in the capture's form ---------------------------------

# Evaluates `expr` (a quoted call; in `target` for the assigning entry points)
# and returns its record: the fixture content hash before the call, the
# outcome, the warnings, the canonical result and, for an assigning call, the
# bindings it created. Staged files are removed after they were read.
.a19_call <- function(handle, expr, target = NULL) {
  db <- .b10_db_tables(handle$conn)$sha256
  before <- if (is.null(target)) character(0) else ls(target, all.names = TRUE)
  warnings <- character(0)
  value <- withCallingHandlers(
    tryCatch(eval(expr, envir = .b10_or(target, parent.frame())), error = function(e) e),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  rec <- list(db_sha256 = db, warnings = warnings)
  dirs <- character(0)
  if (inherits(value, "error")) {
    rec$outcome <- list(status = "error", class = class(value), message = conditionMessage(value),
                        code = tryCatch(if (is.character(value$code)) value$code else NULL,
                                        error = function(e) NULL))
  } else {
    rec$outcome <- list(status = "ok")
    v <- .b10_value(.b10_attach_staged(value))
    rec$result <- .b10_roundtrip(v$tree)
    rec$volatile <- v$volatile
    rec$value <- value
    # each output on its own (person tokens numbered within the output), so that
    # outputs of different calls can be compared
    if (is.list(value) && !is.data.frame(value) && !is.null(names(value)) &&
        !inherits(value, "dsomop_plan_status")) {
      rec$outputs <- lapply(value, function(x) {
        o <- .b10_value(.b10_attach_staged(x))
        list(tree = .b10_roundtrip(o$tree), raw_token_sha256 = o$volatile$raw_token_sha256)
      })
    }
    dirs <- .b10_staged_dirs(value)
  }
  if (!is.null(target)) {
    created <- sort(setdiff(ls(target, all.names = TRUE), before), method = "radix")
    a <- .b10_value(.b10_bindings(target, created))
    rec$assigned <- list(created = created, values = .b10_roundtrip(a$tree))
    rec$assigned_volatile <- a$volatile
    rec$bound <- mget(created, envir = target)
    dirs <- c(dirs, .b10_staged_dirs(rec$bound))
  }
  unlink(unique(dirs), recursive = TRUE)
  rec
}

# A canonical tree's element at a path of names (NULL when absent).
.a19_at <- function(node, ...) {
  for (nm in c(...)) {
    if (!is.list(node)) return(NULL)
    if (identical(node$kind, "list") || identical(node$kind, "data.frame")) {
      names <- vapply(.b10_or(node$names, list()), as.character, "")
      items <- if (identical(node$kind, "list")) node$items else node$columns
      node <- if (nm %in% names) items[[match(nm, names)]] else NULL
    } else {
      node <- node[[nm]]
    }
  }
  node
}
# A canonical tree without every list element named `field` (at any depth).
.a19_drop_field <- function(node, field) {
  if (!is.list(node)) return(node)
  if (identical(node$kind, "list") && !is.null(node$names)) {
    keep <- vapply(node$names, as.character, "") != field
    node$items <- node$items[keep]
    node$names <- node$names[keep]
  }
  for (k in names(node)) if (is.list(node[[k]])) node[[k]] <- .a19_drop_field(node[[k]], field)
  if (is.null(names(node)) && length(node)) node[] <- lapply(node, .a19_drop_field, field = field)
  node
}
.a19_drop_attr <- function(node, attr) {
  if (is.list(node$attrs)) node$attrs[[attr]] <- NULL
  if (!length(node$attrs)) node$attrs <- NULL
  node
}

# The record of a call against its pin: outcome, warnings, fixture, result or
# assigned bindings, raw person tokens.
.a19_expect_same <- function(live, pin, info) {
  expect_identical(live$db_sha256, pin$db_sha256, info = paste(info, "declared fixture content"))
  expect_identical(live$outcome$status, pin$outcome$status, info = paste(info, "outcome"))
  if (identical(pin$outcome$status, "error")) {
    expect_identical(live$outcome$message, pin$outcome$message, info = paste(info, "refusal message"))
  }
  expect_identical(as.list(live$warnings), pin$warnings, info = paste(info, "warnings"))
  if (!is.null(pin$assigned)) {
    expect_identical(as.list(live$assigned$created), pin$assigned$created, info = paste(info, "assigned symbols"))
    .b10_expect_result(live$assigned$values, pin$assigned$values, paste(info, "assigned"))
    expect_identical(live$assigned_volatile$raw_token_sha256, pin$assigned_volatile$raw_token_sha256,
                     info = paste(info, "raw person tokens"))
  } else if (identical(pin$outcome$status, "ok")) {
    .b10_expect_result(live$result, pin$result, info)
    expect_identical(live$volatile$raw_token_sha256, pin$volatile$raw_token_sha256,
                     info = paste(info, "raw person tokens"))
  }
}

# The pin of each call of a block; on the 2.10.2 build every call reproduces it.
.a19_pins_for <- function(desc, n) {
  pins <- .a19_pin(desc)
  if (is.null(pins)) skip(paste("no baseline pin for", desc))
  expect_identical(length(pins), as.integer(n), info = paste(desc, "number of captured calls"))
  pins
}
.a19_baseline_reproduces <- function(desc, live) {
  pins <- .a19_pins_for(desc, length(live))
  for (i in seq_along(live)) .a19_expect_same(live[[i]], pins[[i]], paste(desc, "call", i))
  invisible(pins)
}

.a19_staging <- function(prefix, .local_envir = parent.frame()) {
  base <- tempfile(prefix)
  withr::defer(unlink(base, recursive = TRUE), envir = .local_envir)
  base
}

# ---- item 2: recurrent survival plan on an admissible-support fixture ---------

.a19_recurrent_run <- function(mode) {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  DBI::dbAppendTable(handle$conn, "condition_occurrence", .a19_recurrent_events)
  .a19_scoped_key(handle, "survival-resource", "test://resource-scoped/survival")
  .setHandle("b10_a19_recurrent", handle)
  on.exit(.removeHandle("b10_a19_recurrent"), add = TRUE)
  plan <- .a19_recurrent_plan()
  withr::local_options(list(nfilter.subset = 3L))
  .a19_call(handle, bquote(omopPlanExecuteDS("b10_a19_recurrent", .(plan),
                                             c(recurrent = "recurrent_tbl"), output_mode = .(mode))),
            new.env())
}
# every final component holds >= nfilter.subset = 3 persons
.a19_expect_admissible <- function(rec) {
  expect_identical(rec$outcome$status, "ok")
  expect_setequal(rec$assigned$created, c("recurrent_tbl.events", "recurrent_tbl.riskSets"))
  for (component in rec$assigned$created) {
    x <- rec$bound[[component]]
    persons <- if (inherits(x, "FlowerDatasetDescriptor")) {
      length(unique(unlist(.a19_at(rec$assigned$values, component, "staged_file", "content", "person_id")$values)))
    } else length(unique(x$person_id))
    expect_gte(persons, 3L)
  }
}

test_that("A-19 item 2: recurrent plan on an admissible-support fixture, memory", {
  rec <- .a19_recurrent_run("memory")
  .a19_expect_admissible(rec)
  .a19_baseline_reproduces("A-19 item 2: recurrent plan on an admissible-support fixture, memory", list(rec))
})

test_that("A-19 item 2: recurrent plan on an admissible-support fixture, staged", {
  rec <- .a19_recurrent_run("staged")
  .a19_expect_admissible(rec)
  .a19_baseline_reproduces("A-19 item 2: recurrent plan on an admissible-support fixture, staged", list(rec))
})

# ---- item 3: DP preparation through omopDpPlanExecuteDS() ----------------------

.a19_dp_handle <- function(.local_envir = parent.frame()) {
  handle <- create_test_handle()
  .a19_scoped_key(handle, "dp-prepare", "test://resource-scoped/dp-prepare",
                  .local_envir = .local_envir)
  .setHandle("b10_a19_dp", handle)
  withr::defer({
    .removeHandle("b10_a19_dp")
    cleanup_handle(handle)
  }, envir = .local_envir)
  handle
}

test_that("A-19 item 3: DP preparation seals the plan output, plan loading stays DP-free", {
  desc <- "A-19 item 3: DP preparation seals the plan output, plan loading stays DP-free"
  .a19_dp_state()
  handle <- .a19_dp_handle()
  session <- new.env()
  plan <- .a19_dp_ages_plan()
  loaded <- .a19_call(handle, bquote(omopPlanExecuteDS("b10_a19_dp", .(plan), list(ages = "D_ages"))), session)
  prepared <- .a19_call(handle, bquote(omopDpPlanExecuteDS("b10_a19_dp", .(plan), list(ages = "DP_ages"))), session)
  # in both versions: loading is DP-free, preparation is sealed and keeps the rows
  expect_identical(loaded$outcome$status, "ok")
  expect_identical(prepared$outcome$status, "ok")
  expect_null(attr(loaded$bound$D_ages, "dsomop_dp_provenance", exact = TRUE))
  capsule <- attr(prepared$bound$DP_ages, "dsomop_dp_provenance", exact = TRUE)
  expect_false(is.null(capsule))
  expect_identical(.a19_drop_attr(.a19_at(prepared$assigned$values, "DP_ages"), "dsomop_dp_provenance"),
                   .a19_at(loaded$assigned$values, "D_ages"),
                   info = "the prepared rows are the loaded rows; only the seal differs")
  expect_identical(prepared$assigned_volatile$raw_token_sha256, loaded$assigned_volatile$raw_token_sha256)
  if (.a19_baseline_build()) {
    .a19_baseline_reproduces(desc, list(loaded, prepared))
  } else {
    pins <- .a19_pins_for(desc, 2L)
    .a19_expect_same(loaded, pins[[1L]], paste(desc, "plan loading"))
    expect_identical(prepared$db_sha256, pins[[2L]]$db_sha256, info = "declared fixture content")
    # sealed before commit (design 7.5) and verifiable
    expect_silent(.dsomopDpVerifyPersonLocal(prepared$bound$DP_ages))
    want <- .a19_at(pins[[2L]]$assigned$values, "DP_ages")
    got <- .a19_at(prepared$assigned$values, "DP_ages")
    expect_identical(.a19_drop_attr(got, "dsomop_dp_provenance"), .a19_drop_attr(want, "dsomop_dp_provenance"),
                     info = "values, columns, types and every other attribute identical to 2.10.2")
    expect_identical(prepared$assigned_volatile$raw_token_sha256, pins[[2L]]$assigned_volatile$raw_token_sha256,
                     info = "raw person tokens")
    # Only the producer-bound capsule fields may differ: 2.10.3 seals each
    # publication candidate under its component name ("plan/<type>/<format>/
    # <output>") where 2.10.2 sealed the primary component as ".../primary";
    # lineage_id and mac derive from the producer. The DP contract fields are
    # identical.
    cw <- .b10_decode(want$attrs$dsomop_dp_provenance)
    expect_identical(names(capsule), names(cw))
    stable <- setdiff(names(cw), c("producer", "lineage_id", "mac"))
    expect_identical(capsule[stable], cw[stable], info = "DP contract fields of the capsule")
  }
})

test_that("A-19 item 3: DP preparation refuses a baseline age at index", {
  desc <- "A-19 item 3: DP preparation refuses a baseline age at index"
  .a19_dp_state()
  handle <- .a19_dp_handle()
  session <- new.env()
  plan <- .a19_dp_baseline_plan()
  rec <- .a19_call(handle, bquote(omopDpPlanExecuteDS("b10_a19_dp", .(plan), list(baseline = "DP_base"))), session)
  expect_identical(rec$outcome$status, "error")
  expect_match(rec$outcome$message, "public reference year")
  if (.a19_baseline_build()) {
    .a19_baseline_reproduces(desc, list(rec))
  } else {
    pins <- .a19_pins_for(desc, 1L)
    expect_identical(pins[[1L]]$outcome$status, "error")
    expect_identical(rec$db_sha256, pins[[1L]]$db_sha256, info = "declared fixture content")
    # the preserved person-locality refusal, typed and raised before commit
    expect_true("dsomop_dp_rejection" %in% rec$outcome$class)
    expect_identical(rec$outcome$code, "not_person_local")
    expect_match(rec$outcome$message, "not_person_local")
    expect_length(rec$assigned$created, 0L)
  }
})

# ---- item 4: streamed event_select, staged, with and without a requested root --

.a19_event_select_run <- function(out) {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .a19_scoped_key(handle, "plan-resource", "test://resource-scoped/staging", set_key = TRUE)
  base <- .a19_staging("dsstaging_event_select_")
  plan <- .a19_event_select_plan()
  withr::local_options(list(dsstaging.base_dir = base, nfilter.subset = 3))
  rec <- .a19_call(handle, bquote(.planExecute(.(handle), .(plan), .(out), output_mode = "staged")))
  rec$staged_files <- list.files(base, recursive = TRUE, all.files = TRUE)
  rec
}

test_that("A-19 item 4: streamed event_select, staged, requested root", {
  rec <- .a19_event_select_run(list(first_condition = "first_condition_df"))
  expect_identical(rec$outcome$status, "ok")
  .a19_baseline_reproduces("A-19 item 4: streamed event_select, staged, requested root", list(rec))
})

test_that("A-19 item 4: streamed event_select, staged, no requested root", {
  desc <- "A-19 item 4: streamed event_select, staged, no requested root"
  rec <- .a19_event_select_run(list())
  expect_identical(rec$outcome$status, "ok")
  if (.a19_baseline_build()) {
    .a19_baseline_reproduces(desc, list(rec))
  } else {
    pins <- .a19_pins_for(desc, 1L)
    expect_identical(rec$db_sha256, pins[[1L]]$db_sha256, info = "declared fixture content")
    # 2.10.2's internal .planExecute() returned the unrequested output ...
    expect_false(is.null(.a19_at(pins[[1L]]$result, "first_condition")))
    # ... 2.10.3 publishes nothing without a requested root (design 7.3)
    expect_length(rec$value, 0L)
    expect_length(rec$staged_files, 0L)
  }
})

# ---- item 5 (A-E14): the unrestricted base of a multi-population plan ---------

.a19_population_run <- function(plan, out, mode, k) {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  options <- list(nfilter.subset = k)
  if (identical(mode, "staged")) {
    .a19_scoped_key(handle, "plan-resource", "test://resource-scoped/staging", set_key = TRUE)
    options$dsstaging.base_dir <- .a19_staging("b10_a19_staging_")
  }
  withr::local_options(options)
  .a19_call(handle, bquote(.planExecute(.(handle), .(plan), .(out), output_mode = .(mode))))
}
.a19_population_calls <- function(mode, k) list(
  multi = .a19_population_run(.a19_multi_plan(), list(everyone = "everyone_df", women_out = "women_df"), mode, k),
  single = .a19_population_run(.a19_single_plan(), list(everyone = "everyone_df"), mode, k),
  women = .a19_population_run(.a19_women_plan(), list(women_out = "women_df"), mode, k)
)
.a19_person_count <- function() {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  as.integer(DBI::dbGetQuery(handle$conn, "SELECT COUNT(*) AS n FROM person")$n)
}
.a19_rows <- function(node, mode) {
  if (identical(mode, "memory")) node$nrow else .a19_at(node, "staged_file", "content")$nrow
}
.a19_expect_unrestricted_base <- function(desc, live, mode, k) {
  expect_identical(live$single$outcome$status, "ok")
  expect_identical(live$women$outcome$status, "ok")
  if (.a19_baseline_build()) {
    .a19_baseline_reproduces(desc, unname(live))
  } else {
    pins <- .a19_pins_for(desc, 3L)
    # 2.10.2: the unrestricted base resolved to nobody (A-E14)
    if (k == 0) {
      expect_identical(pins[[1L]]$outcome$status, "ok")
      expect_identical(as.integer(.a19_rows(.a19_at(pins[[1L]]$result, "everyone"), mode)), 0L,
                       info = "2.10.2 returned no person over the unrestricted base")
    } else {
      expect_identical(pins[[1L]]$outcome$status, "error")
      expect_match(pins[[1L]]$outcome$message, "insufficient individuals")
    }
    expect_identical(live$multi$db_sha256, pins[[1L]]$db_sha256, info = "declared fixture content")
    # the single-population plan and the declared population's own output are
    # unchanged, exactly
    .a19_expect_same(live$single, pins[[2L]], paste(desc, "single-population plan"))
    .a19_expect_same(live$women, pins[[3L]], paste(desc, "declared population alone"))
    # 2.10.3: every person in scope, equal to the single-population plan's result
    # (staged: the same descriptor and file except the plan-specific
    # query_semantics_sha256 of the semantic contract)
    expect_identical(live$multi$outcome$status, "ok")
    everyone <- live$multi$outputs$everyone
    single <- live$single$outputs$everyone
    expect_identical(as.integer(.a19_rows(everyone$tree, mode)), .a19_person_count(),
                     info = "every person in scope")
    expect_identical(.a19_drop_field(everyone$tree, "query_semantics_sha256"),
                     .a19_drop_field(single$tree, "query_semantics_sha256"),
                     info = "the unrestricted base output equals the single-population plan's")
    expect_identical(everyone$raw_token_sha256, single$raw_token_sha256,
                     info = "the same raw person tokens")
    women_out <- live$multi$outputs$women_out
    women <- live$women$outputs$women_out
    expect_identical(.a19_drop_field(women_out$tree, "query_semantics_sha256"),
                     .a19_drop_field(women$tree, "query_semantics_sha256"),
                     info = "the declared population's output is unchanged")
    expect_identical(women_out$raw_token_sha256, women$raw_token_sha256)
  }
}

test_that("A-19 item 5: unrestricted base next to a declared population, memory, nfilter.subset 0", {
  .a19_expect_unrestricted_base(
    "A-19 item 5: unrestricted base next to a declared population, memory, nfilter.subset 0",
    .a19_population_calls("memory", 0), "memory", 0)
})

test_that("A-19 item 5: unrestricted base next to a declared population, memory, nfilter.subset 3", {
  .a19_expect_unrestricted_base(
    "A-19 item 5: unrestricted base next to a declared population, memory, nfilter.subset 3",
    .a19_population_calls("memory", 3), "memory", 3)
})

test_that("A-19 item 5: unrestricted base next to a declared population, staged, nfilter.subset 0", {
  .a19_expect_unrestricted_base(
    "A-19 item 5: unrestricted base next to a declared population, staged, nfilter.subset 0",
    .a19_population_calls("staged", 0), "staged", 0)
})

test_that("A-19 item 5: unrestricted base next to a declared population, staged, nfilter.subset 3", {
  .a19_expect_unrestricted_base(
    "A-19 item 5: unrestricted base next to a declared population, staged, nfilter.subset 3",
    .a19_population_calls("staged", 3), "staged", 3)
})

test_that("A-19 item 5: population resolution of the unrestricted base", {
  desc <- "A-19 item 5: population resolution of the unrestricted base"
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  plan <- .a19_multi_plan()
  withr::local_options(list(nfilter.subset = 3))
  rec <- .a19_call(handle, bquote(.planResolvePopulations(.(handle), .(plan), .buildBlueprint(.(handle)))))
  expect_identical(rec$outcome$status, "ok")
  # (the capture records each population's roster too; the base ids decide here)
  base_ids <- rec$value$base$person_ids
  pins <- .a19_pin(desc)
  if (is.null(pins)) skip(paste("no baseline pin for", desc))
  pinned <- .a19_at(pins[[1L]]$result, "base", "person_ids")
  # 2.10.2: integer(0), nobody
  expect_identical(pinned$kind, "atomic")
  expect_identical(pinned$length, 0L)
  if (.a19_baseline_build()) {
    expect_identical(base_ids, integer(0))
  } else {
    # 2.10.3: NULL, every person in scope (as a single-population plan)
    expect_null(base_ids)
  }
})

# ---- item 8: a custom filter on an identifier column --------------------------

.a19_identifier_run <- function(strict, entry = ".planExecute") {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .buildBlueprint(handle)
  plan <- .a19_identifier_plan()
  withr::local_options(list(nfilter.subset = 3, dsomop.query_strict = strict))
  if (identical(entry, ".planExecute")) {
    return(.a19_call(handle, bquote(.planExecute(.(handle), .(plan), list(tte = "tte_df")))))
  }
  # the assign path pseudonymises: declare the staging tests' resource-scoped key
  .a19_scoped_key(handle, "plan-resource", "test://resource-scoped/staging", set_key = TRUE)
  .setHandle("b10_a19_identifier", handle)
  on.exit(.removeHandle("b10_a19_identifier"), add = TRUE)
  .a19_call(handle, bquote(omopPlanExecuteDS("b10_a19_identifier", .(plan), c(tte = "tte_df"))), new.env())
}
.a19_expect_typed_identifier_refusal <- function(rec) {
  expect_identical(rec$outcome$status, "error")
  expect_true("dsomop_ir_validation_error" %in% rec$outcome$class)
  expect_identical(rec$outcome$code, "validation_error")
  expect_match(rec$outcome$message, "/outputs/tte/filters/custom", fixed = TRUE)
  expect_match(rec$outcome$message, "not permitted")
}

test_that("A-19 item 8: identifier filter, non-strict", {
  desc <- "A-19 item 8: identifier filter, non-strict"
  rec <- .a19_identifier_run(FALSE)
  if (.a19_baseline_build()) {
    expect_identical(rec$outcome$status, "ok")
    expect_null(rec$value$tte)
    .a19_baseline_reproduces(desc, list(rec))
  } else {
    pins <- .a19_pins_for(desc, 1L)
    expect_identical(rec$db_sha256, pins[[1L]]$db_sha256, info = "declared fixture content")
    # 2.10.2 dropped the output with a warning and returned no `tte`
    expect_identical(pins[[1L]]$outcome$status, "ok")
    expect_null(.a19_at(pins[[1L]]$result, "tte"))
    expect_match(unlist(pins[[1L]]$warnings), "Plan output 'tte' failed: .*not permitted", all = FALSE)
    # 2.10.3 refuses at validation, in non-strict mode too
    .a19_expect_typed_identifier_refusal(rec)
    expect_length(rec$warnings, 0L)
  }
})

test_that("A-19 item 8: identifier filter, non-strict, omopPlanExecuteDS", {
  desc <- "A-19 item 8: identifier filter, non-strict, omopPlanExecuteDS"
  rec <- .a19_identifier_run(FALSE, "omopPlanExecuteDS")
  if (.a19_baseline_build()) {
    .a19_baseline_reproduces(desc, list(rec))
  } else {
    pins <- .a19_pins_for(desc, 1L)
    expect_identical(rec$db_sha256, pins[[1L]]$db_sha256, info = "declared fixture content")
    # 2.10.2 warned, dropped the root and assigned nothing
    expect_identical(pins[[1L]]$outcome$status, "ok")
    expect_match(unlist(pins[[1L]]$warnings), "not permitted", all = FALSE)
    expect_length(pins[[1L]]$assigned$created, 0L)
    # 2.10.3: the validation refusal reaches the public entry point as the
    # bounded status object (design 7.3): the execution is refused and the root
    # rejected with the typed code (the field path stays server-side), nothing
    # is assigned
    expect_identical(rec$outcome$status, "ok")
    expect_s3_class(rec$value, "dsomop_plan_status")
    expect_identical(rec$value$status, "refused")
    expect_identical(rec$value$rejected$tte$code, "validation_error")
    expect_length(rec$value$published, 0L)
    expect_length(rec$assigned$created, 0L)
    expect_length(rec$warnings, 0L)
  }
})

test_that("A-19 item 8: identifier filter, strict", {
  desc <- "A-19 item 8: identifier filter, strict"
  rec <- .a19_identifier_run(TRUE)
  expect_identical(rec$outcome$status, "error")
  expect_match(rec$outcome$message, "not permitted")
  if (.a19_baseline_build()) {
    .a19_baseline_reproduces(desc, list(rec))
  } else {
    pins <- .a19_pins_for(desc, 1L)
    expect_identical(pins[[1L]]$outcome$status, "error")
    .a19_expect_typed_identifier_refusal(rec)
  }
})

# ---- the classification rows these fixtures evidence (A-19 items 1-6, 8) ------

test_that("v1-corpus-classes.csv carries the A-19 dispositions", {
  skip_if(.a19_baseline_build(), "the 2.10.2 tree predates the frozen classes")
  classes <- utils::read.csv(testthat::test_path("fixtures", "v1-corpus-classes.csv"),
                             stringsAsFactors = FALSE, check.names = FALSE)
  cls <- stats::setNames(classes$class, classes$plan_id)
  oc <- "dsOMOP:test-output-contracts.R:"
  so <- "dsOMOP:test-recipe-setops.R:"
  expected <- c(
    # item 1 (F-26)
    stats::setNames("corrected", paste0(oc, "baseline output produces one row per cohort member#2")),
    stats::setNames("corrected", paste0(oc, "single plan with all output types executes successfully")),
    # open item (ii): r005 has no suppressed band at nfilter.tab = 1;
    # its entire result, including the requested age grid, stays exact.
    stats::setNames("safe", paste0(oc, "baseline accepts only a public common age-grid coarsening#1")),
    # item 2
    stats::setNames(c("newly_refused_publication", "safe"),
                    paste0("dsOMOP:test-survival-sql.R:recurrent plan assignment splits and pseudonymizes both components",
                           c("#below_k", "#admissible"))),
    # item 3
    stats::setNames("corrected", "dsOMOP:test-privacy-dp.R:DP preparation seals plan outputs while plan loading stays DP-free#1"),
    # item 4
    stats::setNames(c("safe", "corrected"),
                    paste0("dsOMOP:test-plan.R:streamed event_select keeps an omitted ordering date private",
                           c("#requested_roots", "#no_requested_roots"))),
    # item 5 (A-E14)
    stats::setNames(rep("corrected", 5L), c(
      paste0(so, "set-op union/intersect/setdiff produce the expected person sets", c("#union", "#intersect", "#setdiff")),
      paste0(so, "scope folds a cohort ref AND a table frame (combine=intersect)"),
      paste0(so, "a scalar plan cohort id NARROWS populations as scope, not as base"))),
    # item 6
    stats::setNames("unchanged_invalid", "dsOMOP:test-cohort.R:cohort creation with inclusion criteria filters persons"),
    # Final per-call audit: private index resolution is not publication;
    # the original narrow-age call lacks a required public reference date.
    stats::setNames("newly_permitted", "dsOMOP:test-plan-index-event.R:index-event populations remain subject to the disclosure gate"),
    stats::setNames(c("unchanged_invalid", "newly_permitted"),
                   paste0("dsOMOP:test-recipe-reach.R:reach SAFETY: narrow age_range blocked while gate is ON",
                          c("#missing_reference", "#explicit_reference"))),
    stats::setNames("unchanged_invalid", "dsOMOP:test-cohort.R:legacy or forged cohort numeric thresholds fail closed#2"),
    # item 8
    stats::setNames(c("unchanged_invalid", "newly_refused_structural"),
                    paste0(oc, "survival custom filter on an identifier column is rejected", c("#strict", "#non_strict")))
  )
  for (id in names(expected)) {
    expect_identical(unname(cls[id]), unname(expected[[id]]), info = id)
  }
  # a block split into sub-rows loses its unsuffixed row
  for (id in unique(sub("#.*$", "", grep("#", names(expected), value = TRUE)))) {
    expect_false(id %in% classes$plan_id, info = paste(id, "unsuffixed row"))
  }
})
