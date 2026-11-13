# WS-2 acceptance, review B10: result fidelity of the safe-class v1 corpus.
#
# Two fixtures pin BASELINE (dsOMOP 2.10.2) canonical results, captured with
# tools/ws2/capture_results.R in two independent runs of the 2.10.2 suites and
# selected by `tools/ws2/compare_results.py pin` (selection rules and provenance
# are recorded in each fixture):
#   fixtures/ir-v1-results.json         safe-class v1 plans the 2.10.2 server
#     suite executed directly with .planExecute() on an unmodified default
#     create_test_handle() fixture, in memory mode;
#   fixtures/ir-v1-results-staged.json  safe-class v1 plans a 2.10.2 test executed
#     with .planExecute() or omopPlanExecuteDS() in STAGED mode with requested
#     roots, on an ADMISSIBLE fixture (every staged component with a person
#     column holds >= nfilter.subset persons) that is rebuilt exactly here.
# Every pin is re-executed with the CURRENT code on a fresh create_test_handle()
# (staged pins: with the recorded person-key handle fields and changed-table rows
# applied, and the rebuilt fixture's content hash checked against the baseline
# call's) under exactly the server options (nfilter.* / dsomop.* / dsstaging.* /
# datashield.* and their default.* forms; others unset; an option that named a
# temporary directory of the baseline session gets a fresh one) and DSOMOP_*
# variables of the baseline call. The result must be identical to the baseline:
# output names and order, structure, column names, column classes and storage
# types, row count, row order and values, the composite/split inventories and
# every attribute. The comparison is LOSSLESS: doubles go through 17 significant
# digits, which round-trip every IEEE-754 double, so equal canonical forms mean
# byte-identical finite numeric values (dates also retain their numeric payload;
# factors keep levels + labels and NA is explicit).
# The two baseline captures agreed at 17 digits on every pinned value.
#
# Staged results are compared as the complete descriptor plus the staged file's
# content (every CSV cell as text, in file order) and the sha256 of its bytes.
# The only parts excluded are the volatile ones the canonical form neutralises
# (`.b10_value`): the staging token (`staged_token`,
# `metadata$bundle_contract$staged_token`, and the token directory inside
# `metadata$file`), the session tempdir and tempfile hex of `metadata$file` (its
# file name is compared) and the `expires_at` timestamp. Pseudonymised person
# tokens are renumbered by first appearance within each output or assigned
# binding (linkage) and their raw values are compared through
# `raw_token_sha256` (the fixtures' person keys are deterministic). For omopPlanExecuteDS() the compared value is the set of
# bindings the call assigns; its return value changed by design (the 2.10.3
# status object, §7.3) and is not part of a pin. A refusal fails the pin. The
# test compares results only; it never inspects the IR.
#
# The only exceptions are the reviewed corrections the endorsed design makes on
# purpose (.B10_CORRECTIONS, not v1 lowering changes): the corrected column is
# compared with the design's rule, recomputed from the fixture, every baseline
# value the correction keeps must still be equal, and every other value of the
# pin is compared as above. A-19 item 1 classifies them `corrected` in
# v1-corpus-classes.csv. The A-19 differential and admissible-support fixtures
# are replayed by test-ir-results-a19.R.

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
    descriptor <- unclass(x)
    # A-27's lossless CSV escape protocol is an explicit descriptor addition.
    # Assert it independently, then compare every historical descriptor field
    # and the unchanged fixture bytes/content against the immutable capture.
    if (exists(".irPrepare", mode = "function") && identical(x$metadata$format, "csv")) {
      expect_identical(descriptor$metadata$csv_encoding, "backslash-v1")
      descriptor$metadata$csv_encoding <- NULL
    }
    return(structure(list(descriptor = descriptor, staged_file = .b10_read_staged(x)),
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

.b10_pins <- function(file = "ir-v1-results.json") {
  path <- testthat::test_path("fixtures", file)
  if (!file.exists(path)) return(NULL)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

# Exactly the baseline call's server options and DSOMOP_* variables; an option
# that named a temporary directory of the baseline session gets a fresh one.
.b10_options <- function(pin, pattern) {
  opts <- lapply(pin$options, .b10_decode)
  for (nm in names(pin$fresh_tempfile_options)) {
    opts[[nm]] <- tempfile(as.character(pin$fresh_tempfile_options[[nm]]))
  }
  stale <- setdiff(grep(pattern, names(options()), value = TRUE), names(opts))
  c(opts, stats::setNames(vector("list", length(stale)), stale))
}
.b10_env <- function(pin) {
  env <- vapply(pin$env, as.character, "")
  stale <- setdiff(grep("^DSOMOP_", names(Sys.getenv()), value = TRUE),
                   c(names(env), "DSOMOP_RESULT_CAPTURE_DIR", "DSOMOP_RESULT_CAPTURE_LABEL",
                     "DSOMOP_RESULT_CAPTURE_PKG"))
  c(env, stats::setNames(rep(NA_character_, length(stale)), stale))
}

# A staged pin's fixture: a fresh create_test_handle() with the baseline call's
# changed-table rows and person-key handle fields.
.b10_fixture_handle <- function(pin) {
  handle <- create_test_handle()
  fx <- pin$fixture
  for (t in names(fx$table_content)) {
    rows <- .b10_decode(fx$table_content[[t]])
    DBI::dbExecute(handle$conn, sprintf('DELETE FROM "main"."%s"', gsub('"', '""', t)))
    if (nrow(rows)) DBI::dbAppendTable(handle$conn, t, rows)
  }
  for (f in names(fx$exact_fields)) assign(f, .b10_decode(fx$exact_fields[[f]]), envir = handle)
  handle
}

# F-26 (design 7.6, endorsed ledger): an assigned baseline age band is no longer
# suppressed when it holds fewer than nfilter.tab persons (2.10.2:
# .computeAgeGroups() with the per-band small-cell rule, which made every band of
# the default fixture NA); it is .bandAges() of the age at index on the server's
# grid, and the published object is gated as a whole (design 7.3).
.B10_F26 <- paste("F-26: assigned age bands without per-band suppression",
                  "(.bandAges() on the server grid; design 7.6)")
.B10_CORRECTIONS <- list(
  list(key = "dsOMOP:test-output-contracts.R:baseline output produces one row per cohort member",
       output = "demo", column = "age_group", reference = .B10_F26),
  list(key = "dsOMOP:test-output-contracts.R:single plan with all output types executes successfully",
       output = "demo", column = "age_group", reference = .B10_F26)
)

# The F-26 age band of each output row: index year (the row's cohort start in
# the plan's results cohort) minus year of birth. Recompute the specified
# half-open grid with base cut(), independently of production .bandAges().
.b10_f26_age_group <- function(conn, person_id, cohort_definition_id, age_breaks = NULL) {
  cohort <- DBI::dbGetQuery(conn, paste0(
    "SELECT subject_id, cohort_start_date FROM cohort WHERE cohort_definition_id = ",
    as.integer(cohort_definition_id)))
  person <- DBI::dbGetQuery(conn, "SELECT person_id, year_of_birth FROM person")
  stopifnot(!anyDuplicated(cohort$subject_id), all(person_id %in% cohort$subject_id))
  index_year <- as.integer(substr(as.character(cohort$cohort_start_date[match(person_id, cohort$subject_id)]), 1L, 4L))
  age <- index_year - person$year_of_birth[match(person_id, person$person_id)]
  grid <- if (is.null(age_breaks)) getOption("dsomop.age_breaks",
    getOption("default.dsomop.age_breaks", seq(0L, 85L, 5L))) else age_breaks
  grid <- as.numeric(unlist(grid))
  labels <- c(paste0(head(grid, -1L), "-", tail(grid, -1L) - 1L),
              paste0(tail(grid, 1L), "+"))
  as.character(cut(age, breaks = c(grid, Inf), labels = labels, right = FALSE))
}

# The baseline result with each reviewed correction applied, checked first
# against the baseline values it keeps.
.b10_corrected <- function(pin, want, conn, info) {
  # the 2.10.2 build itself (compare_results.py validate --side baseline)
  # predates F-26 and is checked against its own pins unchanged
  if (!exists(".bandAges", mode = "function")) return(want)
  for (cr in Filter(function(x) identical(x$key, pin$key), .B10_CORRECTIONS)) {
    k <- match(cr$output, vapply(want$names, as.character, ""))
    item <- want$items[[k]]
    cols <- vapply(item$names, as.character, "")
    j <- match(cr$column, cols)
    stopifnot(!is.na(k), !is.na(j), identical(item$kind, "data.frame"))
    plan <- .b10_decode(pin$plan)
    person_id <- as.integer(vapply(item$columns[[match("person_id", cols)]]$values, as.character, ""))
    expected <- .b10_f26_age_group(conn, person_id, plan$cohort$cohort_definition_id,
                                    plan$outputs[[cr$output]]$age_breaks)
    kept <- !vapply(item$columns[[j]]$values, is.null, logical(1))
    expect_identical(vapply(item$columns[[j]]$values[kept], as.character, ""), expected[kept],
                     info = paste(info, cr$reference, "keeps the unsuppressed baseline bands"))
    item$columns[[j]]$values <- lapply(expected, function(v) if (is.na(v)) NULL else v)
    want$items[[k]] <- item
  }
  want
}

# A pin's classification: safe, or corrected by a reviewed correction (A-19
# item 1); a mixed block's call is pinned only with a reviewed attribution.
.b10_expect_pinned_class <- function(pin, classes, block) {
  for (key in c(pin$key, vapply(pin$also, `[[`, "", "key"))) {
    cls <- unique(classes$class[block == key])
    reviewed <- any(vapply(.B10_CORRECTIONS, function(x) identical(x$key, key), logical(1)))
    expect_true("safe" %in% cls || ("corrected" %in% cls && reviewed), info = paste(pin$id, key, paste(cls, collapse = "+")))
    if (length(cls) > 1L) expect_true(is.character(pin$attribution[[key]]), info = paste(pin$id, key))
  }
}

test_that("the result pins are baseline captures of safe-class plans", {
  classes <- utils::read.csv(testthat::test_path("fixtures", "v1-corpus-classes.csv"),
                             stringsAsFactors = FALSE, check.names = FALSE)
  block <- sub("#.*$", "", classes$plan_id)
  for (file in c("ir-v1-results.json", "ir-v1-results-staged.json")) {
    fx <- .b10_pins(file)
    if (is.null(fx)) {
      fail(paste(file, "is missing"))
      next
    }
    expect_identical(fx$baseline$dsOMOP, "30b7905617dbe3de5d06699c324095635c68011f")
    expect_identical(fx$canonical_digits, 17L)
    expect_gte(length(fx$pins), 1L)
    for (pin in fx$pins) {
      .b10_expect_pinned_class(pin, classes, block)
      # the pinned plan object is reconstructed exactly
      expect_identical(.b10_roundtrip(.b10_canon(.b10_decode(pin$plan), 17L)), pin$plan,
                       info = paste(pin$id, "plan reconstruction"))
    }
  }
})

test_that("safe-class v1 plans reproduce their 2.10.2 results on the default fixture", {
  fx <- .b10_pins()
  for (pin in fx$pins) {
    info <- paste0(pin$id, " (", pin$key, ")")
    handle <- create_test_handle()
    got <- withr::with_options(.b10_options(pin, fx$option_pattern),
      withr::with_envvar(.b10_env(pin), tryCatch(
        .planExecute(handle, .b10_decode(pin$plan), .b10_decode(pin$out_symbols),
                     output_mode = pin$output_mode),
        error = function(e) e)))
    want <- withr::with_options(.b10_options(pin, fx$option_pattern),
                                .b10_corrected(pin, pin$result, handle$conn, info))
    cleanup_handle(handle)
    if (inherits(got, "error")) {
      fail(paste0(info, ": refused: ", conditionMessage(got)))
      next
    }
    if (identical(pin$id, "r006")) {
      # A-27 corrects this translated column from text to an unordered factor.
      # Assert the new class and identity contract before comparing the full
      # historical text view; frozen values, rows and other attributes remain
      # exact. This exception applies only to this named baseline capture.
      expect_identical(pin$key,
        "dsOMOP:test-output-contracts.R:baseline with concept translation works")
      frame <- got$demo
      column <- frame$gender_concept_id
      expect_identical(class(column), c("dsomop_concept_factor", "factor"))
      expect_false(is.ordered(column))
      expect_identical(levels(column), "MALE")
      expect_setequal(names(attributes(frame)),
        c("names", "row.names", "class", "omop_concept_cols", "omop_concepts"))
      expect_identical(attr(frame, "omop_concept_cols"), "gender_concept_id")
      contracts <- attr(frame, "omop_concepts")
      expect_identical(names(contracts), "gender_concept_id")
      contract <- contracts$gender_concept_id
      expect_identical(contract$ids, rep("8507", nrow(frame)))
      expect_identical(contract$resolved$ids, "8507")
      expect_identical(contract$resolved$labels, "MALE")
      expect_identical(contract$resolved$levels, "MALE")
      expect_identical(contract$records[["8507"]]$origin, "vocabulary")
      expect_identical(contract$records[["8507"]]$label, "MALE")
      expect_null(contract$declared)
      expect_null(contract$owner)
      frame$gender_concept_id <- as.character(column)
      attr(frame, "omop_concept_cols") <- NULL
      attr(frame, "omop_concepts") <- NULL
      got$demo <- frame
    }
    got <- .b10_value(.b10_attach_staged(got))
    expect_identical(got$volatile$raw_token_sha256, pin$volatile$raw_token_sha256,
                     info = paste(info, "raw person tokens"))
    .b10_expect_result(.b10_roundtrip(got$tree), want, info)
  }
})

test_that("safe-class v1 plans reproduce their 2.10.2 staged results on admissible fixtures", {
  fx <- .b10_pins("ir-v1-results-staged.json")
  for (pin in fx$pins) {
    info <- paste0(pin$id, " (", pin$key, ", ", pin$fn, ", staged)")
    handle <- .b10_fixture_handle(pin)
    expect_identical(.b10_db_tables(handle$conn)$sha256, pin$fixture$db_sha256,
                     info = paste(info, "rebuilt fixture content"))
    pid <- as.character(Sys.getpid())
    plan <- .b10_decode(pin$plan)
    live <- withr::with_options(.b10_options(pin, fx$option_pattern),
      withr::with_envvar(.b10_env(pin), {
        if (identical(pin$fn, ".planExecute")) {
          got <- tryCatch(.planExecute(handle, plan, .b10_decode(pin$out_symbols),
                                       output_mode = "staged"),
                          error = function(e) e)
          if (inherits(got, "error")) got else
            c(.b10_value(.b10_attach_staged(got)), list(dirs = .b10_staged_dirs(got)))
        } else {
          symbol <- gsub("<pid>", pid, pin$omop_symbol, fixed = TRUE)
          out <- .b10_decode(pin$out)
          unpid <- function(s) gsub("<pid>", pid, s, fixed = TRUE)
          out <- if (is.list(out)) lapply(out, unpid) else stats::setNames(unpid(out), names(out))
          .setHandle(symbol, handle)
          target <- new.env()
          got <- tryCatch(eval(bquote(omopPlanExecuteDS(.(symbol), .(plan), .(out),
                                                         output_mode = "staged")), envir = target),
                          error = function(e) e)
          # read the staged files before the handle (which owns its staging
          # directories) is removed
          live <- if (inherits(got, "error")) got else {
            bound <- .b10_bindings(target, ls(target, all.names = TRUE))
            c(.b10_value(bound),
              list(dirs = .b10_staged_dirs(mget(ls(target, all.names = TRUE), envir = target))))
          }
          .removeHandle(symbol)
          live
        }
      }))
    cleanup_handle(handle)
    if (inherits(live, "error")) {
      fail(paste0(info, ": refused: ", conditionMessage(live)))
      next
    }
    unlink(live$dirs, recursive = TRUE)
    want <- if (identical(pin$fn, ".planExecute")) pin$result else pin$assigned$values
    want_tokens <- if (identical(pin$fn, ".planExecute")) pin$volatile else pin$assigned_volatile
    expect_identical(live$volatile$raw_token_sha256, want_tokens$raw_token_sha256,
                     info = paste(info, "raw person tokens"))
    .b10_expect_result(.b10_roundtrip(live$tree), want, info)
  }
})


test_that("result canonicalisation preserves fractional dates and submicrosecond times", {
  date <- structure(c(0, 0.5, NA_real_), class = "Date")
  time <- structure(c(0, 1e-8, NA_real_), class = c("POSIXct", "POSIXt"), tzone = "UTC")
  for (x in list(date, time)) {
    canonical <- .b10_roundtrip(.b10_canon(x))
    expect_false(identical(canonical$numeric_payload[[1L]], canonical$numeric_payload[[2L]]))
    expect_identical(.b10_decode(canonical), x)
  }
})
