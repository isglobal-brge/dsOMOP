# WS-2 tooling (dsOMOP 2.10.3), review B10: RESULT capture for the v1 corpus.
#
# Not part of the package tests. `python3 tools/ws2/compare_results.py capture`
# copies this file into tests/testthat/helper-zzz-b10-capture.R of a SNAPSHOT
# (git archive) of dsOMOP or dsOMOPClient and runs that snapshot's suite on the
# test pod with DSOMOP_RESULT_CAPTURE_DIR=<dir> (required),
# DSOMOP_RESULT_CAPTURE_LABEL=<run label> and DSOMOP_RESULT_CAPTURE_PKG=<package
# whose suite runs>. Without DSOMOP_RESULT_CAPTURE_DIR it does nothing.
#
# It wraps dsOMOP:::.planExecute() (2.10.2: (handle, plan, out_symbols,
# output_mode); 2.10.3 adds ctx = NULL), dsOMOP::omopPlanExecuteDS() (and
# 2.10.3's internal .omopPlanExecuteDS(), recorded under the same name with its
# target_env as the assign environment), dsOMOP:::.planResolvePopulations() and,
# when a test calls them DIRECTLY (not inside another captured call), the executor
# entry points .cohortCreate(), .cohortCombine(), .extractTable() and .toFeatures()
# in the loaded dsOMOP namespace (immediately when it is loaded, else on its onLoad
# hook). Each body is wrapped in place (same frame, so parent.frame(), on.exit()
# and return() keep their meaning; only supplied arguments and constant defaults
# are read early) and ONE JSON line per call goes to <dir>/<label>-<pkg>-<pid>.jsonl
# (`parent` = the id of an enclosing captured call of the same process):
#
#   context   test file, test_that() line and description, and the block key
#             `package:file:<description>[@k]` computed from the test file with
#             the rule of tools/ws0/corpus_inventory.py (the key of
#             tests/testthat/fixtures/v1-corpus-classes.csv; `#sub` rows are
#             bound per block by the comparison);
#   plan      `plan`: the plan sanitised exactly like tools/ws2/plan_capture_helper.R
#             (the corpus form: data frames -> {"__frame__": structure}, so
#             build_ir_corpus.py's clean() + sha256 gives the ir-v1-corpus.json
#             hash); `plan_exact`: the typed canonical form below with 17
#             significant digits (exact R reconstruction, used by
#             tests/testthat/test-ir-results.R); `scope_frames`: structural summary
#             (path, nrow, names, column classes) of every data frame in the plan,
#             e.g. the server-injected scope$tables_frames;
#   options   every option named (default.)nfilter.* / dsomop.* / dsstaging.* /
#             datashield.* and every DSOMOP_* environment variable at the call;
#   handle    fingerprint: content hash of every main-schema table of an SQLite
#             connection, pre-existing TEMP tables, a hash of every handle field,
#             and `default_fixture` (TRUE only when the data, the TEMP schema and
#             every handle field equal a fresh create_test_handle(), with the
#             derived blueprint cache allowed when it equals the fresh one's);
#   outcome   ok / error (condition classes, message, $code) and the warnings
#             signalled during the call;
#   result    the canonical serialisation (`.b10_canon`, 15 significant digits)
#             of the returned value: every output in returned order with its
#             structure (data frame / list of frames / staged descriptor), class,
#             column names, column classes and storage types, row count, row
#             names when not automatic, the values in returned row order (doubles
#             to 15 significant digits, dates ISO, factors as levels + labels,
#             NA explicit as null) and all non-structural attributes; list
#             (composite / split) outputs keep their element names (the
#             inventory). Staged descriptors additionally carry the content of
#             their staged file (CSV read as text columns, Parquet when arrow is
#             available).
#   For omopPlanExecuteDS() it records the decoded plan, the requested symbol
#   mapping, the returned value, and every binding of the caller's assign
#   environment that the call created or replaced (names + canonical forms).
#   For .planResolvePopulations() the result also carries each population's
#   roster: the rows of its TEMP cohort table, read when the call returns; for
#   .cohortCreate() / .cohortCombine() the returned TEMP table name becomes
#   list(cohort_table, roster). For the executor entry points the `plan` is the
#   list of SUPPLIED arguments (data frames summarised as in the corpus form; the
#   constant defaults in effect are listed in call$constant_defaults) and
#   `inputs` holds the content hashes of data-frame arguments and of referenced
#   cohort tables.
#
# Volatile parts and their neutralisation (each record lists what it replaced):
#   staging tokens (stg_<32 hex>) and context ids -> "<token>"; the R session
#   tempdir() (staging base dir) -> "<tempdir>", and a trailing run of >= 10 hex
#   digits in a path component below it (tempfile() names) -> "<rand>", so the
#   path structure (e.g. a .private/ directory) stays visible; the capturing
#   process id (tests name symbols after Sys.getpid()) -> "<pid>" in names and in
#   strings outside data-frame values; ISO-8601 timestamps
#   (expires_at / created_at) outside data-frame values -> "<timestamp>";
#   pseudonymised person tokens (character person_id / subject_id columns, the
#   omopPlanExecuteDS() assign path and staged files) -> "<p1>", "<p2>", ... by
#   first appearance within the record (linkage preserved), with the sha256 of
#   the raw token vectors kept as `raw_token_sha256` (informational). Random
#   temporary-table suffixes never appear in returned values (checked by the
#   comparison, which reports any such string it finds); the reserved-name suffix
#   of a population's TEMP cohort table (_<12 hex>) -> "_<rand>".
if (nzchar(Sys.getenv("DSOMOP_RESULT_CAPTURE_DIR"))) local({
  if (!requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("openssl", quietly = TRUE)) {
    stop("result capture needs jsonlite and openssl")
  }

  # >>> b10-canonical: this block is copied verbatim into
  # tests/testthat/test-ir-results.R (compare_results.py checks the copies agree)
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

  out_dir <- Sys.getenv("DSOMOP_RESULT_CAPTURE_DIR")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  label <- Sys.getenv("DSOMOP_RESULT_CAPTURE_LABEL", "run")
  suite_pkg <- Sys.getenv("DSOMOP_RESULT_CAPTURE_PKG", "dsOMOP")
  out_file <- file.path(out_dir, sprintf("%s-%s-%d.jsonl", label, suite_pkg, Sys.getpid()))
  helper_env <- parent.env(environment())
  state <- new.env(parent = emptyenv())
  state$seq <- 0L
  state$open <- list()          # id -> partial record, while the call runs
  state$depth <- 0L
  state$keys <- list()          # test file -> data.frame(line, key, desc)
  state$ref <- NULL             # fresh create_test_handle() fingerprint
  sentinel <- structure(list(), class = "b10_no_return_value")
  `%||%` <- function(a, b) if (is.null(a)) b else a
  sha <- function(txt) as.character(openssl::sha256(charToRaw(enc2utf8(paste(txt, collapse = "\n")))))
  to_json <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null",
                                  digits = NA, force = TRUE))
  }
  write_line <- function(rec) {
    txt <- tryCatch(to_json(rec), error = function(e) to_json(list(
      v = 1L, id = rec$id, fn = rec$fn, label = label, pkg = suite_pkg,
      context = rec$context, serialization_error = conditionMessage(e))))
    cat(txt, "\n", sep = "", file = out_file, append = TRUE)
  }
  note <- function(...) cat("[b10-capture]", ..., "\n")

  # ---- plan forms -------------------------------------------------------
  # Verbatim copy of sanitize() of tools/ws2/plan_capture_helper.R (corpus form).
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
  frame_summaries <- function(x, path = "plan", acc = list()) {
    if (is.data.frame(x)) {
      acc[[length(acc) + 1L]] <- list(
        path = path, nrow = nrow(x), names = I(as.character(names(x))),
        classes = lapply(x, function(col) I(class(col))))
      return(acc)
    }
    if (is.list(x) && !is.environment(x)) {
      nms <- names(x) %||% rep("", length(x))
      for (i in seq_along(x)) {
        acc <- frame_summaries(x[[i]], paste0(path, "/", if (nzchar(nms[[i]])) nms[[i]] else i), acc)
      }
    }
    acc
  }
  plan_forms <- function(plan) {
    list(plan = tryCatch(sanitize(plan), error = function(e) list(`__error__` = conditionMessage(e))),
         plan_exact = tryCatch(.b10_canon(plan, 17L), error = function(e) NULL),
         scope_frames = tryCatch(frame_summaries(plan), error = function(e) list()))
  }

  # ---- test context and block key ----------------------------------------
  key_table <- function(path) {
    nm <- basename(path)
    if (!is.null(state$keys[[nm]])) return(state$keys[[nm]])
    lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
    m <- regmatches(lines, regexec("^\\s*test_that\\(([\"'])(.*?)\\1", lines, perl = TRUE))
    hit <- which(lengths(m) == 3L)
    desc <- vapply(m[hit], `[[`, "", 3L)
    k <- ave(seq_along(desc), desc, FUN = seq_along)
    key <- paste0(suite_pkg, ":", nm, ":", desc, ifelse(k > 1L, paste0("@", k), ""))
    tab <- data.frame(line = hit, key = key, desc = desc, stringsAsFactors = FALSE)
    state$keys[[nm]] <- tab
    tab
  }
  test_context <- function() {
    calls <- sys.calls()
    tt <- tryCatch(testthat::test_that, error = function(e) NULL)
    ctx <- list(file = NULL, line = NULL, desc = NULL, key = NULL, in_test = FALSE)
    for (i in rev(seq_along(calls))) {
      f <- tryCatch(sys.function(i), error = function(e) NULL)
      if (!is.null(tt) && identical(f, tt)) {
        ctx$in_test <- TRUE
        ctx$desc <- tryCatch(as.character(get("desc", envir = sys.frame(i)))[1L],
                             error = function(e) NULL)
        sr <- attr(calls[[i]], "srcref")
        if (!is.null(sr)) {
          ctx$line <- as.integer(sr[[1L]])
          sf <- attr(sr, "srcfile")
          fn <- tryCatch(sf$filename, error = function(e) NULL)
          if (!is.null(fn)) {
            wd <- tryCatch(sf$wd, error = function(e) NULL)
            path <- if (file.exists(fn)) fn else if (!is.null(wd)) file.path(wd, fn) else fn
            ctx$file <- basename(fn)
            ctx$path <- path
          }
        }
        break
      }
    }
    if (is.null(ctx$file)) {
      # fall back: the innermost frame with a srcref from a test file
      for (i in rev(seq_along(calls))) {
        sr <- attr(calls[[i]], "srcref")
        if (is.null(sr)) next
        fn <- tryCatch(attr(sr, "srcfile")$filename, error = function(e) "")
        if (length(fn) == 1L && grepl("^test-.*\\.[rR]$", basename(fn))) {
          ctx$file <- basename(fn); ctx$path <- fn
          if (is.null(ctx$line)) ctx$line <- as.integer(sr[[1L]])
          break
        }
      }
    }
    if (!is.null(ctx$path) && !is.null(ctx$line)) {
      tab <- key_table(ctx$path)
      j <- which(tab$line <= ctx$line)
      if (length(j)) {
        j <- max(j)
        ctx$key <- tab$key[[j]]
        ctx$block_line <- tab$line[[j]]
        ctx$block_desc <- tab$desc[[j]]
      }
    }
    ctx$path <- NULL
    ctx
  }

  # ---- options / environment ---------------------------------------------
  option_state <- function() {
    o <- options()
    keep <- grep("^(default\\.)?(nfilter|dsomop|dsstaging|datashield)\\.", names(o), value = TRUE)
    keep <- sort(keep)
    vals <- lapply(o[keep], function(v) tryCatch(.b10_canon(v, 17L), error = function(e) list(kind = "other")))
    env <- Sys.getenv()
    ek <- sort(grep("^DSOMOP_", names(env), value = TRUE))
    ek <- setdiff(ek, c("DSOMOP_RESULT_CAPTURE_DIR", "DSOMOP_RESULT_CAPTURE_LABEL",
                        "DSOMOP_RESULT_CAPTURE_PKG"))
    list(options = vals, env = as.list(env[ek]))
  }

  # ---- handle fingerprint ------------------------------------------------
  col_text <- function(v) {
    if (inherits(v, "blob") || is.list(v)) {
      return(vapply(v, function(b) if (is.null(b)) "\001NA" else paste(as.character(b), collapse = ""), ""))
    }
    s <- if (is.double(v) && !inherits(v, "Date")) sprintf("%.17g", v) else as.character(v)
    s[is.na(v)] <- "\001NA"
    s
  }
  table_hash <- function(conn, schema, tbl) {
    q <- sprintf('SELECT * FROM "%s"."%s"', schema, gsub('"', '""', tbl))
    df <- DBI::dbGetQuery(conn, q)
    hdr <- paste(names(df), vapply(df, function(v) paste(class(v), collapse = "/"), ""), sep = ":", collapse = "|")
    rows <- if (ncol(df)) do.call(paste, c(lapply(df, col_text), sep = "\037")) else character(0)
    rows <- sort(rows, method = "radix")
    list(n = nrow(df), sha = substr(sha(c(hdr, rows)), 1L, 24L))
  }
  db_fingerprint <- function(conn) {
    if (is.null(conn)) return(list(kind = "none"))
    if (!inherits(conn, "SQLiteConnection")) return(list(kind = class(conn)[[1L]]))
    if (!DBI::dbIsValid(conn)) return(list(kind = "invalid"))
    main <- DBI::dbGetQuery(conn, "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY name")$name
    tabs <- lapply(main, function(t) table_hash(conn, "main", t))
    names(tabs) <- main
    temp <- DBI::dbGetQuery(conn, "SELECT name FROM sqlite_temp_master WHERE type = 'table' ORDER BY name")$name
    temps <- lapply(temp, function(t) table_hash(conn, "temp", t))
    list(kind = "sqlite",
         sha256 = sha(paste(main, vapply(tabs, `[[`, "", "sha"), vapply(tabs, function(x) as.character(x$n), ""))),
         tables = tabs, temp_tables = length(temp),
         temp_sha256 = if (length(temp)) sha(sort(vapply(temps, `[[`, "", "sha"))) else NULL)
  }
  field_hash <- function(v) {
    if (is.null(v)) return("null")
    if (is.environment(v)) {
      # plain environments (the blueprint cache) by content; classed ones (R6) by class
      if (!identical(class(v), "environment")) return(paste0("environment:", paste(class(v), collapse = "/")))
      v <- as.list(v, all.names = TRUE, sorted = TRUE)
    }
    if (is.function(v)) return("function")
    tryCatch(substr(as.character(openssl::sha256(serialize(v, NULL, version = 3L))), 1L, 24L),
             error = function(e) "unhashable")
  }
  handle_fields <- function(handle) {
    nms <- sort(setdiff(ls(handle, all.names = TRUE), c("conn", "temp_connection")))
    stats::setNames(lapply(nms, function(n) field_hash(get(n, envir = handle))), nms)
  }
  core_fields <- c("dbms", "target_dialect", "cdm_schema", "vocab_schema", "results_schema",
                   "temp_schema", "resource_client", "config", "temp_tables")
  derived_caches <- c("blueprint", "physical_table_names", "dbms_version",
                      "results_schema_resolved", "results_schema_resolved_done",
                      "has_achilles", "has_ohdsi_results", "achilles_gate_cache",
                      "sql_dialect_profile", "analysis_catalog", "analysis_catalog_mode")
  reference <- function() {
    if (!is.null(state$ref)) return(state$ref)
    state$ref <- list(available = FALSE)
    mk <- get0("create_test_handle", envir = helper_env, inherits = TRUE)
    if (!is.function(mk) || !"dsOMOP" %in% loadedNamespaces()) return(state$ref)
    quiet <- function(expr) suppressWarnings(suppressMessages(expr))
    h <- tryCatch(quiet(mk()), error = function(e) NULL)
    if (is.null(h)) return(state$ref)
    on.exit(try(DBI::dbDisconnect(h$conn), silent = TRUE), add = TRUE)
    fields <- handle_fields(h)
    db <- db_fingerprint(h$conn)
    bp <- tryCatch(field_hash(quiet(dsOMOP:::.buildBlueprint(h))), error = function(e) "unavailable")
    state$ref <- list(available = TRUE, db_sha256 = db$sha256, fields = fields,
                      blueprint = bp, tables = db$tables)
    write_line(list(v = 1L, kind = "reference", label = label, pkg = suite_pkg,
                    dsomop_version = as.character(utils::packageVersion("dsOMOP")),
                    reference = state$ref))
    state$ref
  }
  handle_fingerprint <- function(handle) {
    if (!is.environment(handle)) return(list(kind = "not-an-environment", default_fixture = FALSE))
    db <- tryCatch(db_fingerprint(handle$conn), error = function(e) list(kind = "error", message = conditionMessage(e)))
    fields <- handle_fields(handle)
    ref <- reference()
    diffs <- character(0)
    if (!isTRUE(ref$available)) diffs <- c(diffs, "no-reference")
    if (!identical(db$kind, "sqlite")) diffs <- c(diffs, paste0("db:", db$kind))
    if (isTRUE(ref$available) && identical(db$kind, "sqlite")) {
      if (!identical(db$sha256, ref$db_sha256)) {
        changed <- union(setdiff(names(db$tables), names(ref$tables)), setdiff(names(ref$tables), names(db$tables)))
        common <- intersect(names(db$tables), names(ref$tables))
        changed <- c(changed, common[!vapply(common, function(t) identical(db$tables[[t]]$sha, ref$tables[[t]]$sha), TRUE)])
        diffs <- c(diffs, paste0("table:", sort(changed)))
      }
      if (db$temp_tables > 0L) diffs <- c(diffs, "temp-tables-present")
      for (f in core_fields) {
        if (!identical(fields[[f]] %||% "null", ref$fields[[f]] %||% "null")) {
          diffs <- c(diffs, paste0("field:", f))
        }
      }
      bp <- fields[["blueprint"]] %||% "null"
      if (!bp %in% c("null", ref$blueprint)) diffs <- c(diffs, "field:blueprint")
      extra <- setdiff(names(fields), c(core_fields, derived_caches))
      extra <- extra[vapply(extra, function(f) !identical(fields[[f]], "null"), TRUE)]
      if (length(extra)) diffs <- c(diffs, paste0("field:", extra))
    }
    tabs <- db$tables
    db$tables <- NULL
    list(db = db, fields = fields,
         changed_tables = if (isTRUE(ref$available) && identical(db$kind, "sqlite") &&
                              !identical(db$sha256, ref$db_sha256)) tabs[intersect(names(tabs), sub("^table:", "", grep("^table:", diffs, value = TRUE)))] else NULL,
         default_fixture = length(diffs) == 0L, diffs = I(diffs))
  }

  # ---- volatile neutralisation of canonical trees --------------------------
  vol_patterns <- c(token = "stg_[0-9a-f]{32}",
                    timestamp = "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:?[0-9]{2})?")
  tmp_prefix <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  pid_rx <- paste0("(?<![0-9])", Sys.getpid(), "(?![0-9])")
  neutralise <- function(node, in_data, log, tokens) {
    # node: canonical tree; in_data: inside data-frame column values
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
          log$person_tokens <- (log$person_tokens %||% 0L) + 1L
          node$columns[[j]] <- col
        } else {
          node$columns[[j]] <- neutralise(col, TRUE, log, tokens)
        }
      }
      if (!is.null(node$attrs)) node$attrs <- neutralise(node$attrs, FALSE, log, tokens)
      return(node)
    }
    if (identical(node$kind, "atomic") && identical(node$type, "character")) {
      v <- as.character(node$values)
      if (!length(v)) return(node)
      orig <- v
      v <- gsub(vol_patterns[["token"]], "<token>", v)
      v <- gsub(tmp_prefix, "<tempdir>", v, fixed = TRUE)
      # tempfile() components below the session tempdir (e.g. a test's own
      # staging base dir) end in random hex: keep the path structure, drop the hex
      under_tmp <- !is.na(v) & startsWith(v, "<tempdir>")
      if (any(under_tmp)) {
        v[under_tmp] <- gsub("(/[^/<>]*?)[0-9a-f]{10,}(?=/|$)", "\\1<rand>", v[under_tmp], perl = TRUE)
      }
      if (!in_data) {
        v <- gsub(vol_patterns[["timestamp"]], "<timestamp>", v)
        v <- gsub(pid_rx, "<pid>", v, perl = TRUE)
      }
      ch <- !is.na(orig) & orig != v
      if (any(ch)) {
        log$strings <- (log$strings %||% 0L) + sum(ch)
        node$values <- I(v)
      }
      if (!is.null(node$names)) {
        nn <- as.character(node$names)
        nn2 <- gsub(pid_rx, "<pid>", gsub(vol_patterns[["token"]], "<token>", nn), perl = TRUE)
        if (!identical(nn, nn2)) { node$names <- I(nn2); log$names <- (log$names %||% 0L) + 1L }
      }
      return(node)
    }
    if (identical(node$kind, "list") && !is.null(node$names)) {
      nn <- as.character(node$names)
      nn2 <- gsub(pid_rx, "<pid>", gsub(vol_patterns[["token"]], "<token>", nn), perl = TRUE)
      if (!identical(nn, nn2)) { node$names <- I(nn2); log$names <- (log$names %||% 0L) + 1L }
    }
    for (k in names(node)) {
      if (k %in% c("kind", "type", "length", "nrow")) next
      if (is.list(node[[k]])) node[[k]] <- neutralise(node[[k]], in_data, log, tokens)
    }
    if (is.null(names(node)) && length(node)) {
      for (i in seq_along(node)) if (is.list(node[[i]])) node[[i]] <- neutralise(node[[i]], in_data, log, tokens)
    }
    node
  }
  canon_record_value <- function(x) {
    log <- new.env(parent = emptyenv())
    tokens <- new.env(parent = emptyenv())
    tokens$map <- character(0); tokens$raw <- list()
    tree <- .b10_canon(x, 15L)
    tree <- neutralise(tree, FALSE, log, tokens)
    list(tree = tree,
         volatile = c(as.list(log), list(
           raw_token_sha256 = if (length(tokens$raw)) sha(unlist(tokens$raw)) else NULL)))
  }

  # ---- staged files ------------------------------------------------------
  read_staged <- function(desc) {
    f <- tryCatch(desc$metadata$file, error = function(e) NULL)
    if (!is.character(f) || length(f) != 1L || is.na(f)) return(list(state = "no-file"))
    files <- if (dir.exists(f)) sort(list.files(f, recursive = TRUE, full.names = TRUE)) else f
    files <- files[file.exists(files)]
    if (!length(files)) return(list(state = "missing"))
    fmt <- tolower(desc$metadata$format %||% tools::file_ext(files[[1L]]))
    frames <- lapply(files, function(p) {
      if (grepl("csv", fmt) || grepl("\\.csv$", p)) {
        utils::read.csv(p, colClasses = "character", check.names = FALSE,
                        na.strings = character(0), stringsAsFactors = FALSE)
      } else if (requireNamespace("arrow", quietly = TRUE)) {
        as.data.frame(arrow::read_parquet(p))
      } else NULL
    })
    if (any(vapply(frames, is.null, TRUE))) {
      return(list(state = "unreadable", format = fmt, n_files = length(files)))
    }
    list(state = "read", format = fmt, n_files = length(files),
         content = if (length(frames) == 1L) frames[[1L]] else do.call(rbind, frames))
  }
  attach_staged <- function(x) {
    # Replace every staged descriptor by list(descriptor, staged_file_content)
    if (inherits(x, "FlowerDatasetDescriptor")) {
      return(structure(list(descriptor = unclass(x), staged_file = read_staged(x)),
                       class = c("b10_staged", class(x))))
    }
    if (is.list(x) && !is.data.frame(x)) {
      a <- attributes(x)
      out <- lapply(x, attach_staged)
      attributes(out) <- a
      return(out)
    }
    x
  }

  # ---- executor entry points ---------------------------------------------
  exec_fns <- c(".cohortCreate", ".cohortCombine", ".extractTable", ".toFeatures")
  # The supplied arguments (forced: the body forces them anyway; defaults are
  # never forced early). Only these form the call's `plan`, so a parameter added
  # with a default in a later version cannot change it; the constant defaults in
  # effect are listed apart (informational).
  supplied_args <- function(fn, frame) {
    fmls <- formals(get(fn, envir = asNamespace("dsOMOP")))
    out <- list()
    defaults <- list()
    for (nm in setdiff(names(fmls), c("...", "handle", "ctx"))) {
      is_missing <- tryCatch(eval(call("missing", as.name(nm)), frame), error = function(e) TRUE)
      if (!is_missing) {
        out[nm] <- list(get(nm, envir = frame, inherits = FALSE))
      } else if (is.null(fmls[[nm]]) || (is.atomic(fmls[[nm]]) && !is.symbol(fmls[[nm]]))) {
        defaults[nm] <- list(fmls[[nm]])
      }
    }
    attr(out, "constant_defaults") <- defaults
    out
  }
  attach_cohort <- function(value, handle) {
    # .cohortCreate() / .cohortCombine(): a TEMP cohort table name -> its rows
    if (!is.character(value) || length(value) != 1L || is.na(value) || !is.environment(handle)) return(value)
    roster <- tryCatch(DBI::dbGetQuery(handle$conn, paste0("SELECT * FROM ", value)), error = function(e) NULL)
    if (is.null(roster)) return(value)
    list(cohort_table = sub("_[0-9a-f]{12}$", "_<rand>", value), roster = roster)
  }

  # ---- population rosters -------------------------------------------------
  # .planResolvePopulations() returns, per population, the name of a TEMP cohort
  # table plus person ids: add the table's rows (as returned by SELECT *) so the
  # roster itself is compared, and drop the random suffix of a reserved name.
  attach_rosters <- function(value, handle) {
    if (!is.list(value) || is.data.frame(value)) return(value)
    conn <- tryCatch(handle$conn, error = function(e) NULL)
    for (i in seq_along(value)) {
      pop <- value[[i]]
      if (!is.list(pop) || is.data.frame(pop) || !is.character(pop$cohort_table) ||
          length(pop$cohort_table) != 1L || is.na(pop$cohort_table)) next
      tbl <- pop$cohort_table
      roster <- tryCatch(DBI::dbGetQuery(conn, paste0("SELECT * FROM ", tbl)),
                         error = function(e) structure(list(), class = "b10_unreadable"))
      pop$cohort_table <- sub("_[0-9a-f]{12}$", "_<rand>", tbl)
      pop$roster <- roster
      value[[i]] <- pop
    }
    value
  }

  # ---- call records ------------------------------------------------------
  condition_info <- function(e) {
    list(class = I(class(e)), message = conditionMessage(e),
         code = tryCatch(if (is.character(e$code)) e$code else NULL, error = function(err) NULL))
  }
  enter <- function(fn, frame, caller) {
    # executor entry points are recorded only when a test calls them directly
    # (inside a captured call they are that call's internals)
    if (fn %in% exec_fns && length(state$open)) return(NA_integer_)
    tryCatch({
      state$seq <- state$seq + 1L
      id <- state$seq
      parent <- if (length(state$open)) as.integer(names(state$open)[[length(state$open)]]) else NULL
      rec <- list(v = 1L, kind = "call", id = id, parent = parent, fn = fn, label = label,
                  pkg = suite_pkg, context = test_context(), started = as.numeric(Sys.time()))
      # the effective argument value (a default when not supplied; NULL when absent)
      getv <- function(arg) tryCatch(get(arg, envir = frame, inherits = FALSE), error = function(e) NULL)
      if (fn %in% exec_fns) {
        args <- supplied_args(fn, frame)
        rec$call <- list(arguments = I(names(args)),
                         constant_defaults = tryCatch(.b10_canon(attr(args, "constant_defaults"), 17L),
                                                      error = function(e) NULL))
        attr(args, "constant_defaults") <- NULL
        named_tables <- intersect(names(args), c("cohort_table", "cohort_table_a", "cohort_table_b"))
        for (a in named_tables) {
          if (is.character(args[[a]]) && length(args[[a]]) == 1L && !is.na(args[[a]])) {
            args[[a]] <- sub("_[0-9a-f]{12}$", "_<rand>", args[[a]])
          }
        }
        rec <- c(rec, plan_forms(args))
        rec$plan_exact <- NULL   # replay pins are plan-level only
        h <- getv("handle")
        rec$handle <- if (is.environment(h)) handle_fingerprint(h) else list(kind = "no-handle", default_fixture = NA)
        # content of the inputs the plan form only summarises: data frames and
        # referenced cohort tables
        inputs <- list()
        for (a in names(args)) {
          if (is.data.frame(args[[a]])) inputs[[a]] <- sha(to_json(.b10_canon(args[[a]], 15L)))
        }
        for (a in named_tables) {
          tbl <- getv(a)
          if (is.character(tbl) && length(tbl) == 1L && is.environment(h)) {
            inputs[[a]] <- tryCatch(sha(to_json(.b10_canon(
              DBI::dbGetQuery(h$conn, paste0("SELECT * FROM ", tbl)), 15L))), error = function(e) "unreadable")
          }
        }
        rec$inputs <- inputs
        rec$.handle <- h
      } else if (identical(fn, ".planResolvePopulations")) {
        plan <- getv("plan")
        rec$call <- list(base_cohort_table = !is.null(getv("base_cohort_table")),
                         base_person_ids = length(getv("base_person_ids")),
                         index_cohort_table = !is.null(getv("index_cohort_table")),
                         ctx_supplied = if (exists("ctx", envir = frame, inherits = FALSE)) !is.null(getv("ctx")) else NULL)
        rec <- c(rec, plan_forms(plan))
        h <- getv("handle")
        rec$handle <- handle_fingerprint(h)
        rec$.handle <- h
      } else if (identical(fn, ".planExecute")) {
        plan <- getv("plan")
        rec$call <- list(output_mode = getv("output_mode"),
                         out_symbols = .b10_canon(getv("out_symbols"), 17L),
                         ctx_supplied = if (exists("ctx", envir = frame, inherits = FALSE)) !is.null(getv("ctx")) else NULL)
        rec <- c(rec, plan_forms(plan))
        rec$handle <- handle_fingerprint(getv("handle"))
      } else {
        # omopPlanExecuteDS(), or 2.10.3's internal .omopPlanExecuteDS() (which the
        # exported function and the DP preparation call with an explicit target_env)
        rec$fn <- "omopPlanExecuteDS"
        rec$entry <- fn
        dec <- function(x) tryCatch(dsOMOP:::.ds_arg(x), error = function(e) x)
        plan <- dec(getv("plan"))
        out <- dec(getv("out"))
        rec$call <- list(output_mode = dec(getv("output_mode")), out = .b10_canon(out, 17L),
                         omop_symbol = getv("omop_symbol"),
                         scope_supplied = !is.null(getv("scope")),
                         dots = eval(quote(...length()), frame))
        rec <- c(rec, plan_forms(plan))
        h <- tryCatch(dsOMOP:::.getHandle(getv("omop_symbol")), error = function(e) NULL)
        rec$handle <- handle_fingerprint(h)
        # `caller` is parent.frame() evaluated in the wrapped function's body: the
        # environment omopPlanExecuteDS() assigns into; .omopPlanExecuteDS() assigns
        # into its target_env argument
        target <- if (identical(fn, ".omopPlanExecuteDS")) getv("target_env") else caller
        syms <- tryCatch(as.character(unlist(out, use.names = FALSE)), error = function(e) character(0))
        cand <- unique(c(syms, as.vector(outer(syms, c(".covariates", ".covariateRef", ".temporalCovariates",
                                                     ".timeRef", ".personRef", ".personPeriods", ".events",
                                                     ".riskSets", ".msdata", ".transitionRef"), paste0))))
        before_names <- ls(target, all.names = TRUE)
        before <- lapply(intersect(cand, before_names), function(n) get(n, envir = target, inherits = FALSE))
        names(before) <- intersect(cand, before_names)
        rec$.target <- target
        rec$.before_names <- before_names
        rec$.before <- before
      }
      rec$options <- option_state()
      rec$warnings <- list()
      state$open[[as.character(id)]] <- rec
      id
    }, error = function(e) {
      note("enter failed:", conditionMessage(e))
      NA_integer_
    })
  }
  on_error <- function(id, e) {
    if (is.na(id)) return(invisible(NULL))
    k <- as.character(id)
    if (!is.null(state$open[[k]]) && is.null(state$open[[k]]$error)) {
      state$open[[k]]$error <- tryCatch(condition_info(e), error = function(err) list(message = "?"))
    }
    invisible(NULL)
  }
  on_warning <- function(id, w) {
    if (is.na(id)) return(invisible(NULL))
    k <- as.character(id)
    if (!is.null(state$open[[k]])) {
      ws <- state$open[[k]]$warnings
      if (length(ws) < 50L) {
        state$open[[k]]$warnings <- c(ws, list(tryCatch(condition_info(w), error = function(err) list(message = "?"))))
      }
    }
    invisible(NULL)
  }
  leave <- function(id, value) {
    force(value)
    if (is.na(id)) return(invisible(NULL))
    k <- as.character(id)
    rec <- state$open[[k]]
    state$open[[k]] <- NULL
    if (is.null(rec)) return(invisible(NULL))
    tryCatch({
      returned <- !inherits(value, "b10_no_return_value")
      rec$outcome <- if (returned) list(status = "ok") else
        c(list(status = "error"), rec$error %||% list(message = "<no condition seen>"))
      rec$error <- NULL
      rec$elapsed <- round(as.numeric(Sys.time()) - rec$started, 3)
      rec$started <- NULL
      if (returned && identical(rec$fn, ".planResolvePopulations")) {
        value <- attach_rosters(value, rec$.handle)
      }
      if (returned && rec$fn %in% c(".cohortCreate", ".cohortCombine")) {
        value <- attach_cohort(value, rec$.handle)
      }
      if (returned) {
        cv <- canon_record_value(attach_staged(value))
        rec$result <- cv$tree
        rec$volatile <- cv$volatile
      }
      if (identical(rec$fn, "omopPlanExecuteDS")) {
        target <- rec$.target
        now <- ls(target, all.names = TRUE)
        created <- setdiff(now, rec$.before_names)
        replaced <- names(rec$.before)[vapply(names(rec$.before), function(n) {
          exists(n, envir = target, inherits = FALSE) &&
            !identical(get(n, envir = target, inherits = FALSE), rec$.before[[n]])
        }, TRUE)]
        removed <- setdiff(names(rec$.before), now)
        bound <- sort(c(created, replaced))
        vals <- lapply(bound, function(n) attach_staged(get(n, envir = target, inherits = FALSE)))
        names(vals) <- bound
        cv <- canon_record_value(vals)
        nopid <- function(x) gsub(pid_rx, "<pid>", x, perl = TRUE)
        rec$assigned <- list(created = I(nopid(sort(created))), replaced = I(nopid(sort(replaced))),
                             removed = I(nopid(sort(removed))), values = cv$tree)
        rec$assigned_volatile <- cv$volatile
      }
      rec$.target <- NULL; rec$.before <- NULL; rec$.before_names <- NULL; rec$.handle <- NULL
      write_line(rec)
    }, error = function(e) {
      note("leave failed:", conditionMessage(e))
      write_line(list(v = 1L, kind = "call", id = rec$id, fn = rec$fn, label = label, pkg = suite_pkg,
                      context = rec$context, capture_error = conditionMessage(e)))
    })
    invisible(NULL)
  }

  # ---- wrapping ----------------------------------------------------------
  wrap_one <- function(ns, fn) {
    if (!exists(fn, envir = ns, inherits = FALSE)) return(FALSE)
    orig <- get(fn, envir = ns, inherits = FALSE)
    if (!is.function(orig) || isTRUE(attr(orig, "b10_capture"))) return(FALSE)
    wrapped <- orig
    body(wrapped) <- bquote({
      .__b10_capture_id__ <- .(enter)(.(fn), environment(), parent.frame())
      on.exit(.(leave)(.__b10_capture_id__, returnValue(.(sentinel))), add = TRUE)
      withCallingHandlers(.(body(orig)),
        error = function(e) .(on_error)(.__b10_capture_id__, e),
        warning = function(w) .(on_warning)(.__b10_capture_id__, w))
    })
    attr(wrapped, "b10_capture") <- TRUE
    unlockBinding(fn, ns)
    assign(fn, wrapped, envir = ns)
    lockBinding(fn, ns)
    pkg_env_name <- paste0("package:", environmentName(ns))
    if (pkg_env_name %in% search()) {
      pe <- as.environment(pkg_env_name)
      if (exists(fn, envir = pe, inherits = FALSE)) {
        locked <- bindingIsLocked(fn, pe)
        if (locked) unlockBinding(fn, pe)
        assign(fn, wrapped, envir = pe)
        if (locked) lockBinding(fn, pe)
      }
    }
    TRUE
  }
  install <- function(...) {
    ns <- asNamespace("dsOMOP")
    done <- vapply(c(".planExecute", "omopPlanExecuteDS", ".omopPlanExecuteDS",
                     ".planResolvePopulations", exec_fns),
                   function(f) wrap_one(ns, f), TRUE)
    note("wrapped", paste(names(done)[done], collapse = ", "), "in dsOMOP",
         as.character(utils::packageVersion("dsOMOP")), "->", out_file)
    write_line(list(v = 1L, kind = "install", label = label, pkg = suite_pkg,
                    dsomop_version = as.character(utils::packageVersion("dsOMOP")),
                    wrapped = I(names(done)[done]),
                    r_version = R.version.string))
  }
  write_line(list(v = 1L, kind = "helper", label = label, pkg = suite_pkg,
                  dsomop_loaded = "dsOMOP" %in% loadedNamespaces(), pid = Sys.getpid()))
  if ("dsOMOP" %in% loadedNamespaces()) {
    install()
  } else {
    setHook(packageEvent("dsOMOP", "onLoad"), function(...) install())
    note("dsOMOP not loaded yet; wrapping on its onLoad hook ->", out_file)
  }
})
