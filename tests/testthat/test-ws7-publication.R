# Publication of the native outputs (design-2.10.3 sections 7.1-7.3, 10.2,
# 10.3; WS-7): every format published in memory and staged gives the same
# rows; each patient component is gated on its own support (a sparse
# component with k - 1 contributors fails while the dense frame passes; five
# episodes of one person count once); private joined inputs are never
# assigned; refused families publish no reference companions; and the public
# omopPlanExecuteDS path assigns the inventory's symbols (multi-TAR names
# included) and returns the bounded status.

.ws7_pub_backends <- .ws7_scenario_backends()
withr::defer(.ws4_cleanup(.ws7_pub_backends), testthat::teardown_env())

# A plan exercising every format over the scenario population `t`.
.ws7_pub_plan <- function() {
  W <- .WS7
  vars <- list(n = list(source = list(event = "lab"), reducer = list(fn = "count")),
               age = list(derived = list(kind = "age")))
  plan <- .ws7_scenario_plan(vars, output = list(columns = list("n", "age")),
                             events = list(a = list(table = "condition_occurrence",
                                                    concept_set = list(concepts = list(W$DX)))))
  plan$outputs$l <- list(rows = "t", format = "long", sources = list(list(event = "lab")),
                         options = list(columns_derived = list("days_from_index", "value")))
  plan$outputs$s <- list(rows = "t", format = "survival", options = list(
    outcomes = list(a = list(event = "a")), censor = list("observation_end"),
    tars = list(short = list(start = 1L, end = 30L, anchor = "index"),
                long = list(start = 1L, end = NULL, anchor = "index"))))
  plan$outputs$r <- list(rows = "t", format = "survival", options = list(
    outcomes = list(a = list(event = "a")), censor = list("observation_end"),
    tar = list(start = 1L, end = 90L, anchor = "index"), format = "recurrent_events",
    event_order = "all", complete = TRUE))
  plan$outputs$p <- list(rows = "t", format = "person_period", options = list(
    sources = list(list(event = "lab")), bins = list(calendar = "month"),
    window = list(start = -30L, end = 30L, anchor = "index"), analyses = list("count", "last")))
  plan$outputs$tc <- list(rows = "t", format = "temporal_covariates", options = list(
    sources = list(list(event = "lab")), bin_width = 10L,
    window = list(start = -20L, end = 19L, anchor = "index"), analyses = list("count")))
  plan$outputs$sp <- list(rows = "t", format = "sparse", options = list(
    sources = list(list(event = "lab")), analyses = list("binary", "count")))
  plan$outputs$m <- list(rows = "t", format = "membership")
  plan$outputs$j <- list(rows = "t", format = "joined", grain = "episode", options = list(
    from = list(list(output = "o"), list(output = "m")), cardinality = "one_to_one"))
  plan$outputs$d <- list(rows = "t", format = "dictionary")
  plan$outputs$iv <- list(rows = "t", format = "intervals",
                          options = list(sources = list(list(event = "drug"))))
  plan
}

.ws7_pub_run <- function(handle, plan, frame, mode, threshold = 3L, publish = NULL,
                         commit_policy = "server") {
  if (!is.null(publish)) plan$publish <- as.list(publish)
  run <- .ws7_ctx(handle, plan, mode = mode)
  run$ctx$commit_policy <- commit_policy
  on.exit(.ws7_close(run), add = TRUE)
  pop <- .ws7_population(run, "t", frame)
  .ws7_publish(run, list(t = pop), threshold = threshold)
}

# Values comparable across memory and staged materialisations.
.ws7_canon <- function(df, prototype = df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  attributes(df)[setdiff(names(attributes(df)), c("names", "row.names", "class"))] <- NULL
  class(df) <- "data.frame"
  for (c in names(df)) {
    x <- df[[c]]
    x <- if (is.numeric(prototype[[c]])) {
      format(as.numeric(x), digits = 17, trim = TRUE, scientific = FALSE)
    } else as.character(x)
    x[is.na(df[[c]])] <- NA_character_
    df[[c]] <- x
  }
  rownames(df) <- NULL
  df
}

test_that("every native format publishes the same rows in memory and staged mode", {
  plan <- .ws7_pub_plan()
  frame <- .ws7_scenario_frame(c(7001, 7003, 7004, 7005, 7006, 7007))
  for (b in names(.ws7_pub_backends)) {
    h <- .ws7_pub_backends[[b]]$handle
    .ws7_keys(list(h))
    # support is not under test here (the intervals root has one person)
    mem <- .ws7_pub_run(h, plan, frame, "memory", threshold = 1L)
    stg <- .ws7_pub_run(h, plan, frame, "staged", threshold = 1L)
    expect_identical(mem$status$status, "ok", info = b)
    expect_identical(stg$status$status, "ok", info = b)
    expect_identical(names(mem$values), names(stg$values), info = b)
    for (root in names(mem$values)) {
      expect_identical(names(mem$values[[root]]), names(stg$values[[root]]), info = paste(b, root))
      for (comp in names(mem$values[[root]])) {
        expect_identical(.ws7_canon(mem$values[[root]][[comp]]), .ws7_canon(stg$values[[root]][[comp]], mem$values[[root]][[comp]]),
                         info = paste(b, root, comp))
      }
    }
    inv <- mem$status$published
    expect_identical(names(inv$s), c("long", "short"), info = b)     # TAR components in name order
    expect_identical(inv$s$short$symbol, "D_s.short", info = b)
    expect_identical(names(inv$r), c("events", "risk_sets"), info = b)
    expect_identical(names(inv$tc), c("tc", "covariateRef", "timeRef"), info = b)
    expect_identical(inv$tc$covariateRef$grain, "none", info = b)
    expect_identical(names(inv$sp), c("sp", "covariateRef"), info = b)
    expect_identical(names(inv$j), "j", info = b)
    # tokens, not raw person ids, and the reviewed episode keys are kept
    w <- mem$values$o$o
    expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$", w$person_id)), info = b)
    expect_true(all(c("row_id", "cohort_row_id") %in% names(w)), info = b)
  }
})

test_that("k = 5 gates each component and applies the publication matrix in both modes", {
  plan <- .ws7_pub_plan()
  h <- .ws7_pub_backends$sqlite$handle
  .ws7_keys(list(h))
  for (mode in c("memory", "staged")) {
    # Five episodes of one person are still one contributor.
    frame <- .ws7_scenario_frame(rep(7001, 5))
    frame$cohort_start_date <- as.Date("2020-01-10") + 0:4
    frame$index_key <- paste0("k", 1:5)
    res <- .ws7_pub_run(h, plan, frame, mode, threshold = 5L, publish = "o")
    expect_identical(res$status$status, "refused", info = mode)
    expect_identical(res$status$rejected$o$code, "insufficient_support", info = mode)
    expect_length(res$values, 0L)
    # Five persons contribute to the dense panel, but only four contribute
    # sparse temporal rows. A family refusal includes both its references.
    frame <- .ws7_scenario_frame(c(7001, 7002, 7005, 7006, 7007))
    for (strict in c(TRUE, FALSE)) for (policy in c("server", "all_or_none")) {
      res <- withr::with_options(list(dsomop.query_strict = strict),
        .ws7_pub_run(h, plan, frame, mode, threshold = 5L, publish = c("p", "tc"),
                     commit_policy = policy))
      partial <- !strict && identical(policy, "server")
      info <- paste(mode, strict, policy)
      expect_identical(res$status$status, if (partial) "partial" else "refused", info = info)
      expect_identical(names(res$status$rejected), "tc", info = info)
      expect_identical(res$status$rejected$tc$code, "insufficient_support", info = info)
      expect_identical(names(res$status$published), if (partial) "p" else NULL, info = info)
      expect_false(any(grepl("covariateRef|timeRef", unlist(lapply(res$status$published, names)))),
                   info = info)
      expect_true(.validatePlanStatus(unclass(res$status)), info = info)
    }
  }
})

test_that("a joined root shrinking below k is refused; its private inputs are never assigned", {
  vars <- list(n = list(source = list(event = "lab"), reducer = list(fn = "count")))
  plan <- .ws7_scenario_plan(vars, output = list(columns = list("n")))
  plan$outputs$l <- list(rows = "t", format = "long", sources = list(list(event = "lab")))
  plan$outputs$j <- list(rows = "t", format = "joined", grain = "episode",
                         options = list(from = list(list(output = "l"), list(output = "o")),
                                        cardinality = "many_to_one"))
  h <- .ws7_pub_backends$sqlite$handle
  .ws7_keys(list(h))
  run <- .ws7_ctx(h, c(plan, list(publish = list("j"))), mode = "memory")
  on.exit(.ws7_close(run), add = TRUE)
  pop <- .ws7_population(run, "t", .ws7_scenario_frame(c(7001, 7002, 7003, 7004)))
  res <- .ws7_publish(run, list(t = pop), threshold = 4L)
  # the inner join keeps 7001, 7003 (lab rows), 7004: three persons < 4
  expect_identical(res$status$status, "refused")
  expect_identical(ls(run$ctx$target_env), character(0))
  expect_identical(names(res$compiled), "j")
})

test_that("omopPlanExecuteDS: native execution end to end (memory and staged) with the inventory's names", {
  plan <- .ws7_pub_plan()
  plan$populations$t$index$source <- list(cohort_definition_id = 1L)
  for (b in names(.ws7_pub_backends)) {
    h <- .ws7_pub_backends[[b]]$handle
    .ws7_keys(list(h))
    sym <- paste0("ws7_e2e_", as.integer(stats::runif(1L, 1, 1e6)))
    .setHandle(sym, h)
    for (mode in c("memory", "staged")) {
      target <- new.env(parent = emptyenv())
      out <- list(o = "D_o", s = "D_s", r = "D_r", p = "D_p", m = "D_m", d = "D_d")
      plan$publish <- as.list(names(out))
      before <- h$temp_tables
      restore <- .ws7_seed_cohort(h, .ws7_scenario_frame(c(7001, 7003, 7004, 7005)))
      status <- tryCatch(withr::with_options(list(nfilter.subset = 3L),
        .omopPlanExecuteDS(sym, plan, out, output_mode = mode, target_env = target)),
        finally = restore())
      expect_identical(status$status, "ok", info = paste(b, mode))
      expect_true(.validatePlanStatus(unclass(status)), info = paste(b, mode))
      expect_setequal(ls(target), c("D_o", "D_s.short", "D_s.long", "D_r.events", "D_r.risk_sets", "D_p",
                                    "D_m", "D_d"))
      if (identical(mode, "staged")) {
        expect_s3_class(get("D_p", target), "FlowerDatasetDescriptor")
        expect_silent(omopStagedDatasetPath(get("D_p", target)))
      } else {
        expect_s3_class(get("D_o", target), "data.frame")
      }
      # no temporary object of the execution remains
      expect_setequal(h$temp_tables, before, info = paste(b, mode))
    }
    rm(list = paste0("handle_", sym), envir = .dsomop_env)
  }
})


test_that("reducer caps are bounded technical refusals with cleanup in both publication modes", {
  h <- .ws7_pub_backends$sqlite$handle
  .ws7_keys(list(h))
  sym <- "ws7_cap_publication"
  .setHandle(sym, h)
  on.exit(rm(list = paste0("handle_", sym), envir = .dsomop_env), add = TRUE)
  plan <- .ws7_scenario_plan(list(n = list(source = list(event = "lab"), reducer = list(fn = "count"))))
  restore <- .ws7_seed_cohort(h, .ws7_scenario_frame(7001L))
  on.exit(restore(), add = TRUE)
  # Keep this file runnable independently: lifecycle test-file functions are
  # not shared test helpers. Inspect the actual SQLite session tables here.
  temp_names <- function() DBI::dbGetQuery(h$conn,
    "SELECT name FROM sqlite_temp_master WHERE type = 'table'")$name
  before <- temp_names()
  before_registered <- h$temp_tables
  for (mode in c("memory", "staged")) for (policy in c("server", "all_or_none")) {
    for (cap in c(5L, 4L)) local({
      target <- new.env(parent = emptyenv())
      target$sentinel <- "caller-owned"
      status <- withr::with_options(list(dsomop.max_memory_rows = cap, nfilter.subset = 1L,
                                         dsomop.query_strict = FALSE),
        .omopPlanExecuteDS(sym, plan, list(o = "D_cap"), output_mode = mode,
                           commit_policy = policy, target_env = target))
      ctx <- get(attr(status, "execution_id"), .publicationRegistry)
      on.exit(.publishAbort(ctx), add = TRUE)
      info <- paste(mode, policy, cap)
      expect_true(.validatePlanStatus(unclass(status)), info = info)
      expect_identical(target$sentinel, "caller-owned", info = info)
      expect_setequal(temp_names(), before)
      expect_setequal(h$temp_tables, before_registered)
      # The inner compiler must keep IR until a typed refusal is published;
      # the public boundary must then close both success and refusal paths.
      expect_true(ctx$closed, info = info)
      expect_null(ctx$ir)
      expect_false(exists(ctx$id, .planContextRegistry, inherits = FALSE), info = info)
      expect_length(ctx$compile_context$compile_temp_tables, 0L)
      expect_length(ls(ctx$staging_env, all.names = TRUE), 0L)
      if (cap == 5L) {
        expect_identical(status$status, "ok", info = info)
        frame <- target$D_cap
        if (inherits(frame, "FlowerDatasetDescriptor")) {
          chunks <- list()
          .publishReadChunks(frame, function(x) chunks[[length(chunks) + 1L]] <<- x)
          frame <- do.call(rbind, chunks)
        }
        expect_identical(as.integer(frame$n), 5L, info = info)
      } else {
        expect_identical(status$status, "refused", info = info)
        expect_identical(ls(target), "sentinel", info = info)
        expect_identical(status$rejected$o$code, "validation_error", info = info)
        expect_identical(status$rejected$o$reason, "reducer_row_cap", info = info)
        expect_setequal(names(status$rejected$o), c("code", "message", "reason"))
        expect_false(grepl("7001|97000|/variables/|dsomop.max_memory_rows", status$rejected$o$message),
                     info = info)
      }
    })
  }
})
