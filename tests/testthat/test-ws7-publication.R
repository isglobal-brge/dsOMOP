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
  plan$outputs$m <- list(rows = "t", format = "membership")
  plan$outputs$d <- list(rows = "t", format = "dictionary")
  plan$outputs$iv <- list(rows = "t", format = "intervals",
                          options = list(sources = list(list(event = "drug"))))
  plan
}

.ws7_pub_run <- function(handle, plan, frame, mode, threshold = 3L, publish = NULL) {
  if (!is.null(publish)) plan$publish <- as.list(publish)
  run <- .ws7_ctx(handle, plan, mode = mode)
  on.exit(.ws7_close(run), add = TRUE)
  pop <- .ws7_population(run, "t", frame, cap_end = FALSE)
  .ws7_publish(run, list(t = pop), threshold = threshold)
}

# Values comparable across memory and staged materialisations.
.ws7_canon <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  attributes(df)[setdiff(names(attributes(df)), c("names", "row.names", "class"))] <- NULL
  class(df) <- "data.frame"
  for (c in names(df)) {
    x <- df[[c]]
    x <- if (is.numeric(x)) format(x, digits = 12, trim = TRUE, scientific = FALSE) else as.character(x)
    x[is.na(df[[c]]) | x %in% c("NA", "")] <- NA_character_
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
        expect_identical(.ws7_canon(mem$values[[root]][[comp]]), .ws7_canon(stg$values[[root]][[comp]]),
                         info = paste(b, root, comp))
      }
    }
    inv <- mem$status$published
    expect_identical(names(inv$s), c("long", "short"), info = b)     # TAR components in name order
    expect_identical(inv$s$short$symbol, "D_s.short", info = b)
    expect_identical(names(inv$r), c("events", "risk_sets"), info = b)
    expect_identical(names(inv$tc), c("tc", "covariateRef", "timeRef"), info = b)
    expect_identical(inv$tc$covariateRef$grain, "none", info = b)
    # tokens, not raw person ids, and the reviewed episode keys are kept
    w <- mem$values$o$o
    expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$", w$person_id)), info = b)
    expect_true(all(c("row_id", "cohort_row_id") %in% names(w)), info = b)
  }
})

test_that("each component is gated on its own support; one person's episodes count once", {
  plan <- .ws7_pub_plan()
  h <- .ws7_pub_backends$sqlite$handle
  .ws7_keys(list(h))
  # five episodes of one person: support 1 < 3
  frame <- .ws7_scenario_frame(rep(7001, 5))
  frame$cohort_start_date <- as.Date("2020-01-10") + 0:4
  frame$index_key <- paste0("k", 1:5)
  res <- .ws7_pub_run(h, plan, frame, "memory", publish = "o")
  expect_identical(res$status$status, "refused")
  expect_identical(res$status$rejected$o$code, "insufficient_support")
  # three persons: the dense panel passes; the temporal covariates (only 7001
  # and 7005 have lab rows in the window) fail with k = 3, and the refused
  # family publishes no reference companion
  frame <- .ws7_scenario_frame(c(7001, 7002, 7005))
  res <- .ws7_pub_run(h, plan, frame, "memory", publish = c("p", "tc"))
  expect_identical(res$status$status, "refused")
  expect_identical(names(res$status$rejected), "tc")
  withr::local_options(list(dsomop.query_strict = FALSE))
  res <- .ws7_pub_run(h, plan, frame, "memory", publish = c("p", "tc"))
  expect_identical(res$status$status, "partial")
  expect_identical(names(res$status$published), "p")
  expect_false(any(grepl("covariateRef|timeRef", unlist(lapply(res$status$published, names)))))
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
  pop <- .ws7_population(run, "t", .ws7_scenario_frame(c(7001, 7002, 7003, 7004)), cap_end = FALSE)
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
      testthat::local_mocked_bindings(.outputPopulation = function(cctx, pop, populations = NULL) {
        memo <- .compileMemo(cctx, "ws7_test_pops")
        if (exists(pop, envir = memo, inherits = FALSE)) return(get(pop, envir = memo))
        rec <- .ws7_population(list(cctx = cctx), pop, .ws7_scenario_frame(c(7001, 7003, 7004, 7005)),
                               cap_end = FALSE)
        assign(pop, rec, envir = memo)
        rec
      })
      target <- new.env(parent = emptyenv())
      out <- list(o = "D_o", s = "D_s", r = "D_r", p = "D_p", m = "D_m", d = "D_d")
      plan$publish <- as.list(names(out))
      status <- withr::with_options(list(nfilter.subset = 3L),
        .omopPlanExecuteDS(sym, plan, out, output_mode = mode, target_env = target))
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
      expect_false(any(grepl("^dsomop_ws7_pop_", h$temp_tables %||% character(0))), info = paste(b, mode))
    }
    rm(list = paste0("handle_", sym), envir = .dsomop_env)
  }
})
