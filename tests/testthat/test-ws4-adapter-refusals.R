# Amendment A-18 end to end: a real language-2.0 adapter plan executed through
# omopPlanExecuteDS on the extended reach fixture (PostgreSQL on the pod,
# SQLite everywhere). The section 11 adapter refusals are raised while the
# sources compile, before any output is materialised; the status of every
# requested root is `refused` with code validation_error and the closed reason
# (adapter_person_mismatch, cost_currency_mixed) under both commit policies --
# "server" (client on_small_site = "error") and "all_or_none" ("skip") -- and
# nothing is published. The condition's message, field path and state never
# travel.

.ws4_ar_meta <- list(language_version = "2.0", semantic_version = "2.0.0",
                     generated_by = "ws4-a18-test")

.ws4_ar_members_plan <- function() {
  list(
    version = 2L, meta = .ws4_ar_meta,
    events = list(
      episodes = list(table = "episode", concept_set = list(concepts = list(32531))),
      members = list(episode_members = list(episode = list(event = "episodes")))),
    populations = list(onc = list(
      kind = "index",
      index = list(source = list(event = "episodes"), select = "all",
                   end = list(strategy = "event_end")))),
    variables = list(n_members = list(source = list(event = "members"),
                                      reducer = list(fn = "count"))),
    outputs = list(episodes_out = list(rows = "onc", format = "wide",
                                       columns = list("n_members"))))
}

.ws4_ar_cost_plan <- function(currency = NULL) {
  cost <- list(event = list(event = "visits"), field = "total_cost")
  if (!is.null(currency)) cost$currency <- currency
  list(
    version = 2L, meta = .ws4_ar_meta,
    events = list(visits = list(table = "visit_occurrence"),
                  visit_cost = list(cost = cost)),
    populations = list(all = list(kind = "criteria")),
    variables = list(cost = list(source = list(event = "visit_cost"),
                                 reducer = list(fn = "sum", value = "value"))),
    outputs = list(costs = list(rows = "all", format = "wide", columns = list("cost"))))
}

.ws4_ar_execute <- function(handle, plan, out, policy) {
  sym <- paste0("ws4_a18_", as.integer(stats::runif(1L, 1, 1e6)))
  .setHandle(sym, handle)
  # unregister without closing: the fixture owns the connection
  on.exit(rm(list = paste0("handle_", sym), envir = .dsomop_env), add = TRUE)
  target <- new.env(parent = emptyenv())
  status <- withr::with_options(list(nfilter.subset = 3L),
    .omopPlanExecuteDS(sym, plan, out, target_env = target, commit_policy = policy))
  list(status = status, target = target)
}

.ws4_ar_expect_refused <- function(run, root, reason, info) {
  st <- run$status
  expect_identical(st$status, "refused", info = info)
  expect_identical(names(st$rejected), root, info = info)
  expect_identical(st$rejected[[root]],
                   list(code = "validation_error",
                        message = "Publication refused: validation_error.",
                        reason = reason), info = info)
  expect_length(st$published, 0L)
  expect_identical(ls(run$target, all.names = TRUE), character(0), info = info)
  expect_true(.validatePlanStatus(st), info = info)
  # no raw message, field path, event name or count reaches the status
  flat <- paste(unlist(st), collapse = " ")
  expect_false(grepl("belongs to another person|mix currencies|/events/|members|visit_cost|[0-9]",
                     flat), info = info)
}

.ws4_ar_backends_with <- function(with_defects) {
  b <- .ws4_backends(with_defects = with_defects)
  if (nzchar(Sys.getenv("DSOMOP_TEST_POSTGRES_HOST"))) {
    expect_true("postgresql" %in% names(b))
  }
  b
}

test_that("A-18: adapter_person_mismatch travels with its reason under both commit policies, nothing published", {
  backends <- .ws4_ar_backends_with(with_defects = TRUE)
  on.exit(.ws4_cleanup(backends), add = TRUE)
  for (b in intersect(names(backends), c("sqlite", "postgresql"))) {
    h <- backends[[b]]$handle
    for (policy in c("server", "all_or_none")) {
      run <- .ws4_ar_execute(h, .ws4_ar_members_plan(), list(episodes_out = "D_ep"), policy)
      .ws4_ar_expect_refused(run, "episodes_out", "adapter_person_mismatch", paste(b, policy))
    }
  }
})

test_that("A-18: cost_currency_mixed travels with its reason under both commit policies, nothing published", {
  backends <- .ws4_ar_backends_with(with_defects = FALSE)
  on.exit(.ws4_cleanup(backends), add = TRUE)
  for (b in intersect(names(backends), c("sqlite", "postgresql"))) {
    h <- backends[[b]]$handle
    for (policy in c("server", "all_or_none")) {
      run <- .ws4_ar_execute(h, .ws4_ar_cost_plan(), list(costs = "D_cost"), policy)
      .ws4_ar_expect_refused(run, "costs", "cost_currency_mixed", paste(b, policy))
    }
    # the avoidable refusal: restricted to one currency the source compiles and
    # the plan is never refused for this reason (until the later compilers are
    # installed it is refused as not yet executable, without a reason)
    for (policy in c("server", "all_or_none")) {
      run <- .ws4_ar_execute(h, .ws4_ar_cost_plan(currency = 44818668), list(costs = "D_cost"), policy)
      st <- run$status
      if (identical(st$status, "refused")) {
        expect_false("reason" %in% names(st$rejected$costs), info = paste(b, policy))
        expect_identical(ls(run$target, all.names = TRUE), character(0))
      }
      expect_true(.validatePlanStatus(st))
    }
    # the same members plan on a site whose members agree: no adapter refusal
    run <- .ws4_ar_execute(h, .ws4_ar_members_plan(), list(episodes_out = "D_ep"), "server")
    if (identical(run$status$status, "refused")) {
      expect_false("reason" %in% names(run$status$rejected$episodes_out), info = b)
    }
  }
})

test_that("A-18: the client's refusal diagnostic names the reason for the real status", {
  skip_if_not_installed("dsOMOPClient")
  validate <- get0(".plan_validate_status", envir = asNamespace("dsOMOPClient"), inherits = FALSE)
  skip_if(is.null(validate), "dsOMOPClient without the A-18 status validator")
  backends <- .ws4_ar_backends_with(with_defects = TRUE)
  on.exit(.ws4_cleanup(backends), add = TRUE)
  h <- (backends$postgresql %||% backends$sqlite)$handle
  for (policy in c("server", "all_or_none")) {
    run <- .ws4_ar_execute(h, .ws4_ar_members_plan(), list(episodes_out = "D_ep"), policy)
    expect_silent(validate(run$status, "episodes_out"))
    expect_identical(run$status$rejected$episodes_out$reason, "adapter_person_mismatch")
  }
})
