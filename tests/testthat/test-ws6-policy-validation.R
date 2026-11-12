# Coverage compatibility is a static rule even when a set operation's
# members obtain their policy through another set operation or a match.

test_that("nested set operations validate effective coverage before site access", {
  index <- function(policy) {
    list(kind = "index", index = list(source = list(event = "idx")),
         coverage = list(policy = policy))
  }
  setop <- function(op, ...) list(kind = "setop", setop = list(op = op, members = list(...)))
  pops <- list(
    unique_a = index("unique"), unique_b = index("unique"),
    earliest_a = index("earliest"), earliest_b = index("earliest"),
    unique_union = setop("union", "unique_a", "unique_b"),
    earliest_union = setop("union", "earliest_a", "earliest_b"))
  events <- list(idx = .ws6_event("condition_occurrence"))
  site_access <- 0L
  local_mocked_bindings(.irSiteErrors = function(...) {
    site_access <<- site_access + 1L
    stop("site checks must not run for incompatible coverage")
  })
  for (op in c("union", "intersect", "setdiff")) {
    plan <- .ws6_plan(events, c(pops, list(outer = setop(op, "unique_union", "earliest_union"))))
    err <- tryCatch(.irPrepare(plan, handle = new.env(parent = emptyenv())),
                    dsomop_plan_rejection = function(e) e)
    expect_s3_class(err, "dsomop_plan_rejection")
    expect_match(paste(err$errors, collapse = "\n"),
                 "/populations/outer/setop/members: set-op members must share one coverage policy",
                 fixed = TRUE)
  }
  expect_identical(site_access, 0L)
  # Equivalent effective policies remain valid across several derived levels.
  good <- c(pops[c("unique_a", "unique_b", "unique_union")], list(
    unique_intersect = setop("intersect", "unique_union", "unique_a"),
    outer = setop("union", "unique_union", "unique_intersect")))
  expect_s3_class(.irPrepare(.ws6_plan(events, good)), "dsomop_ir")
})

test_that("a set operation validates a matched member's inherited coverage policy", {
  index <- function(policy) {
    list(kind = "index", index = list(source = list(event = "idx")),
         coverage = list(policy = policy))
  }
  pops <- list(
    cases = index("earliest"),
    controls = list(kind = "criteria", coverage = list(policy = "earliest")),
    matched = list(kind = "match", match = list(cases = "cases", controls = "controls",
                                                 outcome = list(event = "idx"))),
    other = index("unique"),
    outer = list(kind = "setop", setop = list(op = "union", members = list("matched", "other"))))
  plan <- .ws6_plan(list(idx = .ws6_event("condition_occurrence")), pops)
  err <- tryCatch(.irPrepare(plan), dsomop_plan_rejection = function(e) e)
  expect_s3_class(err, "dsomop_plan_rejection")
  expect_match(paste(err$errors, collapse = "\n"),
               "/populations/outer/setop/members: set-op members must share one coverage policy",
               fixed = TRUE)
  plan$populations$other$coverage$policy <- "earliest"
  expect_s3_class(.irPrepare(plan), "dsomop_ir")
})
