# A-20(b), review B5: the WS-2c named-anchor inventory is exercised through
# full IR preparation and the actual final-roster anchor compiler.
test_that("named long-output anchors compile after full IR preparation (B5)", {
  backends <- .ws5_backends()
  on.exit(.ws4_cleanup(backends))
  for (backend in names(backends)) {
    h <- backends[[backend]]$handle
    first_a <- list(source = list(event = "a"))
    explicit_a <- c(first_a, list(which = "first", n = 1L, field = "start", scope = "person", search = NULL))
    first_b <- list(source = list(event = "b"))
    after_b <- list(source = list(event = "a"), search = list(start = 1L, end = NULL, anchor = first_b))
    plan <- .ws5_plan(
      events = list(
        a = list(table = "condition_occurrence", concept_set = list(concepts = list(.WS5$ANCHOR_A))),
        b = list(table = "measurement", concept_set = list(concepts = list(.WS5$ANCHOR_B)))),
      populations = list(t = list(kind = "criteria")),
      outputs = list(events = list(rows = "t", format = "long", sources = list(list(event = "a")),
                                   options = list(anchors = list(first = first_a, same = explicit_a, after = after_b)))))
    ir <- .irPrepare(plan, h)
    expect_s3_class(ir, "dsomop_ir", info = backend)
    expect_length(.irAnchorInventory(ir)$final$t, 3L, info = backend)
    expect_length(.irAnchorInventory(ir)$candidate$t, 0L, info = backend)
    ctx <- .planContext(h, ir)
    roster <- .ws5_literal_roster(ctx, data.frame(
      cohort_row_id = c(31L, 92L), subject_id = c(3201L, 3202L),
      cohort_start_date = "2020-01-01", cohort_end_date = "2020-12-31"),
      name = "pop_t", population = "t", phase = "final")
    compiled <- lapply(list(first_a, explicit_a, after_b, first_b), function(anchor) {
      .compileAnchor(ctx, anchor, roster, "final")
    })
    expect_identical(compiled[[1L]]$name, compiled[[2L]]$name, info = backend)
    expect_identical(compiled[[1L]]$roster_namespace, "t", info = backend)
    rows <- lapply(compiled, function(fragment) .ws5_rows(ctx, fragment, "row_key"))
    expect_identical(vapply(rows, function(x) x$anchor_date[[1L]], ""),
                     c("2019-06-01", "2019-06-01", "2019-06-01", "2019-05-01"), info = backend)
    expect_true(all(vapply(rows, function(x) identical(as.numeric(x$row_key), 31), logical(1))),
                info = backend)
    # The inventory fix never weakens the fail-closed allocator.
    expect_error(.compileAnchor(ctx, list(source = list(event = "a"), which = "last"), roster, "final"),
                 "not declared by the plan's anchor inventory", info = backend)
  }
  .ws5_expect_backends(backends, "ws5 named anchor inventory")
})
