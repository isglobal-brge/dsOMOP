# Custom extraction filter DSL (Phase C).
#
# These tests prove the previously-dead `.compileFilter` DSL is now wired into
# the extraction path (.compileSelect / .extractTable / .planExecute) via the
# new `filters=` argument and the fail-closed `.assertCustomFilterSafe` gate.
#
# Fixture facts used below (parent-level tests/fixtures/create_test_db.R):
#   measurement HbA1c (concept 3004410): persons 1,3,5,7,9
#     value_as_number = 7.2, 6.5, 8.1, 9.0, 7.5
#     unit_concept_id = 8554, measurement_type_concept_id = 44818518
#   measurement body weight (concept 3025315): persons 1..13, unit 9529
#   procedure_occurrence: 5 rows (persons 1,3,5,7,9) with visit_occurrence_id
#     populated, all linking to visit_occurrence.visit_concept_id = 9201
#   default disclosure: nfilter_subset = 3 (>= 3 distinct persons required)

# --- (a) A custom row filter changes the emitted SQL and the returned rows ----

test_that("a client-authored ordered threshold is rejected", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .compileSelect(handle, "measurement",
      columns = c("person_id", "value_as_number"),
      filters = list(var = "value_as_number", op = ">=", value = 7.5)),
    "server-issued value_bin"
  )
})

test_that("an ordered threshold cannot reach extraction", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .extractTable(handle, "measurement",
      columns = c("person_id", "value_as_number"),
      concept_filter = 3004410L, translate_concepts = FALSE,
      filters = list(var = "value_as_number", op = ">=", value = 7.5)),
    "server-issued value_bin"
  )
})

test_that("a value_bin row filter narrows the extracted rows", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(
    list(nfilter.subset = 3, nfilter.tab = 3,
         dsomop.default_date_handling = "remove"), {
    scope <- .test_issue_safe_bins(
      handle, c(0, 7.5, 10, 20), concept_id = 3004410L)
    # value_bin [7.5, 10) keeps 7.5, 8.1, 9.0 (persons 5, 7, 9).
    binned <- .extractTable(handle, "measurement",
                            columns = c("person_id", "value_as_number"),
                            concept_filter = 3004410L,
                            translate_concepts = FALSE,
                            filters = list(var = "value_as_number",
                                           op = "value_bin",
                                           value = list(lower = 7.5,
                                                        upper = 10),
                                           safe_scope = scope))
    expect_equal(nrow(binned), 3L)
    expect_true(all(binned$value_as_number >= 7.5 &
                      binned$value_as_number < 10))
  })
})

test_that("a generic date_range filter resolves and narrows real event rows", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # HbA1c has five rows: one in 2019, three in 2020, and one in 2021. The
  # client emits start_date sentinels; the server must resolve them to
  # measurement_date and return the three 2020 rows (not merely compile SQL).
  date_range <- list(
    var = "start_date", op = "between",
    value = list("2020-01-01", "2020-12-31")
  )
  withr::with_options(
    list(nfilter.subset = 3, dsomop.default_date_handling = "remove"), {
    result <- .extractTable(
      handle, "measurement",
      columns = c("person_id", "value_as_number"),
      concept_filter = 3004410L,
      translate_concepts = FALSE,
      filters = date_range
    )
    expect_equal(nrow(result), 3L)
    expect_equal(sort(result$person_id), c(1L, 3L, 5L))
  })
})

test_that("person_level output applies output-level custom filters", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(labs = list(
      type = "person_level",
      tables = list(measurement = list(
        concept_set = 3004410L,
        features = list(lab_mean = list(
          type = "mean_value", name = "lab_mean",
          concept_set = 3004410L, value_column = "value_as_number"
        ))
      )),
      filters = list(custom = list(
        var = "value_as_number", op = "value_bin",
        value = list(lower = 7.5, upper = 10)
      ))
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(
    list(nfilter.subset = 3, dsomop.default_date_handling = "remove"), {
    plan$outputs$labs$filters$custom$safe_scope <- .test_issue_safe_bins(
      handle, c(0, 7.5, 10, 20), concept_id = 3004410L)
    result <- .planExecute(handle, plan, list(labs = "labs_df"))$labs
    expect_equal(nrow(result), 3L)
    expect_true(all(result$lab_mean >= 7.5 & result$lab_mean < 10))
  })
})

test_that("person_level rejects one raw repeatable event table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(raw_labs = list(
      type = "person_level",
      tables = list(measurement = c("person_id", "value_as_number"))
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(dsomop.query_strict = TRUE), {
    expect_error(
      .planExecute(handle, plan, list(raw_labs = "raw_labs_df")),
      "one row per person|features|event_level"
    )
  })
})

test_that("person_level rejects many-to-many raw table merges", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(raw_join = list(
      type = "person_level",
      tables = list(
        measurement = c("person_id", "value_as_number"),
        condition_occurrence = c("person_id", "condition_concept_id")
      )
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(dsomop.query_strict = TRUE), {
    expect_error(
      .planExecute(handle, plan, list(raw_join = "raw_join_df")),
      "one row per person|features|event_level"
    )
  })
})

test_that("a *_type_concept_id row filter selects the matching subset", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # All measurements share type 44818518; an "in" filter on that type keeps
  # them, a different type keeps none. Proves the DSL reaches the SQL on a
  # *_type_concept_id column (a row-level filter the gate permits via "in").
  sql_match <- .compileSelect(handle, "measurement",
                              columns = c("person_id", "value_as_number"),
                              filters = list(var = "measurement_type_concept_id",
                                             op = "in", value = list(44818518L)))
  expect_match(sql_match, "measurement_type_concept_id IN \\(44818518\\)")

  withr::with_options(
    list(nfilter.subset = 3, nfilter.tab = 3,
         dsomop.default_date_handling = "remove"), {
    kept <- .extractTable(handle, "measurement",
                          columns = c("person_id", "value_as_number"),
                          concept_filter = 3004410L,
                          translate_concepts = FALSE,
                          filters = list(var = "measurement_type_concept_id",
                                         op = "in", value = list(44818518L)))
    expect_equal(nrow(kept), 5L)
  })
})

# --- (b) Nested AND / OR filters --------------------------------------------

test_that("nested AND filter ANDs both leaves into the WHERE", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(
    list(nfilter.subset = 3, nfilter.tab = 3,
         dsomop.default_date_handling = "remove"), {
    scope <- .test_issue_safe_bins(
      handle, c(0, 7, 10, 20), concept_id = 3004410L)
    # Intersecting issued bins [0,10) and [7,20) keeps [7,10).
    res <- .extractTable(handle, "measurement",
                         columns = c("person_id", "value_as_number"),
                         concept_filter = 3004410L,
                         translate_concepts = FALSE,
                         filters = list(and = list(
                           .test_value_bin_leaf(scope, 0, 10),
                           .test_value_bin_leaf(scope, 7, 20)
                         )))
    expect_equal(nrow(res), 4L)
    expect_true(all(res$value_as_number >= 7.0 & res$value_as_number <= 9.0))
  })
})

test_that("nested OR filter emits an OR-joined predicate group", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  scope <- .test_issue_safe_bins(
    handle, c(0, 6, 6.6, 9, 10, 20), concept_id = 3004410L)
  sql <- .compileSelect(handle, "measurement",
                        columns = c("person_id", "value_as_number"),
                        filters = list(or = list(
                          .test_value_bin_leaf(scope, 9, 10),
                          .test_value_bin_leaf(scope, 6, 6.6)
                        )))
  # Both leaves present, joined by OR inside a parenthesised group.
  expect_match(sql, "OR")
  expect_match(sql, "t.value_as_number >= 9", fixed = TRUE)
  expect_match(sql, "t.value_as_number < 6.6", fixed = TRUE)
})

test_that("nested AND-of-OR filter preserves the boolean structure", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(
    list(nfilter.subset = 3, nfilter.tab = 3,
         dsomop.default_date_handling = "remove"), {
    scope <- .test_issue_safe_bins(
      handle, c(0, 6, 6.6, 8.1, 10, 20), concept_id = 3004410L)
    # AND(OR([8.1,10), [6,6.6)), [6,10)) keeps 6.5, 8.1 and 9.0.
    res <- .extractTable(handle, "measurement",
                         columns = c("person_id", "value_as_number"),
                         concept_filter = 3004410L,
                         translate_concepts = FALSE,
                         filters = list(and = list(
                           list(or = list(
                             .test_value_bin_leaf(scope, 8.1, 10),
                             .test_value_bin_leaf(scope, 6, 6.6)
                           )),
                           .test_value_bin_leaf(scope, 6, 10)
                         )))
    expect_equal(sort(res$value_as_number), c(6.5, 8.1, 9.0))
  })
})

# --- (c) recipe -> plan -> SQL forwarding (server reads out$filters) ----------

test_that("plan execute applies output$filters$custom to the extraction", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Mirror what the client's recipe_to_plan emits: a per-variable row filter
  # lands in output$filters$custom (the slot .planExecute forwards to
  # .compileSelect's filters= arg).
  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      labs = list(
        type = "event_level",
        table = "measurement",
        columns = c("person_id", "value_as_number"),
        concept_set = c(3004410),
        representation = list(format = "long"),
        filters = list(
          concept_set = list(ids = c(3004410)),
          custom = list(var = "value_as_number", op = ">=", value = 7.5)
        )
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(
    list(nfilter.subset = 3, dsomop.default_date_handling = "remove"), {
    plan$outputs$labs$filters$custom <- .test_value_bin_leaf(
      .test_issue_safe_bins(
        handle, c(0, 7.5, 10, 20), concept_id = 3004410L),
      7.5, 10
    )
    res <- .planExecute(handle, plan, list(labs = "labs_df"))
    expect_true(is.data.frame(res$labs))
    expect_equal(nrow(res$labs), 3L)
    expect_true(all(res$labs$value_as_number >= 7.5))
  })
})

test_that("plan execute applies a per-variable filter + time_window together", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # filters$time_window is the slot the event_level branch reads into the
  # extraction time_window. Body weight (3025315) rows span 2021-* and a
  # 2022-06-15 repeat; bounding to 2021 plus value >= 60 forwards BOTH a
  # custom row filter and a time window through the same plan output.
  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      bw = list(
        type = "event_level",
        table = "measurement",
        columns = c("person_id", "value_as_number", "measurement_date"),
        concept_set = c(3025315),
        representation = list(format = "long"),
        filters = list(
          concept_set = list(ids = c(3025315)),
          custom = list(var = "value_as_number", op = ">=", value = 60),
          time_window = list(date_column = "measurement_date",
                             start_date = "2021-01-01",
                             end_date = "2021-12-31")
        )
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(
    list(nfilter.subset = 3, dsomop.default_date_handling = "absolute",
         dsomop.allow_absolute_dates = TRUE), {
    plan$outputs$bw$filters$custom <- .test_value_bin_leaf(
      .test_issue_safe_bins(
        handle, c(0, 60, 1000), concept_id = 3025315L),
      60, 1000
    )
    # Baseline: same plan with neither the row filter nor the window.
    bare <- plan
    bare$outputs$bw$filters <- list(concept_set = list(ids = c(3025315)))
    full_res <- .planExecute(handle, bare, list(bw = "bw_df"))$bw

    res <- .planExecute(handle, plan, list(bw = "bw_df"))$bw
    expect_true(is.data.frame(res))
    # Window drops the 2022-06-15 repeats; value filter keeps >= 60.
    expect_true(all(res$value_as_number >= 60))
    expect_true(all(grepl("^2021-", as.character(res$measurement_date))))
    expect_lt(nrow(res), nrow(full_res))
  })
})

# --- (d) concept_col scoping by unit / type ----------------------------------

test_that("concept_col scopes the concept IN-list to an alternate column", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Default: concept_filter scopes the domain concept column.
  sql_default <- .compileSelect(handle, "measurement",
                                columns = c("person_id", "value_as_number"),
                                concept_filter = 9529L)
  # Override: scope the SAME id against unit_concept_id instead.
  sql_unit <- .compileSelect(handle, "measurement",
                             columns = c("person_id", "value_as_number"),
                             concept_filter = 9529L,
                             concept_col = "unit_concept_id")

  expect_match(sql_default, "measurement_concept_id IN \\(9529\\)")
  expect_match(sql_unit, "unit_concept_id IN \\(9529\\)")
  expect_false(grepl("unit_concept_id IN", sql_default))
})

test_that("profileColumnStats concept_col=unit scopes to that unit's subset", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    unscoped <- .profileColumnStats(handle, "measurement", "value_as_number")
    # unit 9529 = body weight only (21 rows); strict subset of all 26 rows.
    by_unit <- .profileColumnStats(handle, "measurement", "value_as_number",
                                   concept_id = 9529L,
                                   concept_col = "unit_concept_id")

    expect_true(!is.na(by_unit$n_total))
    expect_lt(by_unit$n_total, unscoped$n_total)
  })
})

# --- (e) visit-linkage filter -------------------------------------------------

test_that("visit_filter emits an EXISTS join on visit_occurrence", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  sql <- .compileSelect(handle, "procedure_occurrence",
                        columns = c("person_id", "procedure_concept_id"),
                        visit_filter = list(concept_ids = 9201L))
  expect_match(sql, "EXISTS")
  expect_match(sql, "visit_occurrence")
  expect_match(sql, "visit_concept_id IN \\(9201\\)")
})

test_that("visit_filter restricts the extracted rows to matching visits", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(
    list(nfilter.subset = 3, nfilter.tab = 3,
         dsomop.default_date_handling = "remove"), {
    # All 5 procedures link to inpatient visits (9201).
    inpatient <- .extractTable(handle, "procedure_occurrence",
                               columns = c("person_id", "procedure_concept_id"),
                               translate_concepts = FALSE,
                               visit_filter = list(concept_ids = 9201L))
    expect_equal(nrow(inpatient), 5L)
  })
})

# --- (f) DISCLOSURE: fail-closed gate ----------------------------------------

test_that("an unissued narrow threshold is fail-closed before querying", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(
    list(nfilter.subset = 3, dsomop.default_date_handling = "remove"), {
    expect_error(
      .extractTable(handle, "measurement",
                    columns = c("person_id", "value_as_number"),
                    concept_filter = 3004410L,
                    translate_concepts = FALSE,
                    filters = list(var = "value_as_number", op = "<",
                                   value = 6.6)),
      "server-issued value_bin"
    )
  })
})

test_that("a filter on an identifier column is rejected before any SQL", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .compileSelect(handle, "measurement",
                   columns = c("person_id", "value_as_number"),
                   filters = list(var = "person_id", op = ">=", value = 1)),
    "not permitted|identifier"
  )
})

test_that("a filter on a blocked/source column is rejected", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # observation.value_as_string is a blocked (free-text) column: it must be
  # absent from the filterable allowlist and rejected fail-closed.
  bp <- .buildBlueprint(handle)
  valid <- .filterableColumns(bp, "observation")
  blocked <- bp$columns[["observation"]]$column_name[
    bp$columns[["observation"]]$is_blocked]
  expect_true("value_as_string" %in% blocked)
  expect_false("value_as_string" %in% valid)

  expect_error(
    .assertCustomFilterSafe(list(var = "value_as_string", op = "in",
                                 value = list("Current smoker")), valid),
    "not permitted|identifier|blocked"
  )
})

test_that("an exact-match (==) filter is blocked as fingerprinting", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  bp <- .buildBlueprint(handle)
  valid <- .filterableColumns(bp, "measurement")

  # value_as_number is filterable, but == on an arbitrary column is the
  # fingerprinting primitive -> mapped to "custom" -> blocked.
  expect_error(
    .assertCustomFilterSafe(list(var = "value_as_number", op = "==",
                                 value = 7.2), valid),
    "not allowed|fingerprint|custom"
  )
  # Ordered thresholds on the same column must use an issued value_bin.
  expect_error(
    .assertCustomFilterSafe(list(var = "value_as_number", op = ">=",
                                 value = 7.5), valid),
    "server-issued value_bin"
  )
  # Membership remains available for reviewed categorical/concept columns.
  expect_invisible(
    .assertCustomFilterSafe(list(var = "unit_concept_id", op = "in",
                                 value = list(9529L)), valid))
})

test_that("custom filter semantics reject narrow dates and unsafe operators", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  measurement_cols <- .filterableColumns(bp, "measurement")

  withr::with_options(list(nfilter.subset = 3), {
    expect_error(
      .assertCustomFilterSafe(
        list(var = "measurement_date", op = ">=", value = "2020-01-01"),
        measurement_cols),
      "standalone date"
    )
    expect_error(
      .assertCustomFilterSafe(
        list(var = "measurement_date", op = "between",
             value = list("2020-01-01", "2020-01-15")),
        measurement_cols),
      "at least 30 days"
    )
  })
  expect_error(
    .assertCustomFilterSafe(
      list(var = "measurement_date", op = "between",
           value = list("2020-02-30", "2020-12-31")),
      measurement_cols),
    "valid date"
  )
  expect_error(
    .assertCustomFilterSafe(
      list(var = "value_as_number", op = "in", value = list(7.2, 7.5)),
      measurement_cols),
    "concept IDs|categorical"
  )
  expect_error(
    .assertCustomFilterSafe(
      list(var = "value_as_number", op = "between", value = c(7, 10)),
      measurement_cols),
    "use a validated value_bin"
  )
  for (bounds in list(
      list(lower = 10, upper = 7),
      list(lower = -Inf, upper = 7),
      list(lower = 7, upper = NA_real_))) {
    expect_error(
      .assertCustomFilterSafe(
        list(var = "value_as_number", op = "value_bin", value = bounds),
        measurement_cols),
      "finite scalar lower/upper"
    )
  }
})

test_that("compileSelect rejects semantic scopes it cannot apply", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .compileSelect(handle, "measurement", columns = "not_a_column"),
    "Column.*not found"
  )
  expect_error(
    .compileSelect(handle, "concept", concept_filter = 1L),
    "no usable concept column"
  )
  expect_error(
    .compileSelect(handle, "measurement", concept_filter = c(3004410, NA)),
    "finite integer concept"
  )
  expect_error(
    .compileSelect(handle, "concept", person_ids = 1:3),
    "no person_id"
  )
  expect_error(
    .compileSelect(handle, "concept", cohort_table = "some_cohort"),
    "no person_id|join path"
  )
  expect_error(
    .compileSelect(handle, "person",
      visit_filter = list(concept_ids = 9201L)),
    "visit_occurrence_id is unavailable"
  )
  expect_error(
    .compileSelect(handle, "measurement",
      visit_filter = list(concept_ids = integer(0))),
    "one or more finite integer"
  )
})

test_that("non-person-keyed clinical tables cannot bypass the person gate", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, paste(
    "CREATE TABLE cost (cost_id INTEGER, cost_event_id INTEGER,",
    "cost_domain_id TEXT, total_charge REAL, currency_concept_id INTEGER)"
  ))
  handle$blueprint <- NULL

  expect_error(
    .compileSelect(handle, "cost", columns = c("cost_id", "total_charge")),
    "non-person-keyed|person join path"
  )
})

test_that("custom filter grammar rejects mixed and unknown node fields", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  valid <- .filterableColumns(.buildBlueprint(handle), "measurement")
  leaf <- list(var = "value_as_number", op = "value_bin",
               value = list(lower = 7, upper = 10))

  expect_error(
    .assertCustomFilterSafe(list(and = list(leaf), or = list(leaf)), valid),
    "cannot mix"
  )
  expect_error(
    .assertCustomFilterSafe(list(and = list(leaf), var = "value_as_number"),
                            valid),
    "cannot mix"
  )
  expect_error(
    .assertCustomFilterSafe(c(leaf, list(typo = TRUE)), valid),
    "Unknown custom filter leaf field"
  )
})

test_that("numeric bins are authenticated to the resource session and scope", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  valid <- .filterableColumns(.buildBlueprint(handle), "measurement")
  base_leaf <- list(
    var = "value_as_number", op = "value_bin",
    value = list(lower = 7, upper = 10)
  )

  expect_error(
    .assertCustomFilterSafe(base_leaf, valid, handle = handle,
                            table = "measurement"),
    "not issued"
  )

  scope <- .test_issue_safe_bins(
    handle, c(0, 7, 10, 20), concept_id = 3004410L)
  issued_leaf <- c(base_leaf, list(safe_scope = scope))
  expect_true(.assertCustomFilterSafe(
    issued_leaf, valid, handle = handle, table = "measurement"))

  forged_edge <- issued_leaf
  forged_edge$value$lower <- 7.1
  expect_error(
    .assertCustomFilterSafe(forged_edge, valid, handle = handle,
                            table = "measurement"),
    "not issued"
  )

  wrong_scope <- issued_leaf
  wrong_scope$safe_scope$column <- "quantity"
  expect_error(
    .assertCustomFilterSafe(wrong_scope, valid, handle = handle,
                            table = "measurement"),
    "not issued"
  )

  handle$safe_numeric_bins[[1]]$expires_at <- as.numeric(Sys.time()) - 1
  expect_error(
    .assertCustomFilterSafe(issued_leaf, valid, handle = handle,
                            table = "measurement"),
    "not issued"
  )
})

test_that("a filter on an unknown column is rejected", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  bp <- .buildBlueprint(handle)
  valid <- .filterableColumns(bp, "measurement")
  expect_error(
    .assertCustomFilterSafe(list(var = "no_such_column", op = ">=", value = 1),
                            valid),
    "not permitted|unknown"
  )
})

test_that("custom filter complexity caps fail closed before SQL compilation", {
  leaf <- list(var = "unit_concept_id", op = "in", value = 1L)
  valid <- "unit_concept_id"

  withr::local_options(list(dsomop.max_filter_depth = 2L))
  deep <- list(and = list(list(and = list(leaf))))
  expect_error(.assertCustomFilterSafe(deep, valid), "max_filter_depth")
  expect_error(.compileFilter(NULL, deep, valid_columns = valid),
               "max_filter_depth")

  withr::local_options(list(
    dsomop.max_filter_depth = 32L,
    dsomop.max_filter_nodes = 2L
  ))
  wide <- list(and = list(leaf, leaf))
  expect_error(.assertCustomFilterSafe(wide, valid), "max_filter_nodes")

  withr::local_options(list(
    dsomop.max_filter_nodes = 1024L,
    dsomop.max_filter_values = 2L
  ))
  many_values <- list(var = "unit_concept_id", op = "in", value = 1:3)
  expect_error(.assertCustomFilterSafe(many_values, valid),
               "max_filter_values")
})
