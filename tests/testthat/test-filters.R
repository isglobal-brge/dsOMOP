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

test_that("a custom row filter is emitted into the WHERE clause", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  sql_plain <- .compileSelect(handle, "measurement",
                              columns = c("person_id", "value_as_number"))
  sql_filtered <- .compileSelect(handle, "measurement",
                                 columns = c("person_id", "value_as_number"),
                                 filters = list(var = "value_as_number",
                                                op = ">=", value = 7.5))

  expect_false(grepl("value_as_number >=", sql_plain))
  expect_match(sql_filtered, "value_as_number >= 7.5")
})

test_that("a custom row filter narrows the extracted rows vs no filter", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(
    list(nfilter.subset = 3, nfilter.tab = 3,
         dsomop.default_date_handling = "remove"), {
    unfiltered <- .extractTable(handle, "measurement",
                                columns = c("person_id", "value_as_number"),
                                concept_filter = 3004410L,
                                translate_concepts = FALSE)
    # value_as_number >= 7.5 keeps HbA1c values 8.1, 9.0, 7.5 (persons 5, 7, 9).
    filtered <- .extractTable(handle, "measurement",
                              columns = c("person_id", "value_as_number"),
                              concept_filter = 3004410L,
                              translate_concepts = FALSE,
                              filters = list(var = "value_as_number",
                                             op = ">=", value = 7.5))

    expect_equal(nrow(unfiltered), 5L)
    expect_equal(nrow(filtered), 3L)
    expect_true(all(filtered$value_as_number >= 7.5))
    expect_lt(nrow(filtered), nrow(unfiltered))
  })
})

test_that("a value_bin row filter narrows the extracted rows", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(
    list(nfilter.subset = 3, nfilter.tab = 3,
         dsomop.default_date_handling = "remove"), {
    # value_bin [7.5, 10) keeps 7.5, 8.1, 9.0 (persons 5, 7, 9).
    binned <- .extractTable(handle, "measurement",
                            columns = c("person_id", "value_as_number"),
                            concept_filter = 3004410L,
                            translate_concepts = FALSE,
                            filters = list(var = "value_as_number",
                                           op = "value_bin",
                                           value = list(lower = 7.5,
                                                        upper = 10)))
    expect_equal(nrow(binned), 3L)
    expect_true(all(binned$value_as_number >= 7.5 &
                      binned$value_as_number < 10))
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
    # (>= 7.0 AND <= 9.0) on HbA1c keeps 7.2, 8.1, 9.0, 7.5 (persons 1,5,7,9).
    res <- .extractTable(handle, "measurement",
                         columns = c("person_id", "value_as_number"),
                         concept_filter = 3004410L,
                         translate_concepts = FALSE,
                         filters = list(and = list(
                           list(var = "value_as_number", op = ">=", value = 7.0),
                           list(var = "value_as_number", op = "<=", value = 9.0)
                         )))
    expect_equal(nrow(res), 4L)
    expect_true(all(res$value_as_number >= 7.0 & res$value_as_number <= 9.0))
  })
})

test_that("nested OR filter emits an OR-joined predicate group", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  sql <- .compileSelect(handle, "measurement",
                        columns = c("person_id", "value_as_number"),
                        filters = list(or = list(
                          list(var = "value_as_number", op = ">=", value = 9.0),
                          list(var = "value_as_number", op = "<", value = 6.6)
                        )))
  # Both leaves present, joined by OR inside a parenthesised group.
  expect_match(sql, "\\(t\\.value_as_number >= 9 OR t\\.value_as_number < 6.6\\)")
})

test_that("nested AND-of-OR filter preserves the boolean structure", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(
    list(nfilter.subset = 3, nfilter.tab = 3,
         dsomop.default_date_handling = "remove"), {
    # AND( OR(>=8.1, <=6.5), >=6.0 ) on HbA1c.
    #   OR leaf keeps 8.1, 9.0 (>=8.1) and 6.5 (<=6.5) -> persons 3,5,7
    #   AND >=6.0 keeps all of them -> 3 persons, passes the gate.
    res <- .extractTable(handle, "measurement",
                         columns = c("person_id", "value_as_number"),
                         concept_filter = 3004410L,
                         translate_concepts = FALSE,
                         filters = list(and = list(
                           list(or = list(
                             list(var = "value_as_number", op = ">=", value = 8.1),
                             list(var = "value_as_number", op = "<=", value = 6.5)
                           )),
                           list(var = "value_as_number", op = ">=", value = 6.0)
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

test_that("a filter narrowing to < nfilter persons is fail-closed (gated)", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(
    list(nfilter.subset = 3, dsomop.default_date_handling = "remove"), {
    # HbA1c < 6.6 keeps only 6.5 (person 3) -> 1 person < nfilter.subset.
    expect_error(
      .extractTable(handle, "measurement",
                    columns = c("person_id", "value_as_number"),
                    concept_filter = 3004410L,
                    translate_concepts = FALSE,
                    filters = list(var = "value_as_number", op = "<",
                                   value = 6.6)),
      "Disclosive|blocked|insufficient"
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
  # Range/membership ops on the same column are allowed.
  expect_invisible(
    .assertCustomFilterSafe(list(var = "value_as_number", op = ">=",
                                 value = 7.5), valid))
  expect_invisible(
    .assertCustomFilterSafe(list(var = "unit_concept_id", op = "in",
                                 value = list(9529L)), valid))
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
