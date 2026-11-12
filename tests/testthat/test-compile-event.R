# WS-4: the event compiler (design-2.10.3 sections 2.2, 6.2 and 6.6) on the
# extended reach fixture, executed on every available backend, plus rendered
# snapshots for every dialect family.

.ws4_ev_backends <- .ws4_backends()
withr::defer(.ws4_cleanup(.ws4_ev_backends), testthat::teardown_env())

# Extra rows (identical on every backend): reference ranges in two units for
# person 1009, a fill with a non-positive supply for person 1005.
.ws4_ev_seed <- function(h) {
  q <- function(t) .qualifyTable(h, t, h$cdm_schema)
  d <- function(x) .sqlDateLiteral(h, x)
  DBI::dbExecute(h$conn, paste0(
    "INSERT INTO ", q("measurement"), " (measurement_id, person_id, measurement_concept_id, ",
    "measurement_date, measurement_type_concept_id, operator_concept_id, value_as_number, ",
    "value_as_concept_id, unit_concept_id, range_low, range_high) VALUES ",
    "(599001, 1009, 3004410, ", d("2019-12-05"), ", 44818702, 0, 60, 0, 8753, 20, 42), ",
    "(599002, 1009, 3004410, ", d("2019-12-06"), ", 44818702, 0, 5.5, 0, 8554, 4, 6)"))
  DBI::dbExecute(h$conn, paste0(
    "INSERT INTO ", q("drug_exposure"), " (drug_exposure_id, person_id, drug_concept_id, ",
    "drug_exposure_start_date, drug_exposure_end_date, drug_type_concept_id, days_supply) VALUES ",
    "(599101, 1005, 1503297, ", d("2020-04-01"), ", NULL, 38000177, 0)"))
  invisible(TRUE)
}
for (.b in .ws4_ev_backends) .ws4_ev_seed(.b$handle)

.ws4_hba1c <- function(...) {
  utils::modifyList(list(
    table = "measurement", concept_set = list(concepts = list(3004410)),
    value = list(column = "value_as_number"),
    unit = list(target = 8554, convert = list(`8753` = list(factor = 0.0915, offset = 2.15)))),
    list(...))
}

test_that("units are normalised before every predicate; unknown units and censored operators follow their policies", {
  for (b in names(.ws4_ev_backends)) {
    h <- .ws4_ev_backends[[b]]$handle
    plan <- .ws4_plan(events = list(
      conv_le7 = .ws4_hba1c(where = list(var = "value", op = "<=", value = 7)),
      raw_le7 = list(table = "measurement", concept_set = list(concepts = list(3004410)),
                     where = list(var = "value", op = "<=", value = 7)),
      bound_col = .ws4_hba1c(where = list(var = "value_as_number", op = "<=", value = 7)),
      source_col = .ws4_hba1c(where = list(var = "value_source", op = ">=", value = 50)),
      all_rows = .ws4_hba1c(),
      known_unit = .ws4_hba1c(unit = list(target = 8554, convert = list(`8753` = list(factor = 0.0915, offset = 2.15)),
                                          unknown = "exclude")),
      uncensored = .ws4_hba1c(unit = list(target = 8554, convert = list(`8753` = list(factor = 0.0915, offset = 2.15)),
                                          censored = "exclude"))))
    ctx <- .ws4_ctx(h, plan)
    p1006 <- "x.person_id = 1006"
    # conversion BEFORE the threshold: 52 mmol/mol -> 6.908 % passes; the NULL-unit
    # row has an unknown (NA) normalised value and never satisfies the predicate
    conv <- .ws4_rows(ctx, "conv_le7", p1006)
    expect_equal(conv$value, 6.908, tolerance = 1e-9, info = b)
    expect_identical(conv$start_date, "2019-12-20", info = b)
    raw <- .ws4_rows(ctx, "raw_le7", p1006)
    expect_equal(raw$value, 6, info = b)
    # the configured value column binds to the NORMALISED value
    expect_identical(.ws4_rows(ctx, "bound_col", p1006)$event_id, conv$event_id, info = b)
    # source values stay reachable only as the explicit provenance column
    src <- .ws4_rows(ctx, "source_col", p1006)
    expect_identical(src$value_source, 52, info = b)
    expect_equal(src$value, 6.908, tolerance = 1e-9)
    # unknown = missing keeps the row with NA value and unit_unknown = 1
    all <- .ws4_rows(ctx, "all_rows", p1006)
    expect_identical(nrow(all), 7L, info = b)
    unknown <- all[is.na(all$unit_source), ]
    expect_identical(nrow(unknown), 1L)
    expect_true(is.na(unknown$value) && is.na(unknown$unit_concept_id))
    expect_identical(as.integer(unknown$unit_unknown), 1L)
    expect_true(all(all$unit_unknown[!is.na(all$unit_source)] == 0L))
    expect_true(all(all$unit_concept_id[!is.na(all$unit_source)] == 8554L))
    # unknown = exclude drops it
    expect_identical(nrow(.ws4_rows(ctx, "known_unit", p1006)), 6L, info = b)
    # censored = exclude drops only the reviewed inequality operators (<, >);
    # operator 0 / NULL / '=' count as equality
    unc <- .ws4_rows(ctx, "uncensored", "x.person_id IN (1006, 1009)")
    expect_false(any(unc$operator_concept_id %in% c(4171756L, 4172704L)), info = b)
    expect_identical(sum(unc$person_id == 1006L), 5L, info = b)
    expect_true(4172703L %in% unc$operator_concept_id, info = b)
    # reference ranges use the same affine map as the value
    r <- .ws4_rows(ctx, "all_rows", "x.person_id = 1009")
    mm <- r[r$event_id == 599001, ]
    expect_equal(c(mm$value, mm$range_low, mm$range_high),
                 c(60, 20, 42) * 0.0915 + 2.15, tolerance = 1e-9, info = b)
    pc <- r[r$event_id == 599002, ]
    expect_equal(c(pc$value, pc$range_low, pc$range_high), c(5.5, 4, 6), info = b)
  }
})

test_that("the event's own end policy keeps the original recorded end and supply roles", {
  for (b in names(.ws4_ev_backends)) {
    h <- .ws4_ev_backends[[b]]$handle
    metf <- list(concepts = list(1503297))
    plan <- .ws4_plan(events = list(
      fallback = list(table = "drug_exposure", concept_set = metf, end = list(fallback = "days_supply")),
      role_end = list(table = "drug_exposure", concept_set = metf),
      point = list(table = "drug_exposure", concept_set = metf, end = "start")))
    ctx <- .ws4_ctx(h, plan)
    who <- "x.person_id IN (1004, 1005)"
    fb <- .ws4_rows(ctx, "fallback", who)
    expect_identical(fb$start_date, c("2020-01-01", "2020-01-15", "2020-02-01", "2020-03-10", "2020-04-01"), info = b)
    # NULL end + supply 30 -> start + 29; NULL end + NULL supply -> start; a
    # recorded end wins (even before the start: the data-quality policy belongs
    # to intervals); a non-positive supply is never an end source
    expect_identical(fb$end_date, c("2020-01-30", "2020-02-13", "2020-02-01", "2020-03-01", "2020-04-01"), info = b)
    expect_identical(fb$end_recorded, c(NA, NA, NA, "2020-03-01", NA), info = b)
    expect_identical(as.integer(fb$supply), c(30L, 30L, NA, 30L, 0L), info = b)
    re <- .ws4_rows(ctx, "role_end", who)
    expect_identical(re$end_date, c("2020-01-01", "2020-01-15", "2020-02-01", "2020-03-01", "2020-04-01"), info = b)
    expect_identical(re$end_recorded, fb$end_recorded)
    pt <- .ws4_rows(ctx, "point", who)
    expect_identical(pt$end_date, pt$start_date, info = b)
    expect_identical(pt$end_recorded, fb$end_recorded)
  }
})

test_that("visit-type restrictions use the visit link; index-visit events are joined by the consumer", {
  for (b in names(.ws4_ev_backends)) {
    h <- .ws4_ev_backends[[b]]$handle
    plan <- .ws4_plan(events = list(
      inpatient_cond = list(table = "condition_occurrence", visit = list(type = list(9201))),
      outpatient_proc = list(table = "procedure_occurrence", visit = list(type = list(9202))),
      index_visit = list(table = "procedure_occurrence", concept_set = list(concepts = list(4283095)),
                         visit = list(index = TRUE))))
    ctx <- .ws4_ctx(h, plan)
    cond <- .ws4_rows(ctx, "inpatient_cond", "x.person_id IN (1007, 1018)")
    expect_identical(as.integer(table(cond$person_id)[c("1007", "1018")]), c(4L, 2L), info = b)
    expect_true(all(cond$visit_id %in% c(700701, 701801, 701802)), info = b)
    proc <- .ws4_rows(ctx, "outpatient_proc", "x.person_id = 1007")
    expect_identical(proc$start_date, "2020-02-01", info = b)
    iv <- .ws4_rows(ctx, "index_visit", "x.person_id = 1007")
    expect_identical(nrow(iv), 1L, info = b)
    expect_equal(as.numeric(iv$visit_id), 700701)
    expect_true(.eventMeta(ctx, "index_visit")$index_visit)
    expect_false(.eventMeta(ctx, "inpatient_cond")$index_visit)
  }
  h <- .ws4_ev_backends$sqlite$handle
  ctx <- .ws4_ctx(h, .ws4_plan(events = list(d = list(table = "death", visit = list(type = list(9201))))))
  .ws4_expect_rejection(.compileEventByName(ctx, "d"), pattern = "has no visit link")
})

test_that("on_missing_table: unavailable is a typed state, error refuses with unavailable_table", {
  for (b in names(.ws4_ev_backends)) {
    h <- .ws4_ev_backends[[b]]$handle
    plan <- .ws4_plan(events = list(
      notes = list(table = "note", on_missing_table = "unavailable",
                   where = list(var = "note_class_concept_id", op = "==", value = 1)),
      notes_err = list(table = "note"),
      costs = list(table = "cost")))
    ctx <- .ws4_ctx(h, plan)
    f <- .compileEventByName(ctx, "notes")
    expect_identical(f$availability, "unavailable", info = b)
    expect_identical(.eventMeta(ctx, "notes")$availability, "unavailable")
    expect_identical(nrow(.ws4_rows(ctx, "notes")), 0L, info = b)
    expect_true(all(c(.EVENT_SRC_COLUMNS, .EVENT_NORM_COLUMNS) %in% names(f$columns)))
    .ws4_expect_rejection(.compileEventByName(ctx, "notes_err"), code = "unavailable_table",
                          pattern = "not available at this site")
    .ws4_expect_rejection(.compileEventByName(ctx, "costs"), pattern = "is not an event table")
  }
})

test_that("concept columns, value columns and predicates are checked against the role record (B4)", {
  h <- .ws4_ev_backends$sqlite$handle
  ok <- .ws4_ctx(h, .ws4_plan(events = list(
    op_type = list(table = "observation_period", concept_set = list(concepts = list(44814724)),
                   concept_col = "period_type_concept_id"),
    vac = list(table = "measurement", concept_set = list(concepts = list(3004410)),
               where = list(var = "value_as_concept_id", op = "==", value = 4328749)),
    drug_qty = list(table = "drug_exposure", where = list(var = "days_supply", op = "==", value = 0)),
    spec_qty = list(table = "specimen", value = list(column = "quantity"),
                    where = list(var = "value", op = ">=", value = 5)))))
  rows <- .ws4_rows(ok, "op_type")
  expect_gt(nrow(rows), 100L)
  expect_true(all(rows$concept_id == 44814724L))
  vac <- .ws4_rows(ok, "vac")
  expect_identical(vac$person_id, 1006L)
  expect_true(is.na(vac$value))
  expect_equal(as.numeric(.ws4_rows(ok, "drug_qty", "x.person_id = 1005")$event_id), 599101)
  sq <- .ws4_rows(ok, "spec_qty", "x.person_id = 1015")
  expect_identical(nrow(sq), 2L)
  expect_true(all(sq$value == 5) && all(sq$value_source == 5))
  refuse <- function(event, pattern) {
    ctx <- .ws4_ctx(h, .ws4_plan(events = list(e = event)))
    .ws4_expect_rejection(.compileEventByName(ctx, "e"), pattern = pattern)
  }
  m <- list(concepts = list(3004410))
  refuse(list(table = "observation_period", concept_set = list(concepts = list(1))),
         "no domain concept column")
  refuse(list(table = "measurement", concept_col = "measurement_date"), "not a filterable concept column")
  refuse(list(table = "measurement", concept_col = "measurement_source_concept_id"),
         "not a filterable concept column")
  refuse(list(table = "measurement", value = list(column = "measurement_date")),
         "not a filterable numeric column")
  refuse(list(table = "measurement", value = list(column = "value_as_concept_id")),
         "not a filterable numeric column")
  refuse(list(table = "drug_exposure", unit = list(target = 8554)), "no value role")
  refuse(list(table = "specimen", value = list(column = "quantity"),
              unit = list(target = 8554, convert = list(`8554` = list(factor = 2)))),
         "other than the target")
  refuse(list(table = "condition_occurrence", where = list(var = "value", op = ">", value = 1)),
         "has no value role")
  refuse(list(table = "condition_occurrence", where = list(var = "supply", op = ">", value = 1)),
         "no days_supply role")
  refuse(list(table = "measurement", concept_set = m,
              where = list(var = "value_as_number", op = ">", value = "bad")),
         "compares with finite numbers only")
  refuse(list(table = "measurement", where = list(var = "measurement_date", op = ">=", value = "2020-13-01")),
         "valid ISO dates")
  refuse(list(table = "measurement", where = list(var = "start_date", op = ">=", value = 5)),
         "ISO dates only")
  refuse(list(table = "measurement", where = list(var = "unit_concept_id", op = "==", value = 8554.5)),
         "integer concept ids")
  refuse(list(table = "measurement", where = list(var = "measurement_source_value", op = "==", value = "x")),
         "class-1 column")
  refuse(list(table = "measurement", where = list(var = "person_id", op = "==", value = 1)),
         "never filterable")
  refuse(list(table = "measurement", where = list(var = "made_up", op = "==", value = 1)),
         "not a predicate column")
  refuse(list(table = "measurement", where = list(var = "unit_unknown", op = "==", value = 1)),
         "only with a unit conversion")
})

test_that("predicate caps hold inside event predicates", {
  h <- .ws4_ev_backends$sqlite$handle
  ctx <- .ws4_ctx(h, .ws4_plan(events = list(e = list(
    table = "measurement",
    where = list(var = "measurement_concept_id", op = "in", value = list(1, 2, 3))))))
  withr::local_options(dsomop.max_filter_values = 2L)
  .ws4_expect_rejection(.compileEventByName(ctx, "e"), pattern = "max_filter_values")
  ctx2 <- .ws4_ctx(h, .ws4_plan(events = list(e = list(
    table = "measurement",
    where = list(not = list(not = list(not = list(var = "value", op = "is_null"))))))))
  withr::local_options(dsomop.max_filter_depth = 3L)
  .ws4_expect_rejection(.compileEventByName(ctx2, "e"), pattern = "max_filter_depth")
})

test_that("event fragments satisfy the frozen fragment contract and chain src -> norm -> final", {
  h <- .ws4_ev_backends$sqlite$handle
  ctx <- .ws4_ctx(h, .ws4_plan(events = list(hba1c = .ws4_hba1c(where = list(var = "value", op = "not_null")))))
  f <- .compileEventByName(ctx, "hba1c")
  expect_identical(f$name, "ev_hba1c")
  expect_identical(f$kind, "event")
  expect_identical(f$grain, "row")
  expect_identical(f$dependencies, "ev_hba1c_norm")
  expect_identical(.fragmentGet(ctx, "ev_hba1c_norm")$dependencies, "ev_hba1c_src")
  expect_identical(f$person_lineage, list(person_col = "person_id", via_roster = NULL))
  expect_identical(f$source_identity_columns, c("source_ns", "event_id"))
  expect_true(all(c("event_id", "source_ns", "visit_id") %in% f$private_columns))
  expect_identical(f$ordering, c("start_date", "source_ns", "event_id"))
  roles <- vapply(f$columns, function(c) c$role, "")
  expect_identical(roles[c("person_id", "start_date", "end_date", "end_recorded", "supply", "concept_id",
                           "value", "unit_concept_id", "operator_concept_id", "range_low")],
                   c(person_id = "person_key", start_date = "start", end_date = "end",
                     end_recorded = "end_recorded", supply = "supply", concept_id = "concept",
                     value = "value", unit_concept_id = "unit", operator_concept_id = "operator",
                     range_low = "range"))
  # the table's other filterable columns are projected; class-1 columns never
  expect_true(all(c("measurement_type_concept_id", "value_as_concept_id") %in% names(f$columns)))
  expect_false(any(c("measurement_source_value", "measurement_source_concept_id",
                     "unit_source_value", "measurement_id") %in% names(f$columns)))
  sql <- .buildStatement(ctx, f, dialect = ctx)
  expect_match(sql, "^WITH ev_hba1c_src AS \\(SELECT .*\\), ev_hba1c_norm AS \\(SELECT .*\\) SELECT n\\.person_id")
  expect_identical(.compileEventByName(ctx, "hba1c"), f)
  meta <- .eventMeta(ctx, "hba1c")
  expect_identical(meta$fragments, c(src = "ev_hba1c_src", norm = "ev_hba1c_norm", final = "ev_hba1c"))
  expect_true(all(c("value", "unit", "operator", "range", "concept") %in% meta$roles))
  # a long event name keeps every stage name within the grammar and distinct
  long <- paste(rep("a", 64), collapse = "")
  ctx2 <- .ws4_ctx(h, .ws4_plan(events = stats::setNames(list(.ws4_hba1c()), long)))
  fl <- .compileEventByName(ctx2, long)
  names_all <- unname(.eventMeta(ctx2, long)$fragments)
  expect_true(all(nchar(names_all) <= 64L & grepl(.NAME_PATTERN, names_all)))
  expect_identical(anyDuplicated(names_all), 0L)
  expect_identical(fl$name, names_all[[3L]])
})

test_that("a v1 output-level concept scope intersects the event's own set (A-10 legacy_concept_scope)", {
  h <- .ws4_ev_backends$sqlite$handle
  plan <- .ws4_plan(events = list(dm = list(table = "condition_occurrence",
                                            concept_set = list(concepts = list(201820), include_descendants = TRUE))),
                    concept_sets = list(scope = list(concepts = list(201821))))
  ctx <- .ws4_ctx(h, plan)
  node <- .irNamedNode(ctx$ir, "event", "dm")
  node$legacy_concept_scope <- list(concept_set_ref = "scope")
  DBI::dbExecute(h$conn, paste0("INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
                                "condition_concept_id, condition_start_date) VALUES (599201, 1009, 201821, '2019-05-05')"))
  f <- .compileEvent(ctx, node, .compileConceptSet(ctx, node$concept_set))
  rows <- .ws4_rows(ctx, "dm")
  expect_identical(unique(rows$concept_id), 201821L)
  expect_identical(.eventMeta(ctx, "dm")$concept, list(ids = 201821L, state = "set"))
})

test_that("compiled event SQL renders for every dialect family", {
  bp <- .buildBlueprint(.ws4_ev_backends$sqlite$handle)
  plan <- .ws4_plan(events = list(
    hba1c = .ws4_hba1c(unit = list(target = 8554, convert = list(`8753` = list(factor = 0.0915, offset = 2.15)),
                                   unknown = "exclude", censored = "exclude"),
                       where = list(or = list(list(var = "value", op = ">=", value = 6.5),
                                              list(not = list(var = "value_as_concept_id", op = "in",
                                                              value = list(4328749, 0)))))),
    metf = list(table = "drug_exposure", concept_set = list(concepts = list(1503297), include_descendants = FALSE),
                end = list(fallback = "days_supply"),
                where = list(var = "drug_exposure_start_date", op = "between",
                             value = list(lower = "2019-01-01", upper = "2020-12-31", upper_inclusive = FALSE))),
    cond_visit = list(table = "condition_occurrence", visit = list(type = list(9201, 9203)),
                      where = list(and = list(list(var = "condition_type_concept_id", op = "not_in", value = list(0)),
                                              list(var = "end_date", op = "is_null")))),
    notes = list(table = "note", on_missing_table = "unavailable")))
  dbms <- c(postgresql = "postgresql", sqlite = "sqlite", duckdb = "duckdb", mysql = "mysql",
            sqlserver = "sql server", oracle = "oracle", redshift = "redshift", bigquery = "bigquery",
            snowflake = "snowflake", spark = "spark")
  expect_snapshot({
    for (family in names(dbms)) {
      fh <- new.env(parent = emptyenv())
      fh$dbms <- dbms[[family]]
      fh$target_dialect <- .resolve_target_dialect(dbms[[family]])
      fh$blueprint <- bp
      fh$cdm_schema <- if (identical(family, "sqlite")) NULL else "cdm"
      fh$vocab_schema <- fh$cdm_schema
      fh$results_schema <- NULL
      fh$temp_tables <- character(0)
      ctx <- .ws4_ctx(fh, plan)
      cat("--", family, "\n")
      for (e in c("hba1c", "metf", "cond_visit", "notes")) {
        cat(.buildStatement(ctx, .compileEventByName(ctx, e), dialect = ctx), "\n")
      }
    }
  })
})

test_that("same-row vs different-row: one event's conjunction needs one row satisfying both", {
  for (b in names(.ws4_ev_backends)) {
    h <- .ws4_ev_backends[[b]]$handle
    a1c <- list(concepts = list(3004410))
    plan <- .ws4_plan(events = list(
      both = list(table = "measurement", concept_set = a1c,
                  where = list(and = list(list(var = "value", op = ">=", value = 9),
                                          list(var = "operator_concept_id", op = "==", value = 4172704)))),
      high = list(table = "measurement", concept_set = a1c, where = list(var = "value", op = ">=", value = 9)),
      gt_op = list(table = "measurement", concept_set = a1c,
                   where = list(var = "operator_concept_id", op = "==", value = 4172704))))
    ctx <- .ws4_ctx(h, plan)
    has <- function(e) {
      f <- .compileEventByName(ctx, e)
      sql <- .buildStatement(ctx, f, select = paste0(
        "SELECT CASE WHEN EXISTS (SELECT 1 FROM ", f$name,
        " x WHERE x.person_id = 1006) THEN 1 ELSE 0 END AS has"), dialect = ctx)
      as.integer(.executeQuery(h, sql)$has)
    }
    # has(A_x) AND has(A_y) holds through two different rows ...
    expect_identical(c(has("high"), has("gt_op")), c(1L, 1L), info = b)
    # ... while has(A where x & y) needs one row satisfying both
    expect_identical(has("both"), 0L, info = b)
  }
})
