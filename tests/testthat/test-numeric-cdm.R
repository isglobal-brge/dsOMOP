.numeric_definitions()

test_that("pinned numeric CDM preserves source values and units on PostgreSQL and DuckDB", {
  hs <- .numeric_backends()
  for (h in hs) {
    m <- .executeQuery(h, paste0("SELECT COUNT(*) AS n, COUNT(value_as_number) AS v, ",
      "SUM(CASE WHEN unit_concept_id > 0 THEN 1 ELSE 0 END) AS u FROM ", .qualifyTable(h, "measurement")))
    expect_equal(as.numeric(m[1, ]), c(10040, 9107, 8403))
    d <- numeric_measurements(h$conn, h$cdm_schema)
    expect_equal(nrow(d), 108L)
    expect_equal(length(unique(d$person_id)), 28L)
    expect_true(is.double(d$value_as_number))
    expect_equal(unique(d$unit_concept_id), 8713L)
  }
  expect_equal(numeric_measurements(hs$postgresql$conn, "numeric_cdm"),
               numeric_measurements(hs$duckdb$conn, "main"), ignore_attr = TRUE)
})

test_that("all numeric reducers match an independent source oracle in memory and Parquet", {
  hs <- .numeric_backends(); .ws7_keys(hs)
  for (backend in names(hs)) {
    h <- hs[[backend]]
    oracle <- numeric_oracle(numeric_measurements(h$conn, h$cdm_schema))
    for (mode in c("memory", "staged")) {
      r <- .numeric_run(h, numeric_plan(), mode)
      f <- r$values$wide$wide
      want <- oracle[match(as.numeric(r$raw$wide$wide$person_id), oracle$pid), ]
      expect_equal(nrow(f), 28L)
      for (col in setdiff(names(oracle), "pid")) {
        expect_true(is.numeric(f[[col]]), info = paste(backend, mode, col))
        expect_equal(f[[col]], want[[col]], tolerance = 1e-9, info = paste(backend, mode, col))
      }
    }
  }
})

test_that("numeric values survive long panel sparse and survival covariate publication", {
  hs <- .numeric_backends(); .ws7_keys(hs)
  for (backend in names(hs)) {
    h <- hs[[backend]]; d <- numeric_measurements(h$conn, h$cdm_schema)
    oracle <- numeric_oracle(d)
    previous <- NULL
    for (mode in c("memory", "staged")) {
      r <- .numeric_run(h, numeric_formats(), mode)
      for (root in names(r$values)) for (component in names(r$values[[root]])) {
        f <- r$values[[root]][[component]]
        expect_true(nrow(f) > 0L, info = paste(backend, mode, root, component))
        if (!is.null(previous)) for (col in names(f)) {
          expect_equal(f[[col]], previous[[root]][[component]][[col]], info = paste(backend, root, col))
        }
      }
      long <- r$values$long$long
      expect_type(long$value, "double")
      expect_equal(sort(long$value), sort(d$value_as_number))
      expect_true(is.factor(long$concept_id)) # A-27 category, numeric value stays numeric.
      sv <- r$values$survival_covariates$survival_covariates
      want <- oracle[match(as.numeric(r$raw$survival_covariates$survival_covariates$person_id), oracle$pid), ]
      expect_type(sv$mean, "double"); expect_equal(sv$mean, want$mean)
      sp <- r$values$sparse$sparse; ref <- r$values$sparse$covariateRef
      expect_type(sp$covariate_value, "double")
      for (analysis in c("binary", "count")) {
        ids <- ref$covariate_id[ref$analysis == analysis]
        expect_equal(sort(sp$covariate_value[sp$covariate_id %in% ids]), sort(if (analysis == "binary") rep(1, nrow(oracle)) else as.numeric(table(d$person_id))))
      }
      temporal <- r$values$temporal$temporal
      tref <- r$values$temporal$covariateRef
      expect_type(temporal$covariate_value, "double")
      for (analysis in c("mean", "last")) {
        wanted <- vapply(split(d, d$person_id), function(rows) {
          v <- rows$value_as_number[rows$measurement_date <= min(rows$measurement_date) + 3650L]
          if (analysis == "mean") mean(v) else tail(v, 1L)
        }, 0.0)
        ids <- tref$covariate_id[tref$analysis == analysis]
        expect_equal(sort(temporal$covariate_value[temporal$covariate_id %in% ids]), sort(unname(wanted)))
      }
      panel <- r$values$panel$panel
      expect_type(panel$lab_mean, "double"); expect_type(panel$lab_last, "double")
      raw <- r$raw$panel$panel
      for (i in seq_len(nrow(panel))) {
        rows <- d[d$person_id == as.numeric(raw$person_id[[i]]), ]
        offset <- as.integer(rows$measurement_date - min(rows$measurement_date))
        v <- rows$value_as_number[offset >= raw$period_start_days[[i]] & offset <= raw$period_end_days[[i]]]
        expect_equal(panel$lab_mean[[i]], if (length(v)) mean(v) else NA_real_, info = paste(backend, mode, i))
        expect_equal(panel$lab_last[[i]], if (length(v)) tail(v, 1L) else NA_real_)
      }
      previous <- r$values
    }
  }
})

test_that("declared conversions precede private predicates and reference-range reducers", {
  hs <- .numeric_backends(); .ws7_keys(hs)
  for (h in hs) {
    d <- numeric_measurements(h$conn, h$cdm_schema)
    p <- numeric_plan()
    # g/dL -> mg/dL. No source values or units are rewritten.
    p$events$lab$unit <- list(target = 8840L, convert = list(`8713` = list(factor = 1000, offset = 0)))
    p$events$lab$where <- list(var = "value", op = ">=", value = 12000)
    for (mode in c("memory", "staged")) {
      r <- .numeric_run(h, p, mode)
      want <- numeric_oracle(transform(d[d$value_as_number >= 12, ], value_as_number = value_as_number * 1000))
      f <- r$values$wide$wide
      expect_gt(nrow(f), 5L)
      expect_equal(f$mean, want$mean[match(as.numeric(r$raw$wide$wide$person_id), want$pid)])
    }
    # Public synthetic sidecar view: ranges deliberately exercise both tails.
    # Original imported measurements retain their missing reference ranges.
    schema <- .ws4_namespace("numeric_ranges")
    DBI::dbExecute(h$conn, paste0("CREATE SCHEMA ", schema))
    original <- h$cdm_schema
    tryCatch({
      tables <- DBI::dbGetQuery(h$conn, paste0("SELECT table_name FROM information_schema.tables WHERE table_schema = '", original, "'"))$table_name
      for (table in tables) {
        source <- .qualifyTable(h, table)
        columns <- DBI::dbListFields(h$conn, DBI::Id(schema = original, table = table))
        select <- if (table == "measurement") paste(vapply(columns, function(c) {
          if (c == "range_low") "12.0 AS range_low" else if(c == "range_high") "15.0 AS range_high" else c
        }, ""), collapse = ", ") else "*"
        DBI::dbExecute(h$conn, paste0("CREATE VIEW ", schema, ".", table, " AS SELECT ", select, " FROM ", source))
      }
      h$cdm_schema <- schema; h$vocab_schema <- schema; h$results_schema <- schema
      p$events$lab$where <- NULL
      r <- .numeric_run(h, p)
      want <- numeric_oracle(transform(d, range_low = 12, range_high = 15))
      expect_gt(sum(want$high), 0); expect_gt(sum(want$low), 0)
      at <- match(as.numeric(r$raw$wide$wide$person_id), want$pid)
      expect_equal(r$values$wide$wide$high, want$high[at])
      expect_equal(r$values$wide$wide$low, want$low[at])
    }, finally = {
      h$cdm_schema <- original; h$vocab_schema <- original; h$results_schema <- original
      DBI::dbExecute(h$conn, paste0("DROP SCHEMA ", schema, " CASCADE"))
    })
  }
})

test_that("issued value bins retain half-open boundaries on real numeric labs", {
  hs <- .numeric_backends(); .ws7_keys(hs)
  for (h in hs) {
    d <- numeric_measurements(h$conn, h$cdm_schema)
    .buildBlueprint(h)
    withr::local_options(list(nfilter.subset = 5L))
    scope <- .test_issue_safe_bins(h, c(0, 12, 15, 30), concept_id = numeric_lab)
    f <- .extractTable(h, "measurement", columns = c("person_id", "value_as_number"),
      concept_filter = numeric_lab, translate_concepts = FALSE,
      filters = .test_value_bin_leaf(scope, 12, 15))
    expect_gt(nrow(f), 0L)
    expect_equal(sort(f$value_as_number), sort(d$value_as_number[d$value_as_number >= 12 & d$value_as_number < 15]))
    p <- numeric_plan()
    p$events$lab$where <- list(var = "value", op = "between", value = list(lower = 12, upper = 15, upper_inclusive = FALSE))
    for (mode in c("memory", "staged")) {
      r <- .numeric_run(h, p, mode)
      want <- numeric_oracle(d[d$value_as_number >= 12 & d$value_as_number < 15, ])
      expect_equal(sort(r$values$wide$wide$mean), sort(want$mean))
    }
  }
})

test_that("sparse numeric analyses have an explicit supported temporal route", {
  .numeric_enabled()
  p <- numeric_formats()
  p$outputs$sparse$options$analyses <- list("mean")
  expect_error(.irValidateWire(.irJsonForm(p)), "binary.*count")
  expect_silent(.irValidateWire(.irJsonForm(numeric_formats())))
})
