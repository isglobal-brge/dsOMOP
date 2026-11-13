# Publish through real compiler/publisher boundaries; stages use actual files.
.cat_plan <- function() {
  vars <- list(sex = list(derived = list(kind = "sex_mf")),
    band = list(derived = list(kind = "age", bands = TRUE, age_breaks = list(0L, 18L, 65L))),
    birth_band = list(derived = list(kind = "birth_year_band", width = 5L)),
    education = list(source = list(event = "lab"), reducer = list(fn = "last_value", value = "value_as_concept_id")),
    age_at = list(derived = list(kind = "age_at", reference = "index", bands = TRUE, age_breaks = list(0L, 18L, 65L))))
  for (part in c("month", "weekday", "season")) vars[[part]] <- list(derived = list(kind = "date_part", reference = "index", part = part))
  for (precision in c("year", "quarter", "month", "iso_week", "cdc_week", "epi_week", "fiscal_year", "fiscal_quarter")) {
    spec <- list(precision = precision)
    if (precision == "epi_week") spec <- c(spec, list(week_start = 1L, week_min_days = 4L))
    vars[[paste0("date_", precision)]] <- list(derived = list(kind = "calendar_date", reference = "index"), date_spec = spec)
  }
  for (name in setdiff(names(vars), c("sex", "band", "birth_band"))) vars[[name]]$broadcast <- TRUE
  p <- .ws7_scenario_plan(vars)
  p$options <- list(labels = list("0" = "unknown", "45000123" = "declared only"), date_spec = list(precision = "month"))
  p$outputs$long <- list(rows = "t", format = "long", columns = as.list(names(vars)), sources = list(list(event = "lab")))
  p$outputs$survival <- list(rows = "t", format = "survival", columns = as.list(names(vars)), options = list(
    outcomes = list(disease = list(event = "dx")), censor = list("observation_end"), tar = list(start = 1L, end = 30L, anchor = "index")))
  p$outputs$joined <- list(rows = "t", format = "joined", grain = "episode", options = list(
    from = list(list(output = "o"), list(output = "membership")), cardinality = "one_to_one"))
  p$outputs$panel <- list(rows = "t", format = "temporal_covariates", options = list(
    sources = list(list(event = "dx")), bin_width = 30L, window = list(start = -30L, end = 30L, anchor = "index"), analyses = list("count")))
  p$outputs$period <- list(rows = "t", format = "person_period", options = list(
    sources = list(list(event = "dx")), bins = list(width = 100L), partition = list(age_bands = 10L, calendar = "month"), window = list(start = -30L, end = 30L, anchor = "index"), analyses = list("count")))
  p$outputs$sparse <- list(rows = "t", format = "sparse", options = list(sources = list(list(event = "dx")), analyses = list("binary", "count")))
  p$outputs$membership <- list(rows = "t", format = "membership")
  p$outputs$dictionary <- list(rows = "t", format = "dictionary")
  for (name in c("panel", "period", "sparse", "membership", "dictionary")) p$outputs[[name]]$columns <- list()
  p
}

test_that("categorical kinds survive every applicable native output and file path", {
  backends <- .ws7_scenario_backends(); on.exit(.ws4_cleanup(backends), add = TRUE)
  h <- backends$sqlite$handle; .ws7_keys(list(h))
  withr::local_options(list(nfilter.tab = 1L, nfilter.levels.density = 1, nfilter.levels.max = 200L))
  plan <- .cat_plan()
  paths <- c("memory", "csv", if (.arrowAvailable()) "parquet")
  results <- list()
  for (path in paths) {
    run <- .ws7_ctx(h, plan, if (path == "memory") "memory" else "staged")
    tryCatch({
      pop <- .ws7_population(run, "t", .ws7_scenario_frame(c(7001,7003,7004,7005,7006,7007)))
      publish <- function() {
        if (path == "csv") local_mocked_bindings(.arrowAvailable = function() FALSE)
        result <- .ws7_publish(run, list(t = pop), threshold = 1L)
        if (path != "memory") for (root in names(result$status$published)) for (component in names(result$status$published[[root]])) {
          symbol <- result$status$published[[root]][[component]]$symbol
          descriptor <- get(symbol, run$ctx$target_env)
          if (is.null(descriptor$metadata$concept_presentation)) next
          report <- omopFactorLevelsDS(descriptor)
          if (length(report$unsafe)) {
            # Tiny event fixtures can fail the distinct-person level gate.
            # Verify that refusal, while still auditing their private staged
            # round-trip below; never weaken the gate to exercise a rewrite.
            expect_error(omopAsFactorColumnsDS(descriptor, .conceptEncode(list(
              concept_protocol = 1L, columns = list(), evidence = paste0(report$evidence)))),
              "Incomplete disclosure-safe staged concept contract")
            next
          }
          spec <- list(concept_protocol = 1L, columns = lapply(report$concepts, .conceptResolve),
            evidence = paste0(report$evidence), names = if (length(report$generated)) as.list(.conceptGeneratedNames(report$keys, report$generated)) else list())
          updated <- omopAsFactorColumnsDS(descriptor, .conceptEncode(spec))
          assign(symbol, updated, run$ctx$target_env)
          chunks <- list(); .publishReadChunks(updated, function(x) chunks[[length(chunks)+1L]] <<- x)
          result$values[[root]][[component]] <- do.call(rbind, chunks)
        }
        result
      }
      result <- publish()
      expect_identical(result$status$status, "ok", info = path)
      results[[path]] <- result$values
    }, finally = .ws7_close(run))
  }
  reference <- results$memory
  for (path in names(results)) for (root in names(reference)) for (component in names(reference[[root]])) {
    mem <- reference[[root]][[component]]; frame <- results[[path]][[root]][[component]]
    for (col in names(mem)) if (is.factor(mem[[col]])) {
      info <- paste(path, root, component, col)
      expect_true(is.factor(frame[[col]]), info = info)
      expect_identical(is.ordered(frame[[col]]), is.ordered(mem[[col]]), info = info)
      expect_identical(levels(frame[[col]]), levels(mem[[col]]), info = info)
      expect_identical(as.character(frame[[col]]), as.character(mem[[col]]), info = info)
    }
  }
  for (root in c("o", "long", "survival", "joined")) {
    frame <- reference[[root]][[1L]]
    for (col in intersect(names(plan$variables), names(frame))) {
      expect_true(is.factor(frame[[col]]), info = paste(root,col))
      expect_identical(is.ordered(frame[[col]]), !col %in% c("sex","education"), info = paste(root,col))
    }
    if (root != "survival") {
      expect_identical(levels(frame$sex), c("F", "M"))
      expect_identical(levels(frame$band), c("0-17", "18-64", "65+"))
    }
    expect_true(all(c("age_at", "month", "weekday", "season") %in% names(frame)))
  }
  expect_true(is.factor(reference$dictionary$dictionary$concept_id))
  for (root in c("panel", "sparse")) expect_true(is.factor(reference[[root]]$covariateRef$concept_id))
  expect_true(is.ordered(reference$period$period$age_band))
  expect_true(is.ordered(reference$period$period$calendar_month))
  expect_identical(levels(reference$period$period$calendar_month), as.character(1:12))
  expect_false(is.factor(reference$membership$membership$cohort_definition_id))
  expect_true(all(vapply(reference$period$period[grep("_count$",names(reference$period$period))],is.numeric,TRUE)))
})

test_that("ordinary factor domains restore unused levels after CSV and Parquet reads", {
  for (ordered in c(FALSE, TRUE)) {
    x <- data.frame(category = factor(c("a", NA, "a"), levels = c("a", "b"), ordered = ordered))
    d <- .conceptStageCapture(x, list(metadata = list(column_types = c(category = if(ordered) "integer|ordered/factor" else "integer|factor"))))
    csv <- data.frame(category = c("a", NA, "a"))
    y <- .publishRestoreTypes(csv, d)$category
    expect_true(is.factor(y)); expect_identical(is.ordered(y), ordered)
    expect_identical(levels(y), c("a", "b")); expect_identical(as.character(y), as.character(x$category))
    if (.arrowAvailable()) {
      path <- tempfile(fileext = ".parquet"); on.exit(unlink(path), add = TRUE)
      arrow::write_parquet(x, path)
      y <- .publishRestoreTypes(as.data.frame(arrow::read_parquet(path)), d)$category
      expect_true(is.factor(y)); expect_identical(is.ordered(y), ordered)
      expect_identical(levels(y), c("a", "b"))
    }
  }
})


test_that("v1 baseline and features publish categorical domains", {
  h <- create_test_handle(); on.exit(cleanup_handle(h), add = TRUE)
  .ws7_keys(list(h)); .dsomop_handle_ores <- h
  for (mode in c("memory", "staged")) for (translated in c(FALSE, TRUE)) {
    p <- list(version = 1, options = list(translate_concepts = translated),
      cohort = list(type = "cohort_table", cohort_definition_id = 1L),
      outputs = list(baseline = list(type = "baseline", columns = "gender_concept_id", derived = "age_at_index", age_breaks = c(0L,18L,65L)),
        features = list(type = "person_level", tables = list(person = "gender_concept_id"),
          derived_columns = list(list(kind = "sex_mf", name = "sex")))))
    O_exec <- omopPlanExecuteDS("ores", p, list(baseline = "B", features = "F"), output_mode = mode)
    expect_identical(omopPlanStatusDS("O_exec")$status, "ok")
    read <- function(x) {
      if (!inherits(x, "FlowerDatasetDescriptor")) return(x)
      chunks <- list(); .publishReadChunks(x, function(c) chunks[[length(chunks)+1L]] <<- c)
      do.call(rbind,chunks)
    }
    b <- read(B); f <- read(F)
    expect_true(is.ordered(b$age_group)); expect_identical(levels(b$age_group), c("0-17","18-64","65+"))
    expect_true(is.factor(f$sex)); expect_false(is.ordered(f$sex)); expect_identical(levels(f$sex), c("F","M"))
    if (translated) expect_true(is.factor(b$gender_concept_id)) else expect_false(is.factor(b$gender_concept_id))
    omopStagedRevokeDS("O_exec")
  }
})


test_that("legacy coarsened dates become ordered factors only at publication", {
  h <- create_test_handle(); on.exit(cleanup_handle(h), add = TRUE)
  ctx <- list(handle = h, ir = list(outputs = list(out = list(type = "event_level", table = "measurement"))))
  for (width in c("year", "month", "week")) for (n in c(0L, 3L)) {
    frame <- data.frame(person_id = seq_len(n), measurement_date = as.Date(c("2020-05-02", "2020-01-01", NA)[seq_len(n)]))
    raw <- .applyDateHandling(frame, list(mode = "binned", bin_width = width))
    cols <- .conceptLegacyColumns(ctx, raw, "out")
    result <- .conceptPresent(ctx, raw, cols)
    expect_true(is.ordered(result$measurement_date))
    expect_identical(as.character(result$measurement_date), raw$measurement_date)
    expect_identical(levels(result$measurement_date), sort(unique(na.omit(raw$measurement_date))))
    expect_identical(attr(result,"omop_concepts")$measurement_date$kind,"date")
  }
})


test_that("private Parquet presentation writes typed factors including empty streams", {
  skip_if_not_installed("arrow")
  for (n in c(0L, 3L)) {
    path <- tempfile(fileext = ".parquet"); on.exit(unlink(path), add = TRUE)
    arrow::write_parquet(data.frame(code = rep(1L, n)), path)
    descriptor <- list(metadata = list(file = path, format = "parquet", columns = "code", column_types = c(code = "integer|integer")))
    updated <- .conceptStageCapture(data.frame(code = factor(character(), levels = c("1", "2"))), descriptor)
    updated <- .conceptRewritePrivate(descriptor, updated, function(frame) {
      frame$code <- factor(as.character(frame$code), levels = c("1", "2")); frame
    })
    chunks <- list(); .publishReadChunks(updated, function(x) chunks[[length(chunks)+1L]] <<- x)
    restored <- do.call(rbind, chunks)
    expect_true(is.factor(restored$code)); expect_identical(levels(restored$code), c("1", "2"))
    expect_equal(nrow(restored), n)
  }
})


test_that("numeric category order is independent of locally absent bands", {
  column <- .fragmentColumn("integer", TRUE, "measure")
  attr(column, "category_domain") <- list(calendar = list(category = "age_band_index", width = 5L))
  records <- lapply(c(10L, 2L), function(value) {
    f <- .conceptPresent(list(ir = list(options = list(translate_concepts = FALSE))),
      data.frame(band = value), list(band = column))
    expect_true(is.ordered(f$band))
    attr(f,"omop_concepts")$band$records[[1L]]
  })
  expect_identical(.dateResolveRecords(records)$levels, c("2", "10"))
  expect_identical(.dateResolveRecords(rev(records))$levels, c("2", "10"))
})
