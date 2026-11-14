# A-25 integration: lowered owned-population references reach the public v1
# adapter and ordinary publication. Helpers here are independent of other
# test files; shared database and pseudonymisation fixtures remain in helpers.
.ws7f_owned_plan <- function(ref, indexed = FALSE) {
  population <- list(id = "owned", kind = "criteria", cohort_definition_id = ref)
  if (indexed) {
    population$index_event <- list(table = "condition_occurrence",
      concept_set = 201820L, primary_limit = "all")
    # Every real index event matches itself. The borrowed table's dates in
    # 2035 must never replace these anchors or yield the same answer.
    population$filter_tree <- list(type = "has_concept", params = list(
      concept_id = 201820L, table = "condition_occurrence", min_count = 1L,
      window = list(start = 0L, end = 0L)))
  }
  list(populations = list(owned = population,
    everyone = list(id = "everyone", kind = "criteria", filter_tree = list(or = list(
      list(type = "sex", params = list(value = "M")),
      list(type = "sex", params = list(value = "F")))))),
    outputs = list(owned = list(type = "baseline", population_id = "owned",
      columns = "gender_concept_id"), sibling = list(type = "baseline",
      population_id = "everyone", columns = "gender_concept_id")),
    options = list(translate_concepts = FALSE, factor_concepts = FALSE,
      block_sensitive = TRUE))
}

.ws7f_owned_events <- function(handle) {
  DBI::dbExecute(handle$conn, "DELETE FROM condition_occurrence")
  rows <- expand.grid(person_id = 1:15, episode = 1:2)
  DBI::dbWriteTable(handle$conn, "condition_occurrence", data.frame(
    condition_occurrence_id = as.integer(rows$person_id * 10L + rows$episode),
    person_id = as.integer(rows$person_id), condition_concept_id = 201820L,
    condition_start_date = paste0(2020L + rows$episode, "-06-01"),
    condition_end_date = NA_character_, condition_type_concept_id = 44818518L,
    visit_occurrence_id = NA_integer_), append = TRUE)
}

.ws7f_owned_run <- function(symbol, plan, mode) {
  target <- new.env(parent = environment())
  # Evaluate the exported endpoint in its actual assignment environment.
  status <- eval(substitute(omopPlanExecuteDS(SYMBOL, PLAN,
    list(owned = "D_owned", sibling = "D_sibling"), output_mode = MODE),
    list(SYMBOL = symbol, PLAN = plan, MODE = mode)), target)
  execution <- attr(status, "execution_id")
  if (!is.null(execution) && exists(execution, .publicationRegistry, inherits = FALSE)) {
    ctx <- get(execution, .publicationRegistry, inherits = FALSE)
    on.exit(.publishAbort(ctx), add = TRUE)
  }
  bindings <- ls(target)
  frames <- lapply(bindings, function(name) {
    value <- get(name, target, inherits = FALSE)
    if (identical(mode, "staged")) {
      expect_s3_class(value, "FlowerDatasetDescriptor")
      expect_true(file.exists(omopStagedDatasetPath(value)))
      chunks <- list()
      .publishReadChunks(value, function(chunk) chunks[[length(chunks) + 1L]] <<- chunk)
      value <- do.call(rbind, chunks)
    }
    value
  })
  names(frames) <- bindings
  list(status = status, frames = frames, bindings = bindings)
}

test_that("A-25 public adapters preserve owned membership, recurrence and sibling locality", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  key <- .ws7_keys(list(handle))
  withr::local_options(list(nfilter.subset = 5L, nfilter.tab = 1L,
                            dsomop.query_strict = TRUE))
  .ws7f_owned_events(handle)
  owned <- c(1L, 3L, 5L, 7L, 9L, 11L)
  ref <- "WS7F_owned"
  DBI::dbExecute(handle$conn, paste0("CREATE TEMP TABLE ", ref,
    " AS SELECT person_id AS subject_id, '2035-01-01' AS cohort_start_date, ",
    "'2036-01-01' AS cohort_end_date FROM person WHERE person_id IN (",
    paste(owned, collapse = ","), ")"))
  register_test_temp(handle, ref)
  symbol <- "ws7f_owned_handoff"
  .setHandle(symbol, handle)
  on.exit(rm(list = paste0("handle_", symbol), envir = .dsomop_env), add = TRUE)
  physical <- function() sort(DBI::dbGetQuery(handle$conn,
    "SELECT name FROM sqlite_temp_master WHERE type = 'table'")$name)
  borrowed <- DBI::dbReadTable(handle$conn, ref)
  registered <- handle$temp_tables
  tables <- physical()
  tokens <- function(ids) .testPseudonymize(data.frame(person_id = ids), key)$person_id
  canonical <- function(frame) {
    result <- as.data.frame(lapply(frame, as.character), stringsAsFactors = FALSE)
    rownames(result) <- NULL
    result
  }
  for (indexed in c(FALSE, TRUE)) {
    plan <- .ws7f_owned_plan(paste0(" ", ref, " "), indexed)
    ir <- .irPrepare(plan, handle)
    expect_identical(ir$populations$owned$legacy_cohort_table, ref)
    expect_null(ir$scope$cohort)
    expect_null(ir$populations$everyone$legacy_cohort_table)
    expect_identical(ir$populations$owned$kind, if (indexed) "index" else "criteria")
    if (!indexed) expect_identical(ir$populations$owned$legacy_episodes, "observation_periods")
    memory <- NULL
    for (mode in c("memory", "staged")) {
      info <- paste(if (indexed) "recurrent index" else "criteria", mode)
      result <- .ws7f_owned_run(symbol, plan, mode)
      expect_identical(result$status$status, "ok", info = info)
      expect_true(.validatePlanStatus(unclass(result$status)), info = info)
      expect_setequal(result$bindings, c("D_owned", "D_sibling"))
      frame <- result$frames$D_owned
      sibling <- result$frames$D_sibling
      expect_identical(nrow(frame), if (indexed) 12L else 6L, info = info)
      expect_identical(sort(as.character(frame$person_id)),
        sort(tokens(rep(owned, each = if (indexed) 2L else 1L))), info = info)
      expect_equal(as.numeric(as.character(frame$gender_concept_id)),
        rep(8507, nrow(frame)), info = info)
      expect_identical(nrow(sibling), 15L, info = info)
      expect_identical(sort(as.character(sibling$person_id)), sort(tokens(1:15)), info = info)
      expect_identical(DBI::dbReadTable(handle$conn, ref), borrowed, info = info)
      expect_setequal(handle$temp_tables, registered)
      expect_identical(physical(), tables, info = info)
      if (identical(mode, "memory")) memory <- lapply(result$frames, canonical) else
        expect_identical(lapply(result$frames, canonical), memory, info = info)
    }
  }

  # Four owners are below final k despite recurrent events and a sibling with
  # fifteen persons. Neither refusal nor context cleanup may drop the loan.
  DBI::dbExecute(handle$conn, paste0("DELETE FROM ", ref, " WHERE subject_id IN (9,11)"))
  small <- DBI::dbReadTable(handle$conn, ref)
  for (indexed in c(FALSE, TRUE)) for (mode in c("memory", "staged")) {
    info <- paste("four owners", indexed, mode)
    result <- .ws7f_owned_run(symbol, .ws7f_owned_plan(ref, indexed), mode)
    expect_identical(result$status$status, "refused", info = info)
    expect_identical(result$status$rejected$owned$code, "insufficient_support", info = info)
    expect_length(result$status$published, 0L)
    expect_length(result$bindings, 0L)
    expect_identical(DBI::dbReadTable(handle$conn, ref), small, info = info)
    expect_setequal(handle$temp_tables, registered)
    expect_identical(physical(), tables, info = info)
  }
})
