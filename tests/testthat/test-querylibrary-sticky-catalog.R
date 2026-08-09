test_that("executable QueryLibrary redesign catalog is pinned and complete", {
  catalog <- dsOMOP:::.omopQueryLibraryStickyCatalog()
  expect_s3_class(catalog, "data.frame")
  expect_equal(nrow(catalog), 129L)
  expect_identical(anyDuplicated(catalog$upstream_id), 0L)
  expect_setequal(
    unique(catalog$statistic),
    c(
      "count", "bounded_record_count", "categorical_histogram",
      "numeric_histogram", "bounded_distinct", "bounded_mean", "binary_rate"
    )
  )
  expect_true(all(catalog$status ==
                    "mapped_to_bounded_sticky_primitive"))
  expect_true(all(!catalog$literal_sql_authorized))
  expect_true(all(grepl("^[0-9a-f]{64}$", catalog$sha256)))
  expect_true(all(grepl(
    paste0("/blob/", unique(catalog$source_commit), "/"),
    catalog$source_url, fixed = TRUE
  )))

  registry <- jsonlite::fromJSON(
    system.file("queries", "dp_redesign_registry.json", package = "dsOMOP"),
    simplifyVector = FALSE
  )
  documented_ids <- vapply(
    registry$redesigns, `[[`, character(1L), "upstream_id"
  )
  expect_true(all(documented_ids %in% catalog$upstream_id))
  expect_equal(length(setdiff(catalog$upstream_id, documented_ids)), 115L)
})

test_that("executable catalog rejects unsupported metadata contracts", {
  registry_path <- system.file(
    "queries", "dp_redesign_registry.json", package = "dsOMOP"
  )
  audit_path <- system.file(
    "queries", "upstream_querylibrary_audit.json", package = "dsOMOP"
  )
  registry <- jsonlite::fromJSON(registry_path, simplifyVector = FALSE)

  mutations <- list(
    registry_schema = function(value) {
      value$schema_version <- 2L
      value
    },
    registry_role = function(value) {
      value$scope$registry_role <- "documentation_only"
      value
    },
    redesign_count = function(value) {
      value$scope$typed_redesigns_enabled <- 128L
      value
    },
    literal_sql = function(value) {
      value$scope$literal_upstream_sql_enabled <- 1L
      value
    },
    adjacency = function(value) {
      value$privacy_contract$adjacency <- "add_or_remove_one_person"
      value
    }
  )
  paths <- character(0)
  on.exit(unlink(paths), add = TRUE)
  for (name in names(mutations)) {
    path <- tempfile(fileext = ".json")
    paths <- c(paths, path)
    jsonlite::write_json(
      mutations[[name]](registry), path, auto_unbox = TRUE, pretty = TRUE
    )
    expect_error(
      dsOMOP:::.omopQueryLibraryStickyCatalog(path, audit_path),
      "metadata is inconsistent",
      info = name
    )
  }

  audit <- jsonlite::fromJSON(audit_path, simplifyVector = FALSE)
  audit$schema_version <- 3L
  bad_audit_path <- tempfile(fileext = ".json")
  paths <- c(paths, bad_audit_path)
  jsonlite::write_json(
    audit, bad_audit_path, auto_unbox = TRUE, pretty = TRUE
  )
  expect_error(
    dsOMOP:::.omopQueryLibraryStickyCatalog(registry_path, bad_audit_path),
    "metadata is inconsistent"
  )
})

test_that("all bounded aggregate shapes are mapped and unsafe shapes stay out", {
  registry <- jsonlite::fromJSON(
    system.file("queries", "dp_redesign_registry.json", package = "dsOMOP"),
    simplifyVector = FALSE
  )
  catalog <- dsOMOP:::.omopQueryLibraryStickyCatalog()
  audit <- jsonlite::fromJSON(
    system.file(
      "queries", "upstream_querylibrary_audit.json", package = "dsOMOP"
    ),
    simplifyVector = FALSE
  )
  held <- unlist(lapply(
    registry$executable_catalog$held_back, `[[`, "upstream_ids"
  ), use.names = FALSE)
  blocked <- unlist(lapply(
    registry$blocked, `[[`, "upstream_ids"
  ), use.names = FALSE)

  expect_equal(length(held), 0L)
  expect_identical(anyDuplicated(held), 0L)
  expect_length(intersect(catalog$upstream_id, held), 0L)
  expect_length(intersect(catalog$upstream_id, blocked), 0L)
  expect_true(all(c("DEX06", "CE09", "DEX39") %in% catalog$upstream_id))
  expect_true(all(c("PE08", "CO20", "DER02") %in% blocked))

  aggregate_backlog <- vapply(Filter(function(query) {
    query$triage_class %in% c(
      "rewritable_patient_aggregate", "statistical_needs_redesign"
    )
  }, audit$queries), `[[`, character(1L), "upstream_id")
  expect_equal(length(aggregate_backlog), 129L)
  expect_setequal(catalog$upstream_id, aggregate_backlog)
})

test_that("record and distinct mappings advertise explicit person caps", {
  catalog <- dsOMOP:::.omopQueryLibraryStickyCatalog()
  records <- catalog[catalog$record_cap_required, , drop = FALSE]
  expect_setequal(
    records$upstream_id,
    c("CE05", "CO09", "CO19", "CO23", "DER17", "DEX33",
      "OP10", "OP17", "OP18", "CO10", "CO18", "DER08", "DEX25",
      "CE04", "CE09", "CO03", "CO04", "CO14", "CO22", "DER12",
      "DER21", "DEX05", "DEX13", "DEX18", "DEX28", "DEX39", "DEX06")
  )
  expect_setequal(
    records$statistic,
    c("bounded_record_count", "categorical_histogram",
      "numeric_histogram", "bounded_distinct")
  )
  record_histogram <- records[
    records$statistic %in% c("categorical_histogram", "numeric_histogram"),
    , drop = FALSE
  ]
  expect_true(all(record_histogram$reducer == "records"))
  expect_true(all(record_histogram$order_by_required))
  expect_true(all(!records$order_by_required[
    records$statistic %in% c("bounded_record_count", "bounded_distinct")
  ]))
})

test_that("public catalog endpoint never exposes or authorizes SQL", {
  value <- omopQueryLibraryStickyCatalogDS()
  expect_equal(nrow(value), 129L)
  expect_false("sql" %in% tolower(names(value)))
  expect_true(all(!value$literal_sql_authorized))
})

test_that("DP status advertises the complete executable QueryLibrary catalog", {
  value <- dsOMOP:::.dsomopDpQueryLibraryStatus()
  expect_true(value$available)
  expect_identical(value$mapped_query_count, 129L)
  expect_length(value$mapped_to_bounded_sticky_primitive, 129L)
  expect_true(all(c("DEX06", "CE09", "DEX39") %in%
                    value$mapped_to_bounded_sticky_primitive))
  expect_false(value$literal_sql_authorized)
})
