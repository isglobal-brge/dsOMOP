# Phase 5: vocabulary exploration + CDM metadata aggregates.
# Drives the public omop*DS entry points (the wired aggregates) end-to-end
# against the SQLite fixture, registering the handle the way .getHandle resolves
# it (the .dsomop_env fallback used outside a live DSLite session).

# Register a fixture-backed handle under an OMOP symbol and return it.
register_test_omop <- function(symbol = "omop", version = "5.4") {
  conn <- if (identical(version, "5.3")) {
    create_test_omop_db_v53(n_persons = 15)
  } else {
    create_test_omop_db(n_persons = 15)
  }
  handle <- new.env(parent = emptyenv())
  handle$conn            <- conn
  handle$dbms            <- "sqlite"
  handle$target_dialect  <- "sqlite"
  handle$cdm_schema      <- NULL
  handle$vocab_schema    <- NULL
  handle$results_schema  <- NULL
  handle$temp_schema     <- NULL
  handle$resource_client <- NULL
  handle$config          <- list()
  handle$blueprint       <- NULL
  handle$temp_tables     <- character(0)
  assign(paste0("handle_", symbol), handle, envir = dsOMOP:::.dsomop_env)
  handle
}

unregister_test_omop <- function(handle, symbol = "omop") {
  if (!is.null(handle$conn) && DBI::dbIsValid(handle$conn)) {
    DBI::dbDisconnect(handle$conn)
  }
  key <- paste0("handle_", symbol)
  if (exists(key, envir = dsOMOP:::.dsomop_env)) {
    rm(list = key, envir = dsOMOP:::.dsomop_env)
  }
}

# --- ancestors / descendants ------------------------------------------------

test_that("omopConceptAncestorsDS returns the ancestor with direction tag", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  # 4000000 (Metabolic disease) is the parent of 201820 (Diabetes).
  res <- omopConceptAncestorsDS("omop", 201820L)
  anc <- res[res$direction == "ancestor", ]
  expect_true(4000000L %in% anc$concept_id)
  expect_true(all(anc$min_levels_of_separation >= 1))   # self-row dropped
  expect_false(201820L %in% res$concept_id)             # query concept excluded
})

test_that("omopConceptAncestorsDS returns descendants in the same frame", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  # 4000001 (Respiratory disease) is the parent of 255573 (COPD) + 317009 (Asthma)
  res <- omopConceptAncestorsDS("omop", 4000001L)
  desc <- res[res$direction == "descendant", ]
  expect_setequal(desc$concept_id, c(255573L, 317009L))
  expect_true(all(c("direction", "min_levels_of_separation",
                    "max_levels_of_separation") %in% names(res)))
})

# --- synonyms ---------------------------------------------------------------

test_that("omopConceptSynonymsDS returns the synonym rows", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  res <- omopConceptSynonymsDS("omop", 201820L)
  expect_true(is.data.frame(res))
  expect_setequal(res$concept_synonym_name,
                  c("Diabetes mellitus (disorder)", "DM"))
  expect_true(all(res$concept_id == 201820L))
})

test_that("omopConceptSynonymsDS handles multiple concept IDs", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  res <- omopConceptSynonymsDS("omop", c(201820L, 255573L))
  expect_setequal(unique(res$concept_id), c(201820L, 255573L))
})

# --- relationships ----------------------------------------------------------

test_that("omopConceptRelationshipsDS returns BOTH directions", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  # Forward edge stored as 44826430 (ICD9 250.50) -Maps to-> 4174977 (SNOMED).
  fwd <- omopConceptRelationshipsDS("omop", 44826430L)
  expect_true("forward" %in% fwd$direction)
  expect_equal(fwd$related_concept_id[fwd$direction == "forward"], 4174977L)
  expect_equal(fwd$related_concept_name[fwd$direction == "forward"],
               "Diabetic retinopathy")

  # Querying the SNOMED target surfaces the same edge as a reverse relationship.
  rev <- omopConceptRelationshipsDS("omop", 4174977L)
  expect_true("reverse" %in% rev$direction)
  expect_equal(rev$related_concept_id[rev$direction == "reverse"], 44826430L)
})

test_that("omopConceptRelationshipsDS honours the relationship_id filter", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  keep <- omopConceptRelationshipsDS("omop", 44826430L,
                                     relationship_id = "Maps to")
  expect_true(nrow(keep) > 0)
  expect_true(all(keep$relationship_id == "Maps to"))

  none <- omopConceptRelationshipsDS("omop", 44826430L,
                                     relationship_id = "Nonexistent rel")
  expect_equal(nrow(none), 0)
})

# --- paginated concept listing ----------------------------------------------

test_that("omopListConceptsDS paginates with disjoint, ordered pages", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  p1 <- omopListConceptsDS("omop", offset = 0L, limit = 3L)
  p2 <- omopListConceptsDS("omop", offset = 3L, limit = 3L)

  expect_equal(nrow(p1$rows), 3L)
  expect_equal(nrow(p2$rows), 3L)
  expect_equal(p1$total_count, 43)                      # all fixture concepts
  expect_equal(p2$total_count, 43)
  # Pages are disjoint and globally ordered by concept_id.
  expect_length(intersect(p1$rows$concept_id, p2$rows$concept_id), 0)
  expect_true(max(p1$rows$concept_id) < min(p2$rows$concept_id))
})

test_that("omopListConceptsDS lifts the old 500-row cap", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  # A limit above the legacy 500 ceiling must be honoured (echoed, not clamped
  # to 500) and return every matching row.
  res <- omopListConceptsDS("omop", offset = 0L, limit = 600L)
  expect_equal(res$limit, 600L)
  expect_equal(nrow(res$rows), 43L)
  expect_equal(res$total_count, 43)

  # ...but the documented 1000 page-size cap still applies.
  capped <- omopListConceptsDS("omop", offset = 0L, limit = 5000L)
  expect_equal(capped$limit, 1000L)
})

test_that("omopListConceptsDS filters by vocabulary with matching total_count", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  res <- omopListConceptsDS("omop", vocabulary = "SNOMED", limit = 100L)
  expect_true(all(res$rows$vocabulary_id == "SNOMED"))
  expect_equal(nrow(res$rows), 19L)
  expect_equal(res$total_count, 19)        # total reflects the filter, not page
})

# --- vocabulary / domain / concept-class listers ----------------------------

test_that("omopVocabulariesDS returns the distinct vocabulary set", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  res <- omopVocabulariesDS("omop")
  expect_true(all(c("SNOMED", "ICD9CM", "ICD10CM") %in% res$vocabulary_id))
  expect_equal(anyDuplicated(res$vocabulary_id), 0L)
})

test_that("omopDomainsDS returns the distinct domain set", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  res <- omopDomainsDS("omop")
  expect_true(all(c("Condition", "Measurement", "Drug") %in% res$domain_id))
  expect_equal(anyDuplicated(res$domain_id), 0L)
})

test_that("omopConceptClassesDS returns the distinct concept-class set", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  res <- omopConceptClassesDS("omop")
  expect_true(all(c("Clinical Finding", "Ingredient", "Lab Test") %in%
                    res$concept_class_id))
  expect_equal(anyDuplicated(res$concept_class_id), 0L)
})

# --- search: concept_id + validity facets -----------------------------------

test_that("omopSearchConceptsDS supports exact concept_id matching", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  res <- omopSearchConceptsDS("omop", concept_id = c(201820L, 255573L),
                              standard_only = FALSE)
  expect_setequal(res$concept_id, c(201820L, 255573L))
})

test_that("omopSearchConceptsDS filters by validity", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  valid <- omopSearchConceptsDS("omop", pattern = "a", valid = TRUE,
                                standard_only = FALSE)
  expect_true(nrow(valid) > 0)
  expect_true(all(is.na(valid$invalid_reason)))

  # All fixture concepts are currently valid -> invalid filter yields nothing.
  invalid <- omopSearchConceptsDS("omop", pattern = "a", valid = FALSE,
                                  standard_only = FALSE)
  expect_equal(nrow(invalid), 0)
})

test_that("omopSearchConceptsDS keeps explicit standard filter", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  res <- omopSearchConceptsDS("omop", pattern = "a", standard = "S")
  if (nrow(res) > 0) expect_true(all(res$standard_concept == "S"))
})

# --- cdm_source / cdm_version -----------------------------------------------

test_that("omopCdmSourceDS returns the cdm_source row", {
  h <- register_test_omop(); on.exit(unregister_test_omop(h))

  res <- omopCdmSourceDS("omop")
  expect_equal(nrow(res), 1L)
  expect_equal(res$cdm_source_name[1], "dsOMOP Test")
})

test_that("omopCdmVersionDS resolves 5.4 from cdm_source", {
  h <- register_test_omop(version = "5.4"); on.exit(unregister_test_omop(h))

  res <- omopCdmVersionDS("omop")
  expect_equal(res$cdm_version, "v5.4")
  expect_equal(res$source, "cdm_source")
})

test_that("omopCdmVersionDS resolves 5.3 from cdm_source", {
  h <- register_test_omop(version = "5.3"); on.exit(unregister_test_omop(h))

  res <- omopCdmVersionDS("omop")
  expect_equal(res$cdm_version, "v5.3")
  expect_equal(res$source, "cdm_source")
})

# --- manifest <-> NAMESPACE coherence ---------------------------------------

test_that("DATASHIELD manifest lists every new Phase 5 aggregate", {
  pkg_root <- tryCatch(pkgload::pkg_path(), error = function(e) {
    # Fall back to walking up from the test directory.
    normalizePath(file.path(testthat::test_path(), "..", ".."),
                  mustWork = FALSE)
  })

  new_aggs <- c(
    "omopConceptAncestorsDS", "omopConceptSynonymsDS",
    "omopConceptRelationshipsDS", "omopListConceptsDS",
    "omopVocabulariesDS", "omopDomainsDS", "omopConceptClassesDS",
    "omopCdmSourceDS", "omopCdmVersionDS"
  )

  read_field <- function(path, field) {
    dcf <- read.dcf(path)
    if (!field %in% colnames(dcf)) return(character(0))
    items <- strsplit(dcf[1, field], "[,[:space:]]+")[[1]]
    items[nzchar(items)]
  }

  ds_aggs   <- read_field(file.path(pkg_root, "DATASHIELD"), "AggregateMethods")
  desc_aggs <- read_field(file.path(pkg_root, "DESCRIPTION"), "AggregateMethods")
  ns        <- readLines(file.path(pkg_root, "NAMESPACE"))

  for (agg in new_aggs) {
    expect_true(agg %in% ds_aggs,
                info = paste(agg, "missing from DATASHIELD AggregateMethods"))
    expect_true(agg %in% desc_aggs,
                info = paste(agg, "missing from DESCRIPTION AggregateMethods"))
    expect_true(any(grepl(paste0("export\\(", agg, "\\)"), ns)),
                info = paste(agg, "missing export() in NAMESPACE"))
  }
})
