# Independent rereview probes, actual server cryptography and payloads
# (astra-scratch/rereview-typed, ported unchanged except: the client sources
# are skipped when the sibling dsOMOPClient is absent, the status fixture asks
# as a dsOMOPClient 2.10.3 client (R08: older clients are refused), and the
# public-client test loads the sibling client's namespace).
.astra_client <- function() {
  e <- new.env(parent = environment())
  for (f in c("privacy.R", "privacy_protected.R", "analysis.R")) {
    path <- testthat::test_path("../../../dsOMOPClient/R", f)
    if (!file.exists(path)) skip("sibling dsOMOPClient sources not found")
    sys.source(path, e)
  }
  e
}
.astra_client_namespace <- function() {
  cp <- testthat::test_path("../../../dsOMOPClient")
  ok <- dir.exists(cp) && tryCatch({
    suppressMessages(pkgload::load_all(cp, quiet = TRUE, export_all = FALSE))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) skip("sibling dsOMOPClient not loadable")
}
.astra_table <- function(d, domains = NULL) {
  .dsomopDpSealPersonLocal(.testPseudonymize(d),
    producer = "test/astra-rereview", domains = domains,
    dataset_identity = list(resource_identity = "astra-rereview",
      target_dialect = "postgresql", cdm_schema = "main",
      vocabulary_schema = "main", results_schema = "main"))
}

test_that("Astra: one-site binary pooling preserves all positive masks and denominators", {
  .release_state()
  c <- .astra_client()
  x <- .astra_table(data.frame(person_id = 1:2000,
    label = c(rep("b", 1500), rep("a", 400), rep(NA_character_,100)),
    day = 1:2000), domains = list(label = c("a", "b")))
  for (reducer in c("any", "all", "first", "last")) {
    for (denominator in c("all_persons", "nonmissing")) {
      spec <- list(statistic = "binary_rate", variable = "label",
        positive = "b", reducer = reducer, denominator = denominator)
      if (reducer %in% c("first", "last")) spec$order_by <- "day"
      p <- suppressMessages(omopDpReleaseDS(x, spec))
      pooled <- c$.dp_pool_protected(list(one = p), do.call(c$omop_privacy, spec))
      message("ASTRA binary ", reducer, "/", denominator,
        " per-site=", p$noisy_numerator, "/", p$noisy_denominator,
        " pooled=", pooled$noisy_numerator, "/", pooled$noisy_denominator)
      expect_equal(pooled$noisy_numerator, p$noisy_numerator, tolerance = 1e-9)
      expect_equal(pooled$noisy_denominator, p$noisy_denominator, tolerance = 1e-9)
    }
  }
})

test_that("Astra: one-site weighted numeric pooling preserves displayed counts", {
  .release_state()
  c <- .astra_client()
  x <- .astra_table(data.frame(person_id = 1:2000,
    measurement = c(rep(0,500), rep(100,1500))))
  spec <- list(statistic = "numeric_histogram", variable = "measurement",
    breaks = c(-1,.01,.05,200), reducer = "records", max_contributions = 1)
  p <- suppressMessages(omopDpReleaseDS(x, spec))
  pooled <- c$.dp_pool_protected(list(one=p), do.call(c$omop_privacy,spec))
  message("ASTRA weighted numeric per-site=", paste(p$counts, collapse=","),
          " pooled=", paste(pooled$counts, collapse=","))
  expect_equal(pooled$counts, p$counts, tolerance = 1e-9)
})

test_that("Astra: catalogue rejects a heuristic label claiming fixed guarantee scope", {
  c <- .astra_client()
  x <- data.frame(count=10)
  attr(x,"dsomop_protection") <- list(protection="realised_sticky",
    guarantee_scope="fixed_vector_before_exact_support_gate",
    mechanism_epsilon=NA_real_, api_transcript_epsilon=NA_real_)
  expect_error(c$.analysis_protection_labels(list(site=x)), "guarantee|scope|incoherent")
})

test_that("Astra: binary bitset pooling refuses different registered domains", {
  .release_state()
  c <- .astra_client()
  d <- data.frame(person_id=1:2000,label=rep("b",2000))
  spec <- list(statistic="binary_rate",variable="label",positive="b",reducer="any")
  a <- suppressMessages(omopDpReleaseDS(.astra_table(d,list(label=c("a","b"))),spec))
  b <- suppressMessages(omopDpReleaseDS(.astra_table(d,list(label=c("b","z"))),spec))
  # Logical-source metadata differs as it would across independent sites; the
  # compatibility assertion concerns only the actual domains/basis meanings.
  b$source$logical_source_id <- paste0("dsl_",strrep("7",40))
  expect_error(c$.dp_pool_protected(list(a=a,b=b),do.call(c$omop_privacy,spec)),
               "not compatible|domain")
})

test_that("Astra: public current client preserves an actual server binary result", {
  .release_state()
  x <- .astra_table(data.frame(person_id=1:2000,
    label=c(rep("b",1500),rep("a",500))),list(label=c("a","b")))
  spec <- list(statistic="binary_rate",variable="label",positive="b",reducer="any")
  p <- suppressMessages(omopDpReleaseDS(x,spec))
  s <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)
  .astra_client_namespace()
  testthat::local_mocked_bindings(datashield.aggregate=function(conns,expr,...) {
    val <- if (as.character(expr[[1]]) == "omopDpStatusDS") s else p
    setNames(list(val),names(conns))
  }, .package="DSI")
  res <- dsOMOPClient::ds.omop.dp.release("analysis_table",
    do.call(dsOMOPClient::omop_privacy,spec),datasources=list(one="FAKE_1"))
  message("ASTRA public binary per-site=",res$per_site$one$noisy_numerator,
          " pooled=",res$pooled$noisy_numerator)
  expect_equal(res$pooled$noisy_numerator,res$per_site$one$noisy_numerator)
})
