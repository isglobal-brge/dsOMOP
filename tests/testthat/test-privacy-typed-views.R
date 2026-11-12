# Typed views of one complete basis (dsOMOP 2.10.3 round 3, re-review R04,
# R05, R08): binary rates carry public numerator and denominator masks of
# their complete basis and the basis identity names the registered domain;
# weighted numeric histograms own a protected weighted total and derive every
# coarsening from one versioned projection; clients older than dsOMOPClient
# 2.10.3 get an explicit upgrade refusal from omopDpStatusDS. Helpers in
# helper-tools-release.R.

.tv_dataset <- list(resource_identity = "typed-views",
                    target_dialect = "postgresql", cdm_schema = "main",
                    vocabulary_schema = "main", results_schema = "main")

.tv_table <- function(d, domains = NULL) {
  .dsomopDpSealPersonLocal(.testPseudonymize(d), producer = "test/typed-views",
                           dataset_identity = .tv_dataset, domains = domains)
}

.tv_release <- function(x, spec) suppressMessages(omopDpReleaseDS(x, spec))

# The projected complete basis of a payload, by the payload's own rule.
.tv_projected <- function(p) {
  b <- p$components$basis
  lead <- b$families[[1L]] %in% c("N", "T")
  total <- if (lead) b$signed[[1L]] else NA_real_
  cells <- if (lead) b$signed[-1L] else b$signed
  .dsomopTypedProject(p$components$projection, total, cells)
}

# A person table: labels per person over several records; NA labels and a
# label outside the registered domain.
.tv_labels <- function() {
  set.seed(7)
  n <- 1200L
  person <- rep(seq_len(n), each = 2L)
  label <- sample(c("a", "b", "c", "x", NA), length(person), replace = TRUE,
                  prob = c(.35, .3, .15, .1, .1))
  data.frame(person_id = person, label = label,
             day = rep(c(1L, 2L), n), stringsAsFactors = FALSE)
}

test_that("binary rates are mask views of one projected complete basis (R04)", {
  .release_state()
  x <- .tv_table(.tv_labels(), domains = list(label = c("a", "b", "c")))
  for (reducer in c("any", "all", "first", "last")) {
    for (denominator in c("all_persons", "nonmissing")) {
      spec <- list(statistic = "binary_rate", variable = "label",
                   positive = c("b", "a"), reducer = reducer,
                   denominator = denominator)
      if (reducer %in% c("first", "last")) spec$order_by <- "day"
      p <- .tv_release(x, spec)
      info <- paste(reducer, denominator)
      comps <- p$components
      expect_identical(comps$view_kind, "binary_rate_numerator_denominator",
                       info = info)
      expect_identical(comps$projection, "typed-partition-simplex-v1",
                       info = info)
      view <- comps$view_of_basis
      expect_length(view, 2L)
      h <- .tv_projected(p)
      # Displayed values are the mask sums of the one projected basis.
      expect_equal(p$noisy_numerator, sum(h[view[[1L]]]), tolerance = 1e-9,
                   info = info)
      expect_equal(p$noisy_denominator, sum(h[view[[2L]]]), tolerance = 1e-9,
                   info = info)
      # Exported view cells are the signed mask sums.
      b <- comps$basis$signed[-1L]
      expect_equal(comps$signed[-1L], c(sum(b[view[[1L]]]), sum(b[view[[2L]]])),
                   tolerance = 1e-9, info = info)
      # The numerator mask is inside the denominator mask.
      expect_true(all(view[[1L]] %in% view[[2L]]), info = info)
      ncells <- length(b)
      if (denominator == "all_persons") {
        expect_identical(view[[2L]], seq_len(ncells), info = info)
      } else {
        expect_length(view[[2L]], ncells - 1L)
      }
      # Positive-mask order and duplicates are the same view.
      perm <- utils::modifyList(spec, list(positive = c("a", "b", "a")))
      expect_identical(.tv_release(x, perm)$noisy_numerator,
                       p$noisy_numerator, info = info)
    }
  }
  # One-label nonmissing excludes Unknown (no label) but keeps Other (a label
  # outside the registered domain).
  p <- .tv_release(x, list(statistic = "binary_rate", variable = "label",
                           positive = "a", reducer = "first",
                           order_by = "day", denominator = "nonmissing"))
  keys <- p$components$basis$coordinates[-1L]
  den <- keys[p$components$view_of_basis[[2L]]]
  expect_true("stratum=root|other" %in% den)
  expect_false("stratum=root|unknown" %in% den)
})

test_that("masks never enter noise identity; the domain does (R04)", {
  .release_state()
  x <- .tv_table(.tv_labels(), domains = list(label = c("a", "b", "c")))
  spec <- list(statistic = "binary_rate", variable = "label", positive = "a",
               reducer = "any")
  a <- .tv_release(x, spec)
  b <- .tv_release(x, utils::modifyList(spec, list(positive = c("b", "c"))))
  nm <- .tv_release(x, utils::modifyList(spec, list(denominator = "nonmissing")))
  # Different positive sets and denominators: the same basis draws.
  expect_identical(b$components$basis$signed, a$components$basis$signed)
  expect_identical(nm$components$basis$signed, a$components$basis$signed)
  expect_false(identical(b$components$view_of_basis[[1L]],
                         a$components$view_of_basis[[1L]]))
  # Equal-width registered domains with different labels are different bases.
  d <- data.frame(person_id = 1:400, label = rep("b", 400))
  ab <- .tv_release(.tv_table(d, list(label = c("a", "b"))),
                    list(statistic = "binary_rate", variable = "label",
                         positive = "b", reducer = "any"))
  bz <- .tv_release(.tv_table(d, list(label = c("b", "z"))),
                    list(statistic = "binary_rate", variable = "label",
                         positive = "b", reducer = "any"))
  expect_false(identical(ab$components$basis$id, bz$components$basis$id))
  expect_false(identical(ab$components$owner, bz$components$owner))
  expect_identical(ab$components$basis$domain, c("a", "b"))
  expect_identical(bz$components$basis$domain, c("b", "z"))
})

test_that("weighted numeric views derive from one projection with a protected total (R05)", {
  .release_state()
  # Astra's fixture: 2,000 people, one record each, 500 at 0 and 1,500 at 100.
  x <- .tv_table(data.frame(person_id = 1:2000,
                            measurement = c(rep(0, 500), rep(100, 1500))))
  spec <- list(statistic = "numeric_histogram", variable = "measurement",
               breaks = c(-1, .01, .05, 200), reducer = "records",
               max_contributions = 1)
  p <- .tv_release(x, spec)
  comps <- p$components
  expect_identical(comps$projection, "typed-weighted-total-simplex-v1")
  expect_identical(comps$basis$families[[1L]], "T")
  expect_identical(comps$basis$coordinates[[1L]], "stratum=root|records")
  # Atoms are reconciled to the protected total, never to a person total.
  expect_equal(sum(comps$basis$signed[-1L]), comps$basis$signed[[1L]],
               tolerance = 1e-9)
  expect_false("N" %in% comps$basis$families)
  h <- .tv_projected(p)
  expect_equal(p$counts, vapply(comps$view_of_basis, function(i) sum(h[i]),
                                numeric(1L)), tolerance = 1e-9)
  expect_equal(sum(p$counts), max(0, comps$basis$signed[[1L]]),
               tolerance = 1e-9)
  message("[typed-views] weighted numeric fixture (truth 500, 0, 1500): ",
          paste(round(p$counts, 1), collapse = ", "), "; protected total ",
          round(comps$basis$signed[[1L]], 1))
  # Coarse equals fine recombined, signed and displayed: [-inf, .05) is the
  # union of the first two fine bins.
  coarse <- .tv_release(x, utils::modifyList(spec, list(
    breaks = c(-1, .05, 200))))
  expect_identical(coarse$components$basis$signed, comps$basis$signed)
  expect_equal(coarse$counts[[1L]], p$counts[[1L]] + p$counts[[2L]],
               tolerance = 1e-9)
  expect_equal(coarse$counts[[2L]], p$counts[[3L]], tolerance = 1e-9)
  expect_equal(coarse$components$signed[[2L]],
               comps$signed[[2L]] + comps$signed[[3L]], tolerance = 1e-9)
  # Categorical weighted views are single cells clipped one by one.
  y <- .tv_table(.tv_labels(), domains = list(label = c("a", "b", "c")))
  w <- .tv_release(y, list(statistic = "categorical_histogram",
                           variable = "label", levels = c("a", "b"),
                           reducer = "records", max_contributions = 2,
                           order_by = "day"))
  expect_identical(w$components$projection, "typed-weighted-atomwise-clip-v1")
  hw <- .tv_projected(w)
  expect_equal(w$counts, hw[unlist(w$components$view_of_basis)],
               tolerance = 1e-9)
})

test_that("clients older than dsOMOPClient 2.10.3 get an explicit upgrade refusal (R08)", {
  .release_state()
  expect_error(omopDpStatusDS(), "Upgrade dsOMOPClient to 2.10.3")
  expect_error(omopDpStatusDS("dsomop-dp-release-v3"),
               "Upgrade dsOMOPClient to 2.10.3")
  status <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)
  expect_identical(status$release_protocol, .DSOMOP_RELEASE_PROTOCOL)
  # A future client listing several protocols (DataSHIELD-encoded).
  json <- as.character(jsonlite::toJSON(
    list("dsomop-protected-release-v2", .DSOMOP_RELEASE_PROTOCOL),
    auto_unbox = TRUE))
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
  encoded <- paste0("B64:", gsub("=+$", "", gsub("/", "_", gsub("\\+", "-",
                                                                b64))))
  expect_identical(omopDpStatusDS(encoded)$release_protocol,
                   .DSOMOP_RELEASE_PROTOCOL)
  # The community-tool gate still comes first.
  withr::local_options(list(dsomop.tools.enabled = FALSE))
  expect_error(omopDpStatusDS(), "community tools")
})

test_that("the real dsOMOPClient 2.10.2 status call shows the server's upgrade message", {
  .release_state()
  fixture <- function(name) {
    for (path in c(file.path("..", "fixtures", "dsomopclient-2.10.2", name),
                   file.path("tests", "fixtures", "dsomopclient-2.10.2",
                             name))) {
      if (file.exists(path)) return(path)
    }
    skip("dsOMOPClient 2.10.2 fixture not found")
  }
  testthat::skip_if_not_installed("DSI")
  old <- new.env(parent = globalenv())
  old$`%||%` <- function(x, y) if (is.null(x)) y else x
  sys.source(fixture("session.R"), envir = old)
  sys.source(fixture("privacy.R"), envir = old)
  server <- environment(omopDpStatusDS)
  # A DSI transport that evaluates the old client's call on this server.
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, error = NULL, ...) {
      site <- names(conns)[[1L]]
      value <- tryCatch(eval(expr, envir = server), error = function(e) {
        if (is.function(error)) error(site, conditionMessage(e))
        NULL
      })
      if (is.null(value)) return(list())
      stats::setNames(list(value), site)
    }, .package = "DSI")
  message <- tryCatch({
    old$ds.omop.dp.status(datasources = list(newserver = "FAKE"))
    NA_character_
  }, error = conditionMessage)
  message("[typed-views] dsOMOPClient 2.10.2 -> this server: ", message)
  expect_match(message, "Upgrade dsOMOPClient to 2.10.3", fixed = TRUE)
  expect_match(message, "newserver", fixed = TRUE)
  expect_false(grepl("incoherent v3 DP contract field", message, fixed = TRUE))
})
