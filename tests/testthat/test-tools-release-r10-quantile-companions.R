# R10: nested quantile companions must describe the final common-parent
# histogram. Use public server/client endpoints and the production HMAC and
# sampler; only DSI transport is replaced for the one-site client run.

.r10_probs <- c(.1, .25, .5, .75, .9)
.r10_columns <- c("p10_value", "p25_value", "median_value", "p75_value",
                   "p90_value")

# Independent CDF inversion for these bounded public Achilles grids. In
# particular, a tail quantile is its finite edge with an infinite upper bin.
.r10_quantiles <- function(h, breaks) {
  cumulative <- cumsum(h)
  index <- vapply(.r10_probs, function(p) {
    which(cumulative >= (p - 1e-12) * sum(h))[[1L]]
  }, integer(1L))
  lo <- breaks[index]
  hi <- c(breaks[-1L], Inf)[index]
  value <- vapply(seq_along(index), function(i) {
    j <- index[[i]]
    if (is.infinite(hi[[i]])) return(lo[[i]])
    before <- if (j == 1L) 0 else cumulative[[j - 1L]]
    lo[[i]] + (.r10_probs[[i]] * sum(h) - before) / h[[j]] *
      (hi[[i]] - lo[[i]])
  }, numeric(1L))
  data.frame(prob = .r10_probs, value = signif(value, 12),
             bin_lower = lo, bin_upper = hi, open_lower = FALSE,
             open_upper = is.infinite(hi), censored = is.infinite(hi))
}

.r10_server_run <- function(h, id) {
  key <- "handle_r10_quantiles"
  assign(key, h, envir = .dsomop_env)
  on.exit(rm(list = key, envir = .dsomop_env), add = TRUE)
  suppressWarnings(suppressMessages(
    omopAnalysisRunDS("r10_quantiles", id)))
}

.r10_client_run <- function(frame, id) {
  env <- get(".dsomop_client_env", asNamespace("dsOMOPClient"))
  assign("r10_quantiles", list(conns = list(site = "TEST-DSI"),
    res_symbol = "r10_quantiles", server_versions = c(site = "2.10.3")),
    envir = env)
  on.exit(rm(list = "r10_quantiles", envir = env), add = TRUE)
  metadata <- list(name = id, mode = "aggregate",
    pooling_contract = list(version = 1L, strategy = "not_poolable",
                            reason = "Protected histogram components."),
    release_pooling = list(version = 1L, strategy = "protected_components"))
  testthat::local_mocked_bindings(datashield.aggregate = function(conns, expr, ...) {
    if (identical(as.character(expr[[1L]]), "omopAnalysisGetDS")) {
      return(list(site = metadata))
    }
    stopifnot(identical(as.character(expr[[1L]]), "omopAnalysisRunDS"))
    list(site = frame)
  }, .package = "DSI")
  dsOMOPClient::ds.omop.analysis.run(id, symbol = "r10_quantiles")
}

.r10_check <- function(out, id, client = NULL, parent_available = TRUE) {
  owner <- attr(out, "dsomop_release")$owners[[1L]]
  parent <- owner$parent_basis
  expect_true(is.list(parent), info = id)
  expect_identical(parent, attr(out, "dsomop_release")$owners[[2L]])
  keys <- vapply(owner$components, `[[`, character(1L), "stratum")
  signed_n <- vapply(owner$components, `[[`, numeric(1L), "n")
  n <- .dsomopReleaseSimplex(signed_n, max(0, sum(signed_n)))
  root <- parent$components[[1L]]
  root_h <- .dsomopReleaseSimplex(root$h, max(0, root$n))
  signed_h <- do.call(rbind, lapply(owner$components, `[[`, "h"))
  # Reconstruct the server's final transportation projection from the signed
  # public payload. Achilles 102 additionally exposes these cells directly.
  final_h <- .dsomopReleaseTransport(signed_h, n, root_h)
  expect_equal(rowSums(final_h), n, tolerance = 1e-7)
  expect_equal(colSums(final_h), root_h, tolerance = 1e-7)
  qi <- attr(out, "dsomop_protection")$components[[1L]]$quantile_intervals
  available <- parent_available & n >= max(2, getOption("dsomop.nfilter.dist"))
  expect_setequal(if (is.null(qi)) character() else unique(qi$stratum),
                  keys[available])
  if (!is.null(client)) {
    expect_identical(client$per_site$site, out)
    expect_identical(client$meta$protection$site,
                     attr(out, "dsomop_protection"))
    cells <- attr(client$pooled, "cells")
    rows <- client$pooled[client$pooled$owner == owner$owner, ]
    expect_equal(rows$persons[match(keys, rows$stratum)], n,
                 tolerance = 1e-7)
  }
  for (i in seq_along(keys)) {
    key <- keys[[i]]
    companion <- if (is.null(qi)) NULL else qi[qi$stratum == key, -1L,
                                               drop = FALSE]
    if (!is.null(companion)) rownames(companion) <- NULL
    if (available[[i]]) {
      expected <- .r10_quantiles(final_h[i, ], owner$breaks)
      # Exact bin/flag agreement plus the quantiles' final rounded values.
      expect_identical(companion[names(expected)[-2L]],
                       expected[names(expected)[-2L]], info = paste(id, key))
      expect_equal(companion$value, expected$value, tolerance = 1e-7,
                   info = paste(id, key))
    } else {
      expect_true(is.null(companion) || nrow(companion) == 0L,
                  info = paste(id, key, "unavailable"))
    }
    if (endsWith(id, ".102")) {
      genders <- .omopReleaseGenderStrata()$labels
      gender <- genders$gender_name[match(key, genders$stratum)]
      shown <- out[out$gender_name == gender, ]
      expect_equal(shown$count_value[match(owner$breaks, shown$age_band)],
                   as.numeric(final_h[i, ]), tolerance = 1e-7)
    } else {
      row_key <- if ("gender_concept_id" %in% names(out)) {
        paste0("gender=", out$gender_concept_id)
      } else ifelse(is.na(out$age_decile), "age_decile=unknown",
                     paste0("age_decile=", out$age_decile))
      displayed <- as.numeric(out[match(key, row_key), .r10_columns])
      expect_equal(out$count_value[match(key, row_key)], n[[i]],
                   tolerance = 1e-7)
      if (available[[i]]) {
        expect_equal(displayed, expected$value, tolerance = 1e-7)
        expect_equal(displayed, companion$value, tolerance = 1e-7)
      } else expect_true(all(is.na(displayed)))
    }
    if (!is.null(client)) {
      pooled_h <- cells[cells$owner == owner$owner & cells$stratum == key, ]
      pooled_h <- pooled_h$count[match(owner$cell_keys, pooled_h$cell)]
      expect_equal(pooled_h, as.numeric(final_h[i, ]), tolerance = 1e-7,
                   info = paste(id, key, "one-site cells"))
      if (available[[i]]) {
        expect_equal(.r10_quantiles(pooled_h, owner$breaks), companion,
                     tolerance = 1e-7,
                     info = paste(id, key, "one-site companion"))
      }
    }
  }
  invisible(qi)
}

test_that("R10 all nested Achilles endpoints export final histogram companions", {
  skip_if_not_installed("dsOMOPClient")
  .release_state()
  h <- create_test_handle(n_persons = 2000)
  on.exit(cleanup_handle(h), add = TRUE)
  for (id in paste0("dsomop:achilles.", c(102, 104, 106, 107))) {
    out <- .r10_server_run(h, id)
    qi <- .r10_check(out, id, .r10_client_run(out, id))
    expect_gt(nrow(qi), 0L)
    # The original witness has a supported UNKNOWN gender and unavailable
    # age-decile strata. These assertions prevent vacuous regression coverage.
    if (endsWith(id, ".104")) expect_true("gender=0" %in% qi$stratum)
    if (endsWith(id, ".107")) {
      expect_lt(length(unique(qi$stratum)), 13L)
      expect_true("age_decile=unknown" %in% qi$stratum)
    }
  }
})

test_that("R10 unknown and open-tail companions retain the final availability mask", {
  skip_if_not_installed("dsOMOPClient")
  .release_state()
  h <- create_test_handle(n_persons = 2400)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, paste0("UPDATE person SET gender_concept_id = CASE ",
    "person_id % 3 WHEN 0 THEN 0 WHEN 1 THEN 8507 ELSE 8532 END, ",
    "year_of_birth = CASE WHEN person_id % 7 = 0 THEN NULL ELSE 1800 END"))
  DBI::dbExecute(h$conn, paste0("UPDATE observation_period SET ",
    "observation_period_start_date = '2020-01-01', ",
    "observation_period_end_date = '2080-01-01'"))
  for (id in paste0("dsomop:achilles.", c(102, 104, 106, 107))) {
    out <- .r10_server_run(h, id)
    qi <- .r10_check(out, id, .r10_client_run(out, id))
    expect_true(any(qi$censored & qi$open_upper), info = id)
    unknown <- if (endsWith(id, ".107")) "age_decile=unknown" else "gender=0"
    expect_true(unknown %in% qi$stratum, info = id)
  }
  # The existing parent distribution gate can make every quantile unavailable
  # while the protected public histogram still exists. Never recreate an
  # interval from its cells when the existing availability mask is false.
  withr::local_options(dsomop.nfilter.dist = 1000000)
  for (id in paste0("dsomop:achilles.", c(102, 104, 106, 107))) {
    out <- .r10_server_run(h, id)
    qi <- .r10_check(out, id, .r10_client_run(out, id), parent_available = FALSE)
    expect_null(qi)
  }
})
