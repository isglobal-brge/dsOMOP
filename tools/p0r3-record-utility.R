# Reproduce from the package root:
# Rscript tools/p0r3-record-utility.R tools/p0r3-evidence 30
# Synthetic data only. Actual HMAC/sampler and record builder, e=.1.
args <- commandArgs(TRUE)
outdir <- if (length(args)) args[[1]] else "tools/p0r3-evidence"
reps <- if (length(args) > 1) as.integer(args[[2]]) else 30L
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
source("tests/testthat/helper-db.R")
source("tests/testthat/helper-tools-release.R")
set.seed(20261112)
run <- function() {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h))
  rows <- list()
  for (n in c(100L, 2000L, 10000L)) {
    # Four repeated lognormal measurements per person, same capped input
    # across replicate roots. No custodian public scale: H remains usable.
    vals <- exp(rnorm(4L*n, log(30), .7))
    sub <- data.frame(person_id = rep(seq_len(n), each = 4L), stratum = "a",
                      stratum_order = 1, v = vals)
    owner <- .dsomopReleaseRecordOwner("utility/record-values/v1", "synthetic",
                                      function(s) NULL)
    for (i in seq_len(reps)) {
      # Independent public test keys only; production root never exported.
      root <- as.character(openssl::sha256(charToRaw(paste("p0r3-record", n, i))))
      options(dsomop.dp.noise_root = root)
      .release_restart()
      policy <- .omopToolsReleaseRoute(h,
        .omopToolsReleaseContracts()[["dsomop:fe.continuous"]],
        scoped = TRUE)$policy
      b <- .dsomopReleaseBuildRecordMoments(policy, "realised_sticky", owner, sub)
      q <- b$hist$a$quantiles
      qi <- if (is.null(q)) NULL else q[q$prob == .5, ]
      est <- if (is.null(qi)) NA_real_ else qi$value
      rows[[length(rows)+1L]] <- data.frame(
        n=n, replicate=i, epsilon=.1, true_records=length(vals),
        shown_persons=b$strata$n_display, shown_records=b$strata$r_display,
        histogram_total=sum(b$hist$a$bins$records),
        true_median=median(vals), median=est,
        quantile_available=is.finite(est),
        median_bin_contains_truth=if (is.null(qi)) FALSE else
          median(vals) >= qi$bin_lower && median(vals) <= qi$bin_upper,
        median_bin_censored=if (is.null(qi)) NA else qi$censored)
    }
  }
  raw <- do.call(rbind, rows)
  write.csv(raw, file.path(outdir,"record-utility-replicates.csv"), row.names=FALSE)
  summary <- do.call(rbind,lapply(split(raw,raw$n),function(x) {
    err <- x$median-x$true_median
    data.frame(n=x$n[[1]],replicates=nrow(x),epsilon=.1,
      available=mean(x$quantile_available),
      median_bias_original_units=mean(err,na.rm=TRUE),
      median_rmse_original_units=sqrt(mean(err^2,na.rm=TRUE)),
      histogram_total_rmse=sqrt(mean((x$histogram_total-x$true_records)^2)),
      max_mass_disagreement=max(abs(x$histogram_total-x$shown_records)),
      quantile_bin_contains_truth=mean(x$median_bin_contains_truth),
      censored_fraction=mean(x$median_bin_censored,na.rm=TRUE))
  }))
  write.csv(summary,file.path(outdir,"record-utility-summary.csv"),row.names=FALSE)
  print(summary,row.names=FALSE)
  writeLines(c("Seed: 20261112; test-root SHA256('p0r3-record n replicate').",
    "Data: 4 lognormal(log(30),.7) records/person; same data over replicate roots.",
    "Quantile-bin containment is descriptive, NOT confidence-interval coverage.",
    "Bias/RMSE condition on available medians and use original value units.",
    "This measures the actual heuristic builder, not joint DP or useful clinical calibration.",
    trimws(capture.output(sessionInfo()), which = "right")),file.path(outdir,"record-utility-metadata.txt"))
}
run()
