#!/usr/bin/env Rscript
# Rebuild the final combined summaries from retained raw run directories.
# Run from the dsOMOP source root after all final utility runs finish:
# Rscript inst/utility/p0r3-survival-report.R [results-directory]
# Pre-repair interaction runs and the one-replicate crypto smoke are excluded.
args <- commandArgs(TRUE)
root <- if (length(args)) args[[1L]] else "inst/utility/results"
runs <- c("cox", "cox-limits", "sccs", "sccs-limits",
          "interaction-2000-final", "interaction-10000-final")
files <- file.path(root, runs, "p0r3-survival-summary.csv")
stopifnot(all(file.exists(files)))
summary <- do.call(rbind, lapply(files, read.csv))
summary <- summary[order(summary$scenario, summary$level), ]
stopifnot(setequal(summary$scenario, 1:12), nrow(summary) == 16L,
          all(summary$replicates == 100L))
write.csv(summary, file.path(root, "p0r3-survival-summary.csv"), row.names = FALSE)

# Early baseline raw files precede the additional diagnostic columns. Recreate
# just the seeded synthetic exit times (no release/noise/refit) for every Cox
# scenario so realized administrative censoring is comparable across runs.
raw <- lapply(file.path(root, runs, "p0r3-survival-raw.csv"), read.csv)
cox <- do.call(rbind, lapply(raw, function(d) {
  unique(d[d$model %in% c("cox", "interaction_cox"),
           c("scenario", "n", "replicate", "seed")])
}))
cox <- cox[order(cox$scenario, cox$replicate), ]
censoring <- do.call(rbind, lapply(seq_len(nrow(cox)), function(i) {
  d <- cox[i, ]
  horizon <- if (d$scenario == 10L) 7 else 366
  baseline <- if (d$scenario == 9L) 7300 else 730
  set.seed(d$seed)
  duration <- rexp(2L * d$n, rate = rep(c(1.5, 1), each = d$n) / baseline)
  cbind(d, horizon_days = horizon, baseline_event_days = baseline,
        target_censoring = mean(duration[seq_len(d$n)] >= horizon),
        comparator_censoring = mean(duration[d$n + seq_len(d$n)] >= horizon))
}))
write.csv(censoring, file.path(root, "p0r3-censoring-raw.csv"), row.names = FALSE)
censor_summary <- aggregate(cbind(target_censoring, comparator_censoring) ~
                              scenario + n + horizon_days + baseline_event_days,
                            censoring, mean)
censor_summary <- censor_summary[order(censor_summary$scenario), ]
write.csv(censor_summary, file.path(root, "p0r3-censoring-summary.csv"), row.names = FALSE)

# Per-family diagnostics occur on each subgroup row; count each replicate once.
interaction <- do.call(rbind, lapply(tail(raw, 2L), function(d) {
  d[!duplicated(d$replicate), c("scenario", "n", "replicate", "seed",
    "survival_source_sha256", "margin_projection_calls", "margin_repairs_nonzero",
    "margin_repairs_above_1e_minus8_relative", "margin_error_max_relative",
    "margin_initial_error_max_relative")]
}))
write.csv(interaction, file.path(root, "p0r3-c8-margin-diagnostics.csv"), row.names = FALSE)
repair_summary <- do.call(rbind, lapply(split(interaction, interaction$scenario), function(d) {
  data.frame(scenario = d$scenario[[1L]], n = d$n[[1L]], replicates = nrow(d),
    projection_calls = sum(d$margin_projection_calls),
    repairs_nonzero = sum(d$margin_repairs_nonzero),
    repairs_above_1e_minus8_relative = sum(d$margin_repairs_above_1e_minus8_relative),
    replicates_with_material_repair = sum(d$margin_repairs_above_1e_minus8_relative > 0),
    releases_with_material_repair = sum(d$margin_initial_error_max_relative > 1e-8),
    max_error_relative = max(d$margin_error_max_relative),
    max_initial_error_relative = max(d$margin_initial_error_max_relative))
}))
write.csv(repair_summary, file.path(root, "p0r3-c8-margin-summary.csv"), row.names = FALSE)

# Preserve paired changes against exactly the same synthetic seeds. The final
# seed for deterministic refits also includes the repaired released basis, so
# tiny basis changes can alter refit SE even when the point estimate is stable.
pre_files <- file.path(root, "pre-c8-margin-repair",
                       c("interaction-2000", "interaction-10000"),
                       "p0r3-survival-raw.csv")
stopifnot(all(file.exists(pre_files)))
pre <- do.call(rbind, lapply(pre_files, read.csv))
fields <- c("scenario", "n", "replicate", "seed", "level", "log_estimate", "se",
            "finite", "informative", "covered")
paired <- merge(pre[fields], do.call(rbind, tail(raw, 2L))[fields],
                by = c("scenario", "n", "replicate", "seed", "level"),
                suffixes = c("_pre", "_final"))
stopifnot(nrow(paired) == 600L)
paired$log_estimate_change <- paired$log_estimate_final - paired$log_estimate_pre
paired$se_change <- paired$se_final - paired$se_pre
write.csv(paired, file.path(root, "p0r3-c8-paired-raw.csv"), row.names = FALSE)
paired_summary <- do.call(rbind, lapply(split(paired, paired$scenario), function(d) {
  data.frame(scenario = d$scenario[[1L]], n = d$n[[1L]], fits = nrow(d),
    max_abs_log_estimate_change = max(abs(d$log_estimate_change)),
    mean_log_estimate_change = mean(d$log_estimate_change),
    max_abs_se_change = max(abs(d$se_change)), mean_se_change = mean(d$se_change),
    finite_changes = sum(d$finite_pre != d$finite_final),
    informative_changes = sum(d$informative_pre != d$informative_final),
    coverage_changes = sum(d$covered_pre != d$covered_final))
}))
write.csv(paired_summary, file.path(root, "p0r3-c8-paired-summary.csv"), row.names = FALSE)
print(summary, row.names = FALSE)
print(censor_summary, row.names = FALSE)
print(repair_summary, row.names = FALSE)
print(paired_summary, row.names = FALSE)

# Hash the retained inputs and aggregation scripts. The executed survival
# source is separately recorded in every final interaction raw row above.
sha256 <- function(path) unclass(as.character(openssl::sha256(
  readBin(path, "raw", n = file.info(path)$size))))
manifest_paths <- c(file.path(root, runs, "p0r3-survival-raw.csv"),
                    file.path(root, runs, "p0r3-survival-summary.csv"),
                    file.path(root, tail(runs, 2L), "p0r3-survival-run.log"), pre_files,
                    file.path(root, "p0r3-final-interaction-source-sha256.txt"),
                    "inst/utility/p0r3-survival-utility.R",
                    "inst/utility/p0r3-survival-report.R")
manifest <- data.frame(path = manifest_paths,
                       sha256 = vapply(manifest_paths, sha256, character(1L)))
write.csv(manifest, file.path(root, "p0r3-aggregation-manifest.csv"), row.names = FALSE)
metadata <- data.frame(
  field = c("sampler", "reference_epsilon", "replicates_per_scenario",
            "scenarios", "finite_definition", "informative_definition",
            "bias_rmse_scale", "seed_formula", "executed_final_survival_sha256",
            "executed_source_manifest", "aggregation_R_version"),
  value = c("law: deterministic 28-bit inverse-geometric approximation", "0.1", "100",
            "1:12; pre-repair interaction and crypto smoke excluded",
            "finite log estimate, SE, positive finite 95% CI",
            "finite and CI upper/lower <= 4", "log; conditional on finite CI",
            "2026111200 + scenario*10000 + replicate",
            paste(unique(interaction$survival_source_sha256), collapse = ";"),
            "p0r3-final-interaction-source-sha256.txt (final interaction runs only)",
            R.version.string))
write.csv(metadata, file.path(root, "p0r3-aggregation-metadata.csv"), row.names = FALSE)
