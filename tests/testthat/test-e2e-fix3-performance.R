test_that("million-person builder retains exact output while reducing latency", {
  skip_if(Sys.getenv("DSOMOP_E2E_PROFILE") != "1", "opt-in million-person profile")
  .release_state()
  n <- 1000000L
  owner <- .omopReleaseBirthYearOwner()
  sub <- data.frame(person_id = as.character(seq_len(n)),
                    stratum = rep(owner$strata$keys[1:2], length.out = n),
                    cell = as.character(1930 + seq_len(n) %% 70))
  .dsomopDpEnsureRuntime()
  policy <- .dsomopDpPolicy()
  policy$data_generation <- strrep("a", 64)
  old <- new.env(parent = environment(.dsomopReleaseBuild))
  sys.source(test_path("../../tools/e2e-fix3/baseline-build.R"), old)
  sys.source(test_path("../../tools/e2e-fix3/baseline-sampler.R"), old)
  old$.dsomopReleaseNoisyComponent <- .dsomopReleaseNoisyComponent
  environment(old$.dsomopReleaseNoisyComponent) <- old
  for (name in ls(old, all.names = TRUE)) old[[name]] <- compiler::cmpfun(old[[name]])
  for (route in c("fixed_synopsis", "realised_sticky")) {
    before_time <- system.time(before <- old$.dsomopReleaseBuild(policy, route, owner, sub))
    after_time <- system.time(after <- .dsomopReleaseBuild(policy, route, owner, sub))
    cat("\nROUTE", route, "\nBASELINE\n"); print(before_time)
    cat("CURRENT\n"); print(after_time)
    warm_before <- system.time(before <- old$.dsomopReleaseBuild(policy, route, owner, sub))
    warm_after <- system.time(after <- .dsomopReleaseBuild(policy, route, owner, sub))
    cat("WARM BASELINE\n"); print(warm_before)
    cat("WARM CURRENT\n"); print(warm_after)
    expect_identical(after, before)
  }
})
