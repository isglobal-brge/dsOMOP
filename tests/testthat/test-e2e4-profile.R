test_that("million-person complete releases match the pre-acceleration builder", {
  skip_if(Sys.getenv("DSOMOP_E2E_PROFILE") != "1", "opt-in profile")
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
  sys.source(test_path("../../tools/e2e4-fix1/baseline-hashing.R"), old)
  old$.dsomopReleaseBuild <- .dsomopReleaseBuild
  environment(old$.dsomopReleaseBuild) <- old
  for (name in ls(old, all.names = TRUE)) old[[name]] <- compiler::cmpfun(old[[name]])
  for (route in c("fixed_synopsis", "realised_sticky")) {
    for (iteration in 1:2) {
      before <- system.time(expected <- old$.dsomopReleaseBuild(policy, route, owner, sub))
      after <- system.time(actual <- .dsomopReleaseBuild(policy, route, owner, sub))
      cat("\nROUTE", route, "ITERATION", iteration, "\nPORTABLE\n"); print(before)
      cat("ACCELERATED\n"); print(after)
      expect_identical(actual, expected)
    }
  }
})
