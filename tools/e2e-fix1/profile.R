# Targeted pure-mechanism profile; run on the test pod after rtest.sh synchronizes it.
# No clinical data, no cross-request memo, the production crypto remains enabled.
pkgload::load_all('.', quiet = TRUE)
source('tests/testthat/helper-tools-release.R')
local({
  .release_state()
  n <- as.integer(Sys.getenv('E2E_PROFILE_N', '1000000'))
  sub <- data.frame(person_id = as.character(seq_len(n)),
                    stratum = rep(c('gender=8507', 'gender=8532'), length.out = n),
                    cell = as.character(1930 + seq_len(n) %% 70))
  owner <- .omopReleaseBirthYearOwner()
  print(owner$strata)
  sub$stratum <- rep(owner$strata$keys[1:2], length.out = n)
  .dsomopDpEnsureRuntime()
  policy <- .dsomopDpPolicy()
  policy$data_generation <- paste(rep('a', 64), collapse = '')
  Rprof('/tmp/e2e-fix1-profile.out', interval = 0.01)
  timing <- system.time(value <- .dsomopReleaseBuild(policy, 'realised_sticky', owner, sub))
  Rprof(NULL)
  print(timing)
  print(head(summaryRprof('/tmp/e2e-fix1-profile.out')$by.self, 20))
  saveRDS(value, Sys.getenv('E2E_PROFILE_OUT', '/tmp/e2e-fix1-before.rds'))
})
