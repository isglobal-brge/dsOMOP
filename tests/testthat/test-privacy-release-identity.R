# dsOMOP 2.10.3: the noise of a protected component is keyed by its corpus,
# fixed profile, owner, family and coordinate, plus the custodial generation
# and population (fixed synopsis) or the secret fingerprint of the realised
# bounded contributions (realised sticky). Nothing else (request text,
# lineage cosmetics, a database version) enters the key. A small reference
# epsilon makes chance coincidences of two draws negligible.

.identity_policy <- function(...) {
  utils::modifyList(list(
    noise_root = list(key = as.raw(0:31)), domain = "test-domain",
    snapshot_id = "snapshot-1", privacy_epoch = 1,
    release_epsilon = 0.001, mechanism = .DSOMOP_DP_MECHANISM,
    sampler = .DSOMOP_DP_SAMPLER
  ), list(...))
}

.identity_component <- function(...) {
  utils::modifyList(list(
    owner = "test/owner/v1", population = "population-1", family = "H",
    coordinate = "stratum=root|cell=a", value = 100, allocation = 0.8,
    sensitivity = 1, fingerprint = strrep("ab", 32L)
  ), list(...))
}

.identity_draw <- function(protection, policy = .identity_policy(),
                           component = .identity_component()) {
  .dsomopReleaseNoisyComponent(policy, .dsomopReleaseKeys(policy), protection,
                               component)
}

test_that("fixed-synopsis noise binds the corpus, generation, owner and coordinate", {
  first <- .identity_draw("fixed_synopsis")
  expect_identical(.identity_draw("fixed_synopsis"), first)
  changed_policy <- list(domain = "other-domain", snapshot_id = "snapshot-2",
                         privacy_epoch = 2)
  for (field in names(changed_policy)) {
    policy <- do.call(.identity_policy, changed_policy[field])
    expect_false(identical(.identity_draw("fixed_synopsis", policy), first),
                 info = field)
  }
  changed <- list(owner = "test/other-owner/v1", population = "population-2",
                  family = "W", coordinate = "stratum=root|cell=b",
                  allocation = 0.4, sensitivity = 2)
  for (field in names(changed)) {
    component <- do.call(.identity_component, changed[field])
    draw <- .identity_draw("fixed_synopsis", component = component)
    expect_false(identical(draw - component$value, first - 100), info = field)
  }
  # The exact value is added to fixed noise: a changed value never redraws.
  expect_identical(.identity_draw("fixed_synopsis",
                                  component = .identity_component(value = 101)),
                   first + 1)
  # A realised fingerprint is no part of the fixed key.
  expect_identical(.identity_draw("fixed_synopsis", component =
                                    .identity_component(fingerprint = NULL)),
                   first)
  other_root <- .identity_policy(noise_root = list(key = as.raw(32:63)))
  expect_false(identical(.identity_draw("fixed_synopsis", other_root), first))
})

test_that("realised-sticky noise binds the contribution fingerprint, not the population or generation", {
  first <- .identity_draw("realised_sticky")
  expect_identical(.identity_draw("realised_sticky"), first)
  # Neither the custodial generation nor the population (lineage) re-seeds a
  # realised component.
  expect_identical(.identity_draw("realised_sticky",
                                  .identity_policy(snapshot_id = "snapshot-2")),
                   first)
  expect_identical(.identity_draw("realised_sticky", component =
                                    .identity_component(population = "p-2")),
                   first)
  changed <- list(fingerprint = strrep("cd", 32L), owner = "test/other/v1",
                  family = "W", coordinate = "stratum=root|cell=b")
  for (field in names(changed)) {
    component <- do.call(.identity_component, changed[field])
    expect_false(identical(.identity_draw("realised_sticky",
                                          component = component), first),
                 info = field)
  }
  for (field in c("domain", "privacy_epoch")) {
    policy <- do.call(.identity_policy, stats::setNames(
      list(if (field == "domain") "other-domain" else 2), field))
    expect_false(identical(.identity_draw("realised_sticky", policy), first),
                 info = field)
  }
  # A realised component must carry its fingerprint.
  expect_error(.identity_draw("realised_sticky", component =
                                .identity_component(fingerprint = NULL)),
               "lacks its contribution fingerprint")
  expect_error(.identity_draw("standard"), "Unknown protected release route")
})

test_that("release subkeys are domain-separated from each other and from provenance", {
  policy <- .identity_policy()
  keys <- .dsomopReleaseKeys(policy)
  provenance <- .dsomopDpSubkey(policy$noise_root$key, policy$domain,
                                "person-local-provenance/v1")
  all_keys <- c(keys, list(provenance = provenance))
  expect_length(unique(lapply(all_keys, as.character)), length(all_keys))
  expect_true(all(vapply(all_keys, function(k) is.raw(k) && length(k) == 32L,
                         logical(1L))))
  other <- .dsomopReleaseKeys(.identity_policy(domain = "other-domain"))
  for (name in names(keys)) {
    expect_false(identical(keys[[name]], other[[name]]), info = name)
  }
})
