test_that("public request identities bind every public release contract field", {
  policy <- list(
    keys = list(query = as.raw(0:31)), domain = "test-domain",
    policy_hash = "public-policy", snapshot_id = "snapshot-1", privacy_epoch = 1
  )
  semantic <- list(
    protocol = .DSOMOP_DP_PROTOCOL, mechanism = .DSOMOP_DP_MECHANISM,
    privacy_contract = .DSOMOP_DP_CONTRACT, adjacency = "add_remove_person",
    sensitivity = list(l1 = 1, unit = "person"),
    statistic = list(statistic = "count", lineage_id = "audited-lineage")
  )
  first <- .dsomopDpRequestId(policy, semantic)
  expect_identical(first, .dsomopDpRequestId(policy, rev(semantic)))
  for (field in c("domain", "policy_hash", "snapshot_id", "privacy_epoch")) {
    changed <- policy
    changed[[field]] <- if (field == "privacy_epoch") 2 else "other-public-value"
    expect_false(identical(first, .dsomopDpRequestId(changed, semantic)),
                 info = field)
  }
  for (field in c("protocol", "mechanism", "privacy_contract", "adjacency")) {
    changed <- semantic
    changed[[field]] <- "other-public-contract"
    expect_false(identical(first, .dsomopDpRequestId(policy, changed)),
                 info = field)
  }
  changed <- semantic
  changed$sensitivity$l1 <- 2
  expect_false(identical(first, .dsomopDpRequestId(policy, changed)))
  changed <- semantic
  changed$statistic$lineage_id <- "another-admitted-predicate"
  expect_false(identical(first, .dsomopDpRequestId(policy, changed)))
})

test_that("public response reservations cover escaped labels and histogram shapes", {
  # Exercise the public upper allowance with long multibyte labels, quotes,
  # backslashes, large coordinates and both scalar and vector JSON layouts.
  for (size in c(1L, 1000L)) {
    levels <- paste0(seq_len(size), strrep('\u00e9"\\', 50L))
    semantic <- list(
      protocol = .DSOMOP_DP_PROTOCOL, mechanism = .DSOMOP_DP_MECHANISM,
      privacy_contract = .DSOMOP_DP_CONTRACT, adjacency = "add_remove_person",
      sensitivity = list(l1 = 10, unit = "person"),
      statistic = list(statistic = "categorical_histogram", levels = levels)
    )
    response <- jsonlite::fromJSON(.dsomopDpCanonicalJson(list(
      statistic = "categorical_histogram", levels = levels,
      counts = rep(2^53 - 1, size), reducer = "presence",
      max_contributions = 10, value_type = "character",
      protocol = .DSOMOP_DP_PROTOCOL, mechanism = .DSOMOP_DP_MECHANISM,
      privacy_contract = .DSOMOP_DP_CONTRACT, adjacency = "add_remove_person",
      sensitivity = semantic$sensitivity, epsilon = 0.1, delta = 0,
      sticky = TRUE, sampler = .DSOMOP_DP_SAMPLER
    )), simplifyVector = TRUE)
    reservation <- .dsomopDpResponseReservation(semantic)
    expect_lte(length(serialize(response, NULL, version = 3)), reservation - 8192)
    equivalent <- rev(semantic)
    equivalent$sensitivity$l1 <- 10L
    expect_identical(reservation, .dsomopDpResponseReservation(equivalent))
  }
})
