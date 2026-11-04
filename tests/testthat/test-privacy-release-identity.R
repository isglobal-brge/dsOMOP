test_that("release identities bind every public contract field and the private data version", {
  policy <- list(
    keys = list(query = as.raw(0:31)), release_protocol = .DSOMOP_DP_RELEASE_PROTOCOL,
    domain = "test-domain", policy_hash = "public-policy", privacy_epoch = 1,
    data_version = "private-data-version"
  )
  snapshot <- list(snapshot_id = "snapshot-1",
                   protected_fingerprint = "protected-statistic")
  semantic <- list(
    protocol = .DSOMOP_DP_PROTOCOL, mechanism = .DSOMOP_DP_MECHANISM,
    privacy_contract = .DSOMOP_DP_CONTRACT, adjacency = "add_remove_person",
    sensitivity = list(l1 = 1, unit = "person"),
    statistic = list(statistic = "count", lineage_id = "audited-lineage")
  )
  first <- .dsomopDpReleaseId(policy, semantic, snapshot)
  expect_match(first, "^[0-9a-f]{64}$")
  expect_identical(first, .dsomopDpReleaseId(policy, rev(semantic), rev(snapshot)))
  for (field in c("domain", "policy_hash", "privacy_epoch", "data_version")) {
    changed <- policy
    changed[[field]] <- if (field == "privacy_epoch") 2 else "other-value"
    expect_false(identical(first, .dsomopDpReleaseId(changed, semantic, snapshot)),
                 info = field)
  }
  for (field in c("snapshot_id", "protected_fingerprint")) {
    changed <- snapshot
    changed[[field]] <- "other-value"
    expect_false(identical(first, .dsomopDpReleaseId(policy, semantic, changed)),
                 info = field)
  }
  for (field in c("protocol", "mechanism", "privacy_contract", "adjacency")) {
    changed <- semantic
    changed[[field]] <- "other-public-contract"
    expect_false(identical(first, .dsomopDpReleaseId(policy, changed, snapshot)),
                 info = field)
  }
  changed <- semantic
  changed$sensitivity$l1 <- 2
  expect_false(identical(first, .dsomopDpReleaseId(policy, changed, snapshot)))
  changed <- semantic
  changed$statistic$lineage_id <- "another-admitted-predicate"
  expect_false(identical(first, .dsomopDpReleaseId(policy, changed, snapshot)))
  other_key <- policy
  other_key$keys$query <- as.raw(32:63)
  expect_false(identical(first, .dsomopDpReleaseId(other_key, semantic, snapshot)))
})
