# dsOMOP 2.10.8: an automatic vocabulary list climbs the public hierarchy only
# as far as the DataSHIELD rule needs, and declares the descendants of those
# few containers instead of every concept of the observed kinds.

# With gap = TRUE, leaves have no group rows (separations 1 and 3 only), as
# when concept_ancestor holds part of a vocabulary, and 100 more concepts of
# the same kind lie outside the hierarchy.
.hier_handle <- function(gap = FALSE) {
  h <- create_test_handle()
  root <- 990000000L; groups <- 990000001:990000005
  subgroups <- 990000100L + 0:49
  leaves <- 990001000L + 0:4999
  ids <- c(root, groups, subgroups, leaves, if (gap) 990100000L + 1:100)
  DBI::dbWriteTable(h$conn, "concept", data.frame(concept_id = ids, concept_name = paste("H", ids),
    domain_id = "Condition", vocabulary_id = "Hier", concept_class_id = "Clinical Finding", standard_concept = "S",
    concept_code = as.character(ids), valid_start_date = "2000-01-01", valid_end_date = "2099-12-31",
    invalid_reason = NA_character_), append = TRUE)
  group_of_sub <- groups[(seq_along(subgroups) - 1L) %/% 10L + 1L]
  sub_of_leaf <- subgroups[(seq_along(leaves) - 1L) %/% 100L + 1L]
  ca <- rbind(
    data.frame(a = ids, d = ids, s = 0L),
    data.frame(a = root, d = c(groups, subgroups, leaves), s = c(rep(1L, 5L), rep(2L, 50L), rep(3L, 5000L))),
    data.frame(a = group_of_sub, d = subgroups, s = 1L),
    if (!gap) data.frame(a = group_of_sub[match(sub_of_leaf, subgroups)], d = leaves, s = 2L),
    data.frame(a = sub_of_leaf, d = leaves, s = 1L))
  ca <- ca[ca$d %in% c(root, groups, subgroups, leaves), , drop = FALSE]
  names(ca) <- c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation")
  ca$max_levels_of_separation <- ca$min_levels_of_separation
  DBI::dbWriteTable(h$conn, "concept_ancestor", ca, append = TRUE)
  h$blueprint <- NULL
  list(handle = h, root = root, groups = groups, subgroups = subgroups, leaves = leaves)
}

test_that("the hierarchy is climbed only as far as the rule needs", {
  x <- .hier_handle(); on.exit(cleanup_handle(x$handle), add = TRUE)
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33,
                            dsomop.auto_declared_levels_max = NULL))
  # 60 leaves in 6 subgroups: six containers, each with its 100 leaves.
  six <- as.character(x$leaves[c(1:10, 101:110, 201:210, 301:310, 401:410, 501:510)])
  out <- .conceptAutoDeclared(x$handle, six, 200)
  expect_length(out, 606L)
  expect_true(all(six %in% out))
  expect_setequal(grep("^subtree:", attr(out, "kinds"), value = TRUE), paste0("subtree:", x$subgroups[1:6]))
  # 60 leaves over 45 subgroups: the subgroups are too many, so groups hold them.
  spread <- as.character(c(x$leaves[(0:44) * 100L + 1L], x$leaves[(0:14) * 100L + 2L]))
  out <- .conceptAutoDeclared(x$handle, spread, 200)
  expect_true(all(spread %in% out))
  expect_lte(length(grep("^subtree:", attr(out, "kinds"))), 5L)
  expect_length(out, 5055L)
})

test_that("containers, kinds and leftovers follow the rule once, under the limit", {
  x <- .hier_handle(); on.exit(cleanup_handle(x$handle), add = TRUE)
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33,
                            dsomop.auto_declared_levels_max = NULL))
  six <- as.character(x$leaves[c(1:10, 101:110, 201:210, 301:310, 401:410, 501:510)])
  # 40 unknown values stay observed: with any container they exceed the rule.
  expect_length(.conceptAutoDeclared(x$handle, c(six, as.character(980000001:980000040)), 200), 0L)
  # Density applies to containers as to observed levels.
  expect_length(.conceptAutoDeclared(x$handle, six, 3), 0L)
  # A list past the limit is declared from no height (nor from the kinds).
  withr::local_options(list(dsomop.auto_declared_levels_max = 100))
  long <- .conceptAutoDeclared(x$handle, six, 200)
  expect_length(long, 0L)
  expect_true(isTRUE(attr(long, "too_long")))
})

test_that("a vocabulary without hierarchy keeps the kinds' list", {
  h <- create_test_handle(); on.exit(cleanup_handle(h), add = TRUE)
  ids <- 930001:930076
  DBI::dbWriteTable(h$conn, "concept", data.frame(concept_id = ids, concept_name = paste("F", ids),
    domain_id = "Condition", vocabulary_id = "Flat", concept_class_id = "Cause", standard_concept = "S",
    concept_code = as.character(ids), valid_start_date = "2000-01-01", valid_end_date = "2099-12-31",
    invalid_reason = NA_character_), append = TRUE)
  h$blueprint <- NULL
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33,
                            dsomop.auto_declared_levels_max = NULL))
  out <- .conceptAutoDeclared(h, as.character(ids[1:50]), 500)
  expect_setequal(out, as.character(ids))
  expect_false(any(startsWith(attr(out, "kinds"), "subtree:")))
})

test_that("a hierarchy that skips a separation climbs past the gap", {
  x <- .hier_handle(gap = TRUE); on.exit(cleanup_handle(x$handle), add = TRUE)
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33,
                            dsomop.auto_declared_levels_max = NULL))
  # 45 subgroups are too many; at separation 2 the leaves keep their subgroups
  # (no group rows), so the root holds them: its 5,056 concepts, not the 5,156
  # of the kind.
  spread <- as.character(c(x$leaves[(0:44) * 100L + 1L], x$leaves[(0:14) * 100L + 2L]))
  out <- .conceptAutoDeclared(x$handle, spread, 200)
  expect_true(all(spread %in% out))
  expect_identical(grep("^subtree:", attr(out, "kinds"), value = TRUE), paste0("subtree:", x$root))
  expect_length(out, 5056L)
  # Any failure of the climb falls back to the kinds' list.
  testthat::local_mocked_bindings(.conceptAdaptiveContainers = function(...) stop("unavailable"), .package = "dsOMOP")
  out <- .conceptAutoDeclared(x$handle, spread, 200)
  expect_length(out, 5156L)
  expect_false(any(startsWith(attr(out, "kinds"), "subtree:")))
})
