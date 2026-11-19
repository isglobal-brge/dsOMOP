# dsOMOP 2.10.8: observed levels are weighed against distinct person/value
# observations. A person's repeated value counts once; a wide output (one row
# per person) and a frame without a person key count their rows.

test_that("a person's repeated value counts once", {
  wide <- data.frame(person_id = 1:6, x = c("a", "b", "b", NA, "c", "c"))
  expect_identical(.densityObservations(wide, wide$x), 6)
  stacked <- rbind(wide, wide, wide)
  expect_identical(.densityObservations(stacked, stacked$x), 6)
  # (1, a); (2, a), (2, b); (3, NA), (3, c).
  long <- data.frame(person_id = rep(1:3, each = 4L),
                     x = c(rep("a", 4L), "a", "b", "a", "b", NA, NA, "c", "c"))
  expect_identical(.densityObservations(long, long$x), 5)
  expect_identical(.densityObservations(long, factor(long$x)), 5)
  expect_identical(.densityObservations(data.frame(x = c("a", "a")), c("a", "a")), 2)
})

test_that("counts over chunks keep only what the rule needs", {
  withr::local_options(list(nfilter.levels.max = 4, nfilter.levels.density = 0.5))
  expect_identical(.densityBound(), 9)
  keys <- .densityKeys(1:5, rep("a", 5L))
  keys <- .densityKeys(1:5, rep("a", 5L), keys)
  expect_length(keys, 5L)
  expect_length(.densityKeys(c(1L, 1L), c(NA, NA), keys), 6L)
  expect_length(.densityKeys(6:100, rep("a", 95L), keys), 9L)
})

test_that("level reports weigh a repeated person value once", {
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33))
  # Ten people with ten different values: refused by density, however often
  # each person's row is repeated.
  wide <- data.frame(person_id = 1:10, concept_id = as.character(100 + 1:10))
  expect_false(.omopConceptLevelAssessment(wide, "concept_id")$safe)
  repeated <- wide[rep(seq_len(10L), 20L), , drop = FALSE]
  expect_false(.omopConceptLevelAssessment(repeated, "concept_id")$safe)
  # Different values of the same people are observations of their own.
  events <- data.frame(person_id = rep(1:10, each = 20L), concept_id = as.character(100 + rep(1:10, 20L)))
  expect_true(.omopConceptLevelAssessment(events, "concept_id")$safe)
})
