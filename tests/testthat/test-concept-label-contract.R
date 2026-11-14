test_that("A27 maps validate exact IDs and preserve free text", {
  expect_identical(.conceptMap(c('2' = ' yes ', '0' = 'no'), 'labels'), c('0' = 'no', '2' = ' yes '))
  for (bad in list(c('01' = 'a'), c('-1' = 'a'), c('2147483648' = 'a'),
                   c('1' = 'a', '1' = 'b'), c('1' = ' '), c('1' = NA_character_), 1L)) {
    expect_error(.conceptMap(bad, 'labels'), 'labels')
  }
  whitespace <- c('', ' ', '\t\r\n', '\f', '\v', '\u0085', '\u00a0',
                  '\u1680', '\u2000', '\u2003', '\u2028', '\u2029', '\u202f', '\u205f', '\u3000')
  for (label in whitespace) {
    expect_error(.conceptMap(setNames(label, '1'), 'labels'), 'nonempty UTF-8')
    expect_error(.conceptMap(setNames(list(label), '1'), 'labels', wire = TRUE), 'nonempty UTF-8')
    valid <- paste0(label, 'yes', label)
    expect_identical(unname(.conceptMap(setNames(valid, '1'), 'labels')), valid)
  }
  expect_error(.conceptMap(list('x'), 'labels', wire = TRUE), 'object')
  expect_identical(.conceptMap(setNames(list('yes'), '1'), 'labels', wire = TRUE), c('1' = 'yes'))
  for (bad in c('if', 'two words', '1name', '')) {
    expect_error(.conceptMap(c(key = bad), 'column_names', TRUE), 'column_names')
  }
  expect_identical(.conceptMapOverlay(c('1' = 'recipe', '2' = 'other'), c('1' = 'variable')),
                   c('1' = 'variable', '2' = 'other'))
})

.cl_record <- function(id, name = NULL, version = '2026-01-01', vocabulary = 'SNOMED',
                       origin = 'vocabulary', label = name) {
  list(id = id, name = name, version = version, vocabulary = vocabulary, origin = origin, label = label)
}

test_that("A27 aligns identity and uses only comparable vocabulary versions", {
  old <- .cl_record('1', 'Old', '2025-01-01')
  new <- .cl_record('1', 'New', '2026-01-01')
  missing <- .cl_record('1')
  for (rows in list(list(old, new, missing), list(missing, new, old))) {
    expect_identical(.conceptResolve(rows)$levels, 'New')
  }
  expect_identical(.conceptChooseName(list(.cl_record('1', 'z', '1.10'), .cl_record('1', 'a', '1.9')))$name, 'z')
  expect_identical(.conceptChooseName(list(.cl_record('1', 'z', 'opaque'), new))$name, 'New')
  expect_match(.conceptChooseName(list(.cl_record('1', 'z', 'opaque'), new))$rule, 'unresolved')
  expect_null(.conceptVersion('2026-02-30'))
  expect_identical(.conceptResolve(list(.cl_record('45000123')))$levels, '45000123')
})

test_that("A27 explicit merges never merge unrelated vocabulary identities", {
  rows <- list(.cl_record('10', 'Same'), .cl_record('2', 'Same'),
               .cl_record('3', 'Same [2]'), .cl_record('0', 'No matching concept'))
  result <- .conceptResolve(rows)
  expect_identical(result$ids, c('0', '2', '3', '10'))
  expect_identical(result$labels, c('No matching concept', 'Same [2] [2]', 'Same [2] [3]', 'Same [10]'))
  user <- list(.cl_record('1', origin = 'recipe', label = 'yes'),
               .cl_record('2', origin = 'variable', label = 'yes'), .cl_record('3', 'yes'))
  expect_identical(.conceptResolve(user)$levels, c('yes', 'yes [3]'))
})

test_that("A27 generated names preserve explicit names and resolve collisions", {
  expect_identical(.conceptNameToken(c('Education Level', '123', '!!!', 'if')), c('education_level', 'concept_123', 'concept', 'concept_if'))
  keys <- c('education_1', 'education_2', 'person_id')
  auto <- c(education_1 = 'education_yes', education_2 = 'education_yes')
  ids <- c(education_1 = '1', education_2 = '2')
  expect_identical(.conceptFinalNames(keys, auto, character(), ids), c('education_yes_1', 'education_yes_2', 'person_id'))
  expect_error(.conceptFinalNames(keys, auto, c(education_1 = 'person_id'), ids), 'explicit name')
  expect_identical(.conceptFinalNames(keys, auto, c(dormant = 'person_id'), ids), c('education_yes_1', 'education_yes_2', 'person_id'))
})
