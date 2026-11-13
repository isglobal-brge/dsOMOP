test_that("publication counts each component's own genuine persons exactly once", {
  ctx <- .publication_test_context(threshold = 5L)
  calls <- 0L
  candidate <- .publication_test_candidate()
  .publishPrepare(ctx, candidate, {
    calls <- calls + 1L
    data.frame(person_id = rep(1:5, each = 2L), value = 1:10)
  }, already_tokenized = TRUE)
  expect_identical(calls, 1L)
  status <- .publication_test_commit(ctx)
  expect_identical(status$status, "ok")
  expect_equal(nrow(get("out", ctx$target_env)), 10L)
  expect_identical(calls, 1L)
  expect_false(any(c("support", "n_persons", "count") %in% names(status)))
  expect_false(exists(ctx$id, .planContextRegistry, inherits = FALSE))

  ctx <- .publication_test_context(threshold = 5L)
  .publishPrepare(ctx, candidate, data.frame(person_id = rep(1L, 20L)),
                   already_tokenized = TRUE)
  status <- .publication_test_commit(ctx)
  expect_identical(status$status, "refused")
  expect_identical(status$rejected$out$code, "insufficient_support")
  expect_identical(ls(ctx$target_env), character(0))
})

test_that("roster lineage counts participants represented by component rows", {
  roster <- data.frame(rowId = 1:5, person_id = 101:105, set_id = 1L)
  ctx <- .publication_test_context(threshold = 5L)
  .planRegisterRoster(ctx, "matched", roster)
  candidate <- .publication_test_candidate(lineage = .lineage(
    "roster", "rowId", "matched", "rowId", "person_id"
  ))
  .publishPrepare(ctx, candidate, data.frame(rowId = 1L, set_id = 1L),
                   already_tokenized = TRUE)
  expect_identical(.publication_test_commit(ctx)$status, "refused")
  ctx <- .publication_test_context(threshold = 5L)
  .planRegisterRoster(ctx, "matched", roster)
  .publishPrepare(ctx, candidate, data.frame(rowId = 1:5, set_id = 1L),
                   already_tokenized = TRUE)
  expect_identical(.publication_test_commit(ctx)$status, "ok")
  expect_error(.publication_test_candidate(lineage = .lineage(
    "roster", "set_id", "matched", "set_id", "person_id"
  )), "set_id is never person lineage")
  expect_error(.publication_test_candidate(lineage = .lineage("direct", "set_id")),
               "set_id is never person lineage")
})

test_that("numeric roster keys agree with integer and CSV character forms", {
  numeric <- data.frame(rowId = c(100000, 100001, 999999, 1000000, 1000001))
  text <- data.frame(rowId = c("100000", "100001", "999999", "1000000", "1000001"))
  expect_identical(.publishKeys(numeric, "rowId"), .publishKeys(text, "rowId"))
  expect_identical(.publishKeys(transform(numeric, rowId = as.integer(rowId)), "rowId"),
                   .publishKeys(text, "rowId"))
  # One key's fractional digits must not pad another key's decimal form.
  expect_identical(.publishKeys(data.frame(cohort_row_id = c(100000, 100000.5)), "cohort_row_id"),
                   .publishKeys(data.frame(cohort_row_id = c("100000", "100000.5")), "cohort_row_id"))
})

test_that("BIGINT roster keys preserve distinct values above double precision", {
  skip_if_not_installed("bit64")
  text <- data.frame(rowId = c("100000", "1000000", "9007199254740992", "9007199254740993"))
  bigint <- data.frame(rowId = bit64::as.integer64(text$rowId))
  expect_identical(.publishKeys(bigint, "rowId"), .publishKeys(text, "rowId"))
  expect_identical(anyDuplicated(.publishKeys(bigint, "rowId")), 0L)
})

test_that("publication rejects every malformed mapping and locks roster namespaces", {
  scenarios <- list(
    orphan = list(roster = data.frame(rowId = 1:3, person_id = 11:13),
                  frame = data.frame(rowId = c(1L, 9L))),
    duplicate = list(roster = data.frame(rowId = c(1L, 1L), person_id = 11:12),
                     frame = data.frame(rowId = 1L)),
    missing_person = list(roster = data.frame(rowId = 1:3, person_id = c(11L, NA, 13L)),
                          frame = data.frame(rowId = 1L)),
    conflict = list(roster = data.frame(rowId = 1:3, person_id = 11:13),
                    frame = data.frame(rowId = 1L, person_id = 12L)),
    missing_key = list(roster = data.frame(rowId = 1:3, person_id = 11:13),
                       frame = data.frame(rowId = NA_integer_))
  )
  candidate <- .publication_test_candidate(lineage = .lineage(
    "roster", "rowId", "roster", "rowId", "person_id"
  ))
  for (scenario in scenarios) {
    ctx <- .publication_test_context()
    .planRegisterRoster(ctx, "roster", scenario$roster)
    error <- tryCatch(.publishPrepare(ctx, candidate, scenario$frame), error = identity)
    expect_s3_class(error, "dsomop_plan_rejection")
    expect_identical(error$code, "lineage_malformed")
  }
  ctx <- .publication_test_context()
  expect_error(.publishPrepare(ctx, candidate, data.frame(rowId = 1L)),
               "lineage_malformed")
  roster <- data.frame(rowId = 1:3, person_id = 11:13)
  .planRegisterRoster(ctx, "roster", roster)
  expect_silent(.planRegisterRoster(ctx, "roster", roster))
  expect_error(.planRegisterRoster(ctx, "roster", transform(roster, person_id = 21:23)),
               "lineage_malformed")
  expect_error(assign("roster", roster, ctx$rosters), "locked binding")
  direct <- .publication_test_candidate()
  expect_error(.publishPrepare(ctx, direct, data.frame(person_id = c(1L, NA))),
               "lineage_malformed")
  expect_error(.publishPrepare(ctx, direct, data.frame(person_id = c("", "2"))),
               "lineage_malformed")
  reference <- .publication_test_candidate(lineage = .lineage("reference"))
  expect_error(.publishPrepare(ctx, reference, data.frame(person_id = 1:5)),
               "lineage_malformed")
})

test_that("origin-qualified composite roster keys distinguish repeated episodes", {
  ctx <- .publication_test_context(threshold = 3L)
  roster <- data.frame(person_id = c(1L, 2L, 3L), cohort_row_id = rep(1L, 3L))
  .planRegisterRoster(ctx, "episodes", roster)
  candidate <- .publication_test_candidate(lineage = .lineage(
    "roster", c("person_id", "cohort_row_id"), "episodes",
    c("person_id", "cohort_row_id"), "person_id"
  ))
  .publishPrepare(ctx, candidate, roster, already_tokenized = TRUE)
  expect_identical(.publication_test_commit(ctx)$status, "ok")
  # Tuple encoding cannot confuse a delimiter inside either key with another
  # person's origin-qualified episode identity.
  frame <- data.frame(a = c("1", "1:2"), b = c("2:3", "3"))
  expect_false(anyDuplicated(.publishKeys(frame, c("a", "b"))) > 0L)
})

.publication_test_families <- function(ctx) {
  .publishPrepare(ctx, .publication_test_candidate("passing"),
                   data.frame(person_id = 1:5), already_tokenized = TRUE)
  .publishPrepare(ctx, .publication_test_candidate("small"),
                   data.frame(person_id = 1L), already_tokenized = TRUE)
  .publishPrepare(ctx, .publication_test_candidate(
    "small", "dictionary", "small.dictionary", .lineage("reference")
  ), data.frame(label = "public vocabulary"), already_tokenized = TRUE)
}

test_that("complete families obey strict, non-strict and all-or-none policy", {
  for (strict in c(TRUE, FALSE)) for (policy in c("server", "all_or_none")) {
    ctx <- .publication_test_context(threshold = 3L, strict = strict)
    ctx$commit_policy <- policy
    .publication_test_families(ctx)
    status <- .publication_test_commit(ctx)
    partial <- !strict && identical(policy, "server")
    expect_identical(status$status, if (partial) "partial" else "refused")
    expect_setequal(ls(ctx$target_env), if (partial) "passing" else character(0))
    expect_false("small" %in% names(status$published))
    expect_identical(status$rejected$small, list(
      code = "insufficient_support", message = .publishMessage("insufficient_support")
    ))
  }
  ctx <- .publication_test_context(threshold = 3L, strict = FALSE)
  .publication_test_families(ctx)
  ctx$rejected$technical <- list(code = "dialect_unsupported",
                                 message = .publishMessage("dialect_unsupported"))
  expect_identical(.publication_test_commit(ctx)$status, "refused")
  expect_identical(ls(ctx$target_env), character(0))
})

test_that("publication reserves fresh names against every occupied destination", {
  target <- new.env(parent = emptyenv())
  target$D <- "preexisting user object"
  target$D.1 <- "leftover"
  ctx <- .publication_test_context(target = target)
  for (component in c("first", "second")) {
    .publishPrepare(ctx, .publication_test_candidate(component = component, symbol = "D"),
                     data.frame(person_id = 1:5), already_tokenized = TRUE)
  }
  # Even a destination occupied after prepare must be reconsidered at commit.
  target$D.2 <- "late arrival"
  status <- .publication_test_commit(ctx)
  expect_identical(target$D, "preexisting user object")
  expect_identical(target$D.1, "leftover")
  expect_identical(target$D.2, "late arrival")
  published <- vapply(status$published$out, `[[`, character(1L), "symbol")
  expect_identical(length(unique(published)), 2L)
  expect_false(any(published %in% c("D", "D.1", "D.2")))
  expect_true(all(vapply(published, function(symbol) {
    is.data.frame(get(symbol, target))
  }, logical(1L))))
})

test_that("tokenisation, sealing and late assignment failures roll back owned symbols", {
  for (step in c("tokenize", "seal", "assign")) {
    target <- new.env(parent = emptyenv())
    target$untouched <- "original"
    ctx <- .publication_test_context(target = target)
    for (name in c("first", "second")) {
      .publishPrepare(ctx, .publication_test_candidate(name),
                       data.frame(person_id = 1:5), already_tokenized = TRUE)
    }
    hits <- 0L
    ctx$failure_hook <- function(at) {
      if (identical(at, step)) {
        hits <<- hits + 1L
        if (hits == 2L) stop("injected publication failure")
      }
    }
    expect_error(.publication_test_commit(ctx), "injected publication failure")
    expect_identical(ls(target), "untouched")
    expect_identical(target$untouched, "original")
    expect_true(ctx$closed)
    expect_false(exists(ctx$id, .planContextRegistry, inherits = FALSE))
  }
  ctx <- .publication_test_context()
  .publishPrepare(ctx, .publication_test_candidate(), data.frame(person_id = 1:5),
                   already_tokenized = TRUE)
  ctx$seal <- function(value, candidate) {
    expect_identical(ls(ctx$target_env), character(0))
    stop("injected DP seal failure")
  }
  expect_error(.publication_test_commit(ctx), "injected DP seal failure")
  expect_identical(ls(ctx$target_env), character(0))
})

test_that("staged move and manifest failures remove private and moved artefacts", {
  for (step in c("move", "manifest")) {
    ctx <- .publication_test_context()
    directory <- tempfile("publication-staging-")
    private <- file.path(directory, ".private", ctx$id)
    dir.create(private, recursive = TRUE)
    ctx$staging_dir <- directory
    ctx$private_dir <- private
    ctx$owned$directories <- directory
    path <- file.path(private, "out.csv")
    utils::write.csv(data.frame(person_id = 1:5, value = 1:5), path, row.names = FALSE)
    descriptor <- structure(list(
      dataset_id = "omop.plan.out",
      metadata = list(file = path, format = "csv", columns = c("person_id", "value"))
    ), class = "FlowerDatasetDescriptor")
    .publishPrepare(ctx, .publication_test_candidate(), descriptor,
                     already_tokenized = TRUE)
    ctx$failure_hook <- function(at) {
      if (identical(at, step)) stop("injected staged failure")
    }
    expect_error(.publication_test_commit(ctx), "injected staged failure")
    expect_false(dir.exists(directory))
    expect_identical(ls(ctx$target_env), character(0))
    expect_true(ctx$closed)
  }
})

test_that("zero-row patient components fail while reference-only outputs are exempt", {
  ctx <- .publication_test_context()
  .publishPrepare(ctx, .publication_test_candidate(), data.frame(person_id = integer(0)),
                   already_tokenized = TRUE)
  expect_identical(.publication_test_commit(ctx)$status, "refused")
  ctx <- .publication_test_context()
  .publishPrepare(ctx, .publication_test_candidate(lineage = .lineage("reference")),
                   data.frame(label = character(0)), already_tokenized = TRUE)
  expect_identical(.publication_test_commit(ctx)$status, "ok")
})


test_that("vectorised publication keys retain scalar-format tuples exactly", {
  columns <- list(
    as.integer(c(-2147483647, -1, 0, 1, 2147483647)),
    c(-2147483647, -1, 0, 1, 2147483647),
    c(1, 1.25, 100000, 100000.5, 1e-9, 1e16, -1e20, Inf, -Inf),
    rep(c(1.25, 100000, 100000.5, 1e16), 10))
  for (x in columns) {
    expected <- vapply(x, format, "", scientific = FALSE, trim = TRUE, digits = 15L)
    expect_identical(.publishKeys(data.frame(key = x), "key"), paste0(nchar(expected, type = "bytes"), ":", expected))
  }
})
