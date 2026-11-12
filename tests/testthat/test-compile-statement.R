# Amendment A-17: fragment SQL identifiers are allocated per execution context
# from the structured identity (prefix, name, suffix): deterministic for the
# plan, independent of compilation order, within the dialect's identifier
# limit and distinct under its identifier comparison.

.ws4_alloc <- function(ids, family = "postgresql") {
  .fragmentAllocate(lapply(ids, function(x) do.call(.fragmentIdentity, as.list(x))), family)
}

test_that("a final fragment and a stage never share an identifier", {
  ids <- list(list("ev", "a", "src"), list("ev", "a_src"), list("ev", "a"))
  alloc <- .ws4_alloc(ids)
  expect_identical(length(alloc), 3L)
  expect_identical(anyDuplicated(tolower(alloc)), 0L)
  # the unambiguous identity keeps its readable name; both colliding ones are hashed
  expect_identical(unname(alloc[.fragmentIdentityKey(.fragmentIdentity("ev", "a"))]), "ev_a")
  colliding <- alloc[vapply(ids[1:2], function(x) .fragmentIdentityKey(do.call(.fragmentIdentity, as.list(x))), "")]
  expect_true(all(grepl("^ev_a_src_[0-9a-f]{10}$", colliding)))
})

test_that("identifiers that differ only by case, or exceed the dialect limit, are disambiguated", {
  alloc <- .ws4_alloc(list(list("ev", "HbA1c"), list("ev", "hba1c")))
  expect_identical(anyDuplicated(tolower(alloc)), 0L)
  expect_true(all(grepl("^ev_(HbA1c|hba1c)_[0-9a-f]{10}$", alloc)))
  name61 <- paste0("a", strrep("b", 60))
  id <- list(list("ev", name61))
  # 64 characters: readable on SQLite (grammar limit 64), hashed on PostgreSQL (63 bytes)
  expect_identical(unname(.ws4_alloc(id, "sqlite")), paste0("ev_", name61))
  pg <- unname(.ws4_alloc(id, "postgresql"))
  expect_lte(nchar(pg), 63L)
  expect_match(pg, "^ev_abbb+_[0-9a-f]{10}$")
  expect_lte(nchar(unname(.ws4_alloc(id, "oracle"))), 30L)
  # a readable name a CTE could shadow a base table with is never used
  shadow <- unname(.ws4_alloc(list(list("concept", "ancestor")), "sqlite"))
  expect_match(shadow, "^concept_ancestor_[0-9a-f]{10}$")
})

test_that("allocation is independent of compilation order and residual collisions get a counter", {
  ids <- list(list("ev", "x", "src"), list("ev", "x_src"), list("ev", "y"), list("ev", "Y"), list("ev", "z"))
  a <- .ws4_alloc(ids)
  b <- .ws4_alloc(rev(ids))
  expect_identical(a[order(names(a))], b[order(names(b))])
  # a hash-prefix collision is resolved by the first free _2, _3 in canonical order
  local_mocked_bindings(.fragmentHash = function(key) "0123456789")
  c3 <- .ws4_alloc(list(list("ev", "q", "src"), list("ev", "q_src"), list("ev", "Q_src")), "sqlite")
  # canonical (byte) order of the encodings: ("ev","Q_src") < ("ev","q","src") < ("ev","q_src")
  expect_identical(unname(c3[order(names(c3), method = "radix")]),
                   c("ev_Q_src_0123456789", "ev_q_src_0123456789_2", "ev_q_src_0123456789_3"))
  expect_identical(anyDuplicated(tolower(c3)), 0L)
  long <- .ws4_alloc(list(list("ev", strrep("w", 64), "src"), list("ev", paste0(strrep("w", 64), "_src"))), "oracle")
  expect_true(all(nchar(long) <= 30L))
  expect_true(all(grepl("_0123456789(_2)?$", long)))
})

test_that("compilers name their fragments from the context allocation, whatever the compilation order", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  events <- list(
    a = list(table = "condition_occurrence", concept_set = list(concepts = list(201820))),
    a_src = list(table = "drug_exposure", concept_set = list(concepts = list(1503297))),
    HbA1c = list(table = "measurement", concept_set = list(concepts = list(3004410))),
    hba1c = list(table = "measurement", concept_set = list(concepts = list(3004410)),
                 where = list(var = "value", op = "not_null")))
  names_in <- function(order) {
    ctx <- .ws4_ctx(h, .ws4_plan(events = events))
    for (e in order) .compileEventByName(ctx, e)
    lapply(stats::setNames(names(events), names(events)), function(e) .eventMeta(ctx, e)$fragments)
  }
  one <- names_in(names(events))
  two <- names_in(rev(names(events)))
  expect_identical(one, two)
  all_names <- unlist(one, use.names = FALSE)
  expect_identical(anyDuplicated(tolower(all_names)), 0L)
  expect_identical(unname(one$a[["final"]]), "ev_a")
  expect_false(identical(unname(one$a[["src"]]), unname(one$a_src[["final"]])))
  # every reference uses the allocated names: the statements run, and the two
  # case-folded twins coexist in one statement
  ctx <- .ws4_ctx(h, .ws4_plan(events = events))
  f1 <- .compileEventByName(ctx, "HbA1c")
  f2 <- .compileEventByName(ctx, "hba1c")
  both <- .fragment("ws4_both", "result", "none",
                    paste0("SELECT (SELECT COUNT(*) FROM ", f1$name, ") AS n1, (SELECT COUNT(*) FROM ",
                           f2$name, ") AS n2"),
                    columns = list(n1 = .fragmentColumn("integer", FALSE, "measure"),
                                   n2 = .fragmentColumn("integer", FALSE, "measure")),
                    dependencies = c(f1$name, f2$name))
  res <- .executeQuery(h, .buildStatement(ctx, both, dialect = ctx))
  expect_gt(res$n1, 0)
  expect_lte(res$n2, res$n1)
  # an identity no generator declared cannot be named
  expect_error(.fragmentName("iv", "era", ctx = ctx), "was not declared by an identity generator")
  expect_identical(.fragmentName("ev", "a"), "ev_a")
})

test_that("the case-folded twins also coexist on PostgreSQL", {
  config_pg <- .ws4_pg_connect()
  skip_if(is.null(config_pg), "PostgreSQL test host not reachable")
  DBI::dbDisconnect(config_pg)
  backends <- .ws4_backends()
  on.exit(.ws4_cleanup(backends), add = TRUE)
  skip_if(is.null(backends$postgresql), "PostgreSQL fixture not loaded")
  h <- backends$postgresql$handle
  events <- list(HbA1c = list(table = "measurement", concept_set = list(concepts = list(3004410))),
                 hba1c = list(table = "measurement", concept_set = list(concepts = list(3004410)),
                              where = list(var = "value", op = "not_null")))
  ctx <- .ws4_ctx(h, .ws4_plan(events = events))
  f1 <- .compileEventByName(ctx, "HbA1c")
  f2 <- .compileEventByName(ctx, "hba1c")
  expect_false(identical(tolower(f1$name), tolower(f2$name)))
  both <- .fragment("ws4_both", "result", "none",
                    paste0("SELECT (SELECT COUNT(*) FROM ", f1$name, ") AS n1, (SELECT COUNT(*) FROM ",
                           f2$name, ") AS n2"),
                    columns = list(n1 = .fragmentColumn("integer", FALSE, "measure"),
                                   n2 = .fragmentColumn("integer", FALSE, "measure")),
                    dependencies = c(f1$name, f2$name))
  res <- .executeQuery(h, .buildStatement(ctx, both, dialect = ctx))
  expect_gt(as.numeric(res$n1), 0)
  expect_lte(as.numeric(res$n2), as.numeric(res$n1))
})
