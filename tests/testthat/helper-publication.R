.publication_test_context <- function(threshold = 3L, strict = TRUE,
                                      target = new.env(parent = emptyenv()),
                                      .local_envir = parent.frame()) {
  withr::local_options(list(nfilter.subset = threshold,
                            dsomop.query_strict = strict),
                       .local_envir = .local_envir)
  ctx <- .planContext(NULL, list(), target_env = target)
  withr::defer({
    .planContextClose(ctx)
    if (exists(ctx$id, .publicationRegistry, inherits = FALSE)) {
      rm(list = ctx$id, envir = .publicationRegistry)
    }
  }, envir = .local_envir)
  ctx
}

.publication_test_candidate <- function(root = "out", component = root,
                                        symbol = component, lineage = NULL) {
  if (is.null(lineage)) lineage <- .lineage("direct", "person_id")
  .publicationCandidate(root, component, "frame", lineage,
                        requested_symbol = symbol,
                        grain = if (lineage$mode == "reference") "none" else "person")
}

.publication_test_commit <- function(ctx) {
  on.exit(.planContextClose(ctx), add = TRUE)
  .publishCommit(ctx)
}

