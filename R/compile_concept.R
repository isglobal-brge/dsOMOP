# Module: Concept sets (design-2.10.3 sections 2.1 and 6.5)
#
# A concept-set node resolves to list(ids, state):
#   all  -- no concept set was declared (the event is unrestricted);
#   set  -- the declared set resolved to one or more concept ids;
#   none -- a declared set resolved to nothing. It matches NO row (SQL 1 = 0,
#           R integer(0)), never "all" (fix of defect A-E1).
# Resolution: concepts -> + descendants (concept_ancestor) -> + mapped ('Maps
# to' sources of the set so far) -> minus the expansion of `exclude`, where an
# object-valued exclusion is expanded with its own flags by this same resolver
# and a bare vector means exact ids (the v1 meaning; fix of defect A-15).
# Inline exclusions cannot form cycles (they are inline objects). Resolution
# is memoised per plan on the canonical JSON of the node.

.CONCEPT_SET_FIELDS <- c("concepts", "include_descendants", "include_mapped",
                         "exclude")
.CONCEPT_STATES <- c("all", "set", "none")

# Maximum nesting of object-valued exclusions (inline objects only; a deeper
# chain is a malformed plan rather than a resolution problem).
.CONCEPT_EXCLUDE_DEPTH <- 16L

.conceptFlag <- function(x, field, path) {
  if (is.null(x)) return(FALSE)
  if (is.list(x) && length(x) == 1L && is.null(names(x))) x <- x[[1L]]
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .irReject(paste0("concept set ", field, " must be TRUE or FALSE"),
              field = paste0(path, "/", field))
  }
  x
}

.conceptExpandNode <- function(handle, node, path, depth) {
  if (depth > .CONCEPT_EXCLUDE_DEPTH) {
    .irReject("concept-set exclusions are nested too deeply", field = path)
  }
  if (!is.list(node) || is.null(names(node))) {
    .irReject("a concept set is an object with concepts", field = path)
  }
  unknown <- setdiff(names(node), .CONCEPT_SET_FIELDS)
  if (length(unknown)) {
    .irReject(paste0("unknown concept-set field(s): ", paste(unknown, collapse = ", ")),
              field = path)
  }
  if (!"concepts" %in% names(node)) {
    .irReject("a concept set needs concepts", field = paste0(path, "/concepts"))
  }
  base <- tryCatch(.conceptIdList(node[["concepts"]]),
                   error = function(e) .irReject(conditionMessage(e),
                                                 field = paste0(path, "/concepts")))
  descendants <- .conceptFlag(node[["include_descendants"]], "include_descendants", path)
  mapped <- .conceptFlag(node[["include_mapped"]], "include_mapped", path)
  included <- if (length(base) == 0L) {
    integer(0)
  } else if (!descendants && !mapped) {
    base
  } else {
    bp <- .buildBlueprint(handle)
    present <- bp$tables$table_name[bp$tables$present_in_db]
    needed <- c(if (descendants) "concept_ancestor", if (mapped) "concept_relationship")
    lacking <- setdiff(needed, present)
    if (length(lacking)) {
      .irReject(paste0("concept-set expansion needs the vocabulary table ", lacking[[1L]]),
                code = "unavailable_table", field = path)
    }
    tryCatch(
      .vocabExpandConceptSet(handle, list(concepts = base,
                                          include_descendants = descendants,
                                          include_mapped = mapped)),
      error = function(e) .irReject(conditionMessage(e), field = path))
  }
  excl <- node[["exclude"]]
  excluded <- if (is.null(excl)) {
    integer(0)
  } else if (is.list(excl) && !is.null(names(excl))) {
    .conceptExpandNode(handle, excl, paste0(path, "/exclude"), depth + 1L)
  } else {
    tryCatch(.conceptIdList(excl),
             error = function(e) .irReject(conditionMessage(e),
                                           field = paste0(path, "/exclude")))
  }
  ids <- setdiff(unique(as.integer(included)), as.integer(excluded))
  sort(as.integer(ids), method = "radix")
}

#' Resolve one concept-set node (design section 6.5)
#'
#' @param handle CDM handle (vocabulary tables for descendants and mappings).
#' @param node NULL (no concept set declared) or a concept-set object
#'   \code{list(concepts, include_descendants, include_mapped, exclude)}.
#' @param path JSON path used in rejections.
#' @return \code{list(ids, state)}: \code{state = "all"} with \code{ids =
#'   NULL} when no set is declared; \code{"set"} with the sorted ids; or
#'   \code{"none"} with \code{integer(0)} when a declared set resolves to
#'   nothing.
#' @keywords internal
.resolveConceptSetNode <- function(handle, node, path = "/concept_set") {
  if (is.null(node)) return(list(ids = NULL, state = "all"))
  ids <- .conceptExpandNode(handle, node, path, 0L)
  list(ids = ids, state = if (length(ids)) "set" else "none")
}

#' Compile a concept-set node (frozen compiler signature, section 6)
#'
#' Memoised per execution context on the canonical JSON of the node, so equal
#' inline objects and repeated references resolve once.
#'
#' @param ctx Execution context (environment with \code{handle}, \code{ir}).
#' @param node NULL, a registry name (resolved through \code{ctx$ir}) or a
#'   concept-set object.
#' @return \code{list(ids, state)} (see \code{.resolveConceptSetNode()}).
#' @keywords internal
.compileConceptSet <- function(ctx, node) {
  path <- "/concept_set"
  if (is.list(node) && length(node) == 1L && is.null(names(node)) &&
      is.character(node[[1L]])) {
    node <- node[[1L]]
  }
  if (is.character(node)) {
    if (length(node) != 1L || is.na(node)) {
      .irReject("a concept-set reference is one registry name", field = path)
    }
    path <- paste0("/concept_sets/", node)
    resolved <- ctx[["ir"]][["concept_sets"]][[node]]
    if (is.null(resolved)) {
      .irReject(paste0("dangling concept set '", node, "'"), field = path)
    }
    node <- resolved
  }
  if (is.null(node)) return(list(ids = NULL, state = "all"))
  memo <- .compileMemo(ctx, "concept_memo")
  key <- .jcsCanonical(unclass(node))
  if (exists(key, envir = memo, inherits = FALSE)) {
    return(get(key, envir = memo, inherits = FALSE))
  }
  out <- .resolveConceptSetNode(ctx[["handle"]], node, path)
  assign(key, out, envir = memo)
  out
}

#' Intersect two resolved concept sets on the same column
#'
#' Used for the v1 output-level concept scope that also restricts an event
#' (IR variant \code{legacy_concept_scope}, amendment A-10): a row must match
#' both sets.
#' @keywords internal
.conceptIntersect <- function(a, b) {
  if (identical(a$state, "all")) return(b)
  if (identical(b$state, "all")) return(a)
  ids <- sort(intersect(a$ids, b$ids), method = "radix")
  list(ids = as.integer(ids), state = if (length(ids)) "set" else "none")
}

#' SQL predicate of a resolved concept set on one column
#'
#' @param column Trusted SQL column expression.
#' @param concept \code{list(ids, state)}.
#' @return SQL predicate, or NULL for \code{state = "all"} (no restriction).
#'   \code{none} compiles to \code{1 = 0}.
#' @keywords internal
.conceptPredicateSql <- function(column, concept) {
  state <- concept[["state"]]
  if (!is.character(state) || length(state) != 1L || !state %in% .CONCEPT_STATES) {
    stop("A resolved concept set needs a state (all, set or none).", call. = FALSE)
  }
  switch(state,
    all = NULL,
    none = "1 = 0",
    set = .sqlIdInPredicate(column, concept[["ids"]])
  )
}

#' R mask of a resolved concept set over a concept column
#'
#' The R mirror of \code{.conceptPredicateSql()}: \code{all} keeps every row,
#' \code{none} none, \code{set} the rows whose concept is listed (a NULL
#' concept is never listed).
#' @keywords internal
.conceptMask <- function(values, concept) {
  switch(concept[["state"]],
    all = rep(TRUE, length(values)),
    none = rep(FALSE, length(values)),
    set = !is.na(values) & values %in% concept[["ids"]],
    stop("A resolved concept set needs a state (all, set or none).", call. = FALSE)
  )
}

#' The RxNorm ingredient mapping of drug concepts as a fragment
#'
#' Design section 6.5: \code{concept_ancestor} restricted to ancestors with
#' \code{concept_class_id = 'Ingredient'} and \code{vocabulary_id} RxNorm or
#' RxNorm Extension; one fragment per plan (\code{cs_ingredient_map}), joined
#' by eras grouped \code{by = "ingredient"} (\code{descendant_concept_id} =
#' the drug concept, \code{ingredient_concept_id} = its ingredient). A drug
#' concept with several ingredient ancestors maps to each of them.
#'
#' @param ctx Execution context.
#' @return The registered reference fragment (grain none).
#' @keywords internal
.compileIngredientMap <- function(ctx) {
  existing <- .fragmentGet(ctx, "cs_ingredient_map")
  if (!is.null(existing)) return(existing)
  handle <- ctx[["handle"]]
  bp <- .buildBlueprint(handle)
  present <- bp$tables$table_name[bp$tables$present_in_db]
  if (!all(c("concept", "concept_ancestor") %in% present)) {
    .irReject("ingredient grouping needs the concept and concept_ancestor tables",
              code = "unavailable_table", field = "/intervals")
  }
  ca <- .qualifyTable(handle, "concept_ancestor",
                      .resolveTableSchema(handle, "concept_ancestor", "Vocabulary"))
  cc <- .qualifyTable(handle, "concept",
                      .resolveTableSchema(handle, "concept", "Vocabulary"))
  body <- paste0(
    "SELECT ca.descendant_concept_id AS descendant_concept_id, ",
    "ca.ancestor_concept_id AS ingredient_concept_id FROM ", ca, " ca ",
    "INNER JOIN ", cc, " c ON c.concept_id = ca.ancestor_concept_id ",
    "WHERE c.concept_class_id = 'Ingredient' ",
    "AND c.vocabulary_id IN ('RxNorm', 'RxNorm Extension')")
  f <- .fragment(
    name = "cs_ingredient_map", kind = "concept", grain = "none",
    select_body = body,
    columns = list(
      descendant_concept_id = .fragmentColumn("integer", FALSE, "concept"),
      ingredient_concept_id = .fragmentColumn("integer", FALSE, "group")),
    unique_key = c("descendant_concept_id", "ingredient_concept_id"))
  .fragmentRegister(ctx, f)
}

#' Ingredient mapping of a set of drug concepts, resolved in R
#'
#' The same mapping as \code{.compileIngredientMap()}, read once per plan for
#' the given concepts (memoised) for consumers that group in R.
#'
#' @param ctx Execution context.
#' @param concept_ids Integer drug concept ids.
#' @return data.frame(descendant_concept_id, ingredient_concept_id), sorted.
#' @keywords internal
.resolveIngredientMap <- function(ctx, concept_ids) {
  ids <- sort(unique(.conceptIdList(concept_ids)), method = "radix")
  empty <- data.frame(descendant_concept_id = integer(0),
                      ingredient_concept_id = integer(0))
  if (!length(ids)) return(empty)
  memo <- .compileMemo(ctx, "ingredient_memo")
  key <- paste(ids, collapse = ",")
  if (exists(key, envir = memo, inherits = FALSE)) {
    return(get(key, envir = memo, inherits = FALSE))
  }
  f <- .compileIngredientMap(ctx)
  sql <- paste0("SELECT m.descendant_concept_id, m.ingredient_concept_id FROM (",
                f$select_body, ") m WHERE ",
                .sqlIdInPredicate("m.descendant_concept_id", ids),
                " ORDER BY m.descendant_concept_id, m.ingredient_concept_id")
  rows <- .executeQuery(ctx[["handle"]], sql)
  out <- if (nrow(rows)) {
    data.frame(descendant_concept_id = as.integer(rows$descendant_concept_id),
               ingredient_concept_id = as.integer(rows$ingredient_concept_id))
  } else empty
  assign(key, out, envir = memo)
  out
}

#' Reviewed operator classes (design section 6.6)
#'
#' \code{inst/ohdsi/operator_classes.csv}: the equality operator and the four
#' inequality operators. Unknown or NULL operators count as equality by
#' policy, so only the reviewed inequality ids are ever excluded.
#'
#' @return data.frame(concept_id, operator, class).
#' @keywords internal
.operatorClasses <- function() {
  cached <- .irCache[["operator_classes"]]
  if (!is.null(cached)) return(cached)
  path <- system.file("ohdsi", "operator_classes.csv", package = "dsOMOP")
  if (!nzchar(path)) stop("The reviewed operator classes are missing.", call. = FALSE)
  rows <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("concept_id", "operator", "class") %in% names(rows)) ||
      !all(rows$class %in% c("equality", "inequality"))) {
    stop("The reviewed operator classes are malformed.", call. = FALSE)
  }
  rows$concept_id <- as.integer(rows$concept_id)
  assign("operator_classes", rows, envir = .irCache)
  rows
}

#' Concept ids of the reviewed inequality operators
#' @keywords internal
.operatorInequalityIds <- function() {
  oc <- .operatorClasses()
  sort(oc$concept_id[oc$class == "inequality"], method = "radix")
}
