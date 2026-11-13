# Explicit R names are classified under a common UTF-8 locale on both ends.
.conceptValidNames <- function(x) {
  previous <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previous)), add = TRUE)
  selected <- ""
  for (locale in c("C.UTF-8", "en_US.UTF-8", "UTF-8")) {
    selected <- suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (nzchar(selected)) break
  }
  if (!nzchar(selected)) stop("Concept column names require UTF-8 character classification.", call. = FALSE)
  make.names(enc2utf8(x), unique = FALSE, allow_ = TRUE) == enc2utf8(x)
}

# A-27: shared, pure presentation contract. Vendored byte-for-byte in the client.
# Identity is always a decimal concept ID; labels never act as identity keys.
.conceptMap <- function(x, path, names_map = FALSE, wire = FALSE) {
  if (is.null(x)) return(setNames(character(), character()))
  if (wire) {
    if (!is.list(x) || is.null(names(x)) ||
        any(!vapply(x, function(v) is.character(v) && length(v) == 1L, logical(1)))) {
      stop(path, " must be an object of strings.", call. = FALSE)
    }
    x <- setNames(vapply(x, identity, ""), names(x))
  }
  if (!is.character(x) || (length(x) && (is.null(names(x)) ||
      anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))))) {
    stop(path, " must map unique keys to character strings.", call. = FALSE)
  }
  if (anyNA(x) || any(!nzchar(trimws(x))) || anyNA(iconv(x, to = "UTF-8"))) {
    stop(path, " labels must be nonempty UTF-8 strings.", call. = FALSE)
  }
  if (names_map) {
    if (any(!.conceptValidNames(x))) {
      stop(path, " contains an invalid or reserved R column name.", call. = FALSE)
    }
  } else if (length(x)) {
    ids <- names(x)
    if (any(!grepl("^(0|[1-9][0-9]*)$", ids)) ||
        any(nchar(ids) > 10L | (nchar(ids) == 10L & ids > "2147483647"))) {
      stop(path, " keys must be canonical concept IDs in 0..2147483647.", call. = FALSE)
    }
  }
  x[order(names(x), method = "radix")]
}

.conceptMapOverlay <- function(recipe, variable) {
  c(variable, recipe[setdiff(names(recipe), names(variable))])
}

.conceptIdOrder <- function(ids) order(nchar(ids), ids, method = "radix")

.conceptNameToken <- function(x) {
  x <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", enc2utf8(x))
  x <- gsub("[^a-z0-9]+", "_", x, perl = TRUE)
  x <- gsub("^_+|_+$", "", x)
  x[!nzchar(x)] <- "concept"
  bad <- grepl("^[0-9]", x) | make.names(x, unique = FALSE) != x
  x[bad] <- paste0("concept_", x[bad])
  x
}

# Versions are compared only within one vocabulary and representation family.
.conceptVersion <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(NULL)
  hits <- regmatches(x, gregexpr("(?<![0-9])(?:[0-9]{4}-[0-9]{2}-[0-9]{2}|[0-9]{8})(?![0-9])", x, perl = TRUE))[[1L]]
  dates <- suppressWarnings(as.Date(gsub("-", "", hits), "%Y%m%d"))
  valid <- !is.na(dates) & format(dates, "%Y%m%d") == gsub("-", "", hits)
  if (sum(valid) == 1L) return(list(family = "date", parts = format(dates[valid], "%Y%m%d")))
  if (grepl("^[0-9]+(\\.[0-9]+)*$", x)) {
    parts <- strsplit(x, ".", fixed = TRUE)[[1L]]
    parts <- sub("^0+", "", parts); parts[parts == ""] <- "0"
    return(list(family = "number", parts = parts))
  }
  NULL
}

.conceptChooseName <- function(candidates) {
  good <- vapply(candidates, function(x) !is.null(x$name) && !is.na(x$name) &&
                   nzchar(trimws(x$name)), logical(1))
  candidates <- candidates[good]
  if (!length(candidates)) return(list(name = NULL, version = NULL, rule = "ID fallback"))
  versions <- lapply(candidates, function(x) .conceptVersion(x$version))
  comparable <- all(!vapply(versions, is.null, logical(1))) &&
    length(unique(vapply(versions, function(x) x$family, ""))) == 1L &&
    length(unique(vapply(candidates, function(x) x$vocabulary %||% "", ""))) == 1L &&
    all(vapply(candidates, function(x) length(x$vocabulary) == 1L && !is.na(x$vocabulary) && nzchar(x$vocabulary), logical(1)))
  eligible <- seq_along(candidates)
  if (comparable) {
    width <- max(vapply(versions, function(x) length(x$parts), 0L))
    for (j in seq_len(width)) {
      values <- vapply(versions[eligible], function(v) if (length(v$parts) >= j) v$parts[[j]] else "0", "")
      best <- values[.conceptIdOrder(values)[length(values)]]
      eligible <- eligible[values == best]
    }
  }
  labels <- vapply(candidates[eligible], function(x) enc2utf8(x$name), "")
  winner <- candidates[[eligible[order(labels, method = "radix")[[1L]]]]]
  list(name = winner$name, version = winner$version,
       rule = if (comparable) "newest comparable version; bytewise name tie" else
         "recency unresolved; bytewise name tie")
}

.conceptResolve <- function(records) {
  ids <- unique(vapply(records, function(x) x$id, ""))
  ids <- ids[.conceptIdOrder(ids)]
  result <- lapply(ids, function(id) {
    rows <- records[vapply(records, function(x) identical(x$id, id), logical(1))]
    user <- Filter(function(x) x$origin %in% c("variable", "recipe"), rows)
    if (length(user)) {
      priority <- vapply(user, function(x) if (x$origin == "variable") 1L else 2L, 0L)
      user <- user[priority == min(priority)]
      labels <- unique(vapply(user, function(x) x$label, ""))
      if (length(labels) != 1L) stop("Inconsistent declared concept recodes.", call. = FALSE)
      return(list(id = id, label = labels[[1L]], user = TRUE, rule = "user recode"))
    }
    chosen <- .conceptChooseName(rows)
    list(id = id, label = chosen$name %||% id, user = FALSE, version = chosen$version, rule = chosen$rule)
  })
  # Deliberate equal user labels alone merge. Disambiguate automatic collisions
  # repeatedly, including literal vocabulary names containing an ID suffix.
  repeat {
    labels <- vapply(result, function(x) x$label, "")
    duplicate <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
    change <- duplicate & !vapply(result, function(x) x$user, logical(1))
    if (!any(change)) break
    for (i in which(change)) result[[i]]$label <- paste0(result[[i]]$label, " [", result[[i]]$id, "]")
  }
  list(ids = ids, labels = vapply(result, function(x) x$label, ""),
       levels = unique(vapply(result, function(x) x$label, "")),
       vocabulary = vapply(ids, function(id) {
         rows <- records[vapply(records, function(x) identical(x$id, id), logical(1))]
         .conceptChooseName(rows)$name %||% paste0("concept_", id)
       }, ""), decisions = result)
}

.conceptFinalNames <- function(keys, automatic, explicit, ids) {
  final <- setNames(keys, keys)
  final[names(automatic)] <- automatic
  active <- intersect(names(explicit), keys)
  final[active] <- explicit[active]
  auto <- setdiff(names(automatic), active)
  repeat {
    dup <- duplicated(final) | duplicated(final, fromLast = TRUE)
    if (!any(dup)) break
    if (any(names(final)[dup] %in% active)) {
      stop("column_names: explicit name collides in the final component: ",
           final[which(dup)[1L]], call. = FALSE)
    }
    change <- intersect(names(final)[dup], auto)
    if (!length(change)) stop("Duplicate final component column names.", call. = FALSE)
    final[change] <- paste0(final[change], "_", ids[change])
  }
  unname(final)
}

.conceptHasMaps <- function(plan) {
  any(vapply(c(list(plan$options), plan$variables), function(x) {
    length(x$labels) > 0L || length(x$column_names) > 0L
  }, logical(1)))
}

.conceptNeedsPresentation <- function(plan) {
  !isFALSE(plan$options$translate_concepts) || .conceptHasMaps(plan)
}

.conceptValidatePlan <- function(plan, wire = FALSE) {
  options <- plan$options
  if (!isFALSE(options$translate_concepts) && isFALSE(options$factor_concepts)) {
    stop("factor_concepts = FALSE conflicts with concept translation; remove it or set translate_concepts = FALSE.", call. = FALSE)
  }
  nodes <- c(list(options = options), if (length(plan$variables)) setNames(plan$variables, paste0("variables.", names(plan$variables))) else list())
  for (path in names(nodes)) for (key in c("labels", "column_names")) {
    .conceptMap(nodes[[path]][[key]], paste0(path, ".", key), key == "column_names", wire)
  }
  invisible(TRUE)
}

.conceptGeneratedNames <- function(keys, generated) {
  automatic <- explicit <- ids <- setNames(character(), character())
  for (key in names(generated)) {
    g <- generated[[key]]
    chosen <- .conceptChooseName(g$candidates)
    token <- if (is.null(chosen$name)) paste0("concept_", g$id) else .conceptNameToken(chosen$name)
    automatic[[key]] <- if (isTRUE(g$legacy)) paste0(g$prefix, token, g$suffix) else
      .conceptNameToken(paste0(g$prefix, token, g$suffix))
    ids[[key]] <- g$id
    if (length(g$explicit)) explicit[[key]] <- g$explicit
  }
  setNames(.conceptFinalNames(keys, automatic, explicit, ids), keys)
}

.conceptExplain <- function(plan) {
  active <- !isFALSE(plan$options$translate_concepts)
  lines <- if (active) c(
    "Concept presentation: factors; identity is the declared concept class, preserved through aliases and value-preserving reducers.",
    "Label precedence: variable recode, recipe recode, vocabulary name, decimal ID fallback. Explicit equal recodes merge categories.",
    "Federation: permitted ID union, including declared unused levels; newest comparable vocabulary version, then bytewise name tie. Unresolved versions never establish recency.",
    "Generated names: ASCII snake_case vocabulary tokens; colliding automatic names gain concept-ID suffixes. Vocabulary-dependent names are unresolved until permitted federation reconciliation.") else
      "Concept presentation disabled: numeric IDs and untranslated generated keys; labels, column_names and factor_concepts are inactive."
  for (name in names(plan$variables)) {
    v <- plan$variables[[name]]
    source <- if (!is.null(v$raw)) paste(v$raw$table, v$raw$column, sep = ".") else
      if (!is.null(v$reducer)) paste0("reducer ", v$reducer$fn, " over ", v$reducer$value %||% "the declared source value") else NULL
    if (!is.null(source)) lines <- c(lines, paste0("variables.", name, ": concept origin follows ", source, "; explicit variable name is retained."))
  }
  for (name in names(plan$outputs)) {
    o <- plan$outputs[[name]]
    if (!identical(o$format, "person_period")) next
    for (source in o$options$sources %||% o$sources) {
      event <- source$event
      if (!is.character(event) || length(event) != 1L) next
      cs <- plan$events[[event]]$concept_set
      if (is.character(cs)) cs <- plan$concept_sets[[cs]]
      if (!is.list(cs)) next
      ids <- as.character(unlist(cs$concepts, use.names = FALSE))
      if (length(ids) < 2L || isTRUE(cs$include_descendants) || isTRUE(cs$include_mapped)) next
      analyses <- as.character(unlist(o$options$source_analyses[[event]] %||% o$options$analyses %||% list("count"), use.names = FALSE))
      keys <- unlist(lapply(ids, function(id) paste(event, id, analyses, sep = "_")), use.names = FALSE)
      lines <- c(lines, paste0("outputs.", name, " stable generated keys: ", paste(keys, collapse = ", ")))
    }
  }
  nodes <- c(list(options = plan$options), if (length(plan$variables)) setNames(plan$variables, paste0("variables.", names(plan$variables))) else list())
  for (path in names(nodes)) {
    node <- nodes[[path]]
    for (key in c("labels", "column_names")) if (length(node[[key]])) {
      map <- unlist(node[[key]], use.names = TRUE)
      lines <- c(lines, paste0(path, ".", key, if (!active) " (inactive)" else "", ": ",
                              paste(paste(names(map), map, sep = " -> "), collapse = "; ")))
      if (key == "labels" && anyDuplicated(map)) lines <- c(lines, paste0(path, ": equal explicit labels intentionally merge their concept IDs."))
    }
  }
  lines
}

.conceptValidateResolution <- function(shared, records) {
  ids <- as.character(unlist(shared$ids, use.names = FALSE))
  labels <- as.character(unlist(shared$labels, use.names = FALSE))
  levels <- as.character(unlist(shared$levels, use.names = FALSE))
  if (length(ids) != length(labels) || !identical(ids, ids[.conceptIdOrder(ids)])) stop("Invalid ordered concept identity contract.", call. = FALSE)
  .conceptMap(setNames(labels, ids), "shared concept labels")
  if (!identical(levels, unique(labels))) stop("Invalid ordered concept level union.", call. = FALSE)
  for (r in records) if (r$origin %in% c("variable", "recipe")) {
    if (!r$id %in% ids || !identical(labels[[match(r$id, ids)]], r$label)) stop("Shared labels conflict with the declared user recode.", call. = FALSE)
  }
  duplicate <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  for (i in which(duplicate)) {
    r <- records[[ids[[i]]]]
    if (is.null(r) || !r$origin %in% c("variable", "recipe") || !identical(r$label, labels[[i]])) {
      stop("Only explicitly equal user recodes may merge concept identities.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.conceptValidateNames <- function(mapping, keys, generated) {
  if (!setequal(names(mapping), keys) || anyNA(mapping) || anyDuplicated(mapping)) stop("Invalid complete generated-column contract.", call. = FALSE)
  ordinary <- setdiff(keys, names(generated))
  if (!identical(unname(mapping[ordinary]), ordinary)) stop("Presentation cannot rename structural or explicit variable columns.", call. = FALSE)
  for (key in names(generated)) {
    .conceptMap(setNames(mapping[[key]], key), "column_names", TRUE)
    target <- generated[[key]]$explicit
    if (length(target) && !identical(mapping[[key]], target)) stop("Shared name conflicts with explicit column_names.", call. = FALSE)
  }
  invisible(TRUE)
}
