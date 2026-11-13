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
    if (any(make.names(x, unique = FALSE, allow_ = TRUE) != x)) {
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
    length(unique(vapply(candidates, function(x) x$vocabulary %||% "", ""))) == 1L
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
    list(id = id, label = chosen$name %||% id, user = FALSE, rule = chosen$rule)
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
       levels = unique(vapply(result, function(x) x$label, "")), decisions = result)
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
