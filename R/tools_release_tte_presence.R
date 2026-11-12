# Canonical interval-presence companion of capped recurrent event time.
# Each person occupies at most K bins per registered scale, hence the joint
# vector has L1 sensitivity at most 3K. It counts distinct contributors among
# the kept occurrences, not first events or uncapped legacy bin membership.
.dsomopReleaseTtePresenceBasis <- function(basis) {
  views <- stats::setNames(lapply(c(1L, 30L, 365L), function(s) {
    .dsomopReleaseTteView(basis, s)$bins
  }), c("1", "30", "365"))
  fmt <- function(x) format(x, scientific = FALSE, trim = TRUE)
  key <- function(v) paste0("P:[", fmt(v$day_offset), ",", fmt(v$upper), ")")
  all_keys <- unlist(lapply(views, key), use.names = FALSE)
  lower <- unlist(lapply(views, `[[`, "day_offset"), use.names = FALSE)
  upper <- unlist(lapply(views, `[[`, "upper"), use.names = FALSE)
  level <- unlist(lapply(views, `[[`, "level"), use.names = FALSE)
  nodes <- unlist(lapply(views, `[[`, "nodes"), recursive = FALSE)
  keep <- which(!duplicated(all_keys))
  keep <- keep[order(lower[keep], upper[keep], method = "radix")]
  intervals <- data.frame(key = all_keys[keep], lower = lower[keep],
                          upper = upper[keep], level = level[keep],
                          stringsAsFactors = FALSE)
  masks <- lapply(views, function(v) match(key(v), intervals$key))
  contains <- do.call(rbind, lapply(seq_len(nrow(intervals)), function(p) {
    kids <- which(intervals$lower >= intervals$lower[[p]] &
                    intervals$upper <= intervals$upper[[p]])
    kids <- setdiff(kids, p)
    cbind(child = kids, parent = rep.int(p, length(kids)))
  }))
  partitions <- lapply(masks, function(i) list(parent = 0L, children = i))
  for (p in seq_len(nrow(intervals))) {
    for (view in masks) {
      i <- view[intervals$lower[view] >= intervals$lower[[p]] &
                  intervals$upper[view] <= intervals$upper[[p]]]
      if (length(i) < 2L) next
      i <- i[order(intervals$lower[i])]
      if (intervals$lower[i[[1L]]] == intervals$lower[[p]] &&
          intervals$upper[i[[length(i)]]] == intervals$upper[[p]] &&
          all(intervals$upper[i[-length(i)]] == intervals$lower[i[-1L]])) {
        partitions[[length(partitions) + 1L]] <- list(parent = p, children = i)
      }
    }
  }
  unique_partition <- !duplicated(vapply(partitions, function(p) {
    paste(c(p$parent, sort(p$children)), collapse = ":")
  }, character(1L)))
  list(version = "tte-interval-presence-v1", projection = "tte-presence-radial-v1",
       intervals = intervals, nodes = nodes[keep], views = masks,
       contains = contains, partitions = unname(partitions[unique_partition]))
}

# Versioned feasible radial projection, not Euclidean projection. The concave
# anchor f(E)=N*(1-(1-E/R)^(R/N)) satisfies the public bounds, containment and
# partition inequalities. A feasible input is preserved exactly. Otherwise
# move from that anchor toward the bounded noisy target as far as every
# inequality permits. Retreat toward the anchor can bias the displayed P.
.dsomopReleaseTtePresenceProject <- function(signed, events, N, R, K, basis) {
  m <- length(events)
  valid_index <- function(i) is.numeric(i) && !anyNA(i) &&
    all(i == floor(i)) && all(i >= 1L & i <= m) && !anyDuplicated(i)
  if (!identical(basis$version, "tte-interval-presence-v1") ||
      !identical(basis$projection, "tte-presence-radial-v1") ||
      !is.numeric(signed) || length(signed) != m || any(!is.finite(signed)) ||
      !is.numeric(events) || any(!is.finite(events)) || any(events < 0) ||
      !all(is.finite(c(N, R, K))) || N < 0 || R < N || R > K * N || K < 1 ||
      !is.matrix(basis$contains) || ncol(basis$contains) != 2L ||
      !all(vapply(seq_len(nrow(basis$contains)), function(i) {
        valid_index(basis$contains[i, ])
      }, logical(1L))) || !is.list(basis$partitions) ||
      !all(vapply(basis$partitions, function(p) {
        length(p$parent) == 1L && is.numeric(p$parent) && !is.na(p$parent) &&
          p$parent == floor(p$parent) && p$parent >= 0L && p$parent <= m &&
          length(p$children) > 1L && valid_index(p$children)
      }, logical(1L)))) {
    stop("Malformed protected event-time presence basis.", call. = FALSE)
  }
  tol <- 1e-9 * max(1, N, R)
  if (any(events > R + tol)) stop("Incoherent event-time presence totals.",
                                  call. = FALSE)
  if (N == 0) return(numeric(m))
  lower <- events / K
  upper <- pmin(N, events)
  anchor <- if (R == N) events else
    -N * expm1((R / N) * log1p(-pmin(1, events / R)))
  anchor <- pmin(upper, pmax(lower, anchor))
  residual <- function(p) {
    parents <- vapply(basis$partitions, function(q) {
      if (q$parent == 0L) N else p[[q$parent]]
    }, numeric(1L))
    sums <- vapply(basis$partitions, function(q) sum(p[q$children]), numeric(1L))
    c(p[basis$contains[, 1L]] - p[basis$contains[, 2L]],
      parents - sums, sums - K * parents)
  }
  target <- pmin(upper, pmax(lower, signed))
  rt <- residual(target)
  if (all(rt <= tol)) return(target)
  ra <- residual(anchor)
  if (any(ra > tol)) stop("Protected presence anchor is not feasible.", call. = FALSE)
  bad <- which(rt > tol)
  fraction <- min(1, pmax(0, -ra[bad]) / (rt[bad] - ra[bad]))
  out <- anchor + fraction * (target - anchor)
  if (any(!is.finite(out)) || any(residual(out) > tol) ||
      any(out < lower - tol | out > upper + tol)) {
    stop("Protected event-time presence projection did not resolve.", call. = FALSE)
  }
  out
}
