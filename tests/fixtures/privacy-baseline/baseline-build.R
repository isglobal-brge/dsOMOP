.dsomopReleaseBuild <- function(policy, protection, owner, sub,
                                data_strata_keys = NULL, n_only = FALSE) {
  keys <- .dsomopReleaseKeys(policy)
  support <- .dsomopReleaseSupport()
  J <- .DSOMOP_RELEASE_LATTICE
  needed <- c("person_id", "stratum")
  if (!is.data.frame(sub) || !all(needed %in% names(sub))) {
    stop("A protected substrate lacks person/stratum columns.", call. = FALSE)
  }
  is_cell <- !is.null(owner$cells)
  if (is_cell && !"cell" %in% names(sub)) {
    stop("A protected partition substrate lacks its cell column.",
         call. = FALSE)
  }
  if (!is_cell && !"v" %in% names(sub)) {
    stop("A protected value substrate lacks its value column.", call. = FALSE)
  }
  sub <- sub[!is.na(sub$person_id) & !is.na(sub$stratum), , drop = FALSE]
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  sub$stratum <- as.character(sub$stratum)
  if (is_cell) {
    sub$cell <- as.character(sub$cell)
    if (anyNA(sub$cell) || any(!sub$cell %in% owner$cells)) {
      stop("A protected partition substrate has a cell outside its public ",
           "domain.", call. = FALSE)
    }
  } else {
    sub$v <- suppressWarnings(as.numeric(sub$v))
    sub <- sub[is.finite(sub$v), , drop = FALSE]
  }
  if (!identical(owner$strata$kind, "data")) {
    unknown <- setdiff(unique(sub$stratum), owner$strata$keys)
    if (length(unknown)) {
      stop("A protected substrate has a stratum outside its public domain.",
           call. = FALSE)
    }
  }
  sub <- .dsomopReleaseCapSubstrate(sub, owner)
  # Exact eligible-population support (the parent basis), checked before any
  # release; failures use the generic disclosure error.
  n_total <- length(unique(sub$person_id))
  if (n_total < support$floor) .dsomopReleaseDisclosive()
  tokens <- if (nrow(sub)) .dsomopReleasePersonTokens(keys, policy,
                                                      sub$person_id) else
    character(0)
  cap <- max(1L, owner$cap)
  frac <- .DSOMOP_RELEASE_FRACTIONS
  alloc_h <- switch(owner$family, H = frac$H$H, HM = frac$HM$H, M = NA_real_)
  alloc_s <- switch(owner$family, H = NA_real_, HM = frac$HM$S, M = frac$M$S)
  alloc_q <- switch(owner$family, H = NA_real_, HM = frac$HM$Q, M = frac$M$Q)
  has_h <- owner$family %in% c("H", "HM") && !isTRUE(n_only)
  has_m <- owner$family %in% c("HM", "M") && !is.null(owner$bounds) &&
    !isTRUE(n_only)

  # Strata to release.
  exact_n <- table(factor(sub$stratum))
  if (identical(owner$strata$kind, "data")) {
    supported <- names(exact_n)[as.numeric(exact_n) >= support$floor]
    strata_keys <- if (is.null(data_strata_keys)) supported else
      intersect(as.character(data_strata_keys), supported)
    order_value <- suppressWarnings(as.numeric(
      sub$stratum_order[match(strata_keys, sub$stratum)] %||% NA_real_))
    strata_keys <- strata_keys[order(order_value, strata_keys,
                                     method = "radix", na.last = TRUE)]
  } else {
    strata_keys <- owner$strata$keys
  }
  bin_keys <- if (is_cell) owner$cells else if (has_h)
    .dsomopReleaseBinKeys(owner$grid) else character(0)

  # Sort once so every subset used for a count or histogram has canonical
  # token order. Frame unit contributions once instead of rebuilding the same
  # million strings for root, stratum and histogram maps.
  token_order <- order(tokens, method = "radix")
  tokens <- tokens[token_order]
  sub <- sub[token_order, , drop = FALSE]
  unit_records <- paste0(nchar(tokens, type = "bytes"), ":", tokens, ":1")

  noisy <- function(family, coordinate, value, allocation, sensitivity,
                    map_rows, contributions, owner_id = owner$id,
                    population = owner$population) {
    if (length(map_rows) != length(contributions) || anyDuplicated(tokens[map_rows])) {
      stop("A contribution map is internally inconsistent.", call. = FALSE)
    }
    fp <- if (all(contributions == 1)) {
      .dsomopReleaseFingerprintFramed(keys, owner_id, family, coordinate,
                                      unit_records[map_rows])
    } else .dsomopReleaseFingerprint(keys, owner_id, family, coordinate,
                                     tokens[map_rows], contributions)
    .dsomopReleaseNoisyComponent(policy, keys, protection, list(
      owner = owner_id, population = population, family = family,
      coordinate = coordinate, value = value, allocation = allocation,
      sensitivity = sensitivity, fingerprint = fp))
  }

  # Summary statistics (moments, quantiles) need nfilter.dist distinct
  # persons: per released row for data-dependent strata, on the parent
  # eligible population for a complete public partition (whose cells are
  # never re-tested one by one).
  data_kind <- identical(owner$strata$kind, "data")
  parent_dist_ok <- n_total >= support$dist
  # The public marginal hierarchy: one canonical parent person total of the
  # owner's eligible population (shared by every owner that declares the same
  # population), drawn before any child stratification. Children of a public
  # partition are reconciled to it and projected onto its displayed total; a
  # child request never moves the parent.
  public_kind <- identical(owner$strata$kind, "public") &&
    length(strata_keys) > 1L
  root_noisy <- NA_real_
  if (!data_kind) {
    first <- !duplicated(sub$person_id)
    root_noisy <- noisy("N", "stratum=root", n_total, frac$N, cap,
                        which(first), rep(1, sum(first)),
                        owner_id = .DSOMOP_RELEASE_N_OWNER)
  }
  rows <- vector("list", length(strata_keys))
  bins <- vector("list", length(strata_keys))
  for (i in seq_along(strata_keys)) {
    s <- strata_keys[[i]]
    in_s <- sub$stratum == s
    idx <- which(in_s)
    n_exact <- sum(in_s)
    dist_ok <- if (data_kind) n_exact >= support$dist else parent_dist_ok
    n_noisy <- if (!data_kind && !public_kind) root_noisy else
      noisy("N", paste0("stratum=", s), n_exact, frac$N, cap, idx,
            rep(1, n_exact), owner_id = .DSOMOP_RELEASE_N_OWNER)
    h_noisy <- rep(NA_real_, length(bin_keys))
    if (has_h && (dist_ok || !data_kind)) {
      cell <- if (is_cell) match(sub$cell[in_s], owner$cells) else
        .dsomopReleaseBinIndex(sub$v[in_s], owner$grid)
      # Index each record once, retaining empty public cells. Re-scanning
      # the complete person vector for every cell is avoidable linear work.
      members <- split(seq_along(cell), factor(cell, levels = seq_along(bin_keys)))
      h_noisy <- vapply(seq_along(bin_keys), function(b) {
        hit <- members[[b]]
        noisy("H", paste0("stratum=", s, "|cell=", bin_keys[[b]]),
              length(hit), alloc_h, cap, idx[hit], rep(1, length(hit)))
      }, numeric(1L))
    }
    s_noisy <- q_noisy <- NA_real_
    if (has_m && dist_ok) {
      lattice <- .dsomopReleaseLattice(sub$v[in_s], owner$bounds)
      s_int <- lattice$s
      q_int <- lattice$q
      s_noisy <- noisy("S", paste0("stratum=", s), sum(s_int), alloc_s,
                       cap * J, idx, s_int)
      q_noisy <- noisy("Q", paste0("stratum=", s), sum(q_int), alloc_q,
                       cap * J, idx, q_int)
    }
    rows[[i]] <- data.frame(stratum = s, n_noisy = n_noisy,
                            s_noisy = s_noisy, q_noisy = q_noisy,
                            dist_ok = dist_ok, stringsAsFactors = FALSE)
    bins[[i]] <- h_noisy
  }
  strata <- if (length(rows)) do.call(rbind, rows) else
    data.frame(stratum = character(0), n_noisy = numeric(0),
               s_noisy = numeric(0), q_noisy = numeric(0),
               dist_ok = logical(0), stringsAsFactors = FALSE)

  # Displayed values: top-down projection of the signed components.
  if (data_kind) {
    strata$n_signed <- strata$n_noisy
    strata$n_display <- pmax(0, strata$n_noisy)
  } else if (public_kind) {
    strata$n_signed <- strata$n_noisy +
      (root_noisy - sum(strata$n_noisy)) / nrow(strata)
    strata$n_display <- .dsomopReleaseSimplex(strata$n_noisy,
                                              max(0, root_noisy))
  } else {
    strata$n_signed <- strata$n_noisy
    strata$n_display <- max(0, root_noisy)
  }
  display_bins <- lapply(seq_along(bins), function(i) {
    if (!has_h || all(is.na(bins[[i]]))) return(rep(NA_real_, length(bin_keys)))
    .dsomopReleaseSimplex(bins[[i]], strata$n_display[[i]])
  })
  strata$mean <- NA_real_
  strata$sd <- NA_real_
  usable <- strata$n_display >= max(2, support$dist)
  if (has_m) {
    L <- owner$bounds[[1L]]; U <- owner$bounds[[2L]]
    half <- (U - L) / 2; mid <- (U + L) / 2
    for (i in which(usable & strata$dist_ok)) {
      N <- strata$n_display[[i]]
      p <- .dsomopReleaseProjectMoments(strata$s_noisy[[i]] / J,
                                        strata$q_noisy[[i]] / J, N)
      strata$mean[[i]] <- mid + half * p[["S"]] / N
      strata$sd[[i]] <- sqrt(max(0, (p[["Q"]] - p[["S"]]^2 / N) / (N - 1))) *
        half
    }
  }
  quantiles <- lapply(seq_along(display_bins), function(i) {
    if (!has_h || is_cell || !usable[[i]] || !strata$dist_ok[[i]]) {
      return(rep(NA_real_, 5L))
    }
    .dsomopReleaseQuantiles(display_bins[[i]], owner$grid)
  })
  strata$p10 <- vapply(quantiles, `[[`, numeric(1L), 1L)
  strata$p25 <- vapply(quantiles, `[[`, numeric(1L), 2L)
  strata$median <- vapply(quantiles, `[[`, numeric(1L), 3L)
  strata$p75 <- vapply(quantiles, `[[`, numeric(1L), 4L)
  strata$p90 <- vapply(quantiles, `[[`, numeric(1L), 5L)
  quantile_bounds <- .dsomopReleaseQuantileBoundsList(
    display_bins, owner$grid, quantiles, strata_keys)
  for (col in c("n_display", "mean", "sd", "p10", "p25", "median", "p75",
                "p90")) {
    strata[[col]] <- .dsomopReleaseRound(strata[[col]])
  }

  # Signed components for pooling: histogram cells reconciled to the one
  # canonical signed N of their stratum (itself reconciled to the parent of a
  # public partition), plus N, S and Q; no second, independent total.
  export <- lapply(seq_along(strata_keys), function(i) {
    h <- bins[[i]]
    reconciled <- if (has_h && !all(is.na(h)) && length(h) > 0L) {
      h + (strata$n_signed[[i]] - sum(h)) / length(h)
    } else NULL
    list(stratum = strata_keys[[i]], n = strata$n_signed[[i]],
         s = if (has_m) strata$s_noisy[[i]] else NULL,
         q = if (has_m) strata$q_noisy[[i]] else NULL,
         h = reconciled)
  })
  list(
    owner = owner, protection = protection, strata = strata,
    root_n = root_noisy, quantile_bounds = quantile_bounds,
    bins = stats::setNames(display_bins, strata_keys), bin_keys = bin_keys,
    export = list(
      protocol = .DSOMOP_RELEASE_PROTOCOL, owner = owner$id,
      population = owner$population, family = owner$family,
      grid = owner$grid$id %||% NULL,
      breaks = if (!is_cell && has_h) owner$grid$breaks else NULL,
      bounds = owner$bounds, strata_kind = owner$strata$kind,
      root = if (public_kind) list(n = root_noisy) else NULL,
      cell_keys = if (has_h) bin_keys else NULL,
      cells = if (is_cell) owner$cells else NULL,
      lattice = if (has_m) J else NULL, cap = cap,
      allocation = c(list(N = frac$N),
                     if (public_kind) list(N_strata = frac$N),
                     list(H = alloc_h, S = alloc_s, Q = alloc_q)),
      reconciliation = if (public_kind) {
        "strata_N_sum_to_root_N_cells_to_stratum_N"
      } else "histogram_cells_sum_to_stratum_N",
      components = export
    )
  )
}
