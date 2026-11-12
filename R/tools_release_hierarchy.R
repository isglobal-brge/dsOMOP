# Module: Canonical public hierarchies of crossed strata (dsOMOP 2.10.3, B03)
#
# A request that stratifies one population by several public dimensions must
# never create another estimate of a public margin that another request of the
# same population already releases. Every stratification of a population is
# therefore post-processing of one canonical set of components:
#
#   * a fixed public tree of nodes (root, then the public dimensions in a fixed
#     order), each node drawn once with its own deterministic noise;
#   * signed children are reconciled to their already reconciled signed
#     parents (least-squares uniform shifts, the two-way additive adjustment
#     where a node has two parent margins), displayed children are projected
#     onto their parents' already displayed nonnegative values;
#   * a request computes the levels its dimensions need and sums nodes; it
#     never moves a parent and draws no node that another request would draw
#     differently;
#   * a node with two parent margins (a crossed age x year cell under one
#     gender) splits by status, so every display problem is a feasible
#     transportation problem.
#
# The incidence tree is root -> gender -> {age (5-year atoms), calendar year}
# -> age x year. Coarser age bands (10, 20 years) are sums of the 5-year atoms.

.DSOMOP_RELEASE_C6_TREE_VERSION <- "c6-tree-gender-age5-year-v1"
.DSOMOP_RELEASE_RECURRENT_CAP <- 10L
.DSOMOP_RELEASE_RECURRENT_FRACTION <- 0.3
.DSOMOP_RELEASE_TRANSPORT_VERSION <- "l2-transportation-dykstra-feasible-fallback-v2"

# --- Projections -------------------------------------------------------------------

#' Euclidean projection onto a bounded simplex
#'
#' \code{argmin ||x - y||^2} subject to \code{lower <= x <= upper} and
#' \code{sum(x) = total}: \code{x = clamp(y - lambda, lower, upper)} with the
#' exact \code{lambda} of the piecewise-linear sum (no iteration). A total
#' outside \code{[sum(lower), sum(upper)]} is clamped into it. Deterministic.
#'
#' @param y Numeric target.
#' @param total Required sum.
#' @param lower,upper Bounds (recycled; \code{upper} may be \code{Inf}).
#' @return Numeric vector of \code{length(y)}.
#' @keywords internal
.dsomopReleaseBoundedSimplex <- function(y, total, lower = 0, upper = Inf) {
  y <- as.numeric(y)
  n <- length(y)
  if (n == 0L) return(numeric(0))
  lower <- rep_len(as.numeric(lower), n)
  upper <- rep_len(as.numeric(upper), n)
  upper <- pmax(upper, lower)
  total <- min(max(as.numeric(total), sum(lower)), sum(upper))
  f <- function(lambda) sum(pmin(pmax(y - lambda, lower), upper))
  bps <- sort(unique(c(y - upper, y - lower)), method = "radix")
  bps <- bps[is.finite(bps)]
  vals <- vapply(bps, f, numeric(1L))
  m <- length(bps)
  lambda <- if (total >= vals[[1L]]) {
    # Left of every breakpoint only the unbounded-above entries move.
    slope <- sum(is.infinite(upper))
    if (slope == 0L) bps[[1L]] else bps[[1L]] - (total - vals[[1L]]) / slope
  } else if (total <= vals[[m]]) {
    bps[[m]]
  } else {
    k <- max(which(vals >= total))
    if (vals[[k]] == vals[[k + 1L]]) bps[[k]] else
      bps[[k]] + (vals[[k]] - total) * (bps[[k + 1L]] - bps[[k]]) /
        (vals[[k]] - vals[[k + 1L]])
  }
  pmin(pmax(y - lambda, lower), upper)
}

# Rowwise projection onto {x >= 0, sum(x) = totals[i]} (vectorised sort).
.dsomopReleaseSimplexRows <- function(Y, totals) {
  n_row <- nrow(Y)
  n_col <- ncol(Y)
  U <- matrix(apply(Y, 1L, sort, decreasing = TRUE), nrow = n_row,
              ncol = n_col, byrow = TRUE)
  CS <- matrix(apply(U, 1L, cumsum), nrow = n_row, ncol = n_col,
               byrow = TRUE)
  J <- matrix(seq_len(n_col), n_row, n_col, byrow = TRUE)
  rho <- pmax(1L, rowSums(U - (CS - totals) / J > 0))
  tau <- (CS[cbind(seq_len(n_row), rho)] - totals) / rho
  pmax(Y - tau, 0)
}

#' Finite Dykstra display on a transportation polytope with a feasible fallback
#'
#' \code{argmin ||X - Y||^2} subject to \code{X >= 0}, row sums \code{rows}
#' and column sums \code{cols} (nonnegative with equal totals), by Dykstra's
#' alternating projections between the row and column simplex products
#' (\code{.DSOMOP_RELEASE_TRANSPORT_VERSION}). The polytope is never empty
#' for such margins. If the finite iteration cap leaves inconsistent margins,
#' nonnegative row normalization and a surplus-to-deficit column transfer
#' repair feasibility. The algorithm targets the Euclidean projection; the
#' fallback is a feasible display, not an exact minimizer. Deterministic: fixed
#' update, stopping, repair and residual-validation rules.
#'
#' @param Y Numeric matrix (signed target).
#' @param rows,cols Nonnegative margins with equal sums.
#' @param tol Relative tolerance of the margins.
#' @param max_iter Iteration cap.
#' @return Numeric matrix.
#' @keywords internal
.dsomopReleaseTransport <- function(Y, rows, cols, tol = 1e-12,
                                    max_iter = 20000L) {
  rows <- pmax(0, as.numeric(rows))
  cols <- pmax(0, as.numeric(cols))
  total <- sum(rows)
  if (total <= 0) return(Y * 0)
  # Exact equal totals (both margins are displays of one parent).
  cols <- if (sum(cols) > 0) cols * (total / sum(cols)) else
    rep(total / length(cols), length(cols))
  # Zero-margin rows/columns are fixed at zero. Removing them is the exact
  # same Euclidean problem and avoids slow alternating-projection tails on
  # sparse public age/year domains.
  live_rows <- rows > 0
  live_cols <- cols > 0
  if (!all(live_rows) || !all(live_cols)) {
    out <- Y * 0
    out[live_rows, live_cols] <- .dsomopReleaseTransport(
      Y[live_rows, live_cols, drop = FALSE], rows[live_rows], cols[live_cols],
      tol = tol, max_iter = max_iter)
    return(out)
  }
  scale <- max(1, total)
  X <- Y
  P <- Y * 0
  Q <- Y * 0
  for (it in seq_len(max_iter)) {
    Z <- .dsomopReleaseSimplexRows(X + P, rows)
    P <- X + P - Z
    Xn <- t(.dsomopReleaseSimplexRows(t(Z + Q), cols))
    Q <- Z + Q - Xn
    done <- max(abs(rowSums(Xn) - rows)) <= tol * scale &&
      max(abs(Xn - X)) <= tol * scale
    X <- Xn
    if (done) break
  }
  if (max(abs(rowSums(X) - rows), abs(colSums(X) - cols)) > 1e-8 * scale) {
    # A feasible display always exists for nonnegative margins with one
    # total. Preserve the iterative answer where it converged; otherwise
    # normalize rows (including an empty positive-target row) and repair
    # columns while leaving those row totals fixed.
    X <- pmax(X, 0)
    rs <- rowSums(X)
    live <- rs > 0
    X[live, ] <- X[live, , drop = FALSE] * (rows[live] / rs[live])
    if (any(!live)) X[!live, ] <- outer(rows[!live], cols / total)
    X <- .dsomopReleaseRepairColumns(X, cols)
    if (max(abs(rowSums(X) - rows), abs(colSums(X) - cols)) > 1e-8 * scale) {
      stop("Canonical hierarchy projection did not converge.", call. = FALSE)
    }
  }
  X
}

#' Two-way additive least-squares reconciliation
#'
#' The orthogonal projection of a signed table onto the tables with the given
#' (mutually consistent) row and column sums:
#' \code{X + (r - X 1)/J + (c - X^T 1)/I - (total - sum X)/(I J)}.
#'
#' @param X Numeric matrix.
#' @param rows,cols Signed margins with equal sums.
#' @return Numeric matrix.
#' @keywords internal
.dsomopReleaseReconcileTwoWay <- function(X, rows, cols) {
  I <- nrow(X)
  J <- ncol(X)
  X + (as.numeric(rows) - rowSums(X)) / J +
    rep((as.numeric(cols) - colSums(X)) / I, each = I) -
    (sum(rows) - sum(X)) / (I * J)
}

# --- The incidence person-time tree -----------------------------------------------

# Public node domains of the incidence tree.
.dsomopReleaseC6TreeDomains <- function() {
  genders <- .dsomopToolsGenders()
  ages <- seq.int(0L, 110L, by = 5L)
  years <- seq.int(1970L, .dsomopToolsReferenceYear())
  list(
    gender = list(keys = paste0("gender=", genders$gender_concept_id),
                  ids = genders$gender_concept_id,
                  labels = genders$gender_name),
    age5 = list(keys = c(paste0("age5=", ages), "age5=unknown"),
                values = c(ages, NA_integer_)),
    year = list(keys = c("year=before_1970", paste0("year=", years),
                         "year=unknown"),
                labels = c("before 1970", as.character(years), "unknown")))
}

#' Public tree keys of a person-time substrate
#'
#' Gender (the public gender domain, other values as concept 0), the 5-year
#' age atom (0 to 110+, negative or missing ages unknown) and the calendar
#' year (before 1970, 1970 to the reference year, later or missing unknown).
#'
#' @param gender,age,year Raw values (age in whole years).
#' @return Data frame of keys (gender, age5, year).
#' @keywords internal
.dsomopReleaseC6TreeKeys <- function(gender, age, year) {
  year_max <- .dsomopToolsReferenceYear()
  a <- suppressWarnings(as.integer(age))
  y <- suppressWarnings(as.integer(year))
  data.frame(
    gender = .omopReleaseGenderKey(gender),
    age5 = as.character(ifelse(is.na(a) | a < 0L, "age5=unknown",
                  paste0("age5=", pmin((a %/% 5L) * 5L, 110L)))),
    year = as.character(ifelse(is.na(y) | y > year_max, "year=unknown",
                  ifelse(y < 1970L, "year=before_1970", paste0("year=", y)))),
    stringsAsFactors = FALSE)
}

#' Build the canonical person-time tree of one population
#'
#' Root -> gender -> {5-year age atom, calendar year} -> age x year (per
#' gender). Every node owns its components with the established C6 owners and
#' allocations: the person total (eligible-person owner, 0.2e, not drawn at
#' the crossed level), the first-outcome status pair (0.3e), the observed time
#' sum (0.5e, each person clipped to 3650 days) and the capped recurrent
#' outcome count (0.3e, at most K = 10 events per person, sensitivity K).
#' Signed children are reconciled to their reconciled signed parents and
#' displayed children projected onto their parents' displayed values
#' (the bounded simplex at tree levels; per-status transportation problems at
#' the crossed level, whose person totals are event plus no-event). Only the
#' requested levels are computed; a level never depends on a deeper one.
#'
#' @param policy DP policy.
#' @param protection Protected route.
#' @param owner Owner (population, status_owner, time_owner, recurrent_owner,
#'   id, estimand).
#' @param sub Data frame: person_id, gender, age5, year (tree keys), event
#'   (0/1), time (days), recurrent (capped event count).
#' @param levels Subset of c("gender", "age", "year", "age_year").
#' @return List: \code{nodes} (one data frame per computed level with signed
#'   and displayed values), \code{owner}, \code{levels}, \code{protection}.
#' @keywords internal
.dsomopReleaseBuildC6Tree <- function(policy, protection, owner, sub,
                                      levels = character(0)) {
  keys <- .dsomopReleaseKeys(policy)
  frac <- .DSOMOP_RELEASE_FRACTIONS
  T_cap <- .DSOMOP_RELEASE_TIME_CAP
  K <- .DSOMOP_RELEASE_RECURRENT_CAP
  needed <- c("person_id", "gender", "age5", "year", "event", "time",
              "recurrent")
  if (!is.data.frame(sub) || !all(needed %in% names(sub))) {
    stop("A person-time substrate lacks its columns.", call. = FALSE)
  }
  dom <- .dsomopReleaseC6TreeDomains()
  sub <- sub[!is.na(sub$person_id), , drop = FALSE]
  if (anyDuplicated(sub$person_id)) {
    stop("A person-time partition lists one person in two strata.",
         call. = FALSE)
  }
  if (!all(sub$gender %in% dom$gender$keys) ||
      !all(sub$age5 %in% dom$age5$keys) || !all(sub$year %in% dom$year$keys)) {
    stop("A person-time substrate has a stratum outside its public domain.",
         call. = FALSE)
  }
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  sub$event <- as.integer(!is.na(sub$event) & as.numeric(sub$event) > 0)
  sub$time <- pmin(T_cap, pmax(0, floor(suppressWarnings(
    as.numeric(sub$time)))))
  sub$time[is.na(sub$time)] <- 0
  r <- floor(suppressWarnings(as.numeric(sub$recurrent)))
  r[is.na(r)] <- 0
  sub$recurrent <- pmin(K, pmax(0, r))
  tokens <- if (nrow(sub)) .dsomopReleasePersonTokens(keys, policy,
                                                      sub$person_id) else
    character(0)
  status_owner <- owner$status_owner %||% owner$id
  time_owner <- owner$time_owner %||% owner$id
  recurrent_owner <- owner$recurrent_owner %||% owner$id
  draw <- function(family, coordinate, value, allocation, sensitivity,
                   idx, contributions, owner_id) {
    fp <- .dsomopReleaseFingerprint(keys, owner_id, family, coordinate,
                                    tokens[idx], contributions)
    .dsomopReleaseNoisyComponent(policy, keys, protection, list(
      owner = owner_id, population = owner$population, family = family,
      coordinate = coordinate, value = value, allocation = allocation,
      sensitivity = sensitivity, fingerprint = fp))
  }
  # The raw components of one node (rows idx of the substrate).
  node <- function(key, idx, with_n = TRUE) {
    coord <- paste0("stratum=", key)
    ev <- sub$event[idx]
    tm <- sub$time[idx]
    rc <- sub$recurrent[idx]
    c(N = if (with_n) draw("N", coord, length(idx), frac$N, 1, idx,
                            rep(1, length(idx)),
                            .DSOMOP_RELEASE_N_OWNER) else NA_real_,
      E = draw("status", paste0(coord, "|status=event"), sum(ev),
               frac$C6$status, 1, idx[ev == 1L], rep(1, sum(ev)),
               status_owner),
      NE = draw("status", paste0(coord, "|status=no_event"), sum(1L - ev),
                frac$C6$status, 1, idx[ev == 0L], rep(1, sum(1L - ev)),
                status_owner),
      T = draw("T", coord, sum(tm), frac$C6$time, T_cap, idx, tm,
               time_owner),
      R = draw("R", coord, sum(rc), .DSOMOP_RELEASE_RECURRENT_FRACTION, K,
               idx[rc > 0], rc[rc > 0], recurrent_owner))
  }
  all_idx <- seq_len(nrow(sub))
  root_raw <- node("root", all_idx)
  pair <- c(root_raw[["E"]], root_raw[["NE"]])
  pair <- pair + (root_raw[["N"]] - sum(pair)) / 2
  root_n_d <- max(0, root_raw[["N"]])
  root_status_d <- .dsomopReleaseSimplex(pair, root_n_d)
  root_t_d <- min(max(root_raw[["T"]], 0), root_n_d * T_cap)
  root_r_d <- min(max(root_raw[["R"]], root_status_d[[1L]]),
                  K * root_status_d[[1L]])
  root <- data.frame(
    key = "root", parent = NA_character_,
    N_s = root_raw[["N"]], E_s = pair[[1L]], NE_s = pair[[2L]],
    T_s = root_raw[["T"]], R_s = root_raw[["R"]],
    N_d = root_n_d, E_d = root_status_d[[1L]], NE_d = root_status_d[[2L]],
    T_d = root_t_d, R_d = root_r_d, stringsAsFactors = FALSE)
  # Children of one parent row: reconciled signed, projected display.
  children <- function(parent, child_keys, member) {
    raw <- do.call(rbind, lapply(child_keys, function(k) {
      node(k, which(member == k))
    }))
    out <- .dsomopReleaseC6Children(raw, parent)
    data.frame(key = child_keys, parent = parent$key, out,
               stringsAsFactors = FALSE)
  }
  out <- list(root = root)
  need_gender <- length(levels) > 0L
  if (need_gender) {
    out$gender <- children(root, dom$gender$keys, sub$gender)
  }
  per_gender <- function(child_dim) {
    do.call(rbind, lapply(seq_len(nrow(out$gender)), function(i) {
      g <- out$gender[i, , drop = FALSE]
      ck <- paste(g$key, dom[[child_dim]]$keys, sep = "|")
      member <- ifelse(sub$gender == g$key,
                       paste(sub$gender, sub[[child_dim]], sep = "|"), NA)
      children(g, ck, member)
    }))
  }
  if (any(c("age", "age_year") %in% levels)) out$age <- per_gender("age5")
  if (any(c("year", "age_year") %in% levels)) out$year <- per_gender("year")
  if ("age_year" %in% levels) {
    out$age_year <- .dsomopReleaseC6TreeCrossed(out, sub, dom, node)
  }
  list(nodes = out, owner = owner, levels = names(out),
       protection = protection, recurrent_cap = K)
}

#' Reconcile the person-time children of one parent node
#'
#' Signed: person totals, time and recurrent counts by the uniform
#' least-squares shift to the signed parent, the status pairs by the two-way
#' additive adjustment to the children's totals and the parent's status pair.
#' Displayed: totals by the simplex projection onto the parent's displayed
#' total, events by the bounded simplex within \code{[0, total]} onto the
#' parent's displayed events (no events is the remainder), time within
#' \code{[0, total x 3650]}, recurrent counts within
#' \code{[events, 10 x events]}. Every bound set is feasible because the
#' parent satisfies the same bounds.
#'
#' @param raw Matrix with columns N, E, NE, T and optionally R (raw draws).
#' @param parent List or one-row frame: N_s, E_s, NE_s, T_s, (R_s), N_d, E_d,
#'   NE_d, T_d, (R_d).
#' @return Data frame: N_s, E_s, NE_s, T_s, R_s, N_d, E_d, NE_d, T_d, R_d.
#' @keywords internal
.dsomopReleaseC6Children <- function(raw, parent) {
  T_cap <- .DSOMOP_RELEASE_TIME_CAP
  K <- .DSOMOP_RELEASE_RECURRENT_CAP
  raw <- matrix(raw, ncol = ncol(raw), dimnames = list(NULL, colnames(raw)))
  n <- nrow(raw)
  has_r <- "R" %in% colnames(raw) && is.finite(parent$R_s %||% NA_real_)
  N_s <- raw[, "N"] + (parent$N_s - sum(raw[, "N"])) / n
  st <- .dsomopReleaseReconcileTwoWay(cbind(raw[, "E"], raw[, "NE"]),
                                      N_s, c(parent$E_s, parent$NE_s))
  T_s <- raw[, "T"] + (parent$T_s - sum(raw[, "T"])) / n
  N_d <- .dsomopReleaseSimplex(N_s, parent$N_d)
  E_d <- .dsomopReleaseBoundedSimplex((st[, 1L] + N_d - st[, 2L]) / 2,
                                      parent$E_d, 0, N_d)
  T_d <- .dsomopReleaseBoundedSimplex(T_s, parent$T_d, 0, N_d * T_cap)
  R_s <- R_d <- rep(NA_real_, n)
  if (has_r) {
    R_s <- raw[, "R"] + (parent$R_s - sum(raw[, "R"])) / n
    R_d <- .dsomopReleaseBoundedSimplex(R_s, parent$R_d, E_d, K * E_d)
  }
  data.frame(N_s = N_s, E_s = st[, 1L], NE_s = st[, 2L], T_s = T_s,
             R_s = R_s, N_d = N_d, E_d = E_d, NE_d = N_d - E_d, T_d = T_d,
             R_d = R_d)
}

#' The crossed age x year level of the incidence tree
#'
#' Per gender, the age x year cells of each status, of the time and of the
#' capped recurrent count are reconciled to two parent margins (the gender's
#' age atoms and calendar years): signed by the two-way additive
#' least-squares adjustment, displayed by the transportation projection onto
#' the displayed margins. Person totals are event plus no-event, so every
#' display problem is feasible. Time and recurrent displays are nonnegative
#' and add up to both margins; their per-cell coherence bounds hold at the
#' tree levels only.
#'
#' @keywords internal
.dsomopReleaseC6TreeCrossed <- function(out, sub, dom, node) {
  ages <- dom$age5$keys
  years <- dom$year$keys
  do.call(rbind, lapply(seq_len(nrow(out$gender)), function(i) {
    g <- out$gender$key[[i]]
    in_g <- which(sub$gender == g)
    cell <- paste(sub$age5[in_g], sub$year[in_g], sep = "|")
    grid <- expand.grid(age = ages, year = years, stringsAsFactors = FALSE,
                        KEEP.OUT.ATTRS = FALSE)
    raw <- do.call(rbind, lapply(seq_len(nrow(grid)), function(j) {
      key <- paste(grid$age[[j]], grid$year[[j]], sep = "|")
      node(paste(g, key, sep = "|"), in_g[cell == key], with_n = FALSE)
    }))
    ra <- out$age[out$age$parent == g, , drop = FALSE]
    ry <- out$year[out$year$parent == g, , drop = FALSE]
    signed <- function(col, margin) {
      .dsomopReleaseReconcileTwoWay(
        matrix(raw[, col], nrow = length(ages)),
        ra[[margin]], ry[[margin]])
    }
    shown <- function(X, margin) {
      .dsomopReleaseTransport(X, ra[[margin]], ry[[margin]])
    }
    E_s <- signed("E", "E_s"); NE_s <- signed("NE", "NE_s")
    T_s <- signed("T", "T_s"); R_s <- signed("R", "R_s")
    E_d <- shown(E_s, "E_d"); NE_d <- shown(NE_s, "NE_d")
    T_d <- shown(T_s, "T_d"); R_d <- shown(R_s, "R_d")
    data.frame(key = paste(g, grid$age, grid$year, sep = "|"), parent = g,
               N_s = as.numeric(E_s + NE_s), E_s = as.numeric(E_s),
               NE_s = as.numeric(NE_s), T_s = as.numeric(T_s),
               R_s = as.numeric(R_s), N_d = as.numeric(E_d + NE_d),
               E_d = as.numeric(E_d), NE_d = as.numeric(NE_d),
               T_d = as.numeric(T_d), R_d = as.numeric(R_d),
               stringsAsFactors = FALSE)
  }))
}

#' Levels of the incidence tree a stratification needs
#'
#' @param by_gender,by_age,by_year Requested dimensions.
#' @return Character vector of levels (empty for the unstratified root).
#' @keywords internal
.dsomopReleaseC6TreeLevels <- function(by_gender, by_age, by_year) {
  if (by_age && by_year) return("age_year")
  if (by_age) return("age")
  if (by_year) return("year")
  if (by_gender) return("gender")
  character(0)
}

#' One requested stratification of the incidence tree
#'
#' The view's cells are sums of the nodes of the one level it needs (over
#' gender when gender is not requested, over the 5-year atoms of a coarser age
#' band), for signed and displayed values alike, so every view is consistent
#' with every other view of the same tree.
#'
#' @param tree From \code{.dsomopReleaseBuildC6Tree}.
#' @param by_gender,by_age,by_year Requested dimensions.
#' @param band Age band in years (5, 10 or 20).
#' @return List: \code{cells} (key, label, signed and displayed values) and
#'   \code{export} (client-compatible C6 components plus the hierarchy).
#' @keywords internal
.dsomopReleaseC6TreeView <- function(tree, by_gender, by_age, by_year,
                                     band = 10L) {
  dom <- .dsomopReleaseC6TreeDomains()
  level <- .dsomopReleaseC6TreeLevels(by_gender, by_age, by_year)
  level <- if (length(level)) level else "root"
  nodes <- tree$nodes[[level]]
  parts <- strsplit(nodes$key, "|", fixed = TRUE)
  gender_of <- vapply(parts, function(p) p[startsWith(p, "gender=")][1L] %||%
                        NA_character_, character(1L))
  age_of <- vapply(parts, function(p) {
    a <- p[startsWith(p, "age5=")]
    if (length(a)) a[[1L]] else NA_character_
  }, character(1L))
  year_of <- vapply(parts, function(p) {
    y <- p[startsWith(p, "year=")]
    if (length(y)) y[[1L]] else NA_character_
  }, character(1L))
  bands <- seq.int(0L, 110L, by = band)
  band_of <- function(a5) {
    v <- suppressWarnings(as.integer(sub("^age5=", "", a5)))
    ifelse(is.na(v), "age=unknown",
           paste0("age=", pmin((v %/% band) * band, max(bands))))
  }
  view_key <- rep("all", nrow(nodes))
  pieces <- list()
  if (by_gender) pieces$gender <- gender_of
  if (by_age) pieces$age <- band_of(age_of)
  if (by_year) pieces$year <- year_of
  if (length(pieces)) view_key <- do.call(paste, c(pieces, sep = "|"))
  # The view's cells in the public product order of its dimensions.
  dims <- list()
  if (by_gender) {
    dims$gender <- list(keys = dom$gender$keys, labels = dom$gender$labels)
  }
  if (by_age) {
    dims$age <- list(keys = c(paste0("age=", bands), "age=unknown"),
                     labels = c(as.character(bands), "unknown"))
  }
  if (by_year) {
    dims$year <- list(keys = dom$year$keys, labels = dom$year$labels)
  }
  grid <- if (length(dims)) expand.grid(lapply(dims, function(d) {
    seq_along(d$keys)
  }), KEEP.OUT.ATTRS = FALSE) else NULL
  if (is.null(grid)) {
    keys <- "all"
    labels <- "all"
  } else {
    keys <- apply(grid, 1L, function(idx) paste(vapply(seq_along(dims),
      function(j) dims[[j]]$keys[[idx[[j]]]], character(1L)), collapse = "|"))
    labels <- apply(grid, 1L, function(idx) paste(vapply(seq_along(dims),
      function(j) dims[[j]]$labels[[idx[[j]]]], character(1L)),
      collapse = "|"))
  }
  cols <- c("N_s", "E_s", "NE_s", "T_s", "R_s", "N_d", "E_d", "NE_d", "T_d",
            "R_d")
  if (!all(view_key %in% keys)) {
    stop("A person-time view has a cell outside its public domain.",
         call. = FALSE)
  }
  sums <- rowsum(as.matrix(nodes[cols]), group = view_key, reorder = FALSE)
  sums <- sums[match(keys, rownames(sums)), , drop = FALSE]
  sums[is.na(sums)] <- 0
  cells <- data.frame(key = unname(keys), label = unname(labels),
                      unname(sums), stringsAsFactors = FALSE,
                      row.names = NULL)
  names(cells) <- c("key", "label", cols)
  list(cells = cells, level = level,
       view = list(by_gender = by_gender, by_age = by_age, by_year = by_year,
                   band = as.integer(band),
                   view_of_nodes = stats::setNames(lapply(keys, function(k) {
                     which(view_key == k)
                   }), keys)))
}

#' Public label component of a person-time tree view
#'
#' The union of the components actually drawn: per computed level its person
#' totals (the root and the tree levels, keyed by population), status pair,
#' time and capped recurrent count, with sensitivities and noise SDs.
#'
#' @keywords internal
.dsomopReleaseC6TreeLabel <- function(tree, e) {
  frac <- .DSOMOP_RELEASE_FRACTIONS
  T_cap <- .DSOMOP_RELEASE_TIME_CAP
  K <- tree$recurrent_cap
  level_names <- c(root = "root", gender = "gender",
                   age = "gender|age5", year = "gender|year",
                   age_year = "gender|age5|year")
  alloc <- list()
  sens <- list()
  for (lv in tree$levels) {
    tag <- level_names[[lv]]
    if (!identical(lv, "age_year")) {
      nm <- if (identical(lv, "root")) "N" else paste0("N_strata:", tag)
      alloc[[nm]] <- frac$N
      sens[[nm]] <- 1
    }
    alloc[[paste0("status:", tag)]] <- frac$C6$status
    sens[[paste0("status:", tag)]] <- 1
    alloc[[paste0("time:", tag)]] <- frac$C6$time
    sens[[paste0("time:", tag)]] <- T_cap
    alloc[[paste0("recurrent:", tag)]] <- .DSOMOP_RELEASE_RECURRENT_FRACTION
    sens[[paste0("recurrent:", tag)]] <- K
  }
  sd <- stats::setNames(lapply(names(alloc), function(f) {
    .dsomopReleaseNoiseSd(e * alloc[[f]], sens[[f]])
  }), names(alloc))
  o <- tree$owner
  list(owner = o$id, estimand = o$estimand, family = "C6",
       population = o$population,
       status_owner = o$status_owner %||% o$id,
       time_owner = o$time_owner %||% o$id,
       recurrent_owner = o$recurrent_owner %||% o$id,
       hierarchy = .DSOMOP_RELEASE_C6_TREE_VERSION,
       levels = unname(level_names[tree$levels]),
       horizon_days = T_cap, contribution_cap = 1L,
       recurrent_cap = K,
       recurrent_estimand = paste0("capped recurrent outcome events (first ",
                                   K, " per person)"),
       allocation_fraction = alloc,
       allocation_epsilon = lapply(alloc, function(a) a * e),
       sensitivity = sens, noise_sd = sd)
}

# --- Nesting an owner's public strata under its root owner (Achilles) -------------

#' Nest a stratified release under the root release of the same population
#'
#' For an owner whose public strata partition a population that a root owner
#' also releases (Achilles 102/104/106 by gender and 107 by age decile under
#' 101/103/105), the strata's histogram cells reconcile to two parents: the
#' strata person totals (rows) and the root release's cells (columns), signed
#' by the two-way additive adjustment and displayed by the transportation
#' projection onto the displayed strata totals and root cells. Normalised
#' moment sums reconcile to the root's by the uniform shift; displayed
#' moments and quantiles are recomputed from the reconciled components. The
#' root release is drawn exactly as a root request draws it, so neither a
#' root nor a stratified request moves the other.
#'
#' @param child Built release of the stratified owner (public strata).
#' @param parent Built release of the root owner (same substrate, one stratum).
#' @return The child built with reconciled export, bins and strata.
#' @keywords internal
.dsomopReleaseNestUnderRoot <- function(child, parent) {
  st <- child$strata
  G <- nrow(st)
  if (G < 1L) return(child)
  comps <- child$export$components
  pcomp <- parent$export$components[[1L]]
  owner <- child$owner
  J <- .DSOMOP_RELEASE_LATTICE
  support <- .dsomopReleaseSupport()
  if (!is.null(comps[[1L]]$h) && !is.null(pcomp$h) &&
      length(pcomp$h) == length(comps[[1L]]$h)) {
    X <- do.call(rbind, lapply(comps, function(c) as.numeric(c$h)))
    Xs <- .dsomopReleaseReconcileTwoWay(X, st$n_signed, pcomp$h)
    D <- .dsomopReleaseTransport(Xs, st$n_display, parent$bins[["root"]])
    for (i in seq_len(G)) {
      comps[[i]]$h <- Xs[i, ]
      child$bins[[st$stratum[[i]]]] <- D[i, ]
    }
    usable <- st$n_display >= max(2, support$dist) & st$dist_ok
    if (is.null(owner$cells) && !is.null(owner$grid)) {
      q <- lapply(seq_len(G), function(i) {
        if (!usable[[i]]) return(rep(NA_real_, 5L))
        .dsomopReleaseQuantiles(D[i, ], owner$grid)
      })
      child$quantile_bounds <- .dsomopReleaseQuantileBoundsList(
        child$bins, owner$grid, q, st$stratum)
      for (k in seq_along(c("p10", "p25", "median", "p75", "p90"))) {
        col <- c("p10", "p25", "median", "p75", "p90")[[k]]
        st[[col]] <- .dsomopReleaseRound(vapply(q, `[[`, numeric(1L), k))
      }
    }
  }
  if (!is.null(comps[[1L]]$s) && !is.null(pcomp$s) && !is.null(owner$bounds)) {
    S <- vapply(comps, function(c) as.numeric(c$s), numeric(1L))
    Q <- vapply(comps, function(c) as.numeric(c$q), numeric(1L))
    S <- S + (pcomp$s - sum(S)) / G
    Q <- Q + (pcomp$q - sum(Q)) / G
    L <- owner$bounds[[1L]]; U <- owner$bounds[[2L]]
    half <- (U - L) / 2; mid <- (U + L) / 2
    usable <- st$n_display >= max(2, support$dist) & st$dist_ok
    st$mean <- NA_real_
    st$sd <- NA_real_
    for (i in seq_len(G)) {
      comps[[i]]$s <- S[[i]]
      comps[[i]]$q <- Q[[i]]
      if (!usable[[i]]) next
      N <- st$n_display[[i]]
      pm <- .dsomopReleaseProjectMoments(S[[i]] / J, Q[[i]] / J, N)
      st$mean[[i]] <- mid + half * pm[["S"]] / N
      st$sd[[i]] <- sqrt(max(0, (pm[["Q"]] - pm[["S"]]^2 / N) / (N - 1))) *
        half
    }
    st$mean <- .dsomopReleaseRound(st$mean)
    st$sd <- .dsomopReleaseRound(st$sd)
  }
  child$strata <- st
  child$export$components <- comps
  child$export$parent_owner <- parent$owner$id
  child$export$parent_basis <- parent$export
  child$export$reconciliation <- paste0(
    "strata_N_sum_to_root_N; cells two-way reconciled to strata N and the ",
    "root owner's cells; moment sums to the root owner's")
  child
}

# --- Subgroup families of CohortMethod arms (interaction) -----------------------

#' Reconcile a complete subgroup family to its arms
#'
#' Each node of a complete disjoint family (a level, or a 5-year age atom of
#' an age family) is a C6 release of the two arms over its member persons;
#' the arms' own C6 release (the overall subgroup, the same components as the
#' effect estimate's arms) is the parent. Per arm the nodes are reconciled by
#' \code{.dsomopReleaseC6Children}. When \code{combine} is given, the
#' family's levels are sums of the reconciled atoms (signed and displayed), so
#' age families with different public breaks are views of the same atoms.
#'
#' @param parent C6 built of the two arms.
#' @param nodes List of C6 builts (same arm strata) of the family's nodes.
#' @param combine NULL, or a list of node-index vectors (one per level).
#' @param family_meta Public family identity, canonical atom keys and view keys.
#' @return List of C6 builts, one per level, with reconciled strata and export.
#' @keywords internal
.dsomopReleaseC6Family <- function(parent, nodes, combine = NULL,
                                   family_meta = NULL) {
  arms <- parent$strata$stratum
  pexp <- parent$export$components
  for (a in seq_along(arms)) {
    pst <- parent$strata[a, , drop = FALSE]
    p <- list(N_s = pst$n_signed, E_s = pexp[[a]]$status[[1L]],
              NE_s = pexp[[a]]$status[[2L]], T_s = pst$time_noisy,
              N_d = pst$n_display, E_d = pst$events_display,
              NE_d = pst$n_display - pst$events_display,
              T_d = pst$time_display)
    raw <- do.call(rbind, lapply(nodes, function(b) {
      r <- b$strata[b$strata$stratum == arms[[a]], , drop = FALSE]
      c(N = r$n_noisy, E = r$event_noisy, NE = r$no_event_noisy,
        T = r$time_noisy)
    }))
    rec <- .dsomopReleaseC6Children(raw, p)
    for (k in seq_along(nodes)) {
      i <- which(nodes[[k]]$strata$stratum == arms[[a]])
      nodes[[k]]$strata$n_signed[[i]] <- rec$N_s[[k]]
      nodes[[k]]$strata$n_display[[i]] <- .dsomopReleaseRound(rec$N_d[[k]])
      nodes[[k]]$strata$events_display[[i]] <-
        .dsomopReleaseRound(rec$E_d[[k]])
      nodes[[k]]$strata$time_display[[i]] <- .dsomopReleaseRound(rec$T_d[[k]])
      nodes[[k]]$export$components[[i]] <- list(
        stratum = arms[[a]], n = rec$N_s[[k]],
        status = c(rec$E_s[[k]], rec$NE_s[[k]]), time = rec$T_s[[k]])
    }
  }
  for (k in seq_along(nodes)) {
    nodes[[k]]$export$parent_owner <- parent$owner$id
    nodes[[k]]$export$reconciliation <- paste0(
      "subgroup nodes reconciled per arm to the arms' components ",
      "(uniform shifts; status two-way; displayed bounded simplex)")
  }
  basis <- .dsomopReleaseFamilyBasis(parent$export$components,
    lapply(nodes, function(b) b$export$components), combine, family_meta,
    "bounded-simplex-v1")
  out <- if (is.null(combine)) nodes else lapply(combine, function(idx) {
    out <- nodes[[idx[[1L]]]]
    for (a in seq_along(arms)) {
      i <- which(out$strata$stratum == arms[[a]])
      get <- function(col) sum(vapply(nodes[idx], function(b) {
        b$strata[[col]][b$strata$stratum == arms[[a]]]
      }, numeric(1L)))
      out$strata$n_signed[[i]] <- get("n_signed")
      out$strata$n_display[[i]] <- .dsomopReleaseRound(get("n_display"))
      out$strata$events_display[[i]] <-
        .dsomopReleaseRound(get("events_display"))
      out$strata$time_display[[i]] <- .dsomopReleaseRound(get("time_display"))
      comp <- lapply(nodes[idx], function(b) b$export$components[[i]])
      out$export$components[[i]] <- list(
        stratum = arms[[a]],
        n = sum(vapply(comp, `[[`, numeric(1L), "n")),
        status = Reduce(`+`, lapply(comp, `[[`, "status")),
        time = sum(vapply(comp, `[[`, numeric(1L), "time")))
    }
    out$export$atoms <- length(idx)
    out
  })
  for (i in seq_along(out)) {
    out[[i]]$export$family_basis <- c(basis, list(view_index = i))
  }
  out
}

# Complete signed interaction atoms are retained before view coarsening.
# These public names identify the same partition across sites; no membership
# identifiers or raw sufficient statistics are exported.
.dsomopReleaseFamilyBasis <- function(arms, atoms, combine, meta, projection) {
  keys <- meta$atom_keys %||% paste0("atom=", seq_along(atoms))
  views <- combine %||% lapply(seq_along(atoms), function(i) i)
  view_keys <- meta$view_keys %||% paste0("view=", seq_along(views))
  stopifnot(length(keys) == length(atoms), !anyDuplicated(keys),
            length(view_keys) == length(views), !anyDuplicated(view_keys),
            identical(sort(as.integer(unlist(views))), seq_along(atoms)))
  named <- function(x) stats::setNames(x, vapply(x, `[[`, character(1L), "stratum"))
  list(version = "interaction-family-v1", projection = projection,
       family_id = meta$family_id %||% list(kind = "explicit-partition", atoms = keys),
       horizon_days = .DSOMOP_RELEASE_TIME_CAP,
       arms = named(arms), atoms = stats::setNames(lapply(atoms, named), keys),
       view_of_atoms = stats::setNames(lapply(views, as.integer), view_keys))
}

#' Export and built object of a person-time tree view
#'
#' A client-compatible C6 export (one component per view cell: signed person
#' total, status pair, time, plus the capped recurrent count) with the
#' hierarchy metadata, and the label component.
#'
#' @keywords internal
.dsomopReleaseC6TreeBuilt <- function(tree, view, policy) {
  cells <- view$cells
  o <- tree$owner
  frac <- .DSOMOP_RELEASE_FRACTIONS
  root <- tree$nodes$root
  stratified <- !identical(view$level, "root")
  components <- lapply(seq_len(nrow(cells)), function(i) {
    list(stratum = cells$key[[i]], n = cells$N_s[[i]],
         status = c(cells$E_s[[i]], cells$NE_s[[i]]),
         time = cells$T_s[[i]], recurrent = cells$R_s[[i]])
  })
  label <- .dsomopReleaseC6TreeLabel(tree, policy$release_epsilon)
  list(
    owner = c(o, list(family = "C6")), protection = tree$protection,
    label_component = label,
    export = list(
      protocol = .DSOMOP_RELEASE_PROTOCOL, owner = o$id,
      status_owner = o$status_owner %||% o$id,
      time_owner = o$time_owner %||% o$id,
      recurrent_owner = o$recurrent_owner %||% o$id,
      population = o$population, family = "C6",
      strata_kind = "public",
      root = if (stratified) list(n = root$N_s[[1L]]) else NULL,
      horizon_days = .DSOMOP_RELEASE_TIME_CAP, cap = 1L,
      recurrent_cap = tree$recurrent_cap,
      hierarchy = c(list(version = .DSOMOP_RELEASE_C6_TREE_VERSION,
                         levels = label$levels, view_level = view$level,
                         display = .DSOMOP_RELEASE_TRANSPORT_VERSION,
                         horizon_days = .DSOMOP_RELEASE_TIME_CAP,
                         recurrent_cap = tree$recurrent_cap), view$view),
      hierarchy_nodes = lapply(tree$nodes, function(nodes) {
        nodes[c("key", "parent", "N_s", "E_s", "NE_s", "T_s", "R_s")]
      }),
      allocation = label$allocation_fraction,
      reconciliation = paste0(
        "one canonical tree root -> gender -> {age5, year} -> age5 x year: ",
        "signed children reconciled to their signed parents, displayed ",
        "children projected onto their displayed parents; view cells are ",
        "sums of one level's nodes"),
      components = components))
}

# Public allocation union for a complete family: its atoms form one vector
# (sensitivity one), in addition to the parent vector. A coarse age view
# reports the same atom allocation. Repeated rows do not spend it again.
.dsomopReleaseFamilyLabel <- function(built, e, family_kind, kind) {
  o <- built$owner
  frac <- .DSOMOP_RELEASE_FRACTIONS
  tag <- paste0("family:", family_kind)
  alloc <- if (identical(kind, "C6")) {
    stats::setNames(list(frac$N, frac$C6$status, frac$C6$time),
                    paste0(c("N_strata:", "status:", "time:"), tag))
  } else stats::setNames(list(.DSOMOP_RELEASE_SURVIVAL_FRACTIONS$C8),
                         paste0("H:", tag))
  sens <- lapply(names(alloc), function(n) {
    if (startsWith(n, "time:")) .DSOMOP_RELEASE_TIME_CAP else 1
  })
  names(sens) <- names(alloc)
  list(owner = o$id, population = o$population, estimand = o$estimand,
       family = kind, hierarchy = tag, contribution_cap = 1L,
       allocation_fraction = alloc,
       allocation_epsilon = lapply(alloc, function(a) a * e),
       sensitivity = sens,
       noise_sd = stats::setNames(lapply(names(alloc), function(n) {
         .dsomopReleaseNoiseSd(e * alloc[[n]], sens[[n]])
       }), names(alloc)))
}
