# Module: Protected time-to-event histograms (dsOMOP 2.10.3, review R07)
#
# char.time_to_event releases outcome events by day offset from the cohort
# index. Two protected estimands, distinctly labelled:
#
#   * recurrent (first_occurrence_only = 0, the default, as on the standard
#     route): every qualifying outcome event of every cohort episode, as
#     (event, episode) occurrences at their day offset; one whole-person cap
#     keeps each person's first K = 10 occurrences in the public order (event
#     date, event key, episode start) before any bin is selected, so the
#     weighted event histogram has sensitivity K;
#   * first event (first_occurrence_only = 1): each person's first outcome
#     event of their first cohort episode, one atom per person.
#
# Both are views of one registered common signed-day basis
# (.DSOMOP_RELEASE_TTE_BASIS) with explicit tails, released as a three-level
# hierarchy drawn once per level: A = 365-day bins of [-3650, 3650) plus the
# tails (-Inf, -3650) and [3650, Inf); B = the atoms between consecutive
# multiples of 30 and of 365 inside [-3650, 3650) (each inside one A bin);
# C = single days inside [-365, 365) (each inside one B atom). A is reconciled
# to the parent total (the person total N for first events, the capped
# occurrence total R for recurrent events), every B atom to its A bin and every
# C day to its B atom, signed (uniform shift) and displayed (simplex
# projection onto the parent's displayed value). The 1-, 30- and 365-day
# scales are sums of displayed nodes, so they never redraw the same events
# with fresh noise and always add up: a 365-day bin is one A node, a 30-day bin
# its B atoms, a day its C atom.

.DSOMOP_RELEASE_TTE_BASIS <- "tte-signed-day-basis-v1"
.DSOMOP_RELEASE_TTE_SPAN <- 3650L
.DSOMOP_RELEASE_TTE_FINE <- 365L

# Static allocations of the reference epsilon (N is the shared eligible-person
# owner; every level is drawn once, so the levels compose).
.DSOMOP_RELEASE_TTE_FRACTIONS <- list(
  first = list(N = 0.2, A = 0.8 / 3, B = 0.8 / 3, C = 0.8 / 3),
  recurrent = list(N = 0.2, R = 0.2, A = 0.2, B = 0.2, C = 0.2)
)

#' The common signed-day basis of the time-to-event histograms
#'
#' Three public levels with stable interval identities (see the module
#' header). Every node has a key \code{"<level>:<interval>"}, a lower and
#' upper edge and, below A, the index of its parent node.
#'
#' @return List: \code{A}, \code{B}, \code{C} (data frames: key, lower, upper,
#'   parent) and \code{id}.
#' @keywords internal
.dsomopReleaseTteBasis <- function() {
  T <- .DSOMOP_RELEASE_TTE_SPAN
  F <- .DSOMOP_RELEASE_TTE_FINE
  fmt <- function(x) format(x, scientific = FALSE, trim = TRUE)
  interval <- function(lower, upper) {
    ifelse(is.infinite(lower), paste0("(-Inf, ", fmt(upper), ")"),
           ifelse(is.infinite(upper), paste0("[", fmt(lower), ", Inf)"),
                  paste0("[", fmt(lower), ", ", fmt(upper), ")")))
  }
  a_edges <- seq.int(-T, T, by = 365L)
  a <- data.frame(lower = c(-Inf, a_edges[-length(a_edges)], T),
                  upper = c(-T, a_edges[-1L], Inf))
  a$key <- paste0("A:", interval(a$lower, a$upper))
  a$parent <- NA_integer_
  b_edges <- sort(unique(c(a_edges,
                           30L * seq.int(ceiling(-T / 30), floor(T / 30)))))
  b_edges <- b_edges[b_edges >= -T & b_edges <= T]
  b <- data.frame(lower = b_edges[-length(b_edges)], upper = b_edges[-1L])
  b$key <- paste0("B:", interval(b$lower, b$upper))
  b$parent <- findInterval(b$lower, c(-Inf, a_edges))
  c_days <- seq.int(-F, F - 1L)
  cc <- data.frame(lower = c_days, upper = c_days + 1L)
  cc$key <- paste0("C:", interval(cc$lower, cc$upper))
  cc$parent <- findInterval(cc$lower, b$lower)
  stopifnot(all(b$lower >= a$lower[b$parent] & b$upper <= a$upper[b$parent]),
            all(cc$lower >= b$lower[cc$parent] &
                  cc$upper <= b$upper[cc$parent]))
  list(id = .DSOMOP_RELEASE_TTE_BASIS, A = a, B = b, C = cc)
}

#' View bins of one time scale over the common basis
#'
#' \code{scale = 365}: the A nodes; \code{scale = 30}: the 30-day bins
#' \code{[30 j, 30 j + 30)} inside \code{[-3650, 3650)} (the two edge bins are
#' 20 days long) as sums of B atoms, and the A tails; \code{scale = 1}: the
#' days of \code{[-365, 365)} (C atoms), with the tails \code{(-Inf, -365)}
#' and \code{[365, Inf)} as sums of A nodes.
#'
#' @param basis From \code{.dsomopReleaseTteBasis}.
#' @param scale 1, 30 or 365.
#' @return List: \code{levels} (levels to draw) and \code{bins} (list:
#'   \code{day_offset} (lower edges), \code{upper}, \code{level} and
#'   \code{nodes}, the node indices of every bin).
#' @keywords internal
.dsomopReleaseTteView <- function(basis, scale) {
  a <- basis$A
  b <- basis$B
  T <- .DSOMOP_RELEASE_TTE_SPAN
  F <- .DSOMOP_RELEASE_TTE_FINE
  bins <- list(day_offset = numeric(0), upper = numeric(0),
               level = character(0), nodes = list())
  add <- function(bins, lower, upper, level, nodes) {
    bins$day_offset <- c(bins$day_offset, lower)
    bins$upper <- c(bins$upper, upper)
    bins$level <- c(bins$level, level)
    bins$nodes <- c(bins$nodes, list(as.integer(nodes)))
    bins
  }
  if (scale == 365L) {
    for (i in seq_len(nrow(a))) bins <- add(bins, a$lower[[i]], a$upper[[i]],
                                           "A", i)
    return(list(levels = "A", bins = bins))
  }
  if (scale == 30L) {
    bins <- add(bins, -Inf, -T, "A", 1L)
    for (j in seq.int(floor(-T / 30), ceiling(T / 30) - 1L)) {
      lower <- max(30L * j, -T)
      upper <- min(30L * j + 30L, T)
      bins <- add(bins, lower, upper, "B",
                  which(b$lower >= lower & b$upper <= upper))
    }
    bins <- add(bins, T, Inf, "A", nrow(a))
    return(list(levels = c("A", "B"), bins = bins))
  }
  cc <- basis$C
  bins <- add(bins, -Inf, -F, "A", which(a$upper <= -F))
  for (i in seq_len(nrow(cc))) bins <- add(bins, cc$lower[[i]], cc$upper[[i]],
                                          "C", i)
  bins <- add(bins, F, Inf, "A", which(a$lower >= F))
  list(levels = c("A", "B", "C"), bins = bins)
}

# Node index of each value on one level (NA outside the level's range).
.dsomopReleaseTteNodeOf <- function(values, nodes) {
  i <- findInterval(values, nodes$lower)
  i[i < 1L | values >= nodes$upper[pmax(i, 1L)]] <- NA_integer_
  i
}

#' Owner of a protected time-to-event histogram
#'
#' @param outcome_key Framed outcome definition
#'   (\code{.omopReleaseOutcomeKey}).
#' @param recurrent Logical: the capped recurrent estimand.
#' @return Owner list (family "TTE").
#' @keywords internal
.dsomopReleaseTteOwner <- function(outcome_key, recurrent) {
  K <- .DSOMOP_RELEASE_MULTI_CAP
  if (isTRUE(recurrent)) {
    id <- paste0("characterization/recurrent-outcome-day-offset/",
                 .dsomopFrame("v1", outcome_key, K, .DSOMOP_RELEASE_TTE_BASIS))
    estimand <- paste0(
      "capped recurrent outcome events by day offset from each cohort ",
      "episode's index: every qualifying event of every episode, each ",
      "person's first ", K, " occurrences (by event date, event key, episode ",
      "start) across all episodes and bins; a view of the common signed-day ",
      "basis ", .DSOMOP_RELEASE_TTE_BASIS, " with explicit tails (-Inf, -3650) ",
      "and [3650, Inf); persons per bin are not released for this estimand")
    population <- paste0("characterization/outcome-persons/all-episodes/",
                         outcome_key)
  } else {
    id <- paste0("characterization/first-outcome-day-offset/",
                 .dsomopFrame("v2", outcome_key, .DSOMOP_RELEASE_TTE_BASIS))
    estimand <- paste0(
      "day offset of each person's first outcome event from the index of ",
      "their first cohort episode (one event per person); a view of the ",
      "common signed-day basis ", .DSOMOP_RELEASE_TTE_BASIS, " with explicit ",
      "tails (-Inf, -3650) and [3650, Inf)")
    population <- paste0("characterization/outcome-persons/first-episode/",
                         outcome_key)
  }
  list(id = id, population = population, family = "TTE",
       recurrent = isTRUE(recurrent), cap = if (isTRUE(recurrent)) K else 1L,
       allocation = if (isTRUE(recurrent)) .DSOMOP_RELEASE_TTE_FRACTIONS$recurrent
         else .DSOMOP_RELEASE_TTE_FRACTIONS$first,
       estimand = estimand, strata = list(kind = "root"))
}

#' Build, noise and display a protected time-to-event view
#'
#' Draws the person total N, for recurrent events the capped occurrence total
#' R, and the basis levels the requested scale needs (A for 365 days, A and B
#' for 30 days, A, B and C for one day); every node's noise is keyed on its
#' basis level and interval, never on the requested scale, so a node has the
#' same draw in every view. Signed nodes are reconciled top-down to their
#' parents, displayed nodes projected top-down onto their parents' displayed
#' values; the view bins are sums of displayed nodes.
#'
#' @param policy DP policy.
#' @param protection Protected route.
#' @param owner From \code{.dsomopReleaseTteOwner}.
#' @param sub Occurrences: person_id, v (day offset); at most K rows per
#'   person for recurrent events, one for first events.
#' @param scale 1, 30 or 365.
#' @return List: owner, protection, strata, view (data frame: day_offset,
#'   events), levels (signed and displayed nodes), export.
#' @keywords internal
.dsomopReleaseBuildTte <- function(policy, protection, owner, sub, scale) {
  keys <- .dsomopReleaseKeys(policy)
  support <- .dsomopReleaseSupport()
  K <- max(1L, as.integer(owner$cap))
  frac <- owner$allocation
  sub <- sub[!is.na(sub$person_id) & is.finite(sub$v), , drop = FALSE]
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  per_person <- tabulate(match(sub$person_id, unique(sub$person_id)))
  if (length(per_person) && max(per_person) > K) {
    stop("A protected time-to-event substrate exceeds its whole-person cap.",
         call. = FALSE)
  }
  persons <- unique(sub$person_id)
  if (length(persons) < support$floor) .dsomopReleaseDisclosive()
  pid <- match(sub$person_id, persons)
  tokens <- .dsomopReleasePersonTokens(keys, policy, persons)
  # One component from the person indices of its records (each record one
  # unit of its person's contribution).
  noisy <- function(family, coordinate, record_persons, allocation,
                    sensitivity, owner_id = owner$id) {
    who <- sort(unique(record_persons))
    w <- tabulate(match(record_persons, who), length(who))
    fp <- .dsomopReleaseFingerprint(keys, owner_id, family, coordinate,
                                    tokens[who], w)
    .dsomopReleaseNoisyComponent(policy, keys, protection, list(
      owner = owner_id, population = owner$population, family = family,
      coordinate = coordinate, value = sum(w), allocation = allocation,
      sensitivity = sensitivity, fingerprint = fp))
  }
  n_noisy <- noisy("N", "stratum=root", seq_along(persons), frac$N, 1,
                   owner_id = .DSOMOP_RELEASE_N_OWNER)
  r_noisy <- if (isTRUE(owner$recurrent)) {
    noisy("R", "stratum=root", pid, frac$R, K)
  } else NA_real_
  parent_signed <- if (isTRUE(owner$recurrent)) r_noisy else n_noisy
  parent_display <- max(0, parent_signed)
  basis <- .dsomopReleaseTteBasis()
  view <- .dsomopReleaseTteView(basis, as.integer(scale))
  family <- if (isTRUE(owner$recurrent)) "W" else "H"
  levels <- list()
  for (level in view$levels) {
    nodes <- basis[[level]]
    node <- .dsomopReleaseTteNodeOf(sub$v, nodes)
    if (identical(level, "A")) {
      # A covers every value: the tails are its first and last nodes.
      node <- findInterval(sub$v, c(-Inf, nodes$lower[-1L]))
    }
    by_node <- split(pid, factor(node, levels = seq_len(nrow(nodes))))
    raw <- vapply(seq_len(nrow(nodes)), function(k) {
      noisy(family, paste0("basis=", basis$id, "|", nodes$key[[k]]),
            by_node[[k]], frac[[level]], K)
    }, numeric(1L))
    if (identical(level, "A")) {
      signed <- raw + (parent_signed - sum(raw)) / length(raw)
      display <- .dsomopReleaseSimplex(raw, parent_display)
    } else {
      up <- levels[[if (identical(level, "B")) "A" else "B"]]
      signed <- raw
      display <- numeric(length(raw))
      for (p in unique(nodes$parent)) {
        kids <- which(nodes$parent == p)
        signed[kids] <- raw[kids] + (up$signed[[p]] - sum(raw[kids])) /
          length(kids)
        display[kids] <- .dsomopReleaseSimplex(raw[kids], up$display[[p]])
      }
    }
    levels[[level]] <- list(keys = nodes$key, signed = signed,
                            display = display)
  }
  bins <- view$bins
  events <- vapply(seq_along(bins$level), function(i) {
    sum(levels[[bins$level[[i]]]]$display[bins$nodes[[i]]])
  }, numeric(1L))
  export_levels <- lapply(levels, function(l) {
    list(keys = l$keys, signed = l$signed)
  })
  component <- c(list(stratum = "root", n = n_noisy),
                 if (isTRUE(owner$recurrent)) list(r = r_noisy),
                 # First events partition persons: the A level is an ordinary
                 # person histogram reconciled to N; recurrent cells are
                 # weighted (sum to R, not N) and never pooled as h.
                 if (!isTRUE(owner$recurrent)) list(h = levels$A$signed),
                 list(levels = export_levels))
  list(
    owner = owner, protection = protection,
    strata = data.frame(stratum = "root", n_display = max(0, n_noisy),
                        stringsAsFactors = FALSE),
    view = data.frame(day_offset = bins$day_offset,
                      events = .dsomopReleaseRound(events)),
    export = list(
      protocol = .DSOMOP_RELEASE_PROTOCOL, owner = owner$id,
      population = owner$population, family = "TTE",
      estimand_kind = if (isTRUE(owner$recurrent)) "recurrent" else "first",
      basis = basis$id, levels = view$levels, cap = K,
      allocation = frac[c("N", if (isTRUE(owner$recurrent)) "R",
                          view$levels)],
      strata_kind = "root",
      cell_keys = if (!isTRUE(owner$recurrent)) levels$A$keys else NULL,
      reconciliation = paste0(
        "A nodes sum to ", if (isTRUE(owner$recurrent)) "R" else "N",
        "; B atoms to their A bin; C days to their B atom"),
      components = list(component)))
}

#' Public label component of a protected time-to-event view
#'
#' @param built Output of \code{.dsomopReleaseBuildTte}.
#' @param epsilon Reference epsilon.
#' @keywords internal
.dsomopReleaseTteLabel <- function(built, epsilon) {
  o <- built$owner
  K <- max(1L, as.integer(o$cap))
  alloc <- built$export$allocation
  sens <- lapply(names(alloc), function(f) if (identical(f, "N")) 1 else K)
  names(sens) <- names(alloc)
  sd <- lapply(names(alloc), function(f) {
    .dsomopReleaseNoiseSd(epsilon * alloc[[f]], sens[[f]])
  })
  names(sd) <- names(alloc)
  list(owner = o$id, estimand = o$estimand, family = "TTE",
       population = o$population,
       estimand_kind = built$export$estimand_kind,
       basis = built$export$basis, basis_levels = built$export$levels,
       contribution_cap = K,
       cap_scope = if (isTRUE(o$recurrent)) {
         "whole_person_occurrences_across_episodes_and_bins"
       } else "one_event_per_person",
       allocation_fraction = alloc,
       allocation_epsilon = lapply(alloc, function(a) a * epsilon),
       sensitivity = sens, noise_sd = sd,
       views = paste0(
         "time_scale 365: A nodes; 30: 30-day bins [30 j, 30 j + 30) of ",
         "[-3650, 3650) (edge bins 20 days) as sums of B atoms; 1: days of ",
         "[-365, 365) (C atoms) with tails (-Inf, -365) and [365, Inf) as sums ",
         "of A nodes; every view is a sum of displayed nodes of one draw"))
}

#' Protected run of char.time_to_event
#'
#' Honours \code{first_occurrence_only}: 0 (the default) releases the capped
#' recurrent estimand, 1 the first event per person (see the module header).
#' The output keeps the standard columns: \code{day_offset} (each bin's lower
#' edge; \code{-Inf} for the lower tail), \code{num_events} and
#' \code{persons} (first events: equal to the events; recurrent events: NA,
#' since persons per bin are not additive over the common basis).
#'
#' @keywords internal
.omopReleaseRunTimeToEvent <- function(handle, ctx, params, route, snap) {
  if (is.null(ctx$scoped_cohort)) return(list(df = data.frame(), built = list()))
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  domain <- params$domain_code %||% "0"
  out_src <- .omopOutcomeSource(handle, params$outcome_concept_id, domain)
  if (is.null(out_src)) return(list(df = data.frame(), built = list()))
  scale <- as.integer(params$time_scale %||% "1")
  if (!scale %in% c(1L, 30L, 365L)) scale <- 1L
  recurrent <- !identical(as.character(params$first_occurrence_only %||% "0"),
                          "1")
  .omopDiagAssertPersons(handle, ctx, out_src$table, "o", out_src$person_col,
                         where_sql = paste0("o.", out_src$concept_col,
                                            " IN (", out_src$id_list, ")"))
  K <- .DSOMOP_RELEASE_MULTI_CAP
  if (isTRUE(recurrent)) {
    event_key <- .eventPrimaryKeyColumn(.buildBlueprint(handle),
                                        out_src$table_name)
    if (is.null(event_key)) {
      stop("Time-to-event analysis requires a stable OMOP event key.",
           call. = FALSE)
    }
    offset <- .omopDateDiffDays(handle, paste0("o.", out_src$date_col),
                                "c.cohort_start_date")
    occurrences <- paste0(
      "SELECT DISTINCT c.subject_id AS person_id, o.", event_key,
      " AS event_id, o.", out_src$date_col, " AS event_date, ",
      "c.cohort_start_date AS episode_start, ", offset, " AS v FROM ",
      cohort, " c INNER JOIN ", out_src$table, " o ON o.",
      out_src$person_col, " = c.subject_id AND o.", out_src$concept_col,
      " IN (", out_src$id_list, ")")
    raw <- .omopReleaseFetch(handle, paste0(
      "SELECT r.person_id, r.v FROM (SELECT d.person_id, d.v, ROW_NUMBER() ",
      "OVER (PARTITION BY d.person_id ORDER BY d.event_date, d.event_id, ",
      "d.episode_start) AS rn FROM (", occurrences, ") d) r WHERE r.rn <= ",
      K))
  } else {
    first <- .omopFirstCohortEpisodeSql(handle, cohort)
    offset <- .omopDateDiffDays(handle, paste0("MIN(o.", out_src$date_col, ")"),
                                "c.cohort_start_date")
    raw <- .omopReleaseFetch(handle, paste0(
      "SELECT c.subject_id AS person_id, ", offset, " AS v FROM ", first,
      " c INNER JOIN ", out_src$table, " o ON o.", out_src$person_col,
      " = c.subject_id AND o.", out_src$concept_col, " IN (", out_src$id_list,
      ") GROUP BY c.subject_id, c.cohort_start_date"))
  }
  owner <- .dsomopReleaseTteOwner(.omopReleaseOutcomeKey(
    params$outcome_concept_id, domain), recurrent)
  sub <- data.frame(person_id = raw$person_id %||% character(0),
                    v = suppressWarnings(as.numeric(raw$v %||% numeric(0))),
                    stringsAsFactors = FALSE)
  built <- .dsomopReleaseBuildTte(route$policy, route$protection, owner, sub,
                                  scale)
  df <- data.frame(day_offset = as.numeric(built$view$day_offset),
                   num_events = built$view$events,
                   persons = if (isTRUE(recurrent)) NA_real_ else
                     built$view$events)
  list(df = df, built = list(built),
       extra = list(time_scale = scale,
                    estimand_kind = if (isTRUE(recurrent)) "recurrent" else
                      "first",
                    persons_unavailable = if (isTRUE(recurrent)) paste0(
                      "persons per bin are not additive over the common ",
                      "signed-day basis; first_occurrence_only = 1 releases ",
                      "persons by first-event offset")))
}
