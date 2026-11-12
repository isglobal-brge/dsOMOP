# WS-5 acceptance at the stream level (design-2.10.3 sections 10.7 and 12.2):
# the hand-computed WS-0 oracle expectations of the acceptance recipes that
# depend on intervals and coverage (02, 06, 07, 08, 09, 11, 15) reproduced from
# the WS-5 fragments -- eras, recorded / effective relations, coverage
# fragments, observed segments, anchors and windows -- with only the output
# assembly (period grids, survival exits) done by a light harness here; the
# output compilers themselves are WS-7's. On every backend available.

.ws5_acc_backends <- .ws5_backends()
withr::defer(.ws4_cleanup(.ws5_acc_backends), testthat::teardown_env())

# Roster of an index population on a results cohort (select all, event_end),
# with the population's coverage policy from the IR.
.ws5_acc_roster <- function(ctx, pop, cohort_definition_id) {
  cand <- .ws5_candidates(ctx, cohort_definition_id = cohort_definition_id, name = paste0("cand_", pop))
  cov <- .compileCoverage(ctx, pop, ctx$ir$populations[[pop]]$coverage, cand, "candidate")
  roster <- .ws5_roster(ctx, cand, cov, pop = pop)
  rows <- .fetchFragment(ctx, roster, "cohort_row_id")
  rows$person_id <- as.numeric(rows$subject_id)
  rows$index_date <- format(rows$cohort_start_date, "%Y-%m-%d")
  list(cand = cand, cov = cov, roster = roster, rows = rows)
}

.ws5_values <- function(ctx, acc, fn, ...) {
  v <- .intervalReducerValues(ctx, fn, acc$roster, "final", cov = acc$cov, ...)
  v$value[match(as.numeric(acc$rows$cohort_row_id), as.numeric(v$row_key))]
}

# Rows of a fragment for the roster's persons (dates as Date).
.ws5_person_rows <- function(ctx, f, order = c("person_id", "start_date")) {
  r <- .fetchFragment(ctx, f, order)
  r$person_id <- as.numeric(r$person_id)
  r
}

.ws5_key <- function(df) paste(df$person_id, df$index_date)

test_that("recipe 06: coverage, persistence and discontinuation censoring reproduce the oracle", {
  plan <- .ws5_acceptance_plan("06-coverage-persistence")
  want <- .ws5_expected("06-coverage-persistence", "coverage")
  want_s <- .ws5_expected("06-coverage-persistence", "persistence")
  for (b in names(.ws5_acc_backends)) {
    h <- .ws5_acc_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    on.exit(.compileReleaseTempTables(ctx), add = TRUE)
    acc <- .ws5_acc_roster(ctx, "users", 1001L)
    expect_setequal(.ws5_key(acc$rows), .ws5_key(want))
    w90 <- ctx$ir$variables$covered_rec$reducer$window
    got <- data.frame(person_id = acc$rows$person_id, index_date = acc$rows$index_date,
                      covered_rec = .ws5_values(ctx, acc, "covered_days", source = list(interval = "metf_era"), window = w90),
                      covered_eff = .ws5_values(ctx, acc, "covered_days", source = list(interval = "metf_stock"),
                                                window = w90, coverage = "effective_supply"),
                      supply_start = .ws5_values(ctx, acc, "supply_days", source = list(event = "metf"), window = w90),
                      supply_clipped = .ws5_values(ctx, acc, "supply_days", source = list(event = "metf"), window = w90,
                                                   fills = "clipped"))
    # era_count / first_era_days: the course rows overlapping the window
    course <- .compileIntervalByName(ctx, "metf_era")$course
    st <- .compileIntervalStream(ctx, course, acc$roster, w90, "final", "str_acc06_course")
    s <- .fetchFragment(ctx, st, c("row_key", "start_date"))
    idx <- as.Date(acc$rows$cohort_start_date)
    lo <- idx + 1L
    hi <- idx + 90L
    key <- as.numeric(acc$rows$cohort_row_id)
    got$era_count <- vapply(seq_along(key), function(i) sum(as.numeric(s$row_key) == key[[i]]), integer(1))
    got$first_era_days <- vapply(seq_along(key), function(i) {
      e <- s[as.numeric(s$row_key) == key[[i]], , drop = FALSE]
      if (!nrow(e)) return(NA_integer_)
      as.integer(min(as.Date(e$end_date[1L]), hi[[i]]) - max(as.Date(e$start_date[1L]), lo[[i]]) + 1L)
    }, integer(1))
    got <- got[match(.ws5_key(want), .ws5_key(got)), names(want)]
    rownames(got) <- NULL
    expect_equal(got, want, ignore_attr = TRUE, info = b)
    # persistence: TAR [1, 365], censor at observation end, death and the first
    # era end after entry + 30 (the interval censor source)
    eras <- .ws5_person_rows(ctx, course)
    mi <- .ws5_person_rows(ctx, .compileEventByName(ctx, "mi"))
    death <- .ws5_person_rows(ctx, .compileEventByName(ctx, "death"))
    surv <- do.call(rbind, lapply(seq_len(nrow(acc$rows)), function(i) {
      p <- acc$rows$person_id[[i]]
      entry <- idx[[i]] + 1L
      exit <- min(idx[[i]] + 365L, as.Date(acc$rows$op_end[[i]]))
      dd <- as.Date(death$start_date[death$person_id == p])
      if (length(dd)) exit <- min(exit, dd[[1L]])
      ends <- as.Date(eras$end_date[eras$person_id == p])
      ends <- ends[ends >= entry]
      if (length(ends)) exit <- min(exit, min(ends) + 30L)
      m <- as.Date(mi$start_date[mi$person_id == p])
      m <- m[m >= entry & m <= exit]
      exit_days <- if (length(m)) as.integer(min(m) - idx[[i]]) else as.integer(exit - idx[[i]])
      data.frame(person_id = p, index_date = acc$rows$index_date[[i]], outcome_name = "mi",
                 event = as.integer(length(m) > 0L), entry_days_from_index = 1L,
                 exit_days_from_index = exit_days, follow_up_days = exit_days - 1L, stringsAsFactors = FALSE)
    }))
    surv <- surv[match(.ws5_key(want_s), .ws5_key(surv)), names(want_s)]
    rownames(surv) <- NULL
    expect_equal(surv, want_s, ignore_attr = TRUE, info = b)
    # the pins named by the recipe: 1004 recorded 44 vs effective 60 on [1, 90]
    p1004 <- got[got$person_id == 1004, ]
    expect_identical(as.integer(c(p1004$covered_rec, p1004$covered_eff)), c(44L, 60L), info = b)
  }
})

test_that("recipe 15: adherence (MPR and PDC bases over union coverage) reproduces the oracle", {
  plan <- .ws5_acceptance_plan("15-costs-adherence")
  want <- .ws5_expected("15-costs-adherence", "adherence")
  for (b in names(.ws5_acc_backends)) {
    h <- .ws5_acc_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    on.exit(.compileReleaseTempTables(ctx), add = TRUE)
    acc <- .ws5_acc_roster(ctx, "users", 1001L)
    expect_identical(acc$cov$policy, list(policy = "union", on_ambiguous = "drop", on_missing = "drop", gap = 30L))
    expect_setequal(.ws5_key(acc$rows), .ws5_key(want))
    v <- ctx$ir$variables
    r <- function(name) v[[name]]$reducer
    got <- data.frame(person_id = acc$rows$person_id, index_date = acc$rows$index_date)
    got$supply_90 <- .ws5_values(ctx, acc, "supply_days", source = v$supply_90$source, window = r("supply_90")$window)
    for (nm in c("observed_90", "observed_10")) {
      got[[nm]] <- .ws5_values(ctx, acc, "observed_days", window = v[[nm]]$derived$window)
    }
    for (nm in c("pdc_num_eff_90", "pdc_num_rec_90", "covered_eff_90", "covered_rec_90", "pdc_num_eff_10",
                 "covered_eff_10")) {
      got[[nm]] <- .ws5_values(ctx, acc, "covered_days", source = v[[nm]]$source, window = r(nm)$window,
                               coverage = r(nm)$coverage, observed_only = isTRUE(r(nm)$observed_only))
    }
    cols <- intersect(names(want), names(got))
    got <- got[match(.ws5_key(want), .ws5_key(got)), cols]
    rownames(got) <- NULL
    expect_equal(got, want[, cols], ignore_attr = TRUE, info = b)
    # the PDC pin: 1003 gives 4/4 on [1, 10] while its effective coverage is 10
    p <- got[got$person_id == 1003, ]
    expect_identical(as.integer(c(p$pdc_num_eff_10, p$observed_10, p$covered_eff_10)), c(4L, 4L, 10L), info = b)
  }
})

# Calendar-period grid of a person-period output over [L, U] (the oracle's
# assembly), from the roster's coverage bounds and observed segments.
.ws5_periods <- function(starts, ends, lo, hi) {
  ps <- pmax(starts, lo)
  pe <- pmin(ends, hi)
  keep <- ps <= pe
  data.frame(ps = ps[keep], pe = pe[keep])
}

test_that("recipe 07: the dense monthly panel grid follows the coverage fragments", {
  plan <- .ws5_acceptance_plan("07-monthly-panel")
  want <- .ws5_expected("07-monthly-panel", "panel")
  for (b in names(.ws5_acc_backends)) {
    h <- .ws5_acc_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    acc <- .ws5_acc_roster(ctx, "c1", 1L)
    seg <- .fetchFragment(ctx, .compileObservedSegments(ctx, acc$cov, acc$roster, "final"), c("row_key", "start_date"))
    months_s <- as.Date(c("2019-10-01", "2019-11-01", "2019-12-01", "2020-01-01", "2020-02-01"))
    months_e <- as.Date(c("2019-10-31", "2019-11-30", "2019-12-31", "2020-01-31", "2020-02-29"))
    got <- do.call(rbind, lapply(seq_len(nrow(acc$rows)), function(i) {
      idx <- as.Date(acc$rows$cohort_start_date[[i]])
      s <- seg[as.numeric(seg$row_key) == as.numeric(acc$rows$cohort_row_id[[i]]), , drop = FALSE]
      lo <- max(idx - 92L, as.Date(acc$rows$op_start[[i]]))
      hi <- min(idx + 59L, as.Date(acc$rows$op_end[[i]]))
      per <- .ws5_periods(months_s, months_e, lo, hi)
      if (!nrow(per)) return(NULL)
      # each period intersected with the actually observed segments
      days <- vapply(seq_len(nrow(per)), function(k) {
        .coveredDays(data.frame(key = 1, start = s$start_date, end = s$end_date),
                     data.frame(key = 1, lo = per$ps[[k]], hi = per$pe[[k]]))
      }, integer(1))
      data.frame(person_id = acc$rows$person_id[[i]], index_date = acc$rows$index_date[[i]],
                 period_start_days = as.integer(per$ps - idx), period_end_days = as.integer(per$pe - idx),
                 days = days)
    }))
    key <- function(d) paste(d$person_id, d$index_date, d$period_start_days)
    expect_setequal(key(got), key(want))
    got <- got[match(key(want), key(got)), ]
    expect_equal(got$period_end_days, want$period_end_days, info = b)
    expect_equal(got$days, want$days, info = b)
    # 1001 (ambiguous coverage) is dropped privately; 1002 keeps January and February only
    expect_false(1001 %in% got$person_id)
    expect_identical(sort(got$period_start_days[got$person_id == 1002]), c(0L, 31L), info = b)
  }
})

test_that("recipe 08: the SCCS partition at exposure eras and observed segments reproduces the oracle", {
  plan <- .ws5_acceptance_plan("08-sccs-matrix")
  want <- .ws5_expected("08-sccs-matrix", "matrix")
  for (b in names(.ws5_acc_backends)) {
    h <- .ws5_acc_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    acc <- .ws5_acc_roster(ctx, "sccs", 1002L)
    eras <- .ws5_person_rows(ctx, .compileIntervalByName(ctx, "aspirin_era")$course)
    mi <- .ws5_person_rows(ctx, .compileEventByName(ctx, "mi"))
    got <- do.call(rbind, lapply(seq_len(nrow(acc$rows)), function(i) {
      p <- acc$rows$person_id[[i]]
      idx <- as.Date(acc$rows$cohort_start_date[[i]])
      os <- as.Date(acc$rows$op_start[[i]])
      oe <- as.Date(acc$rows$op_end[[i]])
      lo <- max(idx, os)
      hi <- min(idx + 365L, oe, as.Date(acc$rows$cohort_end_date[[i]]))
      e <- eras[eras$person_id == p, , drop = FALSE]
      es <- as.Date(e$start_date)
      ee <- as.Date(e$end_date)
      bnd <- c(lo, hi + 1L, os, oe + 1L, es, ee + 1L)
      k <- 0L
      repeat { bs <- lo + 365L * k; if (bs > hi) break; bnd <- c(bnd, bs); k <- k + 1L }
      for (y in as.integer(format(lo, "%Y")):as.integer(format(hi + 1L, "%Y"))) bnd <- c(bnd, as.Date(sprintf("%d-01-01", y)))
      bnd <- sort(unique(bnd[bnd >= lo & bnd <= hi + 1L]))
      m <- as.Date(mi$start_date[mi$person_id == p])
      do.call(rbind, lapply(seq_len(length(bnd) - 1L), function(j) {
        ps <- max(bnd[[j]], os)
        pe <- min(bnd[[j + 1L]] - 1L, oe)
        if (ps > pe) return(NULL)
        w <- data.frame(key = 1, lo = ps, hi = pe)
        data.frame(person_id = p, index_date = acc$rows$index_date[[i]],
                   period_start_days = as.integer(ps - idx), period_end_days = as.integer(pe - idx),
                   days = as.integer(pe - ps + 1L), calendar_year = as.integer(format(ps, "%Y")),
                   exposed_aspirin = .coveredDays(data.frame(key = rep(1, length(es)), start = es, end = ee), w),
                   active_aspirin = as.integer(any(es <= ps & ee >= ps)),
                   n_mi = sum(m >= ps & m <= pe))
      }))
    }))
    key <- function(d) paste(d$person_id, d$index_date, d$period_start_days)
    expect_setequal(key(got), key(want))
    got <- got[match(key(want), key(got)), ]
    for (col in c("period_end_days", "days", "calendar_year", "exposed_aspirin", "active_aspirin", "n_mi")) {
      expect_equal(got[[col]], as.integer(want[[col]]), info = paste(b, col))
    }
  }
})

test_that("recipe 11: calendar-year incidence periods follow the coverage fragments", {
  plan <- .ws5_acceptance_plan("11-incidence-years")
  want <- .ws5_expected("11-incidence-years", "years")
  for (b in names(.ws5_acc_backends)) {
    h <- .ws5_acc_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    acc <- .ws5_acc_roster(ctx, "c1", 1L)
    resp <- .ws5_person_rows(ctx, .compileEventByName(ctx, "resp"))
    got <- do.call(rbind, lapply(seq_len(nrow(acc$rows)), function(i) {
      idx <- as.Date(acc$rows$cohort_start_date[[i]])
      lo <- max(idx, as.Date(acc$rows$op_start[[i]]))
      hi <- min(idx + 730L, as.Date(acc$rows$op_end[[i]]))
      years <- as.integer(format(lo, "%Y")):as.integer(format(hi, "%Y"))
      per <- .ws5_periods(as.Date(sprintf("%d-01-01", years)), as.Date(sprintf("%d-12-31", years)), lo, hi)
      ev <- as.Date(resp$start_date[resp$person_id == acc$rows$person_id[[i]]])
      data.frame(person_id = acc$rows$person_id[[i]], index_date = acc$rows$index_date[[i]],
                 period_start_days = as.integer(per$ps - idx), period_end_days = as.integer(per$pe - idx),
                 days = as.integer(per$pe - per$ps + 1L), calendar_year = as.integer(format(per$ps, "%Y")),
                 n_resp = vapply(seq_len(nrow(per)), function(k) sum(ev >= per$ps[[k]] & ev <= per$pe[[k]]), integer(1)))
    }))
    key <- function(d) paste(d$person_id, d$index_date, d$period_start_days)
    expect_setequal(key(got), key(want))
    got <- got[match(key(want), key(got)), names(want)]
    rownames(got) <- NULL
    expect_equal(got, want, ignore_attr = TRUE, info = b)
  }
})

test_that("recipe 09: recurrent-event risk sets end at the coverage bound or death", {
  plan <- .ws5_acceptance_plan("09-recurrent-events")
  want <- .ws5_expected("09-recurrent-events", "recurrent.risk_sets")
  for (b in names(.ws5_acc_backends)) {
    h <- .ws5_acc_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    acc <- .ws5_acc_roster(ctx, "c1", 1L)
    death <- .ws5_person_rows(ctx, .compileEventByName(ctx, "death"))
    got <- do.call(rbind, lapply(seq_len(nrow(acc$rows)), function(i) {
      idx <- as.Date(acc$rows$cohort_start_date[[i]])
      exit <- min(idx + 90L, as.Date(acc$rows$op_end[[i]]))
      dd <- as.Date(death$start_date[death$person_id == acc$rows$person_id[[i]]])
      if (length(dd)) exit <- min(exit, dd[[1L]])
      if (exit < idx + 1L) return(NULL)
      data.frame(person_id = acc$rows$person_id[[i]], index_date = acc$rows$index_date[[i]],
                 entry_days_from_index = 1L, exit_days_from_index = as.integer(exit - idx),
                 follow_up_days = as.integer(exit - idx) - 1L)
    }))
    expect_setequal(.ws5_key(got), .ws5_key(want))
    got <- got[match(.ws5_key(want), .ws5_key(got)), names(want)]
    rownames(got) <- NULL
    expect_equal(got, want, ignore_attr = TRUE, info = b)
  }
})

test_that("recipe 02: era arms, first-eligible candidates and interval ends reproduce the oracle", {
  plan <- .ws5_acceptance_plan("02-active-comparator")
  want_b <- .ws5_expected("02-active-comparator", "baseline")
  want <- list(itt = .ws5_expected("02-active-comparator", "tte.itt"),
               on_treatment = .ws5_expected("02-active-comparator", "tte.on_treatment"))
  for (b in names(.ws5_acc_backends)) {
    h <- .ws5_acc_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    arms <- c(acei = 1L, arb = 2L)
    eras <- do.call(rbind, lapply(names(arms), function(a) {
      r <- .ws5_person_rows(ctx, .compileIntervalByName(ctx, a)$course)
      data.frame(person_id = r$person_id, start = as.Date(r$start_date), end = as.Date(r$end_date),
                 arm = a, ord = arms[[a]], key = r$era_key, stringsAsFactors = FALSE)
    }))
    # candidate coverage lookup (unique / drop / drop) at each era start
    cand_tab <- eras[order(eras$person_id, eras$start, eras$ord, eras$key), ]
    cand_tab$cand_key <- seq_len(nrow(cand_tab))
    ops <- .ws5_person_rows(ctx, .coverageSource(ctx))
    covered <- function(p, d) {
      o <- ops[ops$person_id == p & as.Date(ops$start_date) <= d & as.Date(ops$end_date) >= d, , drop = FALSE]
      if (nrow(o) == 1L) o else NULL
    }
    mi <- .ws5_person_rows(ctx, .compileEventByName(ctx, "mi"))
    death <- .ws5_person_rows(ctx, .compileEventByName(ctx, "death"))
    base <- list()
    tte <- list(itt = list(), on_treatment = list())
    for (p in unique(cand_tab$person_id)) {
      cands <- cand_tab[cand_tab$person_id == p, , drop = FALSE]
      chosen <- NULL
      for (k in seq_len(nrow(cands))) {
        cv <- covered(p, cands$start[[k]])
        if (is.null(cv) || as.Date(cv$start_date) > cands$start[[k]] - 365L) next
        prior <- eras$person_id == p & eras$start >= cands$start[[k]] - 365L & eras$start <= cands$start[[k]] - 1L
        if (any(prior)) next
        chosen <- cands[k, ]
        chosen$op_end <- as.Date(cv$end_date)
        break
      }
      if (is.null(chosen)) next
      idx <- chosen$start
      base[[length(base) + 1L]] <- data.frame(person_id = p, index_date = format(idx), arm = chosen$arm)
      entry <- idx + 1L
      m <- as.Date(mi$start_date[mi$person_id == p])
      dd <- as.Date(death$start_date[death$person_id == p])
      if (any(m >= entry - 365L & m < entry) || any(dd >= entry - 365L & dd < entry)) next
      other <- eras[eras$person_id == p & eras$arm != chosen$arm, , drop = FALSE]
      switch_d <- other$start[other$start >= entry]
      index_end <- min(chosen$end, chosen$op_end)          # end = interval_end, capped at op_end
      for (tar in c("itt", "on_treatment")) {
        exit <- chosen$op_end
        if (length(switch_d)) exit <- min(exit, min(switch_d))
        if (tar == "on_treatment") exit <- min(exit, index_end + 30L)
        if (exit < entry) next
        ev <- rbind(data.frame(d = m, name = rep("mi", length(m)), pr = rep(1L, length(m))),
                    data.frame(d = dd, name = rep("death", length(dd)), pr = rep(2L, length(dd))))
        ev <- ev[ev$d >= entry & ev$d <= exit, , drop = FALSE]
        ev <- ev[order(ev$d, ev$pr), , drop = FALSE]
        exit_days <- if (nrow(ev)) as.integer(ev$d[[1L]] - idx) else as.integer(exit - idx)
        tte[[tar]][[length(tte[[tar]]) + 1L]] <- data.frame(
          person_id = p, index_date = format(idx), outcome_name = if (nrow(ev)) ev$name[[1L]] else NA_character_,
          event = as.integer(nrow(ev) > 0L), entry_days_from_index = 1L, exit_days_from_index = exit_days,
          follow_up_days = exit_days - 1L, stringsAsFactors = FALSE)
      }
    }
    base <- do.call(rbind, base)
    expect_setequal(.ws5_key(base), .ws5_key(want_b))
    base <- base[match(.ws5_key(want_b), .ws5_key(base)), ]
    expect_identical(base$arm, want_b$arm, info = b)
    for (tar in names(want)) {
      g <- do.call(rbind, tte[[tar]])
      w <- want[[tar]]
      expect_setequal(.ws5_key(g), .ws5_key(w))
      g <- g[match(.ws5_key(w), .ws5_key(g)), names(w)]
      rownames(g) <- NULL
      expect_equal(g, w, ignore_attr = TRUE, info = paste(b, tar))
    }
    # 1032: the ACE-inhibitor restart is its own arm; two distinct era identities
    k1032 <- eras$key[eras$person_id == 1032 & eras$arm == "acei"]
    expect_identical(sort(k1032), paste0("1:", 1:2), info = b)
  }
})

test_that("recipe 12: per-candidate coverage, the observed criterion and prior observation reproduce the oracle", {
  plan <- .ws5_acceptance_plan("12-circe-baseline")
  want <- .ws5_expected("12-circe-baseline", "baseline")
  for (b in names(.ws5_acc_backends)) {
    h <- .ws5_acc_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    ev <- .compileEventByName(ctx, "type_2_diabetes_dx")
    rn <- .sqlRowNumber(ctx, character(0), c("e.person_id", "e.start_date", "e.source_ns", "e.event_id"))
    cand <- .fragment("cand_t2dm_first", "candidates", "episode",
                      paste0("SELECT ", .sqlLosslessKey(ctx, rn), " AS cand_key, e.person_id AS subject_id, ",
                             "e.start_date AS cohort_start_date, e.end_date AS cohort_end_date FROM ", ev$name, " e"),
                      columns = list(cand_key = .fragmentColumn("bigint", FALSE, "episode_key"),
                                     subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                                     cohort_start_date = .fragmentColumn("date", FALSE, "start"),
                                     cohort_end_date = .fragmentColumn("date", TRUE, "end")),
                      dependencies = ev$name, unique_key = "cand_key",
                      person_lineage = list(person_col = "subject_id", via_roster = NULL),
                      private_columns = "cand_key")
    .fragmentRegister(ctx, cand)
    pop <- ctx$ir$populations$t2dm_first
    cov <- .compileCoverage(ctx, "t2dm_first", pop$coverage, cand, "candidate")
    observed <- pop$where$observed
    ok <- .coverageObservedSql(ctx, "o", "c.cohort_start_date", before = observed$before, after = observed$after)
    elig <- .fragment("q_elig", "result", "episode",
                      paste0("SELECT c.cand_key AS k, c.subject_id AS p, CASE WHEN ", .coverageKeepSql(cov$policy, "o"),
                             " AND ", ok, " THEN 1 ELSE 0 END AS ok FROM ", cand$name, " c INNER JOIN ",
                             cov$coverage$name, " o ON o.row_key = c.cand_key"),
                      columns = list(k = .fragmentColumn("bigint", FALSE, "episode_key"),
                                     p = .fragmentColumn("integer", FALSE, "person_key"),
                                     ok = .fragmentColumn("integer", FALSE, "measure")),
                      dependencies = c(cand$name, cov$coverage$name),
                      person_lineage = list(person_col = "p", via_roster = NULL), private_columns = "k")
    e <- .executeQuery(h, .buildStatement(ctx, elig, select = "SELECT * FROM q_elig ORDER BY k", dialect = ctx))
    e <- e[as.integer(e$ok) == 1L, ]
    chosen <- as.numeric(tapply(as.numeric(e$k), as.numeric(e$p), min))   # first_eligible
    roster <- .fragment("pop_t2dm_first", "roster", "episode",
                        paste0("SELECT c.cand_key AS cohort_row_id, c.subject_id AS subject_id, ",
                               "c.cohort_start_date AS cohort_start_date, o.op_end AS cohort_end_date, ",
                               "'type_2_diabetes_dx' AS index_source, ", .sqlLosslessKeyText(ctx, "c.cand_key"),
                               " AS index_key, o.op_start AS op_start, o.op_end AS op_end, ",
                               "o.component_id AS op_component_id FROM ", cand$name, " c INNER JOIN ",
                               cov$coverage$name, " o ON o.row_key = c.cand_key WHERE ",
                               .sqlIdInPredicate("c.cand_key", chosen)),
                        columns = list(cohort_row_id = .fragmentColumn("bigint", FALSE, "episode_key"),
                                       subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                                       cohort_start_date = .fragmentColumn("date", FALSE, "start"),
                                       cohort_end_date = .fragmentColumn("date", TRUE, "end"),
                                       index_source = .fragmentColumn("text", FALSE, "origin_identity"),
                                       index_key = .fragmentColumn("text", FALSE, "origin_identity"),
                                       op_start = .fragmentColumn("date", TRUE, "start"),
                                       op_end = .fragmentColumn("date", TRUE, "end"),
                                       op_component_id = .fragmentColumn("bigint", TRUE, "private")),
                        dependencies = c(cand$name, cov$coverage$name), unique_key = "cohort_row_id",
                        person_lineage = list(person_col = "subject_id", via_roster = NULL),
                        roster_namespace = "t2dm_first", private_columns = "op_component_id")
    .fragmentRegister(ctx, roster)
    rows <- .fetchFragment(ctx, roster, "cohort_row_id")
    got <- data.frame(person_id = as.numeric(rows$subject_id), index_date = format(rows$cohort_start_date, "%Y-%m-%d"))
    v <- .intervalReducerValues(ctx, "observed_days", roster, "final", window = ctx$ir$variables$prior_obs$derived$window,
                                cov = cov)
    got$prior_obs <- v$value[match(as.numeric(rows$cohort_row_id), as.numeric(v$row_key))]
    expect_setequal(.ws5_key(got), .ws5_key(want))
    got <- got[match(.ws5_key(want), .ws5_key(got)), ]
    expect_equal(got$prior_obs, want$prior_obs, info = b)
  }
})

test_that("the executable WS-5 acceptance tests ran on every configured backend", {
  .ws5_expect_backends(.ws5_acc_backends, "ws5 acceptance")
})
