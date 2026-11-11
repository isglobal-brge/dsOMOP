# =============================================================================
# WS-0: hand-computed expected results of the fifteen acceptance recipes
# (design-2.10.3 section 10.7) on the EXTENDED reach fixture.
#
# Every quantity below is computed with plain SQL over the fixture tables and
# base R, following the semantics stated in each recipe's README.md. Nothing
# here calls the dsOMOP engine. Run on a machine with RSQLite:
#
#   Rscript tools/ws0/acceptance_expected.R <package dir>
#
# Writes inst/plans/acceptance/<dir>/expected.json: for each output, the
# column names and the rows keyed by person_id and index_date (fixture
# identities; the harness tokenises them with the fixture's person key).
# =============================================================================
args <- commandArgs(TRUE)
pkg <- if (length(args) >= 1L) args[[1L]] else "."
source(file.path(pkg, "tests", "fixtures", "create_reach_db.R"))
source(file.path(pkg, "tests", "fixtures", "extend_reach_db.R"))
h <- build_extended_reach_handle(100L)
conn <- h$conn
q <- function(sql) DBI::dbGetQuery(conn, sql)
D <- function(x) as.Date(x)
nday <- function(from, to) as.integer(as.Date(to) - as.Date(from))
iso <- function(x) format(as.Date(x), "%Y-%m-%d")
out_dir <- file.path(pkg, "inst", "plans", "acceptance")
write_expected <- function(dir, outputs, notes = NULL) {
  obj <- list(recipe = dir, fixture = "reach + extension (build_extended_reach_handle(100))",
              nfilter_subset = 5L, outputs = outputs)
  if (!is.null(notes)) obj$notes <- notes
  json <- jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE)
  writeLines(json, file.path(out_dir, dir, "expected.json"))
  cat(dir, ":", paste(vapply(outputs, function(o) paste0(o$name, "=", nrow(o$rows)), ""), collapse = " "), "\n")
}
frame_out <- function(name, df) {
  rownames(df) <- NULL
  list(name = name, n_persons = length(unique(df$person_id)), columns = names(df), rows = df)
}

# ---- fixture accessors --------------------------------------------------------
persons <- q("SELECT person_id, gender_concept_id, year_of_birth FROM person")
ops_all <- q("SELECT person_id, observation_period_id, observation_period_start_date AS s, observation_period_end_date AS e FROM observation_period")
descendants <- function(ids) {
  rows <- q(paste0("SELECT descendant_concept_id FROM concept_ancestor WHERE ancestor_concept_id IN (",
                   paste(ids, collapse = ","), ")"))$descendant_concept_id
  unique(c(as.integer(ids), as.integer(rows)))
}
idlist <- function(ids) paste(ids, collapse = ",")
cohort_episodes <- function(cid) {
  ep <- q(paste0("SELECT subject_id AS person_id, cohort_start_date AS index_date, cohort_end_date AS end_date ",
                 "FROM cohort WHERE cohort_definition_id = ", cid, " ORDER BY subject_id, cohort_start_date, cohort_end_date"))
  ep
}
person_yob <- function(pid) persons$year_of_birth[match(pid, persons$person_id)]
person_sex <- function(pid) ifelse(persons$gender_concept_id[match(pid, persons$person_id)] == 8532L, "F", "M")
annual_age <- function(pid, date) {
  yob <- person_yob(pid); age <- as.integer(format(as.Date(date), "%Y")) - yob
  age[!is.na(age) & age < 0] <- NA_integer_
  age
}
# unique-coverage lookup: the single observation period containing `date`, else NA
covering <- function(pid, date) {
  o <- ops_all[ops_all$person_id == pid & ops_all$s <= date & ops_all$e >= date, , drop = FALSE]
  if (nrow(o) == 1L) o else NULL
}
# union-coverage (gap g): coalesced components of the person's periods
components <- function(pid, gap) {
  o <- ops_all[ops_all$person_id == pid, , drop = FALSE]
  o <- o[order(o$s, o$e), , drop = FALSE]
  comps <- list(); cur <- NULL
  for (i in seq_len(nrow(o))) {
    s <- D(o$s[i]); e <- D(o$e[i])
    if (is.null(cur) || s > cur$e + gap + 1) {
      if (!is.null(cur)) comps[[length(comps) + 1L]] <- cur
      cur <- list(s = s, e = e, rows = list(c(s, e)))
    } else {
      cur$e <- max(cur$e, e); cur$rows[[length(cur$rows) + 1L]] <- c(s, e)
    }
  }
  if (!is.null(cur)) comps[[length(comps) + 1L]] <- cur
  comps
}
# union length of closed day intervals (Date pairs) clipped to [lo, hi]
union_days <- function(intervals, lo, hi) {
  if (length(intervals) == 0L) return(0L)
  m <- do.call(rbind, lapply(intervals, function(iv) c(as.integer(max(iv[1], lo)), as.integer(min(iv[2], hi)))))
  m <- m[m[, 1] <= m[, 2], , drop = FALSE]
  if (nrow(m) == 0L) return(0L)
  m <- m[order(m[, 1]), , drop = FALSE]
  total <- 0L; cs <- m[1, 1]; ce <- m[1, 2]
  for (i in seq_len(nrow(m))[-1L]) {
    if (m[i, 1] > ce + 1L) { total <- total + (ce - cs + 1L); cs <- m[i, 1]; ce <- m[i, 2] } else ce <- max(ce, m[i, 2])
  }
  as.integer(total + (ce - cs + 1L))
}
intersect_days <- function(a, b) {   # list of Date pairs -> list of pairwise intersections
  res <- list()
  for (x in a) for (y in b) {
    s <- max(x[1], y[1]); e <- min(x[2], y[2]); if (s <= e) res[[length(res) + 1L]] <- c(s, e)
  }
  res
}
events_in <- function(table, concept_col, ids, date_col, pid, lo, hi, extra = "") {
  q(paste0("SELECT * FROM ", table, " WHERE person_id = ", pid, " AND ", concept_col, " IN (", idlist(ids), ")",
           " AND ", date_col, " >= '", iso(lo), "' AND ", date_col, " <= '", iso(hi), "'", extra))
}

# metformin fills normalised: recorded end, else start + supply - 1 (positive integer), else start;
# a row with end < start is dropped (data-quality policy); returns Date pairs in (start, id) order
fills_of <- function(pid, concept_ids) {
  f <- q(paste0("SELECT drug_exposure_id AS id, drug_exposure_start_date AS s, drug_exposure_end_date AS e, days_supply AS supply ",
                "FROM drug_exposure WHERE person_id = ", pid, " AND drug_concept_id IN (", idlist(concept_ids), ") ",
                "ORDER BY drug_exposure_start_date, drug_exposure_id"))
  if (nrow(f) == 0L) return(f)
  f$start <- D(f$s)
  f$end <- as.Date(ifelse(!is.na(f$e), f$e, NA_character_))
  sup_ok <- !is.na(f$supply) & f$supply >= 1 & f$supply == floor(f$supply)
  f$end[is.na(f$end) & sup_ok] <- f$start[is.na(f$end) & sup_ok] + f$supply[is.na(f$end) & sup_ok] - 1L
  f$end[is.na(f$end)] <- f$start[is.na(f$end)]
  f$supply_ok <- sup_ok
  f[f$end >= f$start, , drop = FALSE]
}
# gaps-and-islands eras (gap g) over closed intervals ordered by (start, id); returns list of (start, end, n)
eras_of <- function(starts, ends, gap) {
  if (length(starts) == 0L) return(list())
  o <- order(starts, seq_along(starts)); starts <- starts[o]; ends <- ends[o]
  res <- list(); cs <- starts[1]; ce <- ends[1]; n <- 1L
  for (i in seq_along(starts)[-1L]) {
    if (starts[i] > ce + gap + 1) { res[[length(res) + 1L]] <- list(s = cs, e = ce, n = n); cs <- starts[i]; ce <- ends[i]; n <- 1L }
    else { ce <- max(ce, ends[i]); n <- n + 1L }
  }
  res[[length(res) + 1L]] <- list(s = cs, e = ce, n = n)
  res
}
# stockpiled effective-supply intervals (cap 90): supply precedence positive days_supply,
# else recorded length, else one day; carry = min(cap, max(0, prev_eff_end - start + 1))
effective_of <- function(f, cap = 90L) {
  if (nrow(f) == 0L) return(list())
  prev_end <- NULL; res <- list()
  for (i in seq_len(nrow(f))) {
    supply <- if (f$supply_ok[i]) as.integer(f$supply[i]) else as.integer(f$end[i] - f$start[i] + 1L)
    carry <- if (is.null(prev_end)) 0L else min(cap, max(0L, as.integer(prev_end - f$start[i] + 1L)))
    eff_end <- f$start[i] + supply - 1L + carry
    res[[length(res) + 1L]] <- c(f$start[i], eff_end)
    prev_end <- if (is.null(prev_end)) eff_end else max(prev_end, eff_end)
  }
  res
}

# =============================================================================
# 01 characterisation
# =============================================================================
{
  ep <- cohort_episodes(1L)
  idx <- D(ep$index_date); lo <- idx - 365L
  COND <- c(dm = 201820, prediabetes = 201826, htn1 = 320128, htn2 = 316866, mi = 4329847, mi2 = 312327,
            ckd = 46271022, af = 313217, hf = 316139, hld = 432867, obesity = 433736, pregnancy = 4299535,
            stroke = 381316, stroke2 = 372924, sepsis = 132797, cirrhosis = 4064161, asthma = 317009,
            copd = 255573, chest_pain = 77670, resp_inf = 320136, prior_bleed = 192671, major_bleed = 4108832,
            malignancy = 4112853, chronic_pain = 436096, htn_comp = 321318, covar2 = 4000777)
  DRUG <- c(metformin = 1503297, insulin = 1596977, statin = 1539403, warfarin = 1310149, acei = 1308216,
            arb = 1367500, beta_blocker = 1346823, diuretic = 974166, doac = 40228152, sglt2 = 1488000)
  PROC <- c(dialysis = 4032243, pci = 4283095, bariatric = 4322471, cath = 4139525)
  binary_col <- function(table, concept_col, date_col, ids) {
    ids <- descendants(ids)
    hits <- q(paste0("SELECT DISTINCT person_id FROM ", table, " WHERE ", concept_col, " IN (", idlist(ids), ") ",
                     "AND ", date_col, " >= '2019-01-01' AND ", date_col, " <= '2020-01-01'"))$person_id
    as.integer(ep$person_id %in% hits)
  }
  res <- data.frame(person_id = ep$person_id, index_date = ep$index_date,
                    age = annual_age(ep$person_id, ep$index_date), sex = person_sex(ep$person_id), stringsAsFactors = FALSE)
  for (n in names(COND)) res[[paste0("cond_", n)]] <- binary_col("condition_occurrence", "condition_concept_id", "condition_start_date", COND[[n]])
  for (n in names(DRUG)) res[[paste0("drug_", n)]] <- binary_col("drug_exposure", "drug_concept_id", "drug_exposure_start_date", DRUG[[n]])
  for (n in names(PROC)) res[[paste0("proc_", n)]] <- binary_col("procedure_occurrence", "procedure_concept_id", "procedure_date", PROC[[n]])
  norm_a1c <- function(v, u) ifelse(!is.na(u) & u == 8554L, v, ifelse(!is.na(u) & u == 8753L, 0.0915 * v + 2.15, NA_real_))
  norm_gluc <- function(v, u) ifelse(!is.na(u) & u == 8840L, v, ifelse(!is.na(u) & u == 8753L, 18.0182 * v, NA_real_))
  last_norm <- function(pid, concept, normf) {
    m <- q(paste0("SELECT measurement_id AS id, measurement_date AS d, value_as_number AS v, unit_concept_id AS u FROM measurement ",
                  "WHERE person_id = ", pid, " AND measurement_concept_id = ", concept,
                  " AND measurement_date >= '2019-01-01' AND measurement_date <= '2020-01-01' ORDER BY measurement_date, measurement_id"))
    if (nrow(m) == 0L) return(NA_real_)
    m$nv <- normf(m$v, m$u); m <- m[!is.na(m$nv), , drop = FALSE]
    if (nrow(m) == 0L) NA_real_ else m$nv[nrow(m)]
  }
  res$hba1c_last <- vapply(ep$person_id, last_norm, numeric(1), concept = 3004410L, normf = norm_a1c)
  res$glucose_last <- vapply(ep$person_id, last_norm, numeric(1), concept = 3004501L, normf = norm_gluc)
  charlson <- list(mi = list(c(4329847L), 1L), chf = list(c(316139L), 1L), pvd = list(c(321052L), 1L),
                   cvd = list(c(381591L, 434056L), 1L), dementia = list(c(4182210L), 1L), copd = list(c(4063381L), 1L),
                   rheumatic = list(c(257628L, 134442L, 80800L, 80809L, 256197L, 255348L), 1L), pud = list(c(4247120L), 1L),
                   mild_liver = list(c(4212540L, 4064161L), 1L), dm_uncomp = list(c(201820L), 1L),
                   dm_comp = list(c(443767L, 442793L), 2L), hemiplegia = list(c(192606L, 374022L), 2L),
                   renal = list(c(4030518L), 2L), malignancy = list(c(443392L), 2L),
                   mod_liver = list(c(4245975L, 4029488L, 192680L, 24966L), 3L), metastatic = list(c(432851L), 6L),
                   aids = list(c(439727L), 6L))
  score <- integer(nrow(ep))
  for (cat in charlson) {
    ids <- descendants(cat[[1]])
    hits <- q(paste0("SELECT DISTINCT person_id FROM condition_occurrence WHERE condition_concept_id IN (", idlist(ids),
                     ") AND condition_start_date <= '2020-01-01'"))$person_id
    score <- score + cat[[2]] * as.integer(ep$person_id %in% hits)
  }
  res$charlson <- score
  write_expected("01-characterisation", list(table1 = frame_out("table1", res)))
}

# =============================================================================
# 02 active comparator
# =============================================================================
{
  era_rows <- function(concept) q(paste0("SELECT drug_era_id AS id, person_id, drug_era_start_date AS s, drug_era_end_date AS e ",
                                         "FROM drug_era WHERE drug_concept_id IN (", idlist(descendants(concept)), ") ORDER BY person_id, drug_era_start_date, drug_era_id"))
  acei <- era_rows(1308216L); arb <- era_rows(1367500L)
  arm_eras <- function(pid, rows) {
    r <- rows[rows$person_id == pid, , drop = FALSE]
    if (nrow(r) == 0L) return(list())
    eras_of(D(r$s), D(r$e), 30L)
  }
  mi_rows <- q(paste0("SELECT person_id, condition_start_date AS d FROM condition_occurrence WHERE condition_concept_id IN (", idlist(descendants(4329847L)), ")"))
  deaths <- q("SELECT person_id, death_date AS d FROM death")
  base_rows <- list(); tte_rows <- list()
  for (pid in sort(unique(c(acei$person_id, arb$person_id)))) {
    cand <- c(lapply(arm_eras(pid, acei), function(e) c(e, arm = "acei", ord = 1L)),
              lapply(arm_eras(pid, arb), function(e) c(e, arm = "arb", ord = 2L)))
    ord <- order(as.integer(vapply(cand, function(c) as.integer(c$s), 1)), vapply(cand, function(c) c$ord, 1))
    cand <- cand[ord]
    chosen <- NULL
    for (c in cand) {
      cov <- covering(pid, iso(c$s)); if (is.null(cov)) next
      if (D(cov$s) > c$s - 365L) next
      wlo <- c$s - 365L; whi <- c$s - 1L
      prior <- c(vapply(arm_eras(pid, acei), function(e) e$s >= wlo && e$s <= whi, logical(1)),
                 vapply(arm_eras(pid, arb), function(e) e$s >= wlo && e$s <= whi, logical(1)))
      if (any(prior)) next
      chosen <- c; chosen$op_end <- D(cov$e); break
    }
    if (is.null(chosen)) next
    index <- chosen$s; index_end <- min(chosen$e, chosen$op_end)
    base_rows[[length(base_rows) + 1L]] <- data.frame(
      person_id = pid, index_date = iso(index), arm = chosen$arm, age = annual_age(pid, index), sex = person_sex(pid),
      prior_mi = as.integer(any(D(mi_rows$d[mi_rows$person_id == pid]) <= index)), stringsAsFactors = FALSE)
    entry <- index + 1L
    mi_d <- D(mi_rows$d[mi_rows$person_id == pid]); death_d <- D(deaths$d[deaths$person_id == pid])
    if (any(mi_d >= entry - 365L & mi_d < entry)) next   # prior_outcome exclude
    other <- if (chosen$arm == "acei") arm_eras(pid, arb) else arm_eras(pid, acei)
    switch_d <- D(vapply(other, function(e) iso(e$s), ""))
    switch_d <- switch_d[switch_d >= entry]
    for (tar in c("itt", "on_treatment")) {
      exit <- chosen$op_end
      if (length(switch_d) > 0L) exit <- min(exit, min(switch_d))
      if (tar == "on_treatment") exit <- min(exit, index_end + 30L)
      if (exit < entry) next
      ev <- rbind(if (length(mi_d)) data.frame(d = mi_d, name = "mi", pr = 1L) else NULL,
                  if (length(death_d)) data.frame(d = death_d, name = "death", pr = 2L) else NULL)
      ev <- if (!is.null(ev)) ev[ev$d >= entry & ev$d <= exit, , drop = FALSE] else ev
      if (!is.null(ev) && nrow(ev) > 0L) {
        ev <- ev[order(ev$d, ev$pr), , drop = FALSE]
        tte_rows[[length(tte_rows) + 1L]] <- data.frame(person_id = pid, index_date = iso(index), arm = chosen$arm, tar = tar,
          entry_days_from_index = 1L, exit_days_from_index = nday(index, ev$d[1]), event = 1L, outcome_name = ev$name[1], stringsAsFactors = FALSE)
      } else {
        tte_rows[[length(tte_rows) + 1L]] <- data.frame(person_id = pid, index_date = iso(index), arm = chosen$arm, tar = tar,
          entry_days_from_index = 1L, exit_days_from_index = nday(index, exit), event = 0L, outcome_name = NA_character_, stringsAsFactors = FALSE)
      }
    }
  }
  base <- do.call(rbind, base_rows); tte <- do.call(rbind, tte_rows)
  write_expected("02-active-comparator", list(
    baseline = frame_out("baseline", base),
    tte.itt = frame_out("tte.itt", tte[tte$tar == "itt", setdiff(names(tte), "tar")]),
    tte.on_treatment = frame_out("tte.on_treatment", tte[tte$tar == "on_treatment", setdiff(names(tte), "tar")])))
}

# =============================================================================
# 03 distinct days (and 13, its parametrised twin)
# =============================================================================
{
  ep <- cohort_episodes(1L); ids <- descendants(201820L)
  rows <- lapply(seq_len(nrow(ep)), function(i) {
    r <- events_in("condition_occurrence", "condition_concept_id", ids, "condition_start_date", ep$person_id[i],
                   D(ep$index_date[i]) - 365L, D(ep$index_date[i]))
    data.frame(person_id = ep$person_id[i], index_date = ep$index_date[i],
               dm_days = length(unique(r$condition_start_date)), dm_rows = nrow(r), stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, rows); res <- res[res$dm_days >= 2L, , drop = FALSE]
  write_expected("03-distinct-days", list(two_days = frame_out("two_days", res)))
  write_expected("13-parametrised", list(two_days = frame_out("two_days", res)),
                 notes = "identical to 03-distinct-days: the bound plan is the same plan")
}

# =============================================================================
# 04 units and censored operators
# =============================================================================
{
  ep <- cohort_episodes(1L)
  INEQ <- c(4171756L, 4172704L, 4171754L, 4171755L)
  norm <- function(v, u) ifelse(!is.na(u) & u == 8554L, v, ifelse(!is.na(u) & u == 8753L, 0.0915 * v + 2.15, NA_real_))
  rows <- list()
  for (i in seq_len(nrow(ep))) {
    pid <- ep$person_id[i]; idx <- D(ep$index_date[i])
    m <- q(paste0("SELECT measurement_id AS id, measurement_date AS d, value_as_number AS v, unit_concept_id AS u, operator_concept_id AS op ",
                  "FROM measurement WHERE person_id = ", pid, " AND measurement_concept_id = 3004410 AND measurement_date >= '", iso(idx - 180L),
                  "' AND measurement_date <= '", iso(idx), "' ORDER BY measurement_date, measurement_id"))
    if (nrow(m) == 0L) next
    m$nv <- norm(m$v, m$u)
    nn <- m[!is.na(m$nv), , drop = FALSE]
    closest <- if (nrow(nn) == 0L) NA_real_ else {
      dd <- as.integer(idx - D(nn$d)); cand <- nn[dd == min(dd), , drop = FALSE]; cand$nv[nrow(cand)]  # ties: last by (date, id) order
    }
    rows[[length(rows) + 1L]] <- data.frame(
      person_id = pid, index_date = ep$index_date[i], a1c_closest = closest,
      a1c_n_ge65 = sum(!is.na(m$nv) & m$nv >= 6.5),
      a1c_n_uncensored = sum(is.na(m$op) | !(m$op %in% INEQ)),
      a1c_n_known_unit = sum(!is.na(m$u) & m$u %in% c(8554L, 8753L)),
      a1c_max = if (nrow(nn) == 0L) NA_real_ else max(nn$nv), stringsAsFactors = FALSE)
  }
  write_expected("04-units-censored", list(labs = frame_out("labs", do.call(rbind, rows))))
}

# =============================================================================
# 05 same-day index events and same-visit links
# =============================================================================
{
  mi_ids <- descendants(4329847L)
  mi <- q(paste0("SELECT condition_occurrence_id AS id, person_id, condition_start_date AS d, condition_end_date AS e, visit_occurrence_id AS v ",
                 "FROM condition_occurrence WHERE condition_concept_id IN (", idlist(mi_ids), ") ORDER BY person_id, condition_start_date, condition_end_date, condition_occurrence_id"))
  rows <- list()
  for (i in seq_len(nrow(mi))) {
    pid <- mi$person_id[i]; idx <- D(mi$d[i]); cov <- covering(pid, mi$d[i]); if (is.null(cov)) next
    same_day <- sum(mi$person_id == pid & mi$d == mi$d[i])
    v <- mi$v[i]
    pci <- if (is.na(v)) 0L else as.integer(nrow(q(paste0("SELECT 1 FROM procedure_occurrence WHERE person_id = ", pid,
      " AND procedure_concept_id IN (", idlist(descendants(4283095L)), ") AND visit_occurrence_id = ", v))) > 0L)
    cp <- if (is.na(v)) 0L else as.integer(nrow(q(paste0("SELECT 1 FROM condition_occurrence WHERE person_id = ", pid,
      " AND condition_concept_id IN (", idlist(descendants(77670L)), ") AND visit_occurrence_id = ", v))) > 0L)
    inp <- q(paste0("SELECT COUNT(DISTINCT visit_occurrence_id) AS n FROM visit_occurrence WHERE person_id = ", pid,
                    " AND visit_concept_id = 9201 AND visit_start_date >= '", iso(idx), "' AND visit_start_date <= '", iso(idx + 30L), "'"))$n
    rows[[length(rows) + 1L]] <- data.frame(person_id = pid, index_date = mi$d[i], index_key = as.character(mi$id[i]),
      n_same_day_mi = same_day, pci_same_visit = pci, chest_pain_same_visit = cp, inpatient_30 = as.integer(inp), stringsAsFactors = FALSE)
  }
  write_expected("05-same-day-same-visit", list(episodes = frame_out("episodes", do.call(rbind, rows))))
}

# =============================================================================
# 06 coverage, persistence, discontinuation censoring
# =============================================================================
{
  ep <- cohort_episodes(1001L); metf_ids <- descendants(1503297L)
  mi_rows <- q(paste0("SELECT person_id, condition_start_date AS d FROM condition_occurrence WHERE condition_concept_id IN (", idlist(descendants(4329847L)), ")"))
  deaths <- q("SELECT person_id, death_date AS d FROM death")
  cov_rows <- list(); surv_rows <- list()
  for (i in seq_len(nrow(ep))) {
    pid <- ep$person_id[i]; idx <- D(ep$index_date[i]); lo <- idx + 1L; hi <- idx + 90L
    f <- fills_of(pid, metf_ids)
    rec <- if (nrow(f)) lapply(seq_len(nrow(f)), function(k) c(f$start[k], f$end[k])) else list()
    eff <- effective_of(f)
    eras <- if (nrow(f)) eras_of(f$start, f$end, 30L) else list()
    in_win <- Filter(function(e) e$s <= hi && e$e >= lo, eras)
    first_era_days <- if (length(in_win)) as.integer(min(in_win[[1]]$e, hi) - max(in_win[[1]]$s, lo) + 1L) else NA_integer_
    sup_start <- sum(ifelse(f$supply_ok & f$start >= lo & f$start <= hi, f$supply, 0))
    sup_clip <- sum(vapply(seq_len(nrow(f)), function(k) {
      if (!f$supply_ok[k]) return(0L)
      s <- f$start[k]; e <- f$start[k] + f$supply[k] - 1L
      as.integer(max(0L, as.integer(min(e, hi) - max(s, lo) + 1L)))
    }, integer(1)))
    cov_rows[[length(cov_rows) + 1L]] <- data.frame(
      person_id = pid, index_date = ep$index_date[i],
      covered_rec = union_days(rec, lo, hi), covered_eff = union_days(eff, lo, hi),
      era_count = length(in_win), first_era_days = first_era_days,
      supply_start = as.integer(sup_start), supply_clipped = as.integer(sup_clip), stringsAsFactors = FALSE)
    # survival: TAR [1, 365]; censor observation_end, death, first era end after entry + 30
    cov <- covering(pid, ep$index_date[i]); if (is.null(cov)) next
    entry <- idx + 1L; exit <- min(idx + 365L, D(cov$e))
    dd <- D(deaths$d[deaths$person_id == pid]); if (length(dd)) exit <- min(exit, dd[1])
    era_ends <- D(vapply(eras, function(e) iso(e$e), "")); era_ends <- era_ends[era_ends >= entry]
    if (length(era_ends)) exit <- min(exit, min(era_ends) + 30L)
    mi_d <- D(mi_rows$d[mi_rows$person_id == pid]); mi_d <- mi_d[mi_d >= entry & mi_d <= exit]
    event <- as.integer(length(mi_d) > 0L)
    surv_rows[[length(surv_rows) + 1L]] <- data.frame(person_id = pid, index_date = ep$index_date[i],
      entry_days_from_index = 1L, exit_days_from_index = if (event) nday(idx, min(mi_d)) else nday(idx, exit),
      event = event, stringsAsFactors = FALSE)
  }
  write_expected("06-coverage-persistence", list(coverage = frame_out("coverage", do.call(rbind, cov_rows)),
                                                 persistence = frame_out("persistence", do.call(rbind, surv_rows))))
}

# =============================================================================
# 07 monthly dense panel with carry-forward
# =============================================================================
{
  ep <- cohort_episodes(1L)
  norm <- function(v, u) ifelse(!is.na(u) & u == 9529L, v, ifelse(!is.na(u) & u == 8739L, 0.45359237 * v, NA_real_))
  months <- data.frame(s = D(c("2019-10-01", "2019-11-01", "2019-12-01", "2020-01-01", "2020-02-01")),
                       e = D(c("2019-10-31", "2019-11-30", "2019-12-31", "2020-01-31", "2020-02-29")))
  rows <- list()
  for (i in seq_len(nrow(ep))) {
    pid <- ep$person_id[i]; idx <- D(ep$index_date[i]); cov <- covering(pid, ep$index_date[i]); if (is.null(cov)) next
    L <- max(idx - 92L, D(cov$s)); U <- min(idx + 59L, D(cov$e))
    w <- q(paste0("SELECT measurement_id AS id, measurement_date AS d, value_as_number AS v, unit_concept_id AS u FROM measurement ",
                  "WHERE person_id = ", pid, " AND measurement_concept_id = 3025315 ORDER BY measurement_date, measurement_id"))
    w$nv <- if (nrow(w)) norm(w$v, w$u) else numeric(0); w$dd <- D(w$d)
    for (m in seq_len(nrow(months))) {
      ps <- max(months$s[m], L); pe <- min(months$e[m], U); if (ps > pe) next
      inm <- w[w$dd >= ps & w$dd <= pe, , drop = FALSE]
      last <- if (nrow(inm)) inm$nv[nrow(inm)] else NA_real_
      cf <- w[!is.na(w$nv) & w$dd <= ps & w$dd >= ps - 180L, , drop = FALSE]
      carry <- if (nrow(cf)) cf$nv[nrow(cf)] else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(person_id = pid, index_date = ep$index_date[i],
        period_start_days = nday(idx, ps), period_end_days = nday(idx, pe), days = as.integer(pe - ps + 1L),
        weight_count = nrow(inm), weight_last = last, weight_carry = carry, stringsAsFactors = FALSE)
    }
  }
  write_expected("07-monthly-panel", list(panel = frame_out("panel", do.call(rbind, rows))))
}

# =============================================================================
# 08 SCCS matrix
# =============================================================================
{
  ep <- cohort_episodes(1002L); asp_ids <- descendants(1124300L); mi_ids <- descendants(4329847L)
  rows <- list()
  for (i in seq_len(nrow(ep))) {
    pid <- ep$person_id[i]; idx <- D(ep$index_date[i]); cov <- covering(pid, ep$index_date[i]); if (is.null(cov)) next
    L <- max(idx, D(cov$s)); U <- min(idx + 365L, D(cov$e), D(ep$end_date[i]))
    f <- fills_of(pid, asp_ids); eras <- if (nrow(f)) eras_of(f$start, f$end, 30L) else list()
    era_iv <- lapply(eras, function(e) c(e$s, e$e))
    b <- c(L, U + 1L, D(cov$s), D(cov$e) + 1L)
    for (e in eras) b <- c(b, e$s, e$e + 1L)
    for (y in as.integer(format(L, "%Y")):as.integer(format(U + 1L, "%Y"))) b <- c(b, D(sprintf("%d-01-01", y)))
    b <- sort(unique(b[b >= L & b <= U + 1L]))
    mi_d <- D(q(paste0("SELECT condition_start_date AS d FROM condition_occurrence WHERE person_id = ", pid,
                       " AND condition_concept_id IN (", idlist(mi_ids), ")"))$d)
    for (k in seq_len(length(b) - 1L)) {
      ps <- b[k]; pe <- b[k + 1L] - 1L
      ps <- max(ps, D(cov$s)); pe <- min(pe, D(cov$e)); if (ps > pe) next
      age <- annual_age(pid, ps)
      rows[[length(rows) + 1L]] <- data.frame(person_id = pid, index_date = ep$index_date[i],
        period_start_days = nday(idx, ps), period_end_days = nday(idx, pe), days = as.integer(pe - ps + 1L),
        age = age, age_band = if (is.na(age)) NA_integer_ else age %/% 5L, calendar_year = as.integer(format(ps, "%Y")),
        exposed_aspirin = union_days(era_iv, ps, pe),
        active_aspirin = as.integer(any(vapply(era_iv, function(iv) iv[1] <= ps && iv[2] >= ps, logical(1)))),
        n_mi = sum(mi_d >= ps & mi_d <= pe), stringsAsFactors = FALSE)
    }
  }
  write_expected("08-sccs-matrix", list(matrix = frame_out("matrix", do.call(rbind, rows))))
}

# =============================================================================
# 09 recurrent events with a sentinel
# =============================================================================
{
  ep <- cohort_episodes(1L); resp_ids <- descendants(320136L)
  deaths <- q("SELECT person_id, death_date AS d FROM death")
  rows <- list()
  for (i in seq_len(nrow(ep))) {
    pid <- ep$person_id[i]; idx <- D(ep$index_date[i]); cov <- covering(pid, ep$index_date[i]); if (is.null(cov)) next
    entry <- idx + 1L; exit <- min(idx + 90L, D(cov$e)); dd <- D(deaths$d[deaths$person_id == pid]); if (length(dd)) exit <- min(exit, dd[1])
    ev <- q(paste0("SELECT condition_occurrence_id AS id, condition_start_date AS d FROM condition_occurrence WHERE person_id = ", pid,
                   " AND condition_concept_id IN (", idlist(resp_ids), ") AND condition_start_date >= '", iso(entry), "' AND condition_start_date <= '", iso(exit),
                   "' ORDER BY condition_start_date, condition_occurrence_id"))
    if (nrow(ev)) {
      for (k in seq_len(nrow(ev))) rows[[length(rows) + 1L]] <- data.frame(person_id = pid, index_date = ep$index_date[i],
        event = 1L, event_number = k, event_days_from_index = nday(idx, ev$d[k]), entry_days_from_index = 1L,
        exit_days_from_index = nday(idx, exit), stringsAsFactors = FALSE)
    } else {
      rows[[length(rows) + 1L]] <- data.frame(person_id = pid, index_date = ep$index_date[i], event = 0L, event_number = 1L,
        event_days_from_index = NA_integer_, entry_days_from_index = 1L, exit_days_from_index = nday(idx, exit), stringsAsFactors = FALSE)
    }
  }
  write_expected("09-recurrent-events", list(recurrent = frame_out("recurrent", do.call(rbind, rows))))
}

# =============================================================================
# 10 matching (reference sampler, design 6.10)
# =============================================================================
{
  mi_ids <- descendants(4329847L); dm_ids <- descendants(201820L)
  mi <- q(paste0("SELECT condition_occurrence_id AS id, person_id, condition_start_date AS d FROM condition_occurrence WHERE condition_concept_id IN (",
                 idlist(mi_ids), ") ORDER BY person_id, condition_start_date, condition_occurrence_id"))
  deaths <- q("SELECT person_id, death_date AS d FROM death")
  dm <- q(paste0("SELECT person_id, condition_start_date AS d FROM condition_occurrence WHERE condition_concept_id IN (", idlist(dm_ids), ")"))
  # cases: first eligible MI per person (observed >= 365 before it, unique coverage)
  cases <- list()
  for (pid in unique(mi$person_id)) {
    r <- mi[mi$person_id == pid, , drop = FALSE]
    for (k in seq_len(nrow(r))) {
      cov <- covering(pid, r$d[k]); if (is.null(cov)) next
      if (D(cov$s) > D(r$d[k]) - 365L) next
      cases[[length(cases) + 1L]] <- list(pid = pid, t = D(r$d[k]), key = as.character(r$id[k])); break
    }
  }
  f <- function(tag, text = NULL) {
    bytes <- if (is.null(text)) raw(0) else charToRaw(enc2utf8(as.character(text))); n <- length(bytes)
    c(as.raw(tag), as.raw(c(bitwAnd(bitwShiftR(n, 24L), 255L), bitwAnd(bitwShiftR(n, 16L), 255L), bitwAnd(bitwShiftR(n, 8L), 255L), bitwAnd(n, 255L))), bytes)
  }
  hash_pair <- function(seed, case, control) {
    E <- c(f(0x02, "dsomop-match-v1"), f(0x01, seed), f(0x02, "cases"), f(0x01, case$pid), f(0x03, iso(case$t)), f(0x03, iso(case$t)),
           f(0x02, "mi"), f(0x02, case$key), f(0x01, control))
    paste(as.character(openssl::sha256(E)))
  }
  strata_of <- function(pid, t) {
    yob <- person_yob(pid); age <- as.integer(format(t, "%Y")) - yob
    list(sex = person_sex(pid), band = if (is.na(age)) NA_integer_ else age %/% 5L, year = as.integer(format(t, "%Y")),
         dm = any(D(dm$d[dm$person_id == pid]) <= t - 1L))
  }
  ord_cases <- order(vapply(cases, function(c) as.integer(c$t), 1), vapply(cases, function(c) c$pid, 1))
  cases <- cases[ord_cases]
  rows <- list(); set_id <- 0L
  for (case in cases) {
    set_id <- set_id + 1L; t <- case$t; cs <- strata_of(case$pid, t)
    pool <- integer(0)
    for (k in persons$person_id) {
      if (k == case$pid) next
      cov <- covering(k, iso(t)); if (is.null(cov) || D(cov$s) > t - 365L) next
      dd <- D(deaths$d[deaths$person_id == k]); if (length(dd) && dd[1] < t) next
      if (any(D(mi$d[mi$person_id == k]) <= t)) next
      ks <- strata_of(k, t)
      if (is.na(cs$band) || is.na(ks$band)) next
      if (ks$sex != cs$sex || ks$band != cs$band || ks$year != cs$year || ks$dm != cs$dm) next
      pool <- c(pool, k)
    }
    N <- length(pool); m <- min(4L, N)
    chosen <- if (N > 0L) { hs <- vapply(pool, function(k) hash_pair(20261111L, case, k), ""); pool[order(hs, pool)][seq_len(m)] } else integer(0)
    stat <- function(pid, is_case, w) {
      has_statin <- nrow(q(paste0("SELECT 1 FROM drug_exposure WHERE person_id = ", pid, " AND drug_concept_id IN (", idlist(descendants(1539403L)),
                                  ") AND drug_exposure_start_date >= '", iso(t - 365L), "' AND drug_exposure_start_date <= '", iso(t - 1L), "'"))) > 0L
      data.frame(person_id = pid, index_date = iso(t), set_id = set_id, is_case = is_case, w = w,
                 age = annual_age(pid, t), sex = person_sex(pid), statin_365 = as.integer(has_statin), n_eligible = N, stringsAsFactors = FALSE)
    }
    rows[[length(rows) + 1L]] <- stat(case$pid, 1L, 1)
    for (k in chosen) rows[[length(rows) + 1L]] <- stat(k, 0L, N / m)
  }
  write_expected("10-matching", list(cc_table = frame_out("cc_table", do.call(rbind, rows))),
                 notes = "n_eligible (the private pool size N) is included for review only; it is not a released column")
}

# =============================================================================
# 11 incidence years
# =============================================================================
{
  ep <- cohort_episodes(1L); resp_ids <- descendants(320136L)
  rows <- list()
  for (i in seq_len(nrow(ep))) {
    pid <- ep$person_id[i]; idx <- D(ep$index_date[i]); cov <- covering(pid, ep$index_date[i]); if (is.null(cov)) next
    L <- max(idx, D(cov$s)); U <- min(idx + 730L, D(cov$e))
    ev <- D(q(paste0("SELECT condition_start_date AS d FROM condition_occurrence WHERE person_id = ", pid,
                     " AND condition_concept_id IN (", idlist(resp_ids), ")"))$d)
    for (y in as.integer(format(L, "%Y")):as.integer(format(U, "%Y"))) {
      ps <- max(D(sprintf("%d-01-01", y)), L); pe <- min(D(sprintf("%d-12-31", y)), U); if (ps > pe) next
      rows[[length(rows) + 1L]] <- data.frame(person_id = pid, index_date = ep$index_date[i],
        period_start_days = nday(idx, ps), period_end_days = nday(idx, pe), days = as.integer(pe - ps + 1L),
        calendar_year = y, n_resp = sum(ev >= ps & ev <= pe), stringsAsFactors = FALSE)
    }
  }
  write_expected("11-incidence-years", list(years = frame_out("years", do.call(rbind, rows))))
}

# =============================================================================
# 12 Circe import + baseline
# =============================================================================
{
  dm_ids <- descendants(201820L)
  dm <- q(paste0("SELECT condition_occurrence_id AS id, person_id, condition_start_date AS d FROM condition_occurrence WHERE condition_concept_id IN (",
                 idlist(dm_ids), ") ORDER BY person_id, condition_start_date, condition_occurrence_id"))
  breaks <- seq(0L, 85L, by = 5L)
  band <- function(age) {
    if (is.na(age)) return(NA_character_)
    k <- findInterval(age, c(breaks, Inf)); if (k == length(breaks)) paste0(breaks[k], "+") else paste0(breaks[k], "-", breaks[k + 1L] - 1L)
  }
  rows <- list()
  for (pid in unique(dm$person_id)) {
    r <- dm[dm$person_id == pid, , drop = FALSE]
    for (k in seq_len(nrow(r))) {
      cov <- covering(pid, r$d[k]); if (is.null(cov)) next
      idx <- D(r$d[k]); if (D(cov$s) > idx - 365L) next
      prior <- union_days(list(c(D(cov$s), D(cov$e))), idx - 365L, idx)
      rows[[length(rows) + 1L]] <- data.frame(person_id = pid, index_date = iso(idx), age_group = band(annual_age(pid, idx)),
                                              sex = person_sex(pid), prior_obs = prior, stringsAsFactors = FALSE)
      break
    }
  }
  write_expected("12-circe-baseline", list(baseline = frame_out("baseline", do.call(rbind, rows))))
}

# =============================================================================
# 14 oncology episodes
# =============================================================================
{
  eps <- q("SELECT episode_id, person_id, episode_start_date AS s, episode_end_date AS e FROM episode WHERE episode_concept_id = 32531 ORDER BY person_id, episode_start_date, episode_end_date, episode_id")
  links <- q("SELECT episode_id, event_id, episode_event_field_concept_id AS fc FROM episode_event")
  member_dates <- function(eid, tables = c("drug_exposure", "procedure_occurrence")) {
    l <- links[links$episode_id == eid, , drop = FALSE]; out <- D(character(0))
    for (k in seq_len(nrow(l))) {
      if (l$fc[k] == 1147094L && "drug_exposure" %in% tables) out <- c(out, D(q(paste0("SELECT drug_exposure_start_date AS d FROM drug_exposure WHERE drug_exposure_id = ", l$event_id[k]))$d))
      if (l$fc[k] == 1147082L && "procedure_occurrence" %in% tables) out <- c(out, D(q(paste0("SELECT procedure_date AS d FROM procedure_occurrence WHERE procedure_occurrence_id = ", l$event_id[k]))$d))
    }
    out
  }
  rows <- list()
  for (i in seq_len(nrow(eps))) {
    pid <- eps$person_id[i]; idx <- D(eps$s[i]); cov <- covering(pid, eps$s[i]); if (is.null(cov)) next
    all_m <- member_dates(eps$episode_id[i]); drug_m <- member_dates(eps$episode_id[i], "drug_exposure")
    nsp <- q(paste0("SELECT COUNT(*) AS n FROM specimen WHERE person_id = ", pid, " AND specimen_concept_id IN (", idlist(descendants(4001225L)),
                    ") AND specimen_date >= '", iso(idx - 30L), "' AND specimen_date <= '", iso(idx + 365L), "'"))$n
    rows[[length(rows) + 1L]] <- data.frame(person_id = pid, index_date = eps$s[i], index_key = as.character(eps$episode_id[i]),
      n_members = length(all_m), n_member_drugs = length(drug_m),
      first_member_days = if (length(all_m)) nday(idx, min(all_m)) else NA_integer_, n_specimens = as.integer(nsp), stringsAsFactors = FALSE)
  }
  write_expected("14-oncology-episodes", list(episodes = frame_out("episodes", do.call(rbind, rows))))
}

# =============================================================================
# 15 costs and adherence
# =============================================================================
{
  ep <- cohort_episodes(1001L); metf_ids <- descendants(1503297L)
  rows <- list()
  for (i in seq_len(nrow(ep))) {
    pid <- ep$person_id[i]; idx <- D(ep$index_date[i])
    comps <- components(pid, 30L)
    comp <- NULL
    for (cp in comps) if (cp$s <= idx && cp$e >= idx && any(vapply(cp$rows, function(r) r[1] <= idx && r[2] >= idx, logical(1)))) comp <- cp
    if (is.null(comp)) next
    observed <- comp$rows
    obs_days <- function(lo, hi) union_days(observed, lo, hi)
    f <- fills_of(pid, metf_ids)
    rec <- if (nrow(f)) lapply(seq_len(nrow(f)), function(k) c(f$start[k], f$end[k])) else list()
    eff <- effective_of(f)
    cost_sum <- function(domain, event_ids, lo, hi) {
      if (length(event_ids) == 0L) return(0)
      c <- q(paste0("SELECT c.total_cost AS v FROM cost c WHERE c.cost_domain_id = '", domain, "' AND c.currency_concept_id = 44818568 AND c.cost_event_id IN (",
                    idlist(event_ids), ")"))
      sum(c$v)
    }
    visits <- q(paste0("SELECT visit_occurrence_id AS id FROM visit_occurrence WHERE person_id = ", pid, " AND visit_start_date >= '", iso(idx + 1L),
                       "' AND visit_start_date <= '", iso(idx + 60L), "'"))$id
    fills60 <- q(paste0("SELECT drug_exposure_id AS id FROM drug_exposure WHERE person_id = ", pid, " AND drug_concept_id IN (", idlist(metf_ids),
                        ") AND drug_exposure_start_date >= '", iso(idx + 1L), "' AND drug_exposure_start_date <= '", iso(idx + 60L), "'"))$id
    w90 <- c(idx + 1L, idx + 90L); w10 <- c(idx + 1L, idx + 10L)
    rows[[length(rows) + 1L]] <- data.frame(person_id = pid, index_date = ep$index_date[i],
      visit_cost_60 = cost_sum("Visit", visits, idx + 1L, idx + 60L), drug_cost_60 = cost_sum("Drug", fills60, idx + 1L, idx + 60L),
      supply_90 = as.integer(sum(ifelse(f$supply_ok & f$start >= w90[1] & f$start <= w90[2], f$supply, 0))),
      observed_90 = obs_days(w90[1], w90[2]),
      pdc_num_eff_90 = union_days(intersect_days(eff, observed), w90[1], w90[2]),
      pdc_num_rec_90 = union_days(intersect_days(rec, observed), w90[1], w90[2]),
      covered_eff_90 = union_days(eff, w90[1], w90[2]), covered_rec_90 = union_days(rec, w90[1], w90[2]),
      observed_10 = obs_days(w10[1], w10[2]),
      pdc_num_eff_10 = union_days(intersect_days(eff, observed), w10[1], w10[2]),
      covered_eff_10 = union_days(eff, w10[1], w10[2]), stringsAsFactors = FALSE)
  }
  write_expected("15-costs-adherence", list(adherence = frame_out("adherence", do.call(rbind, rows))))
}
DBI::dbDisconnect(conn)
cat("done\n")
