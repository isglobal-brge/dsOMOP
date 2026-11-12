# Module: Closed registry of protected release contracts (dsOMOP 2.10.3)
#
# Each contract names one reviewed estimand family of a built-in catalogue
# entry: its output class, whether an unscoped run is over a public basis
# (fixed-synopsis eligible), the fields that are demonstrated-disclosive on the
# standard route, and a run function producing the protected view from
# person-level bounded contributions. The registry is server-owned and closed:
# external analysis packs cannot assert a contract, and a frame attribute can
# never establish protection.

.DSOMOP_RELEASE_DIST_FIELDS <- c("avg_value", "stdev_value", "p10_value",
                                 "p25_value", "median_value", "p75_value",
                                 "p90_value")
.DSOMOP_RELEASE_BUILTIN_ADAPTERS <- c("achilles", "achilles_live",
                                      "tools_auto", "query", "diagnostic",
                                      "ohdsi_live")

# --- Shared substrate helpers ----------------------------------------------------

.omopReleaseGenderKey <- function(gender_concept_id) {
  genders <- .dsomopToolsGenders()$gender_concept_id
  g <- suppressWarnings(as.integer(gender_concept_id))
  g[is.na(g) | !g %in% genders] <- 0L
  paste0("gender=", g)
}

.omopReleaseGenderStrata <- function() {
  genders <- .dsomopToolsGenders()
  list(kind = "public", keys = paste0("gender=", genders$gender_concept_id),
       labels = data.frame(stratum = paste0("gender=",
                                            genders$gender_concept_id),
                           gender_concept_id = genders$gender_concept_id,
                           gender_name = genders$gender_name,
                           stringsAsFactors = FALSE))
}

# Public age-decile strata of a value at an event (Achilles 107 "age decile").
.omopReleaseDecileStrata <- function() {
  deciles <- 0:11
  keys <- c(paste0("age_decile=", deciles), "age_decile=unknown")
  list(kind = "public", keys = keys,
       labels = data.frame(stratum = keys,
                           age_decile = c(deciles, NA_integer_),
                           stringsAsFactors = FALSE))
}

.omopReleaseFetch <- function(handle, sql) {
  raw <- .executeQuery(handle, .sql_translate(sql, handle$target_dialect))
  if (!is.data.frame(raw)) raw <- data.frame()
  names(raw) <- tolower(names(raw))
  raw
}

.omopReleaseEmptyValues <- function() {
  data.frame(person_id = character(0), stratum = character(0),
             v = numeric(0), stringsAsFactors = FALSE)
}

# Year-only age at an event: YEAR(event) - year_of_birth.
.omopReleaseYearAgeSql <- function(handle, date_expr, yob_expr = "p.year_of_birth") {
  paste0("(", .omopYearExpr(handle, date_expr), " - ", yob_expr, ")")
}

# --- C2: gender x birth-year basis ---------------------------------------------

#' The one public gender x birth-year basis of the person table
#'
#' Achilles 3 and 10 and the QueryLibrary birth-year, age-decade and
#' age-at-reference-year questions are views of this one owner: cells are the
#' six public gender strata times birth years 1900 to the custodial reference
#' year, plus an unknown cell (missing or out-of-range birth year).
#' @keywords internal
.omopReleaseBirthYearOwner <- function() {
  year <- .dsomopToolsReferenceYear()
  cells <- c(as.character(seq.int(.DSOMOP_TOOLS_FIRST_BIRTH_YEAR, year)),
             "unknown")
  .dsomopReleaseOwner(
    id = paste0("person/gender-birth-year/reference-", year, "/v1"),
    family = "H", cells = cells, strata = .omopReleaseGenderStrata(),
    estimand = paste0("persons by gender and year of birth (1900 to ", year,
                      "; other or missing years as unknown)"))
}

.omopReleaseBirthYearSubstrate <- function(handle, ctx) {
  person <- .qualifyTable(handle, "person")
  .omopDiagAssertPersons(handle, ctx, person, "p", "person_id")
  sj <- .omopScopeJoin(ctx, "p", "person_id")
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT p.person_id AS person_id, p.gender_concept_id AS gender, ",
    "p.year_of_birth AS yob FROM ", person, " p", sj$join))
  if (nrow(raw) == 0L) {
    return(data.frame(person_id = character(0), stratum = character(0),
                      cell = character(0), stringsAsFactors = FALSE))
  }
  year <- .dsomopToolsReferenceYear()
  yob <- suppressWarnings(as.integer(raw$yob))
  known <- !is.na(yob) & yob >= .DSOMOP_TOOLS_FIRST_BIRTH_YEAR & yob <= year
  data.frame(person_id = raw$person_id,
             stratum = .omopReleaseGenderKey(raw$gender),
             cell = ifelse(known, as.character(yob), "unknown"),
             stringsAsFactors = FALSE)
}

# Displayed basis as one long frame: gender x birth-year cell.
.omopReleaseBirthYearCells <- function(built) {
  owner <- built$owner
  labels <- owner$strata$labels
  rows <- lapply(seq_len(nrow(labels)), function(i) {
    s <- labels$stratum[[i]]
    data.frame(gender_concept_id = labels$gender_concept_id[[i]],
               gender_name = labels$gender_name[[i]],
               cell = owner$cells,
               n = built$bins[[s]],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$year_of_birth <- suppressWarnings(as.integer(ifelse(
    out$cell == "unknown", NA, out$cell)))
  out
}

.omopReleaseAgeGroup <- function(age) {
  groups <- .DSOMOP_TOOLS_AGE_GROUPS
  out <- rep("unknown", length(age))
  known <- !is.na(age) & age >= 0L
  out[known] <- groups[pmin(age[known] %/% 10L, 9L) + 1L]
  out
}

.omopReleaseRunBirthYear <- function(view) {
  force(view)
  function(handle, ctx, params, route, snap) {
    owner <- .omopReleaseBirthYearOwner()
    sub <- .omopReleaseBirthYearSubstrate(handle, ctx)
    built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
    cells <- .omopReleaseBirthYearCells(built)
    year <- .dsomopToolsReferenceYear()
    sum_by <- function(key, value) {
      keys <- unique(key)
      vapply(keys, function(k) sum(value[key == k]), numeric(1L))
    }
    df <- switch(view,
      achilles_3 = {
        yobs <- c(as.character(seq.int(.DSOMOP_TOOLS_FIRST_BIRTH_YEAR, year)),
                  "unknown")
        n <- vapply(yobs, function(y) sum(cells$n[cells$cell == y]),
                    numeric(1L))
        data.frame(year_of_birth = suppressWarnings(as.integer(
                     ifelse(yobs == "unknown", NA, yobs))),
                   count_value = .dsomopReleaseRound(n),
                   stringsAsFactors = FALSE)
      },
      achilles_10 = data.frame(
        year_of_birth = cells$year_of_birth,
        gender_concept_id = cells$gender_concept_id,
        gender_name = cells$gender_name,
        count_value = .dsomopReleaseRound(cells$n),
        stringsAsFactors = FALSE),
      year_of_birth = {
        yobs <- c(as.character(seq.int(.DSOMOP_TOOLS_FIRST_BIRTH_YEAR, year)),
                  "unknown")
        n <- vapply(yobs, function(y) sum(cells$n[cells$cell == y]),
                    numeric(1L))
        data.frame(year_of_birth = suppressWarnings(as.integer(
                     ifelse(yobs == "unknown", NA, yobs))),
                   n_persons = .dsomopReleaseRound(n),
                   stringsAsFactors = FALSE)
      },
      gender_by_age_decade = {
        age <- year - cells$year_of_birth
        group <- .omopReleaseAgeGroup(age)
        genders <- .dsomopToolsGenders()
        frame <- data.frame(
          gender_concept_id = rep(genders$gender_concept_id,
                                  each = length(.DSOMOP_TOOLS_AGE_GROUPS)),
          gender_name = rep(genders$gender_name,
                            each = length(.DSOMOP_TOOLS_AGE_GROUPS)),
          age_group = rep(.DSOMOP_TOOLS_AGE_GROUPS, nrow(genders)),
          stringsAsFactors = FALSE)
        frame$n_persons <- .dsomopReleaseRound(vapply(
          seq_len(nrow(frame)), function(i) sum(cells$n[
            cells$gender_concept_id == frame$gender_concept_id[[i]] &
              group == frame$age_group[[i]]]), numeric(1L)))
        frame
      },
      age_distribution_decade = {
        # The requested reference year only relabels the released basis
        # (year-only age = reference year - birth year).
        requested <- snap$changes$reference_year$requested %||%
          params$reference_year %||% year
        reference <- suppressWarnings(as.integer(requested))
        if (is.na(reference)) reference <- year
        age <- reference - cells$year_of_birth
        group <- rep("unknown", length(age))
        known <- !is.na(age)
        bands <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                   "60-69", "70-79", "80-89", "90+")
        group[known] <- bands[pmin(pmax(age[known], 0L) %/% 10L, 9L) + 1L]
        levels <- c(bands, "unknown")
        data.frame(age_group = levels,
                   n_persons = .dsomopReleaseRound(vapply(
                     levels, function(g) sum(cells$n[group == g]),
                     numeric(1L))),
                   stringsAsFactors = FALSE)
      })
    rownames(df) <- NULL
    list(df = df, built = list(built),
         extra = list(reference_year = year))
  }
}

# --- C2: birth month ---------------------------------------------------------------

.omopReleaseRunBirthMonth <- function(handle, ctx, params, route, snap) {
  person <- .qualifyTable(handle, "person")
  .omopDiagAssertPersons(handle, ctx, person, "p", "person_id",
                         where_sql = "p.month_of_birth IS NOT NULL")
  sj <- .omopScopeJoin(ctx, "p", "person_id")
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT p.person_id AS person_id, p.month_of_birth AS m FROM ", person,
    " p", sj$join, " WHERE p.month_of_birth IS NOT NULL"))
  month <- suppressWarnings(as.integer(raw$m))
  cells <- c(as.character(1:12), "unknown")
  sub <- data.frame(
    person_id = raw$person_id %||% character(0),
    stratum = rep("root", nrow(raw)),
    cell = ifelse(!is.na(month) & month >= 1L & month <= 12L,
                  as.character(month), "unknown"),
    stringsAsFactors = FALSE)
  owner <- .dsomopReleaseOwner(
    id = "person/birth-month/v1", family = "H", cells = cells,
    estimand = "persons with a recorded birth month, by month")
  built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
  df <- data.frame(month_of_birth = c(1:12, NA_integer_),
                   n_persons = .dsomopReleaseRound(built$bins[["root"]]),
                   stringsAsFactors = FALSE)
  list(df = df, built = list(built))
}

# --- C2/C7: first observation period ---------------------------------------------

.omopReleaseFirstObsSubstrate <- function(handle, ctx, value_kind,
                                          strata = "root") {
  person <- .qualifyTable(handle, "person")
  fop <- .omopAchillesFirstObsPeriodSql(handle)
  .omopDiagAssertPersons(handle, ctx,
                         .qualifyTable(handle, "observation_period"),
                         "op", "person_id")
  sj <- .omopScopeJoin(ctx, "fop", "person_id")
  age <- .omopReleaseYearAgeSql(handle, "fop.op_start")
  value <- if (identical(value_kind, "age")) age else
    .omopDateDiffDays(handle, "fop.op_end", "fop.op_start")
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT fop.person_id AS person_id, ", value, " AS v, ", age,
    " AS age, p.gender_concept_id AS gender FROM ", fop, " fop", sj$join,
    " INNER JOIN ", person, " p ON p.person_id = fop.person_id"))
  if (nrow(raw) == 0L) return(.omopReleaseEmptyValues())
  v <- suppressWarnings(as.numeric(raw$v))
  stratum <- switch(strata,
    root = rep("root", nrow(raw)),
    gender = .omopReleaseGenderKey(raw$gender),
    age_decile = {
      a <- suppressWarnings(as.integer(raw$age))
      d <- ifelse(is.na(a) | a < 0L, NA_integer_, pmin(a %/% 10L, 11L))
      ifelse(is.na(d), "age_decile=unknown", paste0("age_decile=", d))
    })
  data.frame(person_id = raw$person_id, stratum = stratum, v = v,
             stringsAsFactors = FALSE)
}

.omopReleaseFirstObsOwner <- function(value_kind, strata) {
  strata_spec <- switch(strata, root = "root",
                        gender = .omopReleaseGenderStrata(),
                        age_decile = .omopReleaseDecileStrata())
  if (identical(value_kind, "age")) {
    .dsomopReleaseOwner(
      id = paste0("achilles/first-op-age-years/by-", strata, "/v1"),
      family = "HM", grid = .dsomopReleaseGrid("age_5y"),
      strata = strata_spec, bounds = c(0, 110),
      estimand = paste0("year-only age at the first observation period ",
                        "(year of start minus year of birth), clipped to ",
                        "[0, 110]"))
  } else {
    grid <- .dsomopReleaseGrid("duration_days")
    .dsomopReleaseOwner(
      id = paste0("achilles/first-op-length-days/by-", strata, "/v1"),
      family = "HM", grid = grid, strata = strata_spec,
      bounds = c(grid$lower, grid$upper),
      estimand = paste0("length in days of the first observation period, ",
                        "clipped to [0, 10957]"))
  }
}

# One dist row per displayed stratum, in the standard Achilles columns.
.omopReleaseDistColumns <- function(strata) {
  data.frame(count_value = strata$n_display, avg_value = strata$mean,
             stdev_value = strata$sd, p10_value = strata$p10,
             p25_value = strata$p25, median_value = strata$median,
             p75_value = strata$p75, p90_value = strata$p90,
             stringsAsFactors = FALSE)
}

.omopReleaseRunFirstObs <- function(value_kind, strata, view = "dist") {
  force(value_kind); force(strata); force(view)
  function(handle, ctx, params, route, snap) {
    owner <- .omopReleaseFirstObsOwner(value_kind, strata)
    sub <- .omopReleaseFirstObsSubstrate(handle, ctx, value_kind, strata)
    built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
    st <- built$strata
    labels <- owner$strata$labels
    if (identical(view, "bands")) {
      bins <- .dsomopReleaseBinFrame(owner$grid)
      if (identical(strata, "root")) {
        df <- data.frame(age_band = as.integer(bins$bin_lower),
                         count_value = .dsomopReleaseRound(
                           built$bins[["root"]]),
                         stringsAsFactors = FALSE)
      } else {
        df <- do.call(rbind, lapply(seq_len(nrow(labels)), function(i) {
          data.frame(age_band = as.integer(bins$bin_lower),
                     gender_name = labels$gender_name[[i]],
                     count_value = .dsomopReleaseRound(
                       built$bins[[labels$stratum[[i]]]]),
                     stringsAsFactors = FALSE)
        }))
      }
    } else {
      df <- .omopReleaseDistColumns(st)
      if (identical(strata, "gender")) {
        lab <- labels[match(st$stratum, labels$stratum), , drop = FALSE]
        df <- cbind(data.frame(gender_concept_id = lab$gender_concept_id,
                               gender_name = lab$gender_name,
                               stringsAsFactors = FALSE), df)
      } else if (identical(strata, "age_decile")) {
        lab <- labels[match(st$stratum, labels$stratum), , drop = FALSE]
        df <- cbind(data.frame(age_decile = lab$age_decile), df)
      }
    }
    rownames(df) <- NULL
    list(df = df, built = list(built))
  }
}

# --- C7: Achilles per-person distributions -------------------------------------

.omopReleaseRunDistinctConcepts <- function(domain) {
  force(domain)
  function(handle, ctx, params, route, snap) {
    spec <- .omopAchillesDistDomain(domain)
    if (!.omopAchillesTablePresent(handle, spec$table)) {
      return(list(df = data.frame(), built = list()))
    }
    table <- .qualifyTable(handle, spec$table)
    .omopDiagAssertPersons(handle, ctx, table, "e", "person_id")
    sj <- .omopScopeJoin(ctx, "e", "person_id")
    raw <- .omopReleaseFetch(handle, paste0(
      "SELECT e.person_id AS person_id, COUNT(DISTINCT e.", spec$concept_col,
      ") AS v FROM ", table, " e", sj$join, " GROUP BY e.person_id"))
    grid <- .dsomopReleaseGrid("count_per_person")
    owner <- .dsomopReleaseOwner(
      id = paste0("achilles/distinct-", domain, "-concepts-per-person/v1"),
      family = "HM", grid = grid, bounds = c(grid$lower, grid$upper),
      estimand = paste0("number of distinct ", domain, " concepts per person, ",
                        "clipped to [0, 100]"))
    sub <- data.frame(person_id = raw$person_id %||% character(0),
                      stratum = rep("root", nrow(raw)),
                      v = suppressWarnings(as.numeric(raw$v)),
                      stringsAsFactors = FALSE)
    built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
    list(df = .omopReleaseDistColumns(built$strata), built = list(built))
  }
}

# Multi-concept strata: a person contributes to at most K concepts (ascending
# public concept id), so the vector sensitivity is K.
.omopReleaseConceptStrataFrame <- function(df, built, id_col, name_col,
                                           names_by_key) {
  st <- built$strata
  ids <- suppressWarnings(as.numeric(sub("^concept=", "", st$stratum)))
  out <- cbind(
    stats::setNames(data.frame(ids, names_by_key[st$stratum],
                               stringsAsFactors = FALSE),
                    c(id_col, name_col)),
    .omopReleaseDistColumns(st))
  out <- out[order(-out$count_value, out[[id_col]], method = "radix"), ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}

.omopReleaseRunAgeByConcept <- function(domain) {
  force(domain)
  function(handle, ctx, params, route, snap) {
    spec <- .omopAchillesAgeDomain(domain)
    if (!.omopAchillesTablePresent(handle, spec$table)) {
      return(list(df = data.frame(), built = list()))
    }
    table <- .qualifyTable(handle, spec$table)
    person <- .qualifyTable(handle, "person")
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)
    .omopDiagAssertPersons(handle, ctx, table, "e", "person_id")
    sj <- .omopScopeJoin(ctx, "e", "person_id")
    age <- paste0(.omopYearExpr(handle, paste0("MIN(e.", spec$date_col, ")")),
                  " - p.year_of_birth")
    raw <- .omopReleaseFetch(handle, paste0(
      "SELECT pp.concept_id, cc.concept_name, pp.person_id, pp.v FROM (",
      "SELECT e.", spec$concept_col, " AS concept_id, e.person_id AS ",
      "person_id, ", age, " AS v FROM ", table, " e INNER JOIN ", person,
      " p ON p.person_id = e.person_id", sj$join, " WHERE e.",
      spec$concept_col, " IS NOT NULL GROUP BY e.", spec$concept_col,
      ", e.person_id, p.year_of_birth) pp LEFT JOIN ", concept,
      " cc ON cc.concept_id = pp.concept_id"))
    owner <- .dsomopReleaseOwner(
      id = paste0("achilles/age-first-", domain, "-occurrence-by-concept/v1"),
      family = "HM", grid = .dsomopReleaseGrid("age_5y"),
      strata = list(kind = "data"), cap = .DSOMOP_RELEASE_MULTI_CAP,
      bounds = c(0, 110),
      estimand = paste0("year-only age at the first occurrence of each ",
                        domain, " concept, per person, clipped to [0, 110]; ",
                        "each person counted in at most ",
                        .DSOMOP_RELEASE_MULTI_CAP, " concepts (lowest ",
                        "concept ids)"))
    sub <- data.frame(person_id = raw$person_id %||% character(0),
                      stratum = paste0("concept=", raw$concept_id),
                      stratum_order = suppressWarnings(as.numeric(
                        raw$concept_id)),
                      v = suppressWarnings(as.numeric(raw$v)),
                      stringsAsFactors = FALSE)
    sub <- sub[!is.na(sub$stratum_order), , drop = FALSE]
    names_by_key <- stats::setNames(as.character(raw$concept_name),
                                    paste0("concept=", raw$concept_id))
    built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
    list(df = .omopReleaseConceptStrataFrame(sub, built, "concept_id",
                                             "concept_name", names_by_key),
         built = list(built))
  }
}

# 506: one death per person (the earliest death date), so the basis is
# persons, never duplicated death rows.
.omopReleaseRunAgeAtDeath <- function(handle, ctx, params, route, snap) {
  death <- .qualifyTable(handle, "death")
  person <- .qualifyTable(handle, "person")
  .omopDiagAssertPersons(handle, ctx, death, "e", "person_id",
                         where_sql = "e.death_date IS NOT NULL")
  sj <- .omopScopeJoin(ctx, "e", "person_id")
  age <- paste0(.omopYearExpr(handle, "MIN(e.death_date)"),
                " - p.year_of_birth")
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT e.person_id AS person_id, p.gender_concept_id AS gender, ", age,
    " AS v FROM ", death, " e INNER JOIN ", person,
    " p ON p.person_id = e.person_id", sj$join,
    " WHERE e.death_date IS NOT NULL",
    " GROUP BY e.person_id, p.gender_concept_id, p.year_of_birth"))
  owner <- .dsomopReleaseOwner(
    id = "achilles/age-at-death/by-gender/v1", family = "HM",
    grid = .dsomopReleaseGrid("age_5y"), strata = .omopReleaseGenderStrata(),
    bounds = c(0, 110),
    estimand = paste0("year-only age at death (earliest death record per ",
                      "person), by gender, clipped to [0, 110]"))
  sub <- data.frame(person_id = raw$person_id %||% character(0),
                    stratum = .omopReleaseGenderKey(raw$gender),
                    v = suppressWarnings(as.numeric(raw$v)),
                    stringsAsFactors = FALSE)
  built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
  labels <- owner$strata$labels
  st <- built$strata
  lab <- labels[match(st$stratum, labels$stratum), , drop = FALSE]
  df <- cbind(data.frame(concept_id = lab$gender_concept_id,
                         concept_name = lab$gender_name,
                         stringsAsFactors = FALSE),
              .omopReleaseDistColumns(st))
  list(df = df, built = list(built))
}

# 715/716/717: per (drug concept, person) mean of a drug-exposure field.
.omopReleaseRunDrugField <- function(field) {
  force(field)
  function(handle, ctx, params, route, snap) {
    if (!.omopAchillesColumnPresent(handle, "drug_exposure", field)) {
      return(list(df = data.frame(), built = list()))
    }
    table <- .qualifyTable(handle, "drug_exposure")
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)
    .omopDiagAssertPersons(handle, ctx, table, "e", "person_id")
    sj <- .omopScopeJoin(ctx, "e", "person_id")
    raw <- .omopReleaseFetch(handle, paste0(
      "SELECT pp.covariate_id, cc.concept_name AS covariate_name, ",
      "pp.person_id, pp.v FROM (SELECT e.drug_concept_id AS covariate_id, ",
      "e.person_id AS person_id, AVG(CAST(e.", field, " AS FLOAT)) AS v FROM ",
      table, " e", sj$join, " WHERE e.", field, " IS NOT NULL GROUP BY ",
      "e.drug_concept_id, e.person_id) pp LEFT JOIN ", concept,
      " cc ON cc.concept_id = pp.covariate_id"))
    grid <- switch(field,
      days_supply = .dsomopReleaseGrid("days_supply"),
      refills = .dsomopReleaseGrid("count_per_person"),
      quantity = .dsomopReleaseGrid("positive_125"))
    owner <- .dsomopReleaseOwner(
      id = paste0("achilles/drug-", field, "-person-mean-by-concept/v1"),
      family = "HM", grid = grid, strata = list(kind = "data"),
      cap = .DSOMOP_RELEASE_MULTI_CAP,
      bounds = if (is.null(grid$lower)) NULL else c(grid$lower, grid$upper),
      estimand = paste0("per-person mean ", field, " of each drug concept; ",
                        "each person counted in at most ",
                        .DSOMOP_RELEASE_MULTI_CAP, " drug concepts (lowest ",
                        "concept ids)",
                        if (is.null(grid$lower)) paste0(
                          "; no public scale, so the mean and SD are ",
                          "unavailable") else ""))
    sub <- data.frame(person_id = raw$person_id %||% character(0),
                      stratum = paste0("concept=", raw$covariate_id),
                      stratum_order = suppressWarnings(as.numeric(
                        raw$covariate_id)),
                      v = suppressWarnings(as.numeric(raw$v)),
                      stringsAsFactors = FALSE)
    sub <- sub[!is.na(sub$stratum_order), , drop = FALSE]
    names_by_key <- stats::setNames(as.character(raw$covariate_name),
                                    paste0("concept=", raw$covariate_id))
    built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
    list(df = .omopReleaseConceptStrataFrame(sub, built, "covariate_id",
                                             "covariate_name", names_by_key),
         built = list(built))
  }
}

# --- C7: reviewed QueryLibrary histograms (tools_auto) -------------------------

# Public grid of each reviewed histogram question.
.omopReleaseToolsGrid <- function(spec) {
  if (identical(spec$reducer, "count") ||
      identical(spec$breaks, .DSOMOP_TOOLS_COUNT_BREAKS)) {
    return(.dsomopReleaseGrid("count_per_person"))
  }
  if (identical(spec$breaks, .DSOMOP_TOOLS_SUPPLY_BREAKS)) {
    return(.dsomopReleaseGrid("days_supply"))
  }
  .dsomopReleaseGrid("duration_days")
}

.omopReleaseRunToolsHistogram <- function(handle, ctx, params, route, snap) {
  entry <- ctx$entry
  spec <- entry$meta$tools_spec
  concept_id <- .dsomopToolsConceptParam(spec, params)
  values <- .dsomopToolsPersonValues(handle, spec, concept_id)
  grid <- .omopReleaseToolsGrid(spec)
  gender <- identical(spec$strata, "gender")
  owner <- .dsomopReleaseOwner(
    id = paste0("querylibrary/", entry$meta$query_id, "/concept=",
                if (is.null(concept_id)) "none" else concept_id, "/v1"),
    family = "H", grid = grid,
    strata = if (gender) .omopReleaseGenderStrata() else "root",
    estimand = spec$value_label)
  sub <- data.frame(
    person_id = values$person_id,
    stratum = if (gender) .omopReleaseGenderKey(values$stratum) else
      rep("root", nrow(values)),
    v = as.numeric(values$v), stringsAsFactors = FALSE)
  built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
  bins <- .dsomopReleaseBinFrame(grid)
  label_cols <- .dsomopToolsLabelCols(spec)
  if (gender) {
    labels <- owner$strata$labels
    df <- do.call(rbind, lapply(seq_len(nrow(labels)), function(i) {
      cbind(data.frame(gender_concept_id = labels$gender_concept_id[[i]],
                       gender_name = labels$gender_name[[i]],
                       stringsAsFactors = FALSE),
            bins, n_persons = .dsomopReleaseRound(
              built$bins[[labels$stratum[[i]]]]))
    }))
  } else {
    concept_label <- if (!is.null(concept_id) &&
                         identical(concept_id, spec$all_concepts_value %||% -1L)) {
      "All visit types"
    } else .dsomopToolsConceptName(handle, concept_id)
    df <- cbind(stats::setNames(
      data.frame(rep(concept_id, nrow(bins)), rep(concept_label, nrow(bins)),
                 stringsAsFactors = FALSE), label_cols),
      bins, n_persons = .dsomopReleaseRound(built$bins[["root"]]))
  }
  rownames(df) <- NULL
  list(df = df, built = list(built))
}

.omopReleaseRunToolsPartition <- function(handle, ctx, params, route, snap) {
  spec <- ctx$entry$meta$tools_spec
  switch(spec$partition,
    year_of_birth = .omopReleaseRunBirthYear("year_of_birth")(
      handle, ctx, params, route, snap),
    month_of_birth = .omopReleaseRunBirthMonth(handle, ctx, params, route,
                                               snap),
    gender_age = .omopReleaseRunBirthYear("gender_by_age_decade")(
      handle, ctx, params, route, snap))
}

# --- C7: long observation periods (QueryLibrary) --------------------------------

# Persons whose longest observation period is at least min_days: a tail view
# of the one public histogram of the longest period length.
.omopReleaseRunLongPeriod <- function(handle, ctx, params, route, snap) {
  obs <- .qualifyTable(handle, "observation_period")
  .omopDiagAssertPersons(handle, ctx, obs, "op", "person_id")
  length_sql <- .omopDateDiffDays(handle, "op.observation_period_end_date",
                                  "op.observation_period_start_date")
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT op.person_id AS person_id, MAX(", length_sql, ") AS v FROM ", obs,
    " op GROUP BY op.person_id"))
  grid <- .dsomopReleaseGrid("duration_days")
  owner <- .dsomopReleaseOwner(
    id = "querylibrary/observation_period.longest-period-days/v1",
    family = "H", grid = grid,
    estimand = "length in days of each person's longest observation period")
  sub <- data.frame(person_id = raw$person_id %||% character(0),
                    stratum = rep("root", nrow(raw)),
                    v = suppressWarnings(as.numeric(raw$v)),
                    stringsAsFactors = FALSE)
  built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
  min_days <- suppressWarnings(as.numeric(params$min_days %||% "365"))
  first <- match(min_days, grid$breaks)
  if (is.na(first)) first <- .dsomopReleaseBinIndex(min_days, grid)
  counts <- built$bins[["root"]]
  df <- data.frame(n_persons = .dsomopReleaseRound(
    sum(counts[seq.int(first, length(counts))])))
  list(df = df, built = list(built))
}

# --- The closed registry -----------------------------------------------------------

#' Closed registry of protected release contracts
#'
#' Keyed by the canonical catalogue entry name (aliases resolve through their
#' \code{alias_target}). \code{class} is the output class of the inventory
#' (C2 sparse partitions, C6 person-time, C7 per-person distributions, C8
#' time-to-event, C9 effects, mixed); \code{fixed_basis} marks contracts whose
#' unscoped run is over a public population basis; \code{masked_scoped} and
#' \code{masked_always} name the demonstrated-disclosive fields of the
#' standard route.
#'
#' @return Named list of contracts.
#' @keywords internal
.omopToolsReleaseContracts <- function() {
  dist <- .DSOMOP_RELEASE_DIST_FIELDS
  contract <- function(class, run, fixed_basis = TRUE,
                       masked_scoped = character(0),
                       masked_always = character(0)) {
    list(class = class, run = run, fixed_basis = fixed_basis,
         masked_scoped = masked_scoped, masked_always = masked_always)
  }
  out <- list(
    "dsomop:achilles.3" = contract("C2", .omopReleaseRunBirthYear("achilles_3")),
    "dsomop:achilles.10" = contract("C2",
                                    .omopReleaseRunBirthYear("achilles_10")),
    "dsomop:achilles.101" = contract("C2", .omopReleaseRunFirstObs(
      "age", "root", "bands")),
    "dsomop:achilles.102" = contract("C2", .omopReleaseRunFirstObs(
      "age", "gender", "bands")),
    "dsomop:achilles.103" = contract("C7", .omopReleaseRunFirstObs(
      "age", "root"), masked_scoped = dist),
    "dsomop:achilles.104" = contract("C7", .omopReleaseRunFirstObs(
      "age", "gender"), masked_scoped = dist),
    "dsomop:achilles.105" = contract("C7", .omopReleaseRunFirstObs(
      "length", "root"), masked_scoped = dist),
    "dsomop:achilles.106" = contract("C7", .omopReleaseRunFirstObs(
      "length", "gender"), masked_scoped = dist),
    "dsomop:achilles.107" = contract("C7", .omopReleaseRunFirstObs(
      "length", "age_decile"), masked_scoped = dist),
    "dsomop:achilles.506" = contract("C7", .omopReleaseRunAgeAtDeath,
                                     masked_scoped = dist),
    "dsomop:person.year_of_birth_distribution" = contract(
      "C2", .omopReleaseRunToolsPartition),
    "dsomop:person.birth_month_distribution" = contract(
      "C2", .omopReleaseRunToolsPartition),
    "dsomop:person.gender_by_age_decade" = contract(
      "C2", .omopReleaseRunToolsPartition),
    "dsomop:person.age_distribution_decade" = contract(
      "C2", .omopReleaseRunBirthYear("age_distribution_decade")),
    "dsomop:observation_period.long_period_count" = contract(
      "C7", .omopReleaseRunLongPeriod)
  )
  for (x in list(c(203L, "visit"), c(403L, "condition"),
                 c(603L, "procedure"), c(703L, "drug"),
                 c(803L, "observation"), c(1803L, "measurement"))) {
    out[[paste0("dsomop:achilles.", x[[1L]])]] <- contract(
      "C7", .omopReleaseRunDistinctConcepts(x[[2L]]), masked_scoped = dist)
  }
  for (x in list(c(206L, "visit"), c(406L, "condition"),
                 c(606L, "procedure"), c(706L, "drug"),
                 c(806L, "observation"), c(1806L, "measurement"),
                 c(2106L, "device"))) {
    out[[paste0("dsomop:achilles.", x[[1L]])]] <- contract(
      "C7", .omopReleaseRunAgeByConcept(x[[2L]]), masked_scoped = dist)
  }
  for (x in list(c(715L, "days_supply"), c(716L, "refills"),
                 c(717L, "quantity"))) {
    out[[paste0("dsomop:achilles.", x[[1L]])]] <- contract(
      "C7", .omopReleaseRunDrugField(x[[2L]]), masked_scoped = dist)
  }
  for (id in c("condition.duration_stats", "drug.duration_stats",
               "drug.days_supply_stats", "drug_era.exposure_count_stats",
               "condition_era.eras_per_person_stats", "visit.duration_stats",
               "observation_period.length_by_gender")) {
    out[[paste0("dsomop:", id)]] <- contract("C7",
                                             .omopReleaseRunToolsHistogram)
  }
  out
}

#' Resolve the protected release contract of a catalogue entry
#'
#' Only built-in adapters resolve a contract; external packs never can. A
#' reviewed QueryLibrary question in its standard summary shape has none.
#'
#' @param entry Catalog entry.
#' @return Contract (with its canonical \code{name}) or NULL.
#' @keywords internal
.omopToolsReleaseContract <- function(entry) {
  adapter <- entry$meta$adapter %||% ""
  if (!adapter %in% .DSOMOP_RELEASE_BUILTIN_ADAPTERS) return(NULL)
  if (identical(adapter, "tools_auto") &&
      identical(entry$meta$tools_shape, "summary")) {
    return(NULL)
  }
  name <- entry$meta$alias_target %||% entry$name
  contract <- .omopToolsReleaseContracts()[[name]]
  if (is.null(contract)) return(NULL)
  contract$name <- name
  contract
}

#' Run one protected release and attach its public label and components
#'
#' @param handle CDM handle.
#' @param ctx Run-path context (scope, entry).
#' @param params Effective (snapped) parameters.
#' @param contract Release contract.
#' @param route Protected route.
#' @param snap Parameter snapping result.
#' @param entry Catalog entry.
#' @return Released, labelled data frame.
#' @keywords internal
.omopToolsReleaseRun <- function(handle, ctx, params, contract, route, snap,
                                 entry) {
  result <- contract$run(handle, ctx, params, route, snap)
  df <- result$df
  if (!is.data.frame(df)) df <- data.frame()
  expected <- names(entry$meta$release_pooling$columns %||%
                      entry$meta$pooling_contract$columns %||% list())
  if (length(expected)) {
    if (nrow(df) == 0L) {
      df <- as.data.frame(stats::setNames(rep(list(logical(0)),
                                              length(expected)), expected),
                          stringsAsFactors = FALSE, optional = TRUE,
                          check.names = FALSE)
    } else if (!identical(names(df), expected)) {
      stop("Analysis '", entry$name, "' protected output does not match its ",
           "closed schema.", call. = FALSE)
    }
  }
  attr(df, "dsomop_protection") <- .omopToolsReleaseLabel(
    route, result$built, contract, snap, result$extra %||% list())
  attr(df, "dsomop_release") <- list(
    protocol = .DSOMOP_RELEASE_PROTOCOL,
    contract = .DSOMOP_RELEASE_CONTRACT_VERSION,
    entry = contract$name, protection = route$protection,
    reference_epsilon = route$policy$release_epsilon,
    owners = lapply(result$built, function(b) b$export))
  df
}
