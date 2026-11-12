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
    population = "cdm/person/v1",
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
    extra <- list(basis_reference_year = year)
    if (identical(view, "age_distribution_decade")) {
      # On the protected route the requested year is a view of the released
      # birth-year basis (N01): report it as the view year, not as snapped.
      requested <- snap$changes$reference_year$requested %||%
        params$reference_year %||% year
      view_year <- suppressWarnings(as.integer(requested))
      if (is.na(view_year)) view_year <- year
      effective <- snap$changes %||% list()
      effective$reference_year <- NULL
      extra$view_reference_year <- view_year
      extra$effective_parameters <- effective
    }
    list(df = df, built = list(built), extra = extra)
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
    # One eligible population for every stratification of this value: the
    # root person total and the gender/age-decile margins are shared (B03).
    .dsomopReleaseOwner(
      id = paste0("achilles/first-op-age-years/by-", strata, "/v1"),
      population = "cdm/first-observation-period/age/v1",
      family = "HM", grid = .dsomopReleaseGrid("age_5y"),
      strata = strata_spec, bounds = c(0, 110),
      estimand = paste0("year-only age at the first observation period ",
                        "(year of start minus year of birth), clipped to ",
                        "[0, 110]"))
  } else {
    grid <- .dsomopReleaseGrid("duration_days")
    .dsomopReleaseOwner(
      id = paste0("achilles/first-op-length-days/by-", strata, "/v1"),
      population = "cdm/first-observation-period/length/v1",
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
# Parent admission only, then the fixed unavailable shape (B09): the persons
# with an observation period must reach the support floor; the tail at the
# threshold is never counted.
.omopReleaseUnavailableLongPeriod <- function(handle, params) {
  obs <- .qualifyTable(handle, "observation_period")
  n <- .omopReleaseFetch(handle, paste0(
    "SELECT COUNT(DISTINCT op.person_id) AS n FROM ", obs, " op"))$n
  if (!length(n) || is.na(n) ||
      as.numeric(n[[1L]]) < .dsomopReleaseSupport()$floor) {
    .dsomopReleaseDisclosive()
  }
  data.frame(n_persons = NA_real_)
}

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
    population = "cdm/longest-observation-period/v1",
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
    "dsomop:observation_period.long_period_count" = c(contract(
      "C7", .omopReleaseRunLongPeriod),
      list(protected_always = TRUE,
           unavailable = .omopReleaseUnavailableLongPeriod))
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
  rate_fields <- c("person_days", "rate")
  effect_fields <- c("estimate", "ci_lo", "ci_hi", "log_estimate",
                     "se_log_estimate")
  for (id in c("dsomop:incidence.rate")) {
    out[[id]] <- contract("C6", .omopReleaseIncidenceRun, fixed_basis = FALSE,
                          masked_always = rate_fields)
  }
  for (id in c("dsomop:cm.mdrr", "dsomop:ohdsi.cohort_method.cm_result")) {
    out[[id]] <- contract("C6", .omopReleaseCmArmsRun("mdrr"),
                          fixed_basis = FALSE,
                          masked_always = c("person_days", "mdrr"))
  }
  out[["dsomop:cm.effect_estimate"]] <- contract(
    "C9", .omopReleaseCmArmsRun("effect"), fixed_basis = FALSE,
    masked_always = c("person_days", effect_fields))
  for (id in c("dsomop:cm.interaction_estimate",
               "dsomop:ohdsi.cohort_method.cm_interaction_result")) {
    out[[id]] <- contract("C9", .omopReleaseInteractionRun,
                          fixed_basis = FALSE,
                          masked_always = c("target_person_days",
                                            "comparator_person_days",
                                            effect_fields))
  }
  # Exact SCCS scalars are unavailable on every route (window sweeps of exact
  # SEs recover outcome counts and event days); their counts keep the
  # established controls. The protected q-gridded route is set in
  # tools_release_survival.R.
  for (id in c("dsomop:sccs.incidence_rate_ratio")) {
    out[[id]] <- contract("C9", NULL, fixed_basis = FALSE,
                          masked_always = c("irr", "ci_lo", "ci_hi",
                                            "log_irr", "se_log_irr"))
  }
  # Mixed frames: safe binary prevalence rows (or standard summary scalars)
  # keep their established controls; their protected fields are overlaid with
  # the protected components of their owners.
  mixed <- function(overlay, masked_scoped = dist,
                    masked_always = character(0)) {
    c(contract("mixed", NULL, fixed_basis = FALSE,
               masked_scoped = masked_scoped, masked_always = masked_always),
      list(mixed = TRUE, overlay = overlay))
  }
  # The diagnostics summary's MDRR is the cm.mdrr view of the same protected
  # arm components (G-05); its other scalars stay standard.
  for (id in c("dsomop:cm.diagnostics_summary",
               "dsomop:ohdsi.cohort_method.cm_diagnostics_summary")) {
    out[[id]] <- mixed(.omopReleaseOverlayCmDiagnostics,
                       masked_scoped = character(0), masked_always = "mdrr")
  }
  out[["dsomop:fe.continuous"]] <- contract(
    "C7", .omopReleaseRunFeContinuous, fixed_basis = FALSE,
    masked_scoped = dist)
  out[["dsomop:fe.comorbidity_index"]] <- contract(
    "C7", .omopReleaseRunComorbidityIndex, fixed_basis = FALSE,
    masked_scoped = dist)
  out[["dsomop:cohortdx.time_distribution"]] <- contract(
    "C7", .omopReleaseRunTimeDistribution, fixed_basis = FALSE,
    masked_scoped = dist)
  out[["dsomop:cm.followup_distribution"]] <- contract(
    "C7", .omopReleaseRunFollowup, fixed_basis = FALSE, masked_scoped = dist)
  out[["dsomop:ohdsi.cohort_diagnostics.temporal_covariate_value_dist"]] <-
    contract("C7", .omopReleaseRunTemporalDist, fixed_basis = FALSE,
             masked_scoped = dist)
  out[["dsomop:txpath.duration_eras"]] <- contract(
    "C7", .omopReleaseRunDurationEras, fixed_basis = FALSE,
    masked_scoped = dist)
  out[["dsomop:char.time_to_event"]] <- contract(
    "C8", .omopReleaseRunTimeToEvent, fixed_basis = FALSE)
  out[["dsomop:fe.table1"]] <- mixed(.omopReleaseOverlayTable1)
  out[["dsomop:char.target_covariates"]] <-
    mixed(.omopReleaseOverlayTargetCovariates)
  for (id in c("condition.duration_stats", "drug.duration_stats",
               "drug.days_supply_stats", "drug_era.exposure_count_stats",
               "condition_era.eras_per_person_stats", "visit.duration_stats",
               "observation_period.length_by_gender")) {
    out[[paste0("dsomop:", id)]] <- contract("C7",
                                             .omopReleaseRunToolsHistogram)
  }
  # Protected SCCS IRR and standard-mode person-time (tools_release_survival.R).
  out <- .omopToolsReleaseSurvivalContracts(out)
  # Record-weighted QueryLibrary summaries (tools_release_records.R).
  c(out, .omopReleaseRecordSummaryContracts())
}

#' Resolve the protected release contract of a catalogue entry
#'
#' Only built-in adapters resolve a contract; external packs never can. A
#' reviewed QueryLibrary question in its standard summary shape has none,
#' except the record-weighted summaries, whose protected route keeps that
#' shape (\code{summary_shape}).
#'
#' @param entry Catalog entry.
#' @return Contract (with its canonical \code{name}) or NULL.
#' @keywords internal
.omopToolsReleaseContract <- function(entry) {
  adapter <- entry$meta$adapter %||% ""
  if (!adapter %in% .DSOMOP_RELEASE_BUILTIN_ADAPTERS) return(NULL)
  name <- entry$meta$alias_target %||% entry$name
  contract <- .omopToolsReleaseContracts()[[name]]
  if (is.null(contract)) return(NULL)
  if (identical(adapter, "tools_auto") &&
      identical(entry$meta$tools_shape, "summary") &&
      !isTRUE(contract$summary_shape)) {
    return(NULL)
  }
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
    source = list(logical_source_id =
                    .dsomopReleaseLogicalSourceId(route$policy)),
    owners = lapply(result$built, function(b) b$export))
  df
}

# --- C6: incidence and CohortMethod person-time ------------------------------------

# Public product strata (e.g. gender x age band x calendar year).
.omopReleaseProductStrata <- function(dims) {
  if (length(dims) == 0L) {
    return(list(kind = "public", keys = "all",
                labels = data.frame(stratum = "all", label = "all",
                                    stringsAsFactors = FALSE)))
  }
  grid <- expand.grid(lapply(dims, function(d) seq_along(d$keys)),
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  keys <- apply(grid, 1L, function(idx) paste(vapply(seq_along(dims), function(j) {
    dims[[j]]$keys[[idx[[j]]]]
  }, character(1L)), collapse = "|"))
  labels <- apply(grid, 1L, function(idx) paste(vapply(seq_along(dims), function(j) {
    dims[[j]]$labels[[idx[[j]]]]
  }, character(1L)), collapse = "|"))
  list(kind = "public", keys = unname(keys),
       labels = data.frame(stratum = unname(keys), label = unname(labels),
                           stringsAsFactors = FALSE))
}

.omopReleaseIncidenceRun <- function(handle, ctx, params, route, snap) {
  if (is.null(ctx$scoped_cohort)) {
    return(list(df = data.frame(), built = list()))
  }
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))
  person <- .qualifyTable(handle, "person")
  tar_start <- as.integer(params$tar_start_offset %||% "0")
  tar_end <- as.integer(params$tar_end_offset %||% "0")
  anchor_start <- params$tar_anchor_start %||% "start"
  anchor_end <- params$tar_anchor_end %||% "end"
  by_age <- identical(params$strat_by_age %||% "0", "1")
  by_gender <- identical(params$strat_by_gender %||% "0", "1")
  by_year <- identical(params$strat_by_year %||% "0", "1")
  band <- as.integer(params$age_band_years %||% "10")
  out_src <- .omopOutcomeSource(handle, params$outcome_concept_id,
                                params$domain_code %||% "0")
  substrate <- .omopPersonOutcomeSubstrateSql(
    handle, cohort, out_src, tar_start, tar_end, anchor_start, anchor_end,
    horizon = .DSOMOP_RELEASE_TIME_CAP)
  anchor <- if (identical(anchor_start, "end")) "c.cohort_end_date" else
    "c.cohort_start_date"
  birth_dt <- .omopYearStartDateSql(handle, "p.year_of_birth")
  age_days <- .omopDateDiffDays(handle, anchor, "p.birth_dt")
  sql <- .sql_translate(paste0(
    "SELECT c.subject_id AS person_id, c.person_days AS t, ",
    "c.person_outcomes AS e, p.gender_concept_id AS gender, ",
    .omopFloorDivideSql(age_days, 365L), " AS age, ",
    .omopYearExpr(handle, anchor), " AS yr FROM ", substrate, " c ",
    "INNER JOIN ", person, " p ON p.person_id = c.subject_id"),
    handle$target_dialect)
  raw <- .omopReleaseFetch(handle, gsub("p.birth_dt", birth_dt, sql,
                                        fixed = TRUE))
  year_max <- .dsomopToolsReferenceYear()
  dims <- list()
  if (by_gender) {
    genders <- .dsomopToolsGenders()
    dims$gender <- list(keys = paste0("gender=", genders$gender_concept_id),
                        labels = genders$gender_name)
  }
  bands <- seq.int(0L, 110L, by = band)
  if (by_age) {
    dims$age <- list(keys = c(paste0("age=", bands), "age=unknown"),
                     labels = c(as.character(bands), "unknown"))
  }
  years <- seq.int(1970L, year_max)
  if (by_year) {
    dims$year <- list(keys = c("year=before_1970", paste0("year=", years),
                               "year=unknown"),
                      labels = c("before 1970", as.character(years),
                                 "unknown"))
  }
  strata <- .omopReleaseProductStrata(dims)
  stratum_of <- function(r) {
    parts <- character(0)
    if (by_gender) parts <- c(parts, .omopReleaseGenderKey(r$gender))
    if (by_age) {
      a <- suppressWarnings(as.integer(r$age))
      parts <- c(parts, ifelse(is.na(a) | a < 0L, "age=unknown",
                               paste0("age=", pmin((a %/% band) * band,
                                                   max(bands)))))
    }
    if (by_year) {
      y <- suppressWarnings(as.integer(r$yr))
      parts <- c(parts, ifelse(is.na(y) | y > year_max, "year=unknown",
                               ifelse(y < 1970L, "year=before_1970",
                                      paste0("year=", y))))
    }
    if (length(parts) == 0L) return(rep("all", nrow(r)))
    do.call(paste, c(as.list(data.frame(matrix(parts, nrow = nrow(r)),
                                        stringsAsFactors = FALSE)), sep = "|"))
  }
  sub <- if (nrow(raw)) data.frame(
    person_id = raw$person_id, stratum = stratum_of(raw),
    event = suppressWarnings(as.numeric(raw$e)),
    time = suppressWarnings(as.numeric(raw$t)),
    stringsAsFactors = FALSE) else
    data.frame(person_id = character(0), stratum = character(0),
               event = numeric(0), time = numeric(0))
  owner <- c(list(
    id = "incidence/first-outcome-person-time/v2",
    population = "c6/persons-at-risk/v1", strata = strata, cap = 1L,
    estimand = paste0("persons at risk, persons with a first outcome and ",
                      "observed person-days, each person's observed window ",
                      "capped at ", .DSOMOP_RELEASE_TIME_CAP,
                      " days for both events and time")),
    .omopReleaseC6Owners(.omopReleaseOutcomeKey(
      params$outcome_concept_id, params$domain_code %||% "0"), "full"))
  built <- .dsomopReleaseBuildC6(route$policy, route$protection, owner, sub)
  st <- built$strata
  lab <- strata$labels[match(st$stratum, strata$labels$stratum), ,
                       drop = FALSE]
  persons <- st$n_display
  events <- st$events_display
  time <- st$time_display
  df <- data.frame(
    stratum = lab$label, persons_at_risk = persons, person_days = time,
    outcomes = NA_real_, person_outcomes = events,
    proportion = .dsomopReleaseRound(ifelse(persons > 0, events / persons,
                                            NA_real_)),
    rate = .dsomopReleaseRound(ifelse(time > 0, events / time, NA_real_)),
    stringsAsFactors = FALSE)
  list(df = df, built = list(built))
}

# Canonical identity of an outcome definition: the sorted unique concept ids
# and the domain, framed injectively (not the request spelling).
.omopReleaseOutcomeKey <- function(concepts, domain) {
  ids <- trimws(unlist(strsplit(as.character(concepts %||% ""), ",",
                                fixed = TRUE)))
  ids <- suppressWarnings(as.numeric(ids[nzchar(ids)]))
  ids <- sort(unique(ids[is.finite(ids)]))
  .dsomopFrame("outcome/v1", ids, as.character(domain %||% "0"))
}

# The scientific owners of person-time components (B06): first-outcome status
# of one outcome definition, and observed person-time of one time contract.
# Request labels, subgroup predicates, scope names and window spelling never
# enter them; the realised maps carry the population and the window.
.omopReleaseC6Owners <- function(outcome_key, time = c("full", "to_first")) {
  time <- match.arg(time)
  list(
    status_owner = paste0("c6/first-outcome-status/", outcome_key),
    time_owner = if (identical(time, "full")) {
      "c6/observed-person-time/full-window/v1"
    } else paste0("c6/observed-person-time/to-first-outcome/", outcome_key))
}

# The two CohortMethod arms as independent strata of one owner.
.omopReleaseArmStrata <- function() {
  list(kind = "independent", keys = c("arm=target", "arm=comparator"),
       labels = data.frame(stratum = c("arm=target", "arm=comparator"),
                           arm = c("target", "comparator"),
                           stringsAsFactors = FALSE))
}

# The CohortMethod MDRR formula applied to displayed protected components
# (no banding or support re-test of already protected values).
.omopReleaseMdrr <- function(outcomes, target_days, comparator_days,
                             alpha = 0.05, power = 0.8, two_sided = TRUE) {
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05
  if (!is.finite(power) || power <= 0 || power >= 1) power <- 0.8
  D <- as.numeric(outcomes)
  td <- as.numeric(target_days)
  cd <- as.numeric(comparator_days)
  if (!all(is.finite(c(D, td, cd))) || D <= 0 || td <= 0 || cd <= 0) {
    return(NA_real_)
  }
  p <- td / (td + cd)
  z_alpha <- stats::qnorm(1 - (if (isTRUE(two_sided)) alpha / 2 else alpha))
  exp((z_alpha + stats::qnorm(power)) * sqrt(1 / (D * p * (1 - p))))
}

.omopReleaseCmArmsRun <- function(view) {
  force(view)
  function(handle, ctx, params, route, snap) {
    cohorts <- .omopTwoPopCohorts(handle, ctx)
    if (is.null(cohorts)) return(list(df = data.frame(), built = list()))
    tar <- list(
      start = as.integer(params$tar_start_offset %||% "1"),
      end = as.integer(params$tar_end_offset %||% "0"),
      anchor_start = params$tar_anchor_start %||% "start",
      anchor_end = params$tar_anchor_end %||% "end")
    out_src <- .omopOutcomeSource(handle, params$outcome_concept_id,
                                  params$outcome_domain_code %||% "0")
    outcome_key <- .omopReleaseOutcomeKey(
      params$outcome_concept_id, params$outcome_domain_code %||% "0")
    horizon <- .DSOMOP_RELEASE_TIME_CAP
    if (identical(view, "effect")) {
      ta <- .omopCmEffectArmData(handle, cohorts$a, 1L, out_src, tar,
                                 horizon = horizon)
      co <- .omopCmEffectArmData(handle, cohorts$b, 0L, out_src, tar,
                                 horizon = horizon)
      sub <- rbind(
        data.frame(person_id = ta$data$person_id, stratum = rep("arm=target",
                                                                nrow(ta$data)),
                   event = ta$data$event, time = ta$data$tar_days,
                   stringsAsFactors = FALSE),
        data.frame(person_id = co$data$person_id,
                   stratum = rep("arm=comparator", nrow(co$data)),
                   event = co$data$event, time = co$data$tar_days,
                   stringsAsFactors = FALSE))
      id <- "cohortmethod/arms-time-to-first-outcome/v2"
      time_kind <- "to_first"
      estimand <- paste0("per arm: persons, persons with a first outcome and ",
                         "time to first outcome or censoring in the observed ",
                         "window, capped at ", .DSOMOP_RELEASE_TIME_CAP,
                         " days for both events and time")
    } else {
      arm_sub <- function(cohort, label) {
        substrate <- .omopPersonOutcomeSubstrateSql(
          handle, cohort, out_src, tar$start, tar$end, tar$anchor_start,
          tar$anchor_end, horizon = horizon)
        raw <- .omopReleaseFetch(handle, paste0(
          "SELECT s.subject_id AS person_id, s.person_days AS t, ",
          "s.person_outcomes AS e FROM ", substrate, " s"))
        data.frame(person_id = raw$person_id %||% character(0),
                   stratum = rep(label, nrow(raw)),
                   event = suppressWarnings(as.numeric(raw$e)),
                   time = suppressWarnings(as.numeric(raw$t)),
                   stringsAsFactors = FALSE)
      }
      sub <- rbind(arm_sub(cohorts$a, "arm=target"),
                   arm_sub(cohorts$b, "arm=comparator"))
      id <- "cohortmethod/arms-first-outcome-person-time/v2"
      time_kind <- "full"
      estimand <- paste0("per arm: persons, persons with a first outcome and ",
                         "observed person-days, each person's observed window ",
                         "capped at ", .DSOMOP_RELEASE_TIME_CAP,
                         " days for both events and time")
    }
    owner <- c(list(id = id, population = "c6/persons-at-risk/v1",
                    strata = .omopReleaseArmStrata(), cap = 1L,
                    estimand = estimand),
               .omopReleaseC6Owners(outcome_key, time_kind))
    built <- .dsomopReleaseBuildC6(route$policy, route$protection, owner, sub)
    st <- built$strata
    t_row <- st[st$stratum == "arm=target", , drop = FALSE]
    c_row <- st[st$stratum == "arm=comparator", , drop = FALSE]
    floor <- .dsomopReleaseSupport()$floor
    if (identical(view, "mdrr")) {
      alpha <- suppressWarnings(as.numeric(params$alpha %||% "0.05"))
      power <- suppressWarnings(as.numeric(params$power %||% "0.8"))
      two_sided <- !identical(params$two_sided %||% "1", "0")
      ok <- t_row$n_display >= floor && c_row$n_display >= floor &&
        t_row$time_display > 0 && c_row$time_display > 0
      mdrr <- if (isTRUE(ok)) .dsomopReleaseRound(.omopReleaseMdrr(
        t_row$events_display + c_row$events_display, t_row$time_display,
        c_row$time_display, alpha = alpha, power = power,
        two_sided = two_sided)) else NA_real_
      df <- data.frame(arm = c("target", "comparator"),
                       persons = c(t_row$n_display, c_row$n_display),
                       person_days = c(t_row$time_display,
                                       c_row$time_display),
                       outcomes = c(t_row$events_display,
                                    c_row$events_display),
                       mdrr = rep(as.numeric(mdrr), 2L),
                       stringsAsFactors = FALSE)
    } else {
      model_type <- params$model_type %||% "cox"
      cox <- NULL
      rr <- if (identical(model_type, "poisson")) {
        .dsomopReleaseRateRatio(t_row, c_row, built$noise_sd, floor)
      } else {
        # The grouped-time Cox hazard ratio from the protected exit-time x
        # status basis (tools_release_survival.R); a descriptive rate ratio is
        # never relabelled as a hazard ratio.
        cox <- .omopReleaseGroupedCox(handle, cohorts, out_src, tar,
                                      outcome_key, route, built,
                                      list(target = ta$data,
                                           comparator = co$data))
        cox$rr
      }
      row <- function(label, r) data.frame(
        arm = label, model_type = model_type, persons = r$n_display,
        person_days = r$time_display, outcomes = r$events_display,
        estimate = rr[["estimate"]], ci_lo = rr[["ci_lo"]],
        ci_hi = rr[["ci_hi"]], log_estimate = rr[["log_estimate"]],
        se_log_estimate = rr[["se_log_estimate"]], stringsAsFactors = FALSE)
      df <- rbind(row("target", t_row), row("comparator", c_row))
    }
    rownames(df) <- NULL
    is_cox <- identical(view, "effect") &&
      !identical(params$model_type %||% "cox", "poisson")
    extra <- list()
    if (is_cox) {
      extra$estimand <- paste0(
        "grouped-time Cox (Breslow ties) on public exit bins, capped at ",
        .DSOMOP_RELEASE_TIME_CAP, " days; not the raw Efron/continuous ",
        "estimator")
      if (!is.null(cox$reason)) {
        extra$unavailable_reason <- paste0(
          "The protected grouped-time Cox hazard ratio is unavailable (",
          cox$reason, ").")
      }
    }
    list(df = df, built = c(list(built), if (is_cox) list(cox$built)),
         extra = extra)
  }
}

# CohortMethod interaction: the complete subgroup family is admitted as one
# decision on its exact support (the standard family rule), then every level's
# arm components are protected and the Poisson rate ratio derived from them.
.omopReleaseInteractionRun <- function(handle, ctx, params, route, snap) {
  cohorts <- .omopTwoPopCohorts(handle, ctx)
  if (is.null(cohorts)) return(list(df = data.frame(), built = list()))
  model_type <- tolower(params$model_type %||% "poisson")
  tar <- list(
    start = as.integer(params$tar_start_offset %||% "1"),
    end = as.integer(params$tar_end_offset %||% "0"),
    anchor_start = params$tar_anchor_start %||% "start",
    anchor_end = params$tar_anchor_end %||% "end")
  out_src <- .omopOutcomeSource(handle, params$outcome_concept_id,
                                params$outcome_domain_code %||% "0")
  subgroups <- .omopCmInteractionSubgroups(handle, params)
  frame_of <- function(cohort, predicate) {
    fr <- .omopCmSubgroupSubjectFrame(handle, cohort, out_src, tar, predicate,
                                      horizon = .DSOMOP_RELEASE_TIME_CAP)
    list(frame = fr, persons = nrow(fr),
         outcomes = sum(fr$events, na.rm = TRUE),
         outcome_persons = sum(fr$events > 0, na.rm = TRUE),
         person_days = sum(fr$person_days, na.rm = TRUE))
  }
  arms <- list(target = frame_of(cohorts$a, NULL),
               comparator = frame_of(cohorts$b, NULL))
  level_counts <- lapply(subgroups, function(sg) {
    if (is.null(sg$predicate)) return(arms)
    list(target = frame_of(cohorts$a, sg$predicate),
         comparator = frame_of(cohorts$b, sg$predicate))
  })
  family <- .omopCmInteractionFamilyCheck(level_counts, arms)
  if (!isTRUE(family$releasable)) {
    return(list(df = data.frame(), built = list(),
                extra = list(subgroup_family = "withheld")))
  }
  floor <- .dsomopReleaseSupport()$floor
  built_all <- list()
  rows <- lapply(seq_along(subgroups), function(i) {
    sg <- subgroups[[i]]
    lv <- level_counts[[i]]
    to_sub <- function(fr, label) data.frame(
      person_id = fr$frame$person_id %||% character(0),
      stratum = rep(label, nrow(fr$frame)),
      event = as.numeric(fr$frame$events > 0),
      time = as.numeric(fr$frame$person_days), stringsAsFactors = FALSE)
    sub <- rbind(to_sub(lv$target, "arm=target"),
                 to_sub(lv$comparator, "arm=comparator"))
    # The subgroup label and predicate are presentation: membership enters
    # through the realised maps, so equal memberships share their components
    # with each other and with the overall arms (B06).
    owner <- c(list(id = "cohortmethod/interaction-first-outcome-person-time/v2",
                    population = "c6/persons-at-risk/v1",
                    strata = .omopReleaseArmStrata(), cap = 1L,
                    estimand = paste0(
                      "per arm within subgroup '", sg$label, "': persons, ",
                      "persons with a first outcome and observed ",
                      "person-days, each person's observed window capped at ",
                      .DSOMOP_RELEASE_TIME_CAP,
                      " days for both events and time")),
               .omopReleaseC6Owners(.omopReleaseOutcomeKey(
                 params$outcome_concept_id,
                 params$outcome_domain_code %||% "0"), "full"))
    built <- .dsomopReleaseBuildC6(route$policy, route$protection, owner, sub)
    built_all[[length(built_all) + 1L]] <<- built
    st <- built$strata
    t_row <- st[st$stratum == "arm=target", , drop = FALSE]
    c_row <- st[st$stratum == "arm=comparator", , drop = FALSE]
    rr <- if (identical(model_type, "poisson")) {
      .dsomopReleaseRateRatio(t_row, c_row, built$noise_sd, floor)
    } else c(estimate = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
             log_estimate = NA_real_, se_log_estimate = NA_real_)
    data.frame(
      subgroup_label = sg$label, model_type = model_type,
      target_persons = t_row$n_display, comparator_persons = c_row$n_display,
      target_outcomes = t_row$events_display,
      comparator_outcomes = c_row$events_display,
      target_person_days = t_row$time_display,
      comparator_person_days = c_row$time_display,
      estimate = rr[["estimate"]], ci_lo = rr[["ci_lo"]],
      ci_hi = rr[["ci_hi"]], log_estimate = rr[["log_estimate"]],
      se_log_estimate = rr[["se_log_estimate"]], stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, rows)
  rownames(df) <- NULL
  list(df = df, built = built_all,
       extra = list(subgroup_family = "released"))
}

# --- C7/C8: native distributions -----------------------------------------------

# Data-dependent strata ranked on their released person totals only: a first
# pass draws the totals, the display keeps the top N (stable public ids break
# ties), and the full components are drawn for those rows.
.omopReleaseTopStrata <- function(route, owner, sub, top_n) {
  first <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub,
                               n_only = TRUE)
  st <- first$strata
  if (nrow(st) == 0L) return(first)
  ids <- suppressWarnings(as.numeric(sub("^[a-z_]+=", "", st$stratum)))
  ord <- order(-st$n_display, ids, st$stratum, method = "radix",
               na.last = TRUE)
  keep <- utils::head(st$stratum[ord], max(0L, as.integer(top_n)))
  .dsomopReleaseBuild(route$policy, route$protection, owner, sub,
                      data_strata_keys = keep)
}

.omopReleaseMetricRow <- function(built, metric) {
  st <- built$strata
  cbind(data.frame(metric = rep(metric, nrow(st)), stringsAsFactors = FALSE),
        data.frame(count_value = st$n_display, n_persons = st$n_display),
        .omopReleaseDistColumns(st)[, -1L, drop = FALSE])
}

# One bounded value per person: the person-weighted mean of episode values.
.omopReleasePersonMeans <- function(vals) {
  if (!is.data.frame(vals) || nrow(vals) == 0L) {
    return(data.frame(person_id = character(0), stratum = character(0),
                      v = numeric(0), stringsAsFactors = FALSE))
  }
  names(vals) <- tolower(names(vals))
  v <- suppressWarnings(as.numeric(vals$v))
  keep <- !is.na(v) & !is.na(vals$person_id)
  if (!any(keep)) return(.omopReleaseEmptyValues())
  agg <- stats::aggregate(v ~ person_id, data = data.frame(
    person_id = .dsomopReleasePersonIdText(vals$person_id[keep]),
    v = v[keep], stringsAsFactors = FALSE), FUN = mean)
  data.frame(person_id = agg$person_id, stratum = rep("root", nrow(agg)),
             v = agg$v, stringsAsFactors = FALSE)
}

.omopReleaseAgeAtIndexValues <- function(handle, ctx) {
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  person <- .qualifyTable(handle, "person")
  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))
  age_at <- .omopDateDiffDays(handle, "c.cohort_start_date", "p.birth_dt")
  sql <- .sql_translate(paste0(
    "SELECT c.subject_id AS person_id, ", .omopFloorDivideSql(age_at, 365L),
    " AS v FROM ", cohort, " c INNER JOIN ", person,
    " p ON p.person_id = c.subject_id"), handle$target_dialect)
  sql <- gsub("p.birth_dt", .omopYearStartDateSql(handle, "p.year_of_birth"),
              sql, fixed = TRUE)
  .omopReleasePersonMeans(.executeQuery(handle, sql))
}

.omopReleaseAgeAtIndexOwner <- function() {
  .dsomopReleaseOwner(
    id = "cohort/age-at-index-years/v1", family = "HM",
    grid = .dsomopReleaseGrid("age_5y"), bounds = c(0, 110),
    estimand = paste0("age in whole years at cohort entry (person mean over ",
                      "episodes), clipped to [0, 110]"))
}

.omopReleaseTimeInCohortValues <- function(handle, ctx) {
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))
  end_col <- .omopCohortEndDateCol(handle, cohort)
  diff <- .omopDateDiffDays(handle, paste0("c.", end_col), "c.cohort_start_date")
  .omopReleasePersonMeans(.executeQuery(handle, .sql_translate(paste0(
    "SELECT c.subject_id AS person_id, ", diff, " AS v FROM ", cohort, " c"),
    handle$target_dialect)))
}

.omopReleaseDurationOwner <- function(id, estimand,
                                      grid = .dsomopReleaseGrid("duration_days")) {
  .dsomopReleaseOwner(id = id, family = "HM", grid = grid,
                      bounds = c(grid$lower, grid$upper), estimand = estimand)
}

.omopReleaseConceptOwner <- function(id, value_kind, estimand) {
  grid <- if (identical(value_kind, "value")) .dsomopReleaseGrid("signed_125") else
    .dsomopReleaseGrid("count_per_person")
  .dsomopReleaseOwner(
    id = id, family = "HM", grid = grid, strata = list(kind = "data"),
    cap = .DSOMOP_RELEASE_MULTI_CAP,
    bounds = if (is.null(grid$lower)) NULL else c(grid$lower, grid$upper),
    estimand = paste0(estimand, "; each person counted in at most ",
                      .DSOMOP_RELEASE_MULTI_CAP, " concepts (lowest concept ",
                      "ids)", if (is.null(grid$lower)) paste0(
                        "; no public scale, so the mean and SD are ",
                        "unavailable and quantiles come from a public ",
                        "1-2-5 histogram") else ""))
}

.omopReleaseConceptSubstrate <- function(raw) {
  if (!is.data.frame(raw) || nrow(raw) == 0L) {
    return(data.frame(person_id = character(0), stratum = character(0),
                      stratum_order = numeric(0), v = numeric(0)))
  }
  sub <- data.frame(person_id = raw$person_id,
                    stratum = paste0("concept=", raw$covariate_id),
                    stratum_order = suppressWarnings(as.numeric(
                      raw$covariate_id)),
                    v = suppressWarnings(as.numeric(raw$v)),
                    stringsAsFactors = FALSE)
  sub[!is.na(sub$stratum_order), , drop = FALSE]
}

.omopReleaseConceptRows <- function(built, raw, extra_cols = list()) {
  st <- built$strata
  ids <- suppressWarnings(as.numeric(sub("^concept=", "", st$stratum)))
  names_by_id <- stats::setNames(as.character(raw$covariate_name),
                                 as.character(raw$covariate_id))
  base <- data.frame(covariate_id = ids,
                     covariate_name = unname(names_by_id[as.character(ids)]),
                     count_value = st$n_display, n_persons = st$n_display,
                     stringsAsFactors = FALSE)
  out <- cbind(base, .omopReleaseDistColumns(st)[, -1L, drop = FALSE])
  for (name in rev(names(extra_cols))) {
    out <- cbind(stats::setNames(data.frame(rep(extra_cols[[name]],
                                                nrow(out)),
                                            stringsAsFactors = FALSE), name),
                 out)
  }
  out <- out[order(-out$count_value, out$covariate_id, method = "radix"), ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}

.omopReleaseRunFeContinuous <- function(handle, ctx, params, route, snap) {
  if (is.null(ctx$scoped_cohort)) return(list(df = data.frame(), built = list()))
  metric <- params$metric %||% "measurement_value"
  if (identical(metric, "age") || identical(metric, "time_in_cohort")) {
    if (identical(metric, "age")) {
      owner <- .omopReleaseAgeAtIndexOwner()
      sub <- .omopReleaseAgeAtIndexValues(handle, ctx)
    } else {
      owner <- .omopReleaseDurationOwner(
        "cohort/time-in-cohort-days/v1",
        "days in cohort (person mean over episodes), clipped to [0, 10957]")
      sub <- .omopReleaseTimeInCohortValues(handle, ctx)
    }
    built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
    st <- built$strata
    df <- cbind(data.frame(metric = metric, covariate_id = NA_integer_,
                           covariate_name = metric,
                           unit_concept_id = NA_real_,
                           unit_name = if (identical(metric, "age")) "years"
                             else "days",
                           count_value = st$n_display,
                           n_persons = st$n_display, stringsAsFactors = FALSE),
                .omopReleaseDistColumns(st)[, -1L, drop = FALSE])
    return(list(df = df, built = list(built)))
  }
  # Measurement/observation values: per person within concept x unit (B13).
  domain <- params$domain_code %||% "3"
  raw <- .omopReleaseConceptUnitValues(handle, ctx, domain)
  res <- .omopReleaseUnitStrataBuild(
    route, raw, paste0("fe/person-mean-value-by-concept-unit/domain=", domain),
    "per-person mean value of each concept in the cohort",
    top_n = as.integer(params$top_n %||% "50"))
  df <- .omopReleaseUnitRows(res$strata, raw, list(metric = metric))
  list(df = df, built = res$built, extra = list(weighting = "person"))
}

.omopComorbiditySeeds <- function(index_type) {
  switch(tolower(index_type %||% "charlson"),
    "dcsi"        = c(443767L, 4209145L, 4271003L, 321822L, 4030518L),
    "chads2"      = c(316139L, 320128L, 201820L, 381591L, 313217L),
    "cha2ds2vasc" = c(316139L, 320128L, 201820L, 381591L, 313217L, 321822L),
    "hfrs"        = c(316139L, 4182210L, 201820L, 192671L, 198199L),
    c(4329847L, 316139L, 381591L, 4182210L, 4063381L, 255573L, 201820L,
      192671L, 4030518L, 443392L, 4178338L, 432851L, 4212540L, 439727L))
}

.omopReleaseComorbidity <- function(handle, ctx, route, index_type) {
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  cond <- .qualifyTable(handle, "condition_occurrence")
  seeds <- .omopComorbiditySeeds(index_type)
  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))
  comp <- vapply(seeds, function(s) {
    ids <- .resolveConceptSet(handle, list(concepts = s,
                                           include_descendants = TRUE))
    if (length(ids) == 0) ids <- s
    paste0("(CASE WHEN EXISTS (SELECT 1 FROM ", cond, " co WHERE ",
           "co.person_id = c.subject_id AND co.condition_concept_id IN (",
           paste(ids, collapse = ", "), ")) THEN 1 ELSE 0 END)")
  }, character(1L))
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT c.subject_id AS person_id, ", paste(comp, collapse = " + "),
    " AS v FROM (SELECT DISTINCT subject_id FROM ", cohort, ") c"))
  # The public grid spans exactly the index's possible scores, one bin each.
  grid <- list(id = paste0("integer-score-0-", length(seeds), "-v1"),
               breaks = seq.int(0L, length(seeds)), lower = 0,
               upper = length(seeds), unit = "points")
  owner <- .dsomopReleaseOwner(
    id = paste0("fe/component-burden-score/", tolower(index_type %||%
                                                         "charlson"), "/v1"),
    family = "HM", grid = grid, bounds = c(0, length(seeds)),
    estimand = paste0(tolower(index_type %||% "charlson"),
                      " component-burden score per person (0 to ",
                      length(seeds), ")"))
  sub <- data.frame(person_id = raw$person_id %||% character(0),
                    stratum = rep("root", nrow(raw)),
                    v = suppressWarnings(as.numeric(raw$v)),
                    stringsAsFactors = FALSE)
  .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
}

.omopReleaseRunComorbidityIndex <- function(handle, ctx, params, route, snap) {
  if (is.null(ctx$scoped_cohort)) return(list(df = data.frame(), built = list()))
  index_type <- params$index_type %||% "charlson"
  built <- .omopReleaseComorbidity(handle, ctx, route, index_type)
  list(df = .omopReleaseMetricRow(built, paste0(index_type, "_index")),
       built = list(built))
}

.omopReleaseRunTimeDistribution <- function(handle, ctx, params, route, snap) {
  if (is.null(ctx$scoped_cohort)) return(list(df = data.frame(), built = list()))
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  metric <- params$metric %||% "time_in_cohort"
  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))
  if (metric %in% c("prior_observation", "post_observation")) {
    observation <- .temporalObservationSqlContract(handle, cohort)
    invalid <- suppressWarnings(as.numeric(
      .executeQuery(handle, observation$invalid)[[1L]][[1L]]))
    if (length(invalid) != 1L || is.na(invalid) || invalid != 0) {
      stop("Every cohort episode must have exactly one observation_period ",
           "covering its index date.", call. = FALSE)
    }
    source_sql <- observation$observed
    value_expr <- if (identical(metric, "prior_observation")) {
      .omopDateDiffDays(handle, "c.cohort_start_date",
                        "c.observation_period_start_date")
    } else {
      .omopDateDiffDays(handle, "c.observation_period_end_date",
                        "c.cohort_start_date")
    }
  } else {
    ranked <- .rankedCohortSql(cohort, handle)
    source_sql <- paste0(
      "SELECT r.cohort_row_id, r.subject_id AS person_id, ",
      "r.cohort_start_date, r.cohort_end_date FROM ", ranked, " r")
    value_expr <- .omopDateDiffDays(handle, "c.cohort_end_date",
                                    "c.cohort_start_date")
  }
  vals <- .executeQuery(handle, .sql_translate(paste0(
    "SELECT x.person_id, x.v FROM (SELECT c.person_id, ", value_expr,
    " AS v FROM (", source_sql, ") c) x WHERE x.v IS NOT NULL"),
    handle$target_dialect))
  owner <- .omopReleaseDurationOwner(
    paste0("cohortdx/", metric, "-days/v1"),
    paste0(gsub("_", " ", metric), " in days (person mean over episodes), ",
           "clipped to [0, 10957]"))
  built <- .dsomopReleaseBuild(route$policy, route$protection, owner,
                               .omopReleasePersonMeans(vals))
  list(df = .omopReleaseMetricRow(built, metric), built = list(built))
}

.omopReleaseRunFollowup <- function(handle, ctx, params, route, snap) {
  if (is.null(ctx$scoped_cohort)) return(list(df = data.frame(), built = list()))
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  tar_start <- as.integer(params$tar_start_offset %||% "0")
  tar_end <- as.integer(params$tar_end_offset %||% "0")
  anchor_start <- params$tar_anchor_start %||% "start"
  anchor_end <- params$tar_anchor_end %||% "end"
  end_col <- .omopCohortEndDateCol(handle, cohort)
  anchor <- function(a) if (identical(a, "end")) end_col else "cohort_start_date"
  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))
  lo <- paste0("DATEADD(day, ", tar_start, ", c.", anchor(anchor_start), ")")
  hi <- paste0("DATEADD(day, ", tar_end, ", c.", anchor(anchor_end), ")")
  vals <- .executeQuery(handle, .sql_translate(paste0(
    "SELECT c.subject_id AS person_id, ", .omopDateDiffDays(handle, hi, lo),
    " AS v FROM ", cohort, " c"), handle$target_dialect))
  owner <- .omopReleaseDurationOwner(
    paste0("cohortmethod/time-at-risk-days/tar=", tar_start, ",", tar_end,
           ",", anchor_start, ",", anchor_end, "/v1"),
    paste0("requested time-at-risk length in days (person mean over ",
           "episodes), clipped to [0, ", .DSOMOP_RELEASE_TIME_CAP, "]"),
    grid = .dsomopReleaseGrid("tar_days"))
  built <- .dsomopReleaseBuild(route$policy, route$protection, owner,
                               .omopReleasePersonMeans(vals))
  list(df = .omopReleaseMetricRow(built, "time_at_risk"), built = list(built))
}

.omopReleaseRunTemporalDist <- function(handle, ctx, params, route, snap) {
  if (is.null(ctx$scoped_cohort)) return(list(df = data.frame(), built = list()))
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  domain <- params$domain_code %||% "3"
  src <- .omopCovariateSource(handle, domain)
  value_kind <- params$value_kind %||% "value"
  start_d <- as.integer(params$temporal_start_days %||% "-365")
  end_d <- as.integer(params$temporal_end_days %||% "-1")
  win_pred <- .ohdsiCohortDxWindowPredicate(
    handle, paste0("e.", src$date_col), start_d, end_d)
  cov_pred <- .ohdsiCohortDxCovariatePredicate(
    handle, paste0("e.", src$concept_col), params$covariate_ids)
  extra <- paste0(" AND ", win_pred,
                  if (nzchar(cov_pred)) paste0(" AND ", cov_pred) else "")
  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col)
  event_id_col <- .eventPrimaryKeyColumn(.buildBlueprint(handle),
                                         src$table_name)
  if (is.null(event_id_col)) {
    stop("Temporal covariate distributions require a stable OMOP event key.",
         call. = FALSE)
  }
  use_value <- identical(value_kind, "value") && !is.na(src$value_col)
  window <- paste0("[", start_d, ",", end_d, "]")
  if (use_value) {
    # Values per person within concept x unit (B13), after the public unit
    # conversions of the measurement scale registry.
    conv <- .omopReleaseUnitConversionSql(
      src, .dsomopReleaseMeasurementScales(),
      has_unit = .omopReleaseHasUnit(handle, src))
    matched <- paste0(
      "SELECT DISTINCT e.", event_id_col, " AS event_id, e.",
      src$concept_col, " AS covariate_id, e.", src$person_col,
      " AS person_id, ", conv$value, " AS v, ", conv$unit,
      " AS unit_concept_id FROM ", src$table, " e INNER JOIN ", cohort,
      " c ON c.subject_id = e.", src$person_col, " WHERE e.", src$value_col,
      " IS NOT NULL", extra)
    raw <- .omopReleaseFetch(handle, paste0(
      "SELECT pp.covariate_id, cc.concept_name AS covariate_name, ",
      "pp.unit_concept_id, uu.concept_name AS unit_name, pp.person_id, pp.v ",
      "FROM (SELECT me.covariate_id, me.unit_concept_id, me.person_id, ",
      "AVG(me.v) AS v FROM (", matched, ") me GROUP BY me.covariate_id, ",
      "me.unit_concept_id, me.person_id) pp LEFT JOIN ", concept,
      " cc ON cc.concept_id = pp.covariate_id LEFT JOIN ", concept,
      " uu ON uu.concept_id = pp.unit_concept_id"))
    res <- .omopReleaseUnitStrataBuild(
      route, raw, paste0("cohortdx/temporal-value-by-concept-unit/domain=",
                         domain, "/window=", start_d, ",", end_d,
                         "/covariates=", params$covariate_ids %||% "all"),
      paste0("per-person mean value of each concept in the index-relative ",
             "window ", window),
      top_n = as.integer(params$top_n %||% "50"))
    rows <- .omopReleaseUnitRows(res$strata, raw, list(time_window = window))
    return(list(df = rows, built = res$built,
                extra = list(weighting = "person")))
  }
  matched <- paste0(
    "SELECT DISTINCT e.", event_id_col, " AS event_id, e.", src$concept_col,
    " AS covariate_id, e.", src$person_col, " AS person_id",
    " FROM ", src$table, " e INNER JOIN ", cohort, " c ON c.subject_id = e.",
    src$person_col, " WHERE 1 = 1", extra)
  per_person <- paste0(
    "SELECT me.covariate_id, me.person_id, COUNT(*) AS v FROM (",
    matched, ") me GROUP BY me.covariate_id, me.person_id")
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT pp.covariate_id, cc.concept_name AS covariate_name, pp.person_id, ",
    "pp.v FROM (", per_person, ") pp LEFT JOIN ", concept,
    " cc ON cc.concept_id = pp.covariate_id"))
  kind <- "count"
  owner <- .omopReleaseConceptOwner(
    paste0("cohortdx/temporal-", kind, "-by-concept/domain=", domain,
           "/window=", start_d, ",", end_d, "/covariates=",
           params$covariate_ids %||% "all", "/v1"), kind,
    paste0("per-person ", if (use_value) "mean value" else "record count",
           " of each concept in the index-relative window ", window))
  built <- .omopReleaseTopStrata(route, owner,
                                 .omopReleaseConceptSubstrate(raw),
                                 as.integer(params$top_n %||% "50"))
  rows <- .omopReleaseConceptRows(built, raw, list(time_window = window))
  if (nrow(rows)) {
    # Record counts carry no unit.
    rows <- cbind(rows[, 1:3, drop = FALSE],
                  data.frame(unit_concept_id = rep(NA_real_, nrow(rows)),
                             unit_name = rep(NA_character_, nrow(rows)),
                             stringsAsFactors = FALSE),
                  rows[, -(1:3), drop = FALSE])
  }
  list(df = rows, built = list(built))
}

.omopReleaseRunDurationEras <- function(handle, ctx, params, route, snap) {
  if (is.null(ctx$scoped_cohort)) return(list(df = data.frame(), built = list()))
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  domain <- params$domain_code %||% "1"
  src <- .omopCovariateSource(handle, domain)
  filter_pred <- .omopTxPathFilterPredicate(
    handle, paste0("e.", src$concept_col), params$filter_treatments)
  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col,
                         where_sql = if (nzchar(filter_pred)) filter_pred else
                           NULL)
  end_col <- switch(as.character(domain), "1" = "drug_exposure_end_date",
                    src$date_col)
  dur <- .omopDateDiffDays(
    handle, paste0("COALESCE(e.", end_col, ", e.", src$date_col, ")"),
    paste0("e.", src$date_col))
  where <- paste0("e.", src$concept_col, " IS NOT NULL",
                  if (nzchar(filter_pred)) paste0(" AND ", filter_pred) else "")
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT pp.covariate_id, cc.concept_name AS covariate_name, pp.person_id, ",
    "pp.v FROM (SELECT e.", src$concept_col, " AS covariate_id, e.",
    src$person_col, " AS person_id, AVG(CAST(", dur, " AS FLOAT)) AS v FROM ",
    src$table, " e INNER JOIN (SELECT DISTINCT subject_id FROM ", cohort,
    ") c ON c.subject_id = e.", src$person_col, " WHERE ", where,
    " GROUP BY e.", src$concept_col, ", e.", src$person_col, ") pp LEFT JOIN ",
    concept, " cc ON cc.concept_id = pp.covariate_id"))
  grid <- .dsomopReleaseGrid("duration_days")
  owner <- .dsomopReleaseOwner(
    id = paste0("txpath/era-duration-days-by-treatment/", .dsomopFrame(
      "v1", as.character(domain),
      .omopReleaseOutcomeKey(params$filter_treatments, domain))),
    family = "HM", grid = grid, strata = list(kind = "data"),
    cap = .DSOMOP_RELEASE_MULTI_CAP, bounds = c(grid$lower, grid$upper),
    estimand = paste0("per-person mean era duration in days of each ",
                      "treatment, clipped to [0, 10957]; each person counted ",
                      "in at most ", .DSOMOP_RELEASE_MULTI_CAP,
                      " treatments (lowest concept ids)"))
  built <- .omopReleaseTopStrata(route, owner,
                                 .omopReleaseConceptSubstrate(raw),
                                 as.integer(params$top_n %||% "50"))
  rows <- .omopReleaseConceptRows(built, raw)
  rows$n_persons <- NULL
  list(df = rows, built = list(built))
}

# C8: the first outcome event per person relative to the canonical index, on a
# public signed grid whose bin width is the requested time scale (edge bins
# hold every earlier or later first event).
.omopReleaseRunTimeToEvent <- function(handle, ctx, params, route, snap) {
  if (is.null(ctx$scoped_cohort)) return(list(df = data.frame(), built = list()))
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  out_src <- .omopOutcomeSource(handle, params$outcome_concept_id,
                                params$domain_code %||% "0")
  if (is.null(out_src)) return(list(df = data.frame(), built = list()))
  scale <- as.integer(params$time_scale %||% "1")
  if (!scale %in% c(1L, 30L, 365L)) scale <- 1L
  .omopDiagAssertPersons(handle, ctx, out_src$table, "o", out_src$person_col,
                         where_sql = paste0("o.", out_src$concept_col,
                                            " IN (", out_src$id_list, ")"))
  first <- .omopFirstCohortEpisodeSql(handle, cohort)
  offset <- .omopDateDiffDays(handle, paste0("MIN(o.", out_src$date_col, ")"),
                              "c.cohort_start_date")
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT c.subject_id AS person_id, ", offset, " AS v FROM ", first,
    " c INNER JOIN ", out_src$table, " o ON o.", out_src$person_col,
    " = c.subject_id AND o.", out_src$concept_col, " IN (", out_src$id_list,
    ") GROUP BY c.subject_id, c.cohort_start_date"))
  span <- if (scale == 1L) 365L else 3650L
  breaks <- seq.int(-span, span - scale, by = scale)
  grid <- list(id = paste0("signed-day-offset-", scale, "d-", span, "-v1"),
               breaks = breaks, lower = NULL, upper = NULL, unit = "days")
  owner <- .dsomopReleaseOwner(
    id = paste0("characterization/first-outcome-day-offset/", .dsomopFrame(
      "v1", .omopReleaseOutcomeKey(params$outcome_concept_id,
                                   params$domain_code %||% "0"),
      as.integer(scale))),
    family = "H", grid = grid,
    estimand = paste0("day offset of each person's first outcome event from ",
                      "the cohort index, in ", scale, "-day bins over [",
                      -span, ", ", span, ") (edge bins hold earlier and ",
                      "later events)"))
  sub <- data.frame(person_id = raw$person_id %||% character(0),
                    stratum = rep("root", nrow(raw)),
                    v = suppressWarnings(as.numeric(raw$v)),
                    stringsAsFactors = FALSE)
  built <- .dsomopReleaseBuild(route$policy, route$protection, owner, sub)
  counts <- built$bins[["root"]]
  df <- data.frame(day_offset = as.numeric(breaks),
                   num_events = .dsomopReleaseRound(counts),
                   persons = .dsomopReleaseRound(counts))
  list(df = df, built = list(built))
}

# --- Mixed frames: protected distribution rows over standard binary rows -------

.omopReleaseOverlayTable1 <- function(handle, ctx, params, route, snap, df) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(list(df = df, built = list()))
  built <- list()
  for (metric in c("charlson_index", "age")) {
    rows <- which(df$unit == "dist" & df$characteristic == metric)
    if (length(rows) == 0L) next
    b <- if (identical(metric, "age")) {
      .dsomopReleaseBuild(route$policy, route$protection,
                          .omopReleaseAgeAtIndexOwner(),
                          .omopReleaseAgeAtIndexValues(handle, ctx))
    } else .omopReleaseComorbidity(handle, ctx, route, "charlson")
    built[[length(built) + 1L]] <- b
    st <- b$strata
    df$avg_value[rows] <- st$mean
    df$stdev_value[rows] <- st$sd
    df$p10_value[rows] <- st$p10
    df$p25_value[rows] <- st$p25
    df$median_value[rows] <- st$median
    df$p75_value[rows] <- st$p75
    df$p90_value[rows] <- st$p90
  }
  list(df = df, built = built)
}

.omopReleaseOverlayTargetCovariates <- function(handle, ctx, params, route,
                                                snap, df) {
  if (!is.data.frame(df) || nrow(df) == 0L || !any(df$kind == "continuous")) {
    return(list(df = df, built = list()))
  }
  # The continuous rows are concept x unit strata of the fe.continuous owners
  # (B13): the same draws as that question.
  raw <- .omopReleaseConceptUnitValues(handle, ctx, "3")
  cont <- which(df$kind == "continuous")
  keys <- .omopReleaseUnitKey(df$covariate_id[cont], df$unit_concept_id[cont])
  res <- .omopReleaseUnitStrataBuild(
    route, raw, "fe/person-mean-value-by-concept-unit/domain=3",
    "per-person mean value of each concept in the cohort", keys = keys)
  st <- res$strata
  if (!is.data.frame(st) || nrow(st) == 0L) {
    st <- data.frame(stratum = character(0), mean = numeric(0),
                     sd = numeric(0), p10 = numeric(0), p25 = numeric(0),
                     median = numeric(0), p75 = numeric(0), p90 = numeric(0),
                     stringsAsFactors = FALSE)
  }
  at <- match(keys, st$stratum)
  for (col in c("avg_value", "stdev_value", "p10_value", "p25_value",
                "median_value", "p75_value", "p90_value")) {
    src <- switch(col, avg_value = st$mean, stdev_value = st$sd,
                  p10_value = st$p10, p25_value = st$p25,
                  median_value = st$median, p75_value = st$p75,
                  p90_value = st$p90)
    df[[col]][cont] <- src[at]
  }
  list(df = df, built = res$built)
}

# The diagnostics summary's MDRR from the protected arm components of cm.mdrr
# (same owner, same effective parameters, hence the same draws).
.omopReleaseOverlayCmDiagnostics <- function(handle, ctx, params, route, snap,
                                             df) {
  if (!is.data.frame(df) || nrow(df) == 0L || !"mdrr" %in% names(df)) {
    return(list(df = df, built = list()))
  }
  arms <- .omopReleaseCmArmsRun("mdrr")(handle, ctx, params, route, snap)
  mdrr <- if (is.data.frame(arms$df) && nrow(arms$df) > 0L) {
    as.numeric(arms$df$mdrr[[1L]])
  } else NA_real_
  df$mdrr <- rep(mdrr, nrow(df))
  list(df = df, built = arms$built,
       blocks = list(summary = "standard", mdrr = route$protection))
}

#' Overlay the protected distribution rows of a mixed frame
#'
#' The frame's binary prevalence rows were released under their established
#' standard controls; its distribution rows' summary fields are replaced by
#' the protected components of their owners. The label is top-level "mixed"
#' with the protection of each block.
#'
#' @keywords internal
.omopToolsReleaseOverlay <- function(handle, ctx, params, df, contract, route,
                                     snap) {
  result <- contract$overlay(handle, ctx, params, route, snap, df)
  out <- result$df
  label <- .omopToolsReleaseLabel(route, result$built, contract, snap, list(
    protection = "mixed",
    blocks = result$blocks %||%
      list(binary = "standard", distribution = route$protection)))
  attr(out, "dsomop_protection") <- label
  attr(out, "dsomop_release") <- list(
    protocol = .DSOMOP_RELEASE_PROTOCOL,
    contract = .DSOMOP_RELEASE_CONTRACT_VERSION,
    entry = contract$name, protection = "mixed",
    reference_epsilon = route$policy$release_epsilon,
    source = list(logical_source_id =
                    .dsomopReleaseLogicalSourceId(route$policy)),
    owners = lapply(result$built, function(b) b$export))
  out
}
