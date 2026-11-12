# =============================================================================
# WS-0 (CR-2 / amendment A-2): paired v1 / v2 expectations of the derived
# comorbidity scores on the extended reach fixture, computed INDEPENDENTLY of
# the dsOMOP engine with plain SQL over the fixture tables and base R.
#
#   legacy  v1 semantics: every condition row of the person counts (all-time),
#           age at the explicit reference date (v1 requires one for CHADS2 and
#           CHA2DS2-VASc); the IR flag `legacy_all_time_conditions` selects it.
#   native  v2 semantics (A-2): only condition rows with
#           condition_start_date <= reference_date count; reference_date = NULL
#           resolves to the episode's index date; age at the reference date.
#
# Concept categories are the engine's FeatureExtraction-traced definitions (the
# semantics under test are the history cut-off and the age reference, not the
# concept sets). Age points: CHADS2 +1 at >= 75; CHA2DS2-VASc +2 at >= 75, +1 at
# 65-74, +1 for female sex (gender_concept_id 8532); a NULL or future year of
# birth gives no age points (age NA, as in the engine's v1 adapter).
# =============================================================================
score_reference_definitions <- function() {
  list(
    charlson = list(age = "none", categories = list(
      mi = list(c(4329847L), 1L), chf = list(c(316139L), 1L), pvd = list(c(321052L), 1L),
      cvd = list(c(381591L, 434056L), 1L), dementia = list(c(4182210L), 1L), copd = list(c(4063381L), 1L),
      rheumatic = list(c(257628L, 134442L, 80800L, 80809L, 256197L, 255348L), 1L), pud = list(c(4247120L), 1L),
      mild_liver = list(c(4212540L, 4064161L), 1L), dm_uncomp = list(c(201820L), 1L),
      dm_comp = list(c(443767L, 442793L), 2L), hemiplegia = list(c(192606L, 374022L), 2L),
      renal = list(c(4030518L), 2L), malignancy = list(c(443392L), 2L),
      mod_liver = list(c(4245975L, 4029488L, 192680L, 24966L), 3L), metastatic = list(c(432851L), 6L),
      aids = list(c(439727L), 6L))),
    chads2 = list(age = "chads2", categories = list(
      chf = list(c(316139L), 1L), hypertension = list(c(316866L), 1L), diabetes = list(c(201820L), 1L),
      stroke = list(c(381591L, 434056L), 2L))),
    chadsvasc = list(age = "chadsvasc", categories = list(
      chf = list(c(316139L, 314378L, 318773L, 321319L), 1L),
      hypertension = list(c(320128L, 442604L, 201313L), 1L),
      diabetes = list(c(201820L, 442793L), 1L),
      stroke = list(c(4043731L, 4110192L, 375557L, 4108356L, 373503L, 434656L, 433505L, 376714L, 312337L), 2L),
      vascular = list(c(312327L, 43020432L, 314962L, 312939L, 315288L, 317309L, 134380L, 196438L, 200138L, 194393L,
                        319047L, 40486130L, 317003L, 4313767L, 321596L, 317305L, 321886L, 314659L, 321887L, 437312L,
                        134057L), 1L)))
  )
}

# The paired cases: a fixed reference date on cohort 1 (index 2020-01-01) equal
# to and ten years after the index, and cohort 1002 (index 2019-07-01) with the
# native index-date resolution and with a fixed date after the index.
score_reference_cases <- function() {
  list(
    list(cohort_definition_id = 1L, reference = "fixed_2020", resolution = "fixed", reference_date = "2020-01-01"),
    list(cohort_definition_id = 1L, reference = "fixed_2030", resolution = "fixed", reference_date = "2030-01-01"),
    list(cohort_definition_id = 1002L, reference = "index", resolution = "index", reference_date = NULL),
    list(cohort_definition_id = 1002L, reference = "fixed_2020", resolution = "fixed", reference_date = "2020-01-01")
  )
}

compute_score_reference <- function(conn) {
  q <- function(sql) DBI::dbGetQuery(conn, sql)
  descendants <- function(ids) {
    rows <- q(paste0("SELECT descendant_concept_id FROM concept_ancestor WHERE ancestor_concept_id IN (",
                     paste(ids, collapse = ","), ")"))$descendant_concept_id
    unique(c(as.integer(ids), as.integer(rows)))
  }
  persons <- q("SELECT person_id, year_of_birth, gender_concept_id FROM person")
  conditions <- q("SELECT person_id, condition_concept_id, condition_start_date FROM condition_occurrence")
  defs <- score_reference_definitions()
  expanded <- lapply(defs, function(d) lapply(d$categories, function(cat) list(ids = descendants(cat[[1]]), weight = cat[[2]])))
  age_points <- function(kind, age, gender) {
    pts <- integer(length(age))
    if (kind == "chads2") pts <- pts + ifelse(!is.na(age) & age >= 75, 1L, 0L)
    if (kind == "chadsvasc") {
      pts <- pts + ifelse(!is.na(age) & age >= 75, 2L, ifelse(!is.na(age) & age >= 65, 1L, 0L))
      pts <- pts + ifelse(!is.na(gender) & gender == 8532L, 1L, 0L)
    }
    pts
  }
  cases <- lapply(score_reference_cases(), function(cs) {
    members <- q(paste0("SELECT subject_id AS person_id, MIN(cohort_start_date) AS index_date FROM cohort WHERE cohort_definition_id = ",
                        cs$cohort_definition_id, " GROUP BY subject_id ORDER BY subject_id"))
    ref <- if (identical(cs$resolution, "index")) members$index_date else rep(cs$reference_date, nrow(members))
    pinfo <- persons[match(members$person_id, persons$person_id), , drop = FALSE]
    age <- as.integer(substr(ref, 1L, 4L)) - pinfo$year_of_birth
    scores <- lapply(names(defs), function(kind) {
      legacy <- integer(nrow(members)); native <- integer(nrow(members))
      for (cat in expanded[[kind]]) {
        rows <- conditions[conditions$condition_concept_id %in% cat$ids, , drop = FALSE]
        for (i in seq_len(nrow(members))) {
          mine <- rows[rows$person_id == members$person_id[i], , drop = FALSE]
          if (nrow(mine) > 0L) legacy[i] <- legacy[i] + cat$weight
          if (any(as.Date(mine$condition_start_date) <= as.Date(ref[i]))) native[i] <- native[i] + cat$weight
        }
      }
      pts <- age_points(defs[[kind]]$age, age, pinfo$gender_concept_id)
      list(person_id = members$person_id, legacy = legacy + pts, native = native + pts)
    })
    names(scores) <- names(defs)
    list(cohort_definition_id = cs$cohort_definition_id, reference = cs$reference, resolution = cs$resolution,
         reference_date = if (identical(cs$resolution, "index")) unique(members$index_date) else cs$reference_date,
         n_persons = nrow(members), scores = scores)
  })
  find_case <- function(cid, reference) Find(function(c) c$cohort_definition_id == cid && c$reference == reference, cases)
  diff_persons <- function(cs, kind) cs$scores[[kind]]$person_id[cs$scores[[kind]]$legacy != cs$scores[[kind]]$native]
  c1_20 <- find_case(1L, "fixed_2020"); c1_30 <- find_case(1L, "fixed_2030"); c2_ix <- find_case(1002L, "index")
  shift <- function(kind) {
    a <- c1_20$scores[[kind]]; b <- c1_30$scores[[kind]]
    a$person_id[a$native != b$native[match(a$person_id, b$person_id)]]
  }
  list(
    fixture = "reach + extension (build_extended_reach_handle(100))",
    semantics = list(
      legacy = "v1 adapter (IR flag legacy_all_time_conditions): all condition rows count; age at the explicit reference_date",
      native = "v2 (A-2): condition rows with condition_start_date <= reference_date; reference_date NULL resolves to the episode index date; age at the reference date",
      age_points = "CHADS2 +1 at age >= 75; CHA2DS2-VASc +2 at >= 75, +1 at 65-74, +1 female; NA age (NULL/future year of birth) gives no age points"),
    cases = cases,
    pins = list(
      charlson_history = list(cohort_definition_id = 1L, reference = "fixed_2020", person_id = 1011L,
                              legacy = c1_20$scores$charlson$legacy[c1_20$scores$charlson$person_id == 1011L],
                              native = c1_20$scores$charlson$native[c1_20$scores$charlson$person_id == 1011L],
                              note = "MI dated 2020-02-01 is after the 2020-01-01 index: counted by v1, not by v2"),
      charlson_history_persons = diff_persons(c1_20, "charlson"),
      age_reference_shift = list(cohort_definition_id = 1L, from = "fixed_2020", to = "fixed_2030",
                                 chads2_persons = shift("chads2"), chadsvasc_persons = shift("chadsvasc"),
                                 note = "moving the reference date ten years forward changes the age points only"),
      index_resolution = list(cohort_definition_id = 1002L, reference = "index", reference_date = c2_ix$reference_date,
                              chads2_persons = diff_persons(c2_ix, "chads2"), chadsvasc_persons = diff_persons(c2_ix, "chadsvasc"),
                              charlson_persons = diff_persons(c2_ix, "charlson"),
                              note = "conditions dated after the 2019-07-01 index (heart failure 2019-07-10, MI 2019-07-01 counts) are excluded natively"))
  )
}
