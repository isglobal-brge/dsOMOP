#!/usr/bin/env python3
"""WS-0: the fifteen acceptance recipes of design-2.10.3 section 10.7.

Writes, for each recipe, inst/plans/acceptance/NN-name/{recipe.R, plan.json,
README.md}: the public-client recipe (frozen 2.10.3 API of design section 2),
its validated v2 plan and the semantics the hand-computed expected results
(expected.json, produced on the extended reach fixture by
tools/ws0/acceptance_expected.R) follow. Every plan is validated against
inst/schema/plan-v2.json before it is written.

Usage: PYTHONPATH=<dir with jsonschema> python3 tools/ws0/gen_acceptance_corpus.py SCHEMA_DIR OUT_DIR
"""
import copy, json, os, sys
SCHEMA_DIR, OUT = sys.argv[1], sys.argv[2]
sys.path.insert(0, os.environ.get("JSONSCHEMA_PATH", ""))
from jsonschema import Draft202012Validator
plan_schema = json.load(open(os.path.join(SCHEMA_DIR, "plan-v2.json")))
V = Draft202012Validator(plan_schema)

META = {"language_version": "2.0", "semantic_version": "2.0.0", "generated_by": "dsOMOPClient",
        "client_version": "2.10.3"}
def plan(**kw):
    p = {"version": 2, "meta": dict(META)}
    p.update(kw)
    return p
def cs(ids, desc=True):
    return {"concepts": ids if isinstance(ids, list) else [ids], "include_descendants": desc}
def ev(table, concepts=None, **kw):
    e = {"table": table}
    if concepts is not None:
        e["concept_set"] = concepts
    e.update(kw)
    return e
def win(start, end, anchor="index"):
    return {"start": start, "end": end, "anchor": anchor}
def cohort_index(cid):
    return {"kind": "index", "index": {"source": {"cohort_definition_id": cid}, "select": "all",
                                       "end": {"strategy": "event_end"}}}
def red(fn, **kw):
    r = {"fn": fn}; r.update(kw); return r
def var(source, reducer):
    return {"source": source, "reducer": reducer}
def out(rows, fmt, **kw):
    o = {"rows": rows, "format": fmt}; o.update(kw); return o
W365 = win(-365, 0)

recipes = []
def add(num, name, p, recipe_r, readme):
    recipes.append((num, name, p, recipe_r, readme))

# ---------------------------------------------------------------------------
# 01 characterisation
# ---------------------------------------------------------------------------
COND = {"dm": 201820, "prediabetes": 201826, "htn1": 320128, "htn2": 316866, "mi": 4329847, "mi2": 312327,
        "ckd": 46271022, "af": 313217, "hf": 316139, "hld": 432867, "obesity": 433736, "pregnancy": 4299535,
        "stroke": 381316, "stroke2": 372924, "sepsis": 132797, "cirrhosis": 4064161, "asthma": 317009,
        "copd": 255573, "chest_pain": 77670, "resp_inf": 320136, "prior_bleed": 192671, "major_bleed": 4108832,
        "malignancy": 4112853, "chronic_pain": 436096, "htn_comp": 321318, "covar2": 4000777}
DRUG = {"metformin": 1503297, "insulin": 1596977, "statin": 1539403, "warfarin": 1310149, "acei": 1308216,
        "arb": 1367500, "beta_blocker": 1346823, "diuretic": 974166, "doac": 40228152, "sglt2": 1488000}
PROC = {"dialysis": 4032243, "pci": 4283095, "bariatric": 4322471, "cath": 4139525}
events, variables = {}, {}
for n, c in COND.items():
    events["cond_" + n] = ev("condition_occurrence", cs(c)); variables["cond_" + n] = var({"event": "cond_" + n}, red("binary", window=W365))
for n, c in DRUG.items():
    events["drug_" + n] = ev("drug_exposure", cs(c)); variables["drug_" + n] = var({"event": "drug_" + n}, red("binary", window=W365))
for n, c in PROC.items():
    events["proc_" + n] = ev("procedure_occurrence", cs(c)); variables["proc_" + n] = var({"event": "proc_" + n}, red("binary", window=W365))
events["hba1c"] = ev("measurement", cs(3004410, False), value={"column": "value_as_number"},
                     unit={"target": 8554, "convert": {"8753": {"factor": 0.0915, "offset": 2.15}}, "unknown": "missing", "censored": "keep"})
events["glucose"] = ev("measurement", cs(3004501, False), value={"column": "value_as_number"},
                       unit={"target": 8840, "convert": {"8753": {"factor": 18.0182, "offset": 0}}, "unknown": "missing", "censored": "keep"})
variables.update({
    "age": {"derived": {"kind": "age", "reference": "index", "bands": False}},
    "sex": {"derived": {"kind": "sex_mf"}},
    "hba1c_last": var({"event": "hba1c"}, red("last_value", value="value", window=W365)),
    "glucose_last": var({"event": "glucose"}, red("last_value", value="value", window=W365)),
    "charlson": {"derived": {"kind": "charlson"}},
})
p01 = plan(events=events, populations={"c1": cohort_index(1)}, variables=variables,
           outputs={"table1": out("c1", "wide", grain="episode", result_symbol="D_table1")})
add(1, "characterisation", p01, r'''
# Acceptance recipe 01 - characterisation table (design 10.7 item 1)
library(dsOMOPClient)
w365 <- omop_window(-365, 0)
cond <- c(dm = 201820, prediabetes = 201826, htn1 = 320128, htn2 = 316866, mi = 4329847, mi2 = 312327,
          ckd = 46271022, af = 313217, hf = 316139, hld = 432867, obesity = 433736, pregnancy = 4299535,
          stroke = 381316, stroke2 = 372924, sepsis = 132797, cirrhosis = 4064161, asthma = 317009,
          copd = 255573, chest_pain = 77670, resp_inf = 320136, prior_bleed = 192671,
          major_bleed = 4108832, malignancy = 4112853, chronic_pain = 436096, htn_comp = 321318,
          covar2 = 4000777)
drug <- c(metformin = 1503297, insulin = 1596977, statin = 1539403, warfarin = 1310149, acei = 1308216,
          arb = 1367500, beta_blocker = 1346823, diuretic = 974166, doac = 40228152, sglt2 = 1488000)
proc <- c(dialysis = 4032243, pci = 4283095, bariatric = 4322471, cath = 4139525)
binary_vars <- function(prefix, table, ids) {
  lapply(stats::setNames(names(ids), paste0(prefix, "_", names(ids))), function(n)
    omop_variable(event = omop_event(table, ids[[n]], descendants = TRUE, name = paste0(prefix, "_", n)),
                  reducer = omop_reduce("binary", window = w365)))
}
hba1c <- omop_event("measurement", 3004410, value = "value_as_number",
                    unit = omop_unit(8554, convert = list("8753" = c(factor = 0.0915, offset = 2.15))),
                    name = "hba1c")
glucose <- omop_event("measurement", 3004501, value = "value_as_number",
                      unit = omop_unit(8840, convert = list("8753" = c(factor = 18.0182, offset = 0))),
                      name = "glucose")
rec <- omop_recipe(
  populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all",
                                                             end = omop_end("event_end")))),
  variables = c(
    binary_vars("cond", "condition_occurrence", cond),
    binary_vars("drug", "drug_exposure", drug),
    binary_vars("proc", "procedure_occurrence", proc),
    list(age = omop_age(), sex = omop_sex(),
         hba1c_last = omop_variable(name = "hba1c_last", event = hba1c, reducer = omop_reduce("last_value", window = w365)),
         glucose_last = omop_variable(name = "glucose_last", event = glucose, reducer = omop_reduce("last_value", window = w365)),
         charlson = omop_score("charlson"))),
  outputs = list(omop_output("table1", rows = "c1", format = "wide", grain = "episode")))
plan <- recipe_to_plan(rec)   # equals plan.json after normalisation
''', '''
Rows: every episode of results cohort 1 (index 2020-01-01, end 2025-12-31; the reach
persons 1..100 plus the extension members of cohort 1). Columns: exact annual age
(2020 - year_of_birth; NA for a NULL or future birth year), sex (M/F), forty binary
covariates (any row of the concept, descendants included, with start date in
[index-365, index] = [2019-01-01, 2020-01-01]; `cond_covar2` is present in exactly
persons 1013 and 1014), two unit-normalised last values in the same window (HbA1c in %
with mmol/mol converted as 0.0915 x + 2.15; glucose in mg/dL with the fixture's 8753
rows converted x 18.0182; unknown units give NA; the last row by date then id), and
the Charlson index (sum of category weights over condition rows with start date on
or before the index date; categories and weights as `.computeComorbidityScore`,
descendants from the fixture's concept_ancestor). WS-0 decision: the v2 score counts
conditions on or before the reference date (the v1 adapter keeps the all-time rule).
''')

# ---------------------------------------------------------------------------
# 02 new-user active comparator
# ---------------------------------------------------------------------------
p02 = plan(
    events={"acei_era_rows": ev("drug_era", cs(1308216)), "arb_era_rows": ev("drug_era", cs(1367500)),
            "mi": ev("condition_occurrence", cs(4329847)), "death": ev("death")},
    intervals={"acei": {"source": {"event": "acei_era_rows"}, "gap": 30},
               "arb": {"source": {"event": "arb_era_rows"}, "gap": 30}},
    populations={"new_users": {
        "kind": "index",
        "index": {"source": {"any_of": [{"interval": "acei"}, {"interval": "arb"}]}, "select": "first_eligible",
                  "end": {"strategy": "interval_end"}},
        "coverage": {"policy": "unique", "on_ambiguous": "drop", "on_missing": "drop", "gap": 0},
        "where": {"and": [
            {"observed": {"before": 365}},
            {"has": {"source": {"interval": "acei"}, "op": "==", "n": 0, "window": win(-365, -1)}},
            {"has": {"source": {"interval": "arb"}, "op": "==", "n": 0, "window": win(-365, -1)}}]}}},
    variables={"arm": {"population_column": {"name": "index_source"}},
               "age": {"derived": {"kind": "age"}}, "sex": {"derived": {"kind": "sex_mf"}},
               "prior_mi": var({"event": "mi"}, red("binary", window=win(None, 0)))},
    outputs={
        "baseline": out("new_users", "wide", grain="episode", result_symbol="D_baseline"),
        "tte": out("new_users", "survival", result_symbol="D_tte", options={
            "outcomes": {"mi": {"event": "mi"}, "death": {"event": "death"}},
            "tars": {"itt": win(1, None),
                     "on_treatment": {"start": {"anchor": "index", "offset": 1}, "end": {"anchor": "index_end", "offset": 30}, "anchor": "index"}},
            "censor": ["observation_end",
                       {"censor": {"source": {"interval": "acei"}, "which": "first_start_after_entry"}},
                       {"censor": {"source": {"interval": "arb"}, "which": "first_start_after_entry"}}],
            "prior_outcome": {"lookback": 365, "action": "exclude"},
            "format": "competing_risk", "event_order": "first", "outcome_mode": "named"})})
add(2, "active-comparator", p02, r'''
# Acceptance recipe 02 - new-user active comparator, two era arms (design 10.7 item 2)
library(dsOMOPClient)
acei <- omop_era(omop_event("drug_era", 1308216, descendants = TRUE, name = "acei_era_rows"), gap = 30, name = "acei")
arb  <- omop_era(omop_event("drug_era", 1367500, descendants = TRUE, name = "arb_era_rows"), gap = 30, name = "arb")
mi   <- omop_event("condition_occurrence", 4329847, descendants = TRUE, name = "mi")
death <- omop_event("death", name = "death")
rec <- omop_recipe(
  intervals = list(acei, arb), events = list(mi, death),
  populations = list(omop_population("new_users",
    index = omop_index(omop_any_of(acei, arb), select = "first_eligible", end = omop_end("interval_end")),
    coverage = omop_coverage("unique"),
    filters = list(omop_observed(before = 365),
                   omop_has_not(acei, window = omop_window(-365, -1)),
                   omop_has_not(arb, window = omop_window(-365, -1))))),
  variables = list(arm = omop_population_column("index_source"), age = omop_age(), sex = omop_sex(),
                   prior_mi = omop_variable(name = "prior_mi", event = mi, reducer = omop_reduce("binary", window = omop_window(NULL, 0)))),
  outputs = list(
    omop_output("baseline", rows = "new_users", format = "wide", grain = "episode"),
    omop_output("tte", rows = "new_users", format = "survival", options = list(
      outcomes = list(mi = mi, death = death), format = "competing_risk", event_order = "first", outcome_mode = "named",
      tars = list(itt = omop_window(1, NULL),
                  on_treatment = omop_window(omop_bound("index", 1), omop_bound("index_end", 30))),
      censor = list("observation_end", omop_censor(acei, "first_start_after_entry"), omop_censor(arb, "first_start_after_entry")),
      prior_outcome = list(lookback = 365, action = "exclude")))))
plan <- recipe_to_plan(rec)
''', '''
Arms are eras (gap 30) over the recorded drug_era rows of ACE inhibitors (1308216) and
ARBs (1367500); the index is the first candidate (by start date, then declared arm
order ACEI before ARB, then key) whose criteria hold: >= 365 observed days before it
under the unique-coverage policy, and no era of either arm starting in [index-365,
index-1] (washout; an era "occurrence" in a window is its start date). `index_end`
is the selected era's end (course end). Baseline: arm (index_source = "acei" or "arb"),
age, sex, prior MI (any MI on or before the index). Survival (competing risk, first of
mi/death): entry = index + 1 for both TARs; ITT exit = observation end; on-treatment
exit = course end + 30; censoring also at the first start of the OTHER arm's era after
entry (the own arm's eras never censor), observation end, and for the competing-risk
format death is an outcome, not a censor; episodes with an MI in [entry-365, entry)
are excluded (prior_outcome). Reach fixture: ACEI eras 2019-06-01..2020-06-01 for
persons 20..45, ARB eras 2020-01-01..2020-08-01 for 15..40, MI 2019-07-01 for 1..25,
deaths 2020-06-15 for persons divisible by 4.
''')

# ---------------------------------------------------------------------------
# 03 distinct days
# ---------------------------------------------------------------------------
p03 = plan(
    events={"dm": ev("condition_occurrence", cs(201820))},
    populations={"c1": cohort_index(1),
                 "dm2": {"kind": "criteria", "parent": "c1",
                         "where": {"has": {"source": {"event": "dm"}, "op": ">=", "n": 2, "window": W365, "distinct": "day"}}}},
    variables={"dm_days": var({"event": "dm"}, red("count", window=W365, distinct="day")),
               "dm_rows": var({"event": "dm"}, red("count", window=W365))},
    outputs={"two_days": out("dm2", "wide", grain="episode", result_symbol="D_two_days")})
add(3, "distinct-days", p03, r'''
# Acceptance recipe 03 - two diagnoses on distinct days (design 10.7 item 3)
library(dsOMOPClient)
dm <- omop_event("condition_occurrence", 201820, descendants = TRUE, name = "dm")
w365 <- omop_window(-365, 0)
rec <- omop_recipe(
  events = list(dm),
  populations = list(
    omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end"))),
    omop_population("dm2", parent_id = "c1", filters = list(omop_count_is(dm, ">=", 2, window = w365, distinct = "day")))),
  variables = list(dm_days = omop_variable(name = "dm_days", event = dm, reducer = omop_reduce("count", window = w365, distinct = "day")),
                   dm_rows = omop_variable(name = "dm_rows", event = dm, reducer = omop_reduce("count", window = w365))),
  outputs = list(omop_output("two_days", rows = "dm2", format = "wide", grain = "episode")))
plan <- recipe_to_plan(rec)
''', '''
`dm2` inherits the episodes of cohort 1 (parent) and keeps those with diabetes
(201820 + descendants) rows on at least two distinct days in [index-365, index].
Columns: distinct diagnosis days and raw row count in the window. Reach fixture:
persons 1..25 have rows on 2019-06-01, 2019-06-05 and 2019-06-08 (3 days), 26..50 one
day; extension person 1008 has 2019-01-01 (the window's first day) and 2020-01-01
(2 days; the 2018-12-31 row is outside).
''')

# ---------------------------------------------------------------------------
# 04 units and censored operators
# ---------------------------------------------------------------------------
unit_keep = {"target": 8554, "convert": {"8753": {"factor": 0.0915, "offset": 2.15}}, "unknown": "missing", "censored": "keep"}
unit_excl = {"target": 8554, "convert": {"8753": {"factor": 0.0915, "offset": 2.15}}, "unknown": "missing", "censored": "exclude"}
unit_unk_excl = {"target": 8554, "convert": {"8753": {"factor": 0.0915, "offset": 2.15}}, "unknown": "exclude", "censored": "keep"}
W180 = win(-180, 0)
p04 = plan(
    events={"hba1c": ev("measurement", cs(3004410, False), value={"column": "value_as_number"}, unit=unit_keep),
            "hba1c_ge65": ev("measurement", cs(3004410, False), value={"column": "value_as_number"}, unit=unit_keep,
                             where={"var": "value", "op": ">=", "value": 6.5}),
            "hba1c_uncensored": ev("measurement", cs(3004410, False), value={"column": "value_as_number"}, unit=unit_excl),
            "hba1c_known_unit": ev("measurement", cs(3004410, False), value={"column": "value_as_number"}, unit=unit_unk_excl)},
    populations={"c1": cohort_index(1),
                 "with_a1c": {"kind": "criteria", "parent": "c1",
                              "where": {"has": {"source": {"event": "hba1c"}, "op": ">=", "n": 1, "window": W180}}}},
    variables={"a1c_closest": var({"event": "hba1c"}, red("closest_value", value="value", window=W180, side="before")),
               "a1c_n_ge65": var({"event": "hba1c_ge65"}, red("count", window=W180)),
               "a1c_n_uncensored": var({"event": "hba1c_uncensored"}, red("count", window=W180)),
               "a1c_n_known_unit": var({"event": "hba1c_known_unit"}, red("count", window=W180)),
               "a1c_max": var({"event": "hba1c"}, red("max", value="value", window=W180))},
    outputs={"labs": out("with_a1c", "wide", grain="episode", result_symbol="D_labs")})
add(4, "units-censored", p04, r'''
# Acceptance recipe 04 - unit-normalised threshold and nearest value with censored operators (design 10.7 item 4)
library(dsOMOPClient)
conv <- list("8753" = c(factor = 0.0915, offset = 2.15))
hba1c <- omop_event("measurement", 3004410, value = "value_as_number", unit = omop_unit(8554, convert = conv), name = "hba1c")
hba1c_ge65 <- omop_event("measurement", 3004410, value = "value_as_number", unit = omop_unit(8554, convert = conv),
                         where = omop_where(value >= 6.5), name = "hba1c_ge65")
hba1c_uncensored <- omop_event("measurement", 3004410, value = "value_as_number",
                               unit = omop_unit(8554, convert = conv, censored = "exclude"), name = "hba1c_uncensored")
hba1c_known_unit <- omop_event("measurement", 3004410, value = "value_as_number",
                               unit = omop_unit(8554, convert = conv, unknown = "exclude"), name = "hba1c_known_unit")
w180 <- omop_window(-180, 0)
rec <- omop_recipe(
  events = list(hba1c, hba1c_ge65, hba1c_uncensored, hba1c_known_unit),
  populations = list(
    omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end"))),
    omop_population("with_a1c", parent_id = "c1", filters = list(omop_has(hba1c, window = w180)))),
  variables = list(
    a1c_closest = omop_variable(name = "a1c_closest", event = hba1c, reducer = omop_reduce("closest_value", window = w180, side = "before")),
    a1c_n_ge65 = omop_variable(name = "a1c_n_ge65", event = hba1c_ge65, reducer = omop_reduce("count", window = w180)),
    a1c_n_uncensored = omop_variable(name = "a1c_n_uncensored", event = hba1c_uncensored, reducer = omop_reduce("count", window = w180)),
    a1c_n_known_unit = omop_variable(name = "a1c_n_known_unit", event = hba1c_known_unit, reducer = omop_reduce("count", window = w180)),
    a1c_max = omop_variable(name = "a1c_max", event = hba1c, reducer = omop_reduce("max", window = w180))),
  outputs = list(omop_output("labs", rows = "with_a1c", format = "wide", grain = "episode")))
plan <- recipe_to_plan(rec)
''', '''
Rows: cohort-1 episodes with at least one HbA1c row (any unit, any operator) in
[index-180, index] = [2019-07-05, 2020-01-01]. Normalisation happens before every
predicate and reducer: `value` = the % value when the unit is 8554, 0.0915 x + 2.15 when
the unit is 8753 (mmol/mol), NA when the unit is NULL or anything else (unknown =
missing keeps the row). `a1c_closest`: the row nearest to the index on or before it
whose normalised value is not NULL (missing = skip), ties by date then id.
`a1c_n_ge65`: rows whose normalised value >= 6.5 (NULL values never satisfy).
`a1c_n_uncensored`: rows whose operator is not a reviewed inequality (4171756, 4172704,
4171754, 4171755); NULL/0/= operators count. `a1c_n_known_unit`: rows whose unit is 8554
or 8753. `a1c_max`: maximum normalised value (NULLs ignored). Extension person 1006 is
the adversarial case (mmol/mol, NULL unit, < and > operators, NULL value).
''')

# ---------------------------------------------------------------------------
# 05 same-day index events and same-visit links
# ---------------------------------------------------------------------------
p05 = plan(
    events={"mi": ev("condition_occurrence", cs(4329847)),
            "pci_index_visit": ev("procedure_occurrence", cs(4283095), visit={"index": True}),
            "chest_pain_index_visit": ev("condition_occurrence", cs(77670), visit={"index": True}),
            "inpatient": ev("visit_occurrence", cs(9201, False))},
    populations={"mi_all": {"kind": "index",
                            "index": {"source": {"event": "mi"}, "select": "all", "end": {"strategy": "event_end"}},
                            "coverage": {"policy": "unique", "on_ambiguous": "drop", "on_missing": "drop", "gap": 0}}},
    variables={"n_same_day_mi": var({"event": "mi"}, red("count", window=win(0, 0))),
               "pci_same_visit": var({"event": "pci_index_visit"}, red("binary")),
               "chest_pain_same_visit": var({"event": "chest_pain_index_visit"}, red("binary")),
               "inpatient_30": var({"event": "inpatient"}, red("count", window=win(0, 30), distinct="visit"))},
    outputs={"episodes": out("mi_all", "wide", grain="episode", result_symbol="D_episodes")})
add(5, "same-day-same-visit", p05, r'''
# Acceptance recipe 05 - same-day repeated index events and same-visit links (design 10.7 item 5)
library(dsOMOPClient)
mi <- omop_event("condition_occurrence", 4329847, descendants = TRUE, name = "mi")
pci_v <- omop_event("procedure_occurrence", 4283095, descendants = TRUE, visit = "index", name = "pci_index_visit")
cp_v <- omop_event("condition_occurrence", 77670, descendants = TRUE, visit = "index", name = "chest_pain_index_visit")
inpatient <- omop_event("visit_occurrence", 9201, name = "inpatient")
rec <- omop_recipe(
  events = list(mi, pci_v, cp_v, inpatient),
  populations = list(omop_population("mi_all", index = omop_index(mi, select = "all", end = omop_end("event_end")),
                                     coverage = omop_coverage("unique"))),
  variables = list(
    n_same_day_mi = omop_variable(name = "n_same_day_mi", event = mi, reducer = omop_reduce("count", window = omop_window(0, 0))),
    pci_same_visit = omop_variable(name = "pci_same_visit", event = pci_v, reducer = omop_reduce("binary")),
    chest_pain_same_visit = omop_variable(name = "chest_pain_same_visit", event = cp_v, reducer = omop_reduce("binary")),
    inpatient_30 = omop_variable(name = "inpatient_30", event = inpatient, reducer = omop_reduce("count", window = omop_window(0, 30), distinct = "visit"))),
  outputs = list(omop_output("episodes", rows = "mi_all", format = "wide", grain = "episode")))
plan <- recipe_to_plan(rec)
''', '''
One episode per MI row (4329847, descendants included; the alternative code 312327 is a
different concept) whose date lies in exactly one observation period; two rows on the
same day give two episodes ordered by (person, date, end, source, key). Columns:
MI rows on the index day (window [0, 0], counts the index row itself), a PCI procedure
or a chest-pain condition on the index row's visit (`visit_occurrence_id` equal to the
index row's), and distinct inpatient visits (9201) starting in [index, index+30].
Reach fixture: MI 2019-07-01 for persons 1..25 whose condition rows carry
visit_occurrence_id = person id (the person's 2019-05-01 outpatient visit); the base
procedure rows have no visit. Extension: 1007 (two MI rows and a same-visit PCI and
chest pain on visit 700701), 1011 (MI after death, still an episode), 1012, 1019
(two episodes), 1020, 1021, 1022.
''')

# ---------------------------------------------------------------------------
# 06 drug coverage, persistence, discontinuation censoring
# ---------------------------------------------------------------------------
metf_fill = ev("drug_exposure", cs(1503297), end={"fallback": "days_supply"})
p06 = plan(
    events={"metf": metf_fill, "mi": ev("condition_occurrence", cs(4329847)), "death": ev("death")},
    intervals={"metf_era": {"source": {"event": "metf"}, "gap": 30, "fallback": "days_supply"},
               "metf_stock": {"source": {"event": "metf"}, "gap": 30, "fallback": "days_supply", "stockpile": True}},
    populations={"users": cohort_index(1001)},
    variables={"covered_rec": var({"interval": "metf_era"}, red("covered_days", window=win(1, 90))),
               "covered_eff": var({"interval": "metf_stock"}, red("covered_days", window=win(1, 90), coverage="effective_supply")),
               "era_count": var({"interval": "metf_era"}, red("era_count", window=win(1, 90))),
               "first_era_days": var({"interval": "metf_era"}, red("first_era_days", window=win(1, 90))),
               "supply_start": var({"event": "metf"}, red("supply_days", window=win(1, 90))),
               "supply_clipped": var({"event": "metf"}, red("supply_days", window=win(1, 90), fills="clipped"))},
    outputs={"coverage": out("users", "wide", grain="episode", result_symbol="D_coverage"),
             "persistence": out("users", "survival", result_symbol="D_persistence", options={
                 "outcomes": {"mi": {"event": "mi"}},
                 "tar": win(1, 365),
                 "censor": ["observation_end", "death",
                            {"censor": {"source": {"interval": "metf_era"}, "which": "first_end_after_entry", "offset": 30}}],
                 "format": "survival", "event_order": "first"})})
add(6, "coverage-persistence", p06, r'''
# Acceptance recipe 06 - drug coverage, persistence and discontinuation censoring (design 10.7 item 6)
library(dsOMOPClient)
metf <- omop_event("drug_exposure", 1503297, descendants = TRUE, end = list(fallback = "days_supply"), name = "metf")
metf_era <- omop_era(metf, gap = 30, fallback = "days_supply", name = "metf_era")
metf_stock <- omop_era(metf, gap = 30, fallback = "days_supply", stockpile = TRUE, name = "metf_stock")
mi <- omop_event("condition_occurrence", 4329847, descendants = TRUE, name = "mi")
death <- omop_event("death", name = "death")
w90 <- omop_window(1, 90)
rec <- omop_recipe(
  events = list(metf, mi, death), intervals = list(metf_era, metf_stock),
  populations = list(omop_population("users", index = omop_index(list(cohort_definition_id = 1001), select = "all", end = omop_end("event_end")))),
  variables = list(
    covered_rec = omop_variable(name = "covered_rec", event = metf_era, reducer = omop_reduce("covered_days", window = w90)),
    covered_eff = omop_variable(name = "covered_eff", event = metf_stock, reducer = omop_reduce("covered_days", window = w90, coverage = "effective_supply")),
    era_count = omop_variable(name = "era_count", event = metf_era, reducer = omop_reduce("era_count", window = w90)),
    first_era_days = omop_variable(name = "first_era_days", event = metf_era, reducer = omop_reduce("first_era_days", window = w90)),
    supply_start = omop_variable(name = "supply_start", event = metf, reducer = omop_reduce("supply_days", window = w90)),
    supply_clipped = omop_variable(name = "supply_clipped", event = metf, reducer = omop_reduce("supply_days", window = w90, fills = "clipped"))),
  outputs = list(
    omop_output("coverage", rows = "users", format = "wide", grain = "episode"),
    omop_output("persistence", rows = "users", format = "survival", options = list(
      outcomes = list(mi = mi), tar = omop_window(1, 365), format = "survival", event_order = "first",
      censor = list("observation_end", "death", omop_censor(metf_era, "first_end_after_entry", offset = 30))))))
plan <- recipe_to_plan(rec)
''', '''
Rows: cohort 1001 (index 2019-12-31; base metformin users 1..40, the adherence
fixtures 1003/1004/1005 and the cost person 1017). Metformin fills are normalised to
closed intervals: recorded end when present, else start + days_supply - 1 when the
supply is a positive integer, else the start day alone; a row whose end precedes its
start contributes nothing. `metf_era`: eras with a 30-day gap (a fill joins the era
when start <= previous max end + 31). Window [1, 90] = 2020-01-01..2020-03-30.
`covered_rec` = union length of the recorded fill intervals clipped to the window;
`covered_eff` = the same over the stockpiled relation (carry = min(90, max(0,
previous effective end - start + 1)), effective end = start + supply - 1 + carry,
supply precedence: positive days_supply, else recorded length, else one day);
`era_count` = eras overlapping the window; `first_era_days` = envelope length of the
first era overlapping the window, clipped to it; `supply_start` = sum of days_supply
of fills starting in the window (invalid supply contributes 0); `supply_clipped` =
per-fill supplied days inside the window, overlaps not unioned. Survival: outcome MI,
TAR [1, 365], censoring at observation end, death, and 30 days after the end of the
first metformin era ending after entry (persons without such an era are not
censored by it); exit = min(outcome, censor); `event` = 1 when the outcome comes
first (ties: outcome first). Fixture: base fills 2019-07-01/2019-10-15/2020-02-01
(end = start, supply 90 -> one-day intervals), 1003 one 10-day fill, 1004 two
overlapping 30-day fills (recorded 44, effective 60), 1005 data-quality rows.
''')

# ---------------------------------------------------------------------------
# 07 monthly dense panel with carry-forward
# ---------------------------------------------------------------------------
wt = ev("measurement", cs(3025315, False), value={"column": "value_as_number"},
        unit={"target": 9529, "convert": {"8739": {"factor": 0.45359237, "offset": 0}}, "unknown": "missing", "censored": "keep"})
p07 = plan(
    events={"weight": wt},
    populations={"c1": cohort_index(1)},
    outputs={"panel": out("c1", "person_period", result_symbol="D_panel", options={
        "representation": "dense", "sources": [{"event": "weight"}],
        "bins": {"calendar": "month"}, "window": win(-92, 59),
        "analyses": ["last", "count"],
        "carry_forward": {"weight": {"lookback": 180, "expire": None}}})})
add(7, "monthly-panel", p07, r'''
# Acceptance recipe 07 - numeric monthly dense panel with carry-forward (design 10.7 item 7)
library(dsOMOPClient)
weight <- omop_event("measurement", 3025315, value = "value_as_number",
                     unit = omop_unit(9529, convert = list("8739" = c(factor = 0.45359237, offset = 0))), name = "weight")
rec <- omop_recipe(
  events = list(weight),
  populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end")))),
  outputs = list(omop_output("panel", rows = "c1", format = "person_period", options = list(
    representation = "dense", sources = list(weight), bins = list(calendar = "month"),
    window = omop_window(-92, 59), analyses = c("last", "count"),
    carry_forward = list(weight = list(lookback = 180, expire = NULL))))))
plan <- recipe_to_plan(rec)
''', '''
Rows: one row per cohort-1 episode and calendar month overlapping the window
[index-92, index+59] = [2019-10-01, 2020-02-29] intersected with the episode's
observation period (five months for every episode, each fully observed). Columns:
period_start_days/period_end_days (relative to the index), days (observed days in the
period), weight_count (rows in the month), weight_last (normalised value of the last
row in the month by date then id; NA when none), weight_carry (value at the period
start: the last non-missing normalised value dated <= period start and >= period start
- 180 days; NA when none; no expiry). Weights are in kg; the fixture's pound rows (unit
8739) are converted x 0.45359237. Fixture: persons 1..40 weigh 80 kg on 2019-12-01 and
1..20 have 176 lb on 2020-02-01; extension 1023 (Oct 80, Dec 82, Feb 85), 1024 (no
rows), 1025 (70 kg on 2019-09-01, before the window: carried), 1026 (90 and 91 in Jan).
''')

# ---------------------------------------------------------------------------
# 08 SCCS matrix
# ---------------------------------------------------------------------------
asp = ev("drug_exposure", cs(1124300), end={"fallback": "days_supply"})
p08 = plan(
    events={"aspirin": asp, "mi": ev("condition_occurrence", cs(4329847))},
    intervals={"aspirin_era": {"source": {"event": "aspirin"}, "gap": 30, "fallback": "days_supply"}},
    populations={"sccs": cohort_index(1002)},
    outputs={"matrix": out("sccs", "person_period", result_symbol="D_matrix", options={
        "representation": "dense", "bins": {"width": 365}, "window": win(0, 365),
        "partition": {"boundaries": [{"interval": "aspirin_era"}], "age_bands": 5, "calendar": "year"},
        "outcomes": {"mi": {"event": "mi"}}, "exposures": {"aspirin": {"interval": "aspirin_era"}},
        "analyses": ["count"]})})
add(8, "sccs-matrix", p08, r'''
# Acceptance recipe 08 - SCCS matrix: exposure, age-band and calendar partitions (design 10.7 item 8)
library(dsOMOPClient)
aspirin <- omop_event("drug_exposure", 1124300, descendants = TRUE, end = list(fallback = "days_supply"), name = "aspirin")
aspirin_era <- omop_era(aspirin, gap = 30, fallback = "days_supply", name = "aspirin_era")
mi <- omop_event("condition_occurrence", 4329847, descendants = TRUE, name = "mi")
rec <- omop_recipe(
  events = list(aspirin, mi), intervals = list(aspirin_era),
  populations = list(omop_population("sccs", index = omop_index(list(cohort_definition_id = 1002), select = "all", end = omop_end("event_end")))),
  outputs = list(omop_output("matrix", rows = "sccs", format = "person_period", options = list(
    representation = "dense", bins = list(width = 365), window = omop_window(0, 365),
    partition = list(boundaries = list(aspirin_era), age_bands = 5, calendar = "year"),
    outcomes = list(mi = mi), exposures = list(aspirin = aspirin_era), analyses = "count"))))
plan <- recipe_to_plan(rec)
''', '''
Rows: cohort 1002 (index 2019-07-01, end 2020-06-30; base aspirin users 1..30 and the
SCCS persons 1019..1022). Effective window [L, U] = [2019-07-01, 2020-06-30]
intersected with the eligibility span. Boundaries: L and U+1; starts and ends+1 of the
actual observed segments; starts and ends+1 of each aspirin era (gap 30, supply
fallback); the 1 January of every year (age bands of 5 years change on 1 January and
calendar years start there); the bin boundary L+365 is beyond U. Periods are
[b_i, b_{i+1}-1] intersected with the observed segments; empty periods are dropped.
Columns: period_start_days, period_end_days, days, age (annual: year of period start
- year_of_birth), age_band (floor(age/5)), calendar_year, exposed_aspirin (days of
the period inside the union of aspirin eras), active_aspirin (the union covers the
period start), n_mi (MI rows dated inside the period and inside an observed segment).
Fixture: base aspirin 2019-07-01 (end = start, supply 30: one-day exposure), base MI
2019-07-01 (persons 1..25); extension exposures and MI straddle 1 January 2020.
''')

# ---------------------------------------------------------------------------
# 09 recurrent events with a sentinel
# ---------------------------------------------------------------------------
p09 = plan(
    events={"resp": ev("condition_occurrence", cs(320136)), "death": ev("death")},
    populations={"c1": cohort_index(1)},
    outputs={"recurrent": out("c1", "survival", result_symbol="D_recurrent", options={
        "outcomes": {"resp": {"event": "resp"}}, "tar": win(1, 90), "censor": ["observation_end", "death"],
        "format": "recurrent_events", "event_order": "all", "complete": True})})
add(9, "recurrent-events", p09, r'''
# Acceptance recipe 09 - recurrent events with very few outcomes, complete = TRUE (design 10.7 item 9)
library(dsOMOPClient)
resp <- omop_event("condition_occurrence", 320136, descendants = TRUE, name = "resp")
death <- omop_event("death", name = "death")
rec <- omop_recipe(
  events = list(resp, death),
  populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end")))),
  outputs = list(omop_output("recurrent", rows = "c1", format = "survival", options = list(
    outcomes = list(resp = resp), tar = omop_window(1, 90), censor = list("observation_end", "death"),
    format = "recurrent_events", event_order = "all", complete = TRUE))))
plan <- recipe_to_plan(rec)
''', '''
Rows: every cohort-1 episode; TAR [index+1, index+90] = [2020-01-02, 2020-03-31];
exit = min(TAR end, observation end, death date). Event rows: every respiratory
infection (320136) dated in [entry, exit], numbered per episode by date then key
(`event = 1`, event_days_from_index); `complete = TRUE` adds exactly one sentinel
row (`event = 0`, event_number = last + 1, event_days_from_index = NA) for every
episode without a retained event, so the dense frame has one row per episode at
least. Fixture: the base rows of 320136 are dated 2020-04-01 (day 91, outside the
TAR) so only extension person 1027 (days 9, 19, 45) has events; everyone else gets a
sentinel; persons divisible by 4 die on 2020-06-15 (after the TAR, no effect).
''')

# ---------------------------------------------------------------------------
# 10 nested case-control matching (example B on the extended fixture)
# ---------------------------------------------------------------------------
p10 = plan(
    concept_sets={"mi": {"concepts": [4329847], "include_descendants": True},
                  "statins": {"concepts": [1539403], "include_descendants": True}},
    events={"mi": ev("condition_occurrence", "mi"), "statin_rx": ev("drug_exposure", "statins"),
            "diabetes": ev("condition_occurrence", cs(201820))},
    populations={
        "cases": {"kind": "index", "label": "First MI",
                  "index": {"source": {"event": "mi"}, "select": "first_eligible", "end": {"strategy": "fixed", "days": 0}},
                  "where": {"observed": {"before": 365}}},
        "eligible": {"kind": "criteria", "label": "Observed one year", "where": {"observed": {"before": 365}}},
        "cc": {"kind": "match", "label": "Nested case-control",
               "match": {"cases": "cases", "controls": "eligible", "outcome": {"event": "mi"},
                         "match": ["sex", {"age_band": 5}, "calendar_year",
                                   {"strata": {"source": {"event": "diabetes"}, "window": win(None, -1)}}],
                         "controls_per_case": 4, "prior_outcome": "exclude", "future_cases": "eligible",
                         "replacement": "between_sets", "seed": 20261111, "insufficient": "keep_available",
                         "missing_strata": "ineligible", "max_candidates": 1000000}}},
    variables={"set_id": {"population_column": {"name": "set_id"}}, "is_case": {"population_column": {"name": "is_case"}},
               "w": {"population_column": {"name": "sampling_weight"}},
               "age": {"derived": {"kind": "age", "reference": "index", "bands": False}}, "sex": {"derived": {"kind": "sex_mf"}},
               "statin_365": var({"event": "statin_rx"}, red("binary", window=win(-365, -1)))},
    outputs={"cc_table": out("cc", "wide", grain="episode", result_symbol="D_cc",
                             columns=["set_id", "is_case", "w", "age", "sex", "statin_365"])})
add(10, "matching", p10, r'''
# Acceptance recipe 10 - nested case-control via omop_match() (design 10.7 item 10; canonical example B)
library(dsOMOPClient)
mi <- omop_event("condition_occurrence", omop_concept_set(4329847, descendants = TRUE, name = "mi"), name = "mi")
statin_rx <- omop_event("drug_exposure", omop_concept_set(1539403, descendants = TRUE, name = "statins"), name = "statin_rx")
diabetes <- omop_event("condition_occurrence", 201820, descendants = TRUE, name = "diabetes")
rec <- omop_recipe(
  events = list(mi, statin_rx, diabetes),
  populations = list(
    omop_population("cases", label = "First MI", index = omop_index(mi, select = "first_eligible", end = omop_end("fixed", days = 0)),
                    filters = list(omop_observed(before = 365))),
    omop_population("eligible", label = "Observed one year", filters = list(omop_observed(before = 365))),
    omop_population("cc", label = "Nested case-control", match = omop_match(
      cases = "cases", controls = "eligible", outcome = mi,
      match = list("sex", omop_age_band(5), "calendar_year", omop_strata(diabetes, window = omop_window(NULL, -1))),
      controls_per_case = 4, seed = 20261111))),
  variables = list(set_id = omop_population_column("set_id"), is_case = omop_population_column("is_case"),
                   w = omop_population_column("sampling_weight"), age = omop_age(), sex = omop_sex(),
                   statin_365 = omop_variable(name = "statin_365", event = statin_rx, reducer = omop_reduce("binary", window = omop_window(-365, -1)))),
  outputs = list(omop_output("cc_table", rows = "cc", format = "wide", grain = "episode",
                             columns = c("set_id", "is_case", "w", "age", "sex", "statin_365"))))
plan <- recipe_to_plan(rec)
''', '''
Cases: the first MI row per person (date, then key) with >= 365 observed days before
it in its unique covering observation period; case time t = that date. Controls:
persons observed >= 365 days before t (template evaluated at t), not the case, alive at
t, with no MI on or before t (default exclusion; same-time outcomes excluded), same
sex, same 5-year annual age band at t (floor((year(t) - yob)/5); a NULL yob is
ineligible), same calendar year, and the same diabetes-before-t status (any 201820 row
dated <= t-1). The eligible pairs are ordered per case by the SHA-256 of the published
encoding (seed 20261111, case identity = cases namespace, person, t, t, "mi", MI row
key as text; control person id); the first min(4, N) controls are taken
(between_sets), weights 1 for cases and N/m for controls, set_id = rank of the case
in (t, identity) order. Expected results were computed with the reference encoding
of tests/testthat/fixtures/match-vectors.json in base R.
''')

# ---------------------------------------------------------------------------
# 11 incidence denominators by calendar year
# ---------------------------------------------------------------------------
p11 = plan(
    events={"resp": ev("condition_occurrence", cs(320136))},
    populations={"c1": cohort_index(1)},
    outputs={"years": out("c1", "person_period", result_symbol="D_years", options={
        "representation": "dense", "bins": {"calendar": "year"}, "window": win(0, 730),
        "outcomes": {"resp": {"event": "resp"}}, "analyses": ["count"]})})
add(11, "incidence-years", p11, r'''
# Acceptance recipe 11 - incidence denominators: calendar years with observed days (design 10.7 item 11)
library(dsOMOPClient)
resp <- omop_event("condition_occurrence", 320136, descendants = TRUE, name = "resp")
rec <- omop_recipe(
  events = list(resp),
  populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end")))),
  outputs = list(omop_output("years", rows = "c1", format = "person_period", options = list(
    representation = "dense", bins = list(calendar = "year"), window = omop_window(0, 730),
    outcomes = list(resp = resp), analyses = "count"))))
plan <- recipe_to_plan(rec)
''', '''
Rows: one row per cohort-1 episode and calendar year overlapping [index, index+730]
= [2020-01-01, 2021-12-31] intersected with the observation period (2020 and 2021 for
every episode). Columns: period_start_days, period_end_days, days (observed days:
366 for 2020, 365 for 2021 when fully observed), calendar_year, n_resp (respiratory
infection rows dated inside the period and inside an observed segment). Fixture: 320136
on 2020-04-01 for persons 1..30; extension 1018 (2020-01-02, 2020-03-02), 1027 (three
rows in Jan/Feb 2020).
''')

# ---------------------------------------------------------------------------
# 12 Circe import + baseline
# ---------------------------------------------------------------------------
CIRCE = {
    "ConceptSets": [{"id": 0, "name": "Type 2 diabetes", "expression": {"items": [
        {"concept": {"CONCEPT_ID": 201820, "CONCEPT_NAME": "Type 2 diabetes mellitus", "DOMAIN_ID": "Condition",
                     "VOCABULARY_ID": "SNOMED", "STANDARD_CONCEPT": "S", "CONCEPT_CODE": "44054006"},
         "includeDescendants": True, "isExcluded": False, "includeMapped": False}]}}],
    "PrimaryCriteria": {"CriteriaList": [{"ConditionOccurrence": {"CodesetId": 0}}],
                        "ObservationWindow": {"PriorDays": 365, "PostDays": 0},
                        "PrimaryCriteriaLimit": {"Type": "First"}},
    "QualifiedLimit": {"Type": "First"}, "ExpressionLimit": {"Type": "First"},
    "InclusionRules": [], "EndStrategy": None, "CensoringCriteria": [],
    "CollapseSettings": {"CollapseType": "ERA", "EraPad": 0}, "CensorWindow": {}, "cdmVersionRange": ">=5.0.0"}
p12 = plan(
    concept_sets={"t2dm": {"concepts": [201820], "include_descendants": True, "include_mapped": False, "exclude": None}},
    events={"t2dm_dx": ev("condition_occurrence", "t2dm")},
    populations={"t2dm_first": {"kind": "index", "label": "Type 2 diabetes (Circe)",
                                "index": {"source": {"event": "t2dm_dx"}, "select": "first_eligible"},
                                "coverage": {"policy": "unique", "on_ambiguous": "drop", "on_missing": "drop", "gap": 0},
                                "where": {"observed": {"before": 365, "after": 0}}}},
    variables={"age_group": {"derived": {"kind": "age", "reference": "index", "bands": True}},
               "sex": {"derived": {"kind": "sex_mf"}},
               "prior_obs": {"derived": {"kind": "observed_days", "window": win(-365, 0)}}},
    outputs={"baseline": out("t2dm_first", "wide", grain="episode", result_symbol="D_baseline")})
add(12, "circe-baseline", p12, r'''
# Acceptance recipe 12 - ATLAS Circe import + baseline (design 10.7 item 12)
library(dsOMOPClient)
pop <- recipe_import_circe("circe.json", id = "t2dm_first", label = "Type 2 diabetes (Circe)")
rec <- omop_recipe(
  populations = list(pop),
  variables = list(age_group = omop_age(bands = TRUE), sex = omop_sex(), prior_obs = omop_observed_days(omop_window(-365, 0))),
  outputs = list(omop_output("baseline", rows = "t2dm_first", format = "wide", grain = "episode")))
plan <- recipe_to_plan(rec)   # the importer must produce the population of plan.json
''', '''
circe.json is an ATLAS cohort expression: first condition occurrence of concept set 0
(201820 with descendants), observation window 365 days before and 0 after, limits
First. The importer lowers it to the index population of plan.json: the first eligible
diabetes row per person whose date lies in exactly one observation period that starts
at least 365 days before it (`observed before = 365, after = 0`). Baseline: legacy
age bands (`age_group`, annual age on the server's default 5-year grid, NA for a NULL
or negative age), sex, and prior_obs = observed days in [index-365, index] (366 when
fully observed). Fixture: persons 1..50 (2019-06-01), extension 1003/1004/1005
(2019-12-31), 1006, 1008 (first row 2018-12-31), 1009, 1010, 1011, 1012 (2019-01-15),
1013/1014 (2019-10-01), 1029 (2019-03-01); 1001 is dropped (two covering periods),
1002 and 1030 fail the 365-day requirement.
''')

# ---------------------------------------------------------------------------
# 13 parametrised recipe 03
# ---------------------------------------------------------------------------
p13 = copy.deepcopy(p03)
add(13, "parametrised", p13, r'''
# Acceptance recipe 13 - recipe 03 parametrised with omop_param() and bound on the client (design 10.7 item 13)
library(dsOMOPClient)
w365 <- omop_window(-365, 0)
rec <- omop_recipe(
  events = list(omop_event("condition_occurrence", omop_param("dx", type = "concept_set"), name = "dm")),
  populations = list(
    omop_population("c1", index = omop_index(list(cohort_definition_id = omop_param("cohort", type = "integer", default = 1)),
                                             select = "all", end = omop_end("event_end"))),
    omop_population("dm2", parent_id = "c1",
                    filters = list(omop_count_is(omop_event("condition_occurrence", omop_param("dx", type = "concept_set"), name = "dm"),
                                                 ">=", omop_param("min_days", type = "integer", default = 2), window = w365, distinct = "day")))),
  variables = list(
    dm_days = omop_variable(name = "dm_days", event = omop_event("condition_occurrence", omop_param("dx", type = "concept_set"), name = "dm"),
                            reducer = omop_reduce("count", window = w365, distinct = "day")),
    dm_rows = omop_variable(name = "dm_rows", event = omop_event("condition_occurrence", omop_param("dx", type = "concept_set"), name = "dm"),
                            reducer = omop_reduce("count", window = w365))),
  outputs = list(omop_output("two_days", rows = "dm2", format = "wide", grain = "episode")))
stopifnot(setequal(names(recipe_params(rec)), c("dx", "cohort", "min_days")))
bound <- recipe_bind(rec, dx = omop_concept_set(201820, descendants = TRUE), cohort = 1L, min_days = 2L)
plan <- recipe_to_plan(bound)   # identical to recipe 03's plan.json after normalisation
''', '''
The same study as recipe 03, written with three parameters (the concept set, the
cohort id and the day threshold) and bound on the client before emission. The bound
plan equals plan.json of recipe 03 exactly (this directory's plan.json is a copy) and
the expected results are recipe 03's.
''')

# ---------------------------------------------------------------------------
# 14 oncology episodes
# ---------------------------------------------------------------------------
p14 = plan(
    events={"episodes": ev("episode", cs(32531, False)),
            "members": {"episode_members": {"episode": {"event": "episodes"}}},
            "member_drugs": {"episode_members": {"episode": {"event": "episodes"}, "tables": ["drug_exposure"]}},
            "specimens": ev("specimen", cs(4001225))},
    populations={"onc": {"kind": "index", "label": "Treatment episodes",
                         "index": {"source": {"event": "episodes"}, "select": "all", "end": {"strategy": "event_end"}},
                         "coverage": {"policy": "unique", "on_ambiguous": "drop", "on_missing": "drop", "gap": 0}}},
    variables={"n_members": var({"event": "members"}, red("count")),
               "n_member_drugs": var({"event": "member_drugs"}, red("count")),
               "first_member_days": var({"event": "members"}, red("first_date")),
               "n_specimens": var({"event": "specimens"}, red("count", window=win(-30, 365)))},
    outputs={"episodes": out("onc", "wide", grain="episode", result_symbol="D_onc")})
add(14, "oncology-episodes", p14, r'''
# Acceptance recipe 14 - oncology episodes with episode_event members (design 10.7 item 14)
library(dsOMOPClient)
episodes <- omop_event("episode", 32531, name = "episodes")
members <- omop_episode_members(episodes, name = "members")
member_drugs <- omop_episode_members(episodes, tables = "drug_exposure", name = "member_drugs")
specimens <- omop_event("specimen", 4001225, descendants = TRUE, name = "specimens")
rec <- omop_recipe(
  events = list(episodes, members, member_drugs, specimens),
  populations = list(omop_population("onc", label = "Treatment episodes",
                                     index = omop_index(episodes, select = "all", end = omop_end("event_end")),
                                     coverage = omop_coverage("unique"))),
  variables = list(
    n_members = omop_variable(name = "n_members", event = members, reducer = omop_reduce("count")),
    n_member_drugs = omop_variable(name = "n_member_drugs", event = member_drugs, reducer = omop_reduce("count")),
    first_member_days = omop_variable(name = "first_member_days", event = members, reducer = omop_reduce("first_date")),
    n_specimens = omop_variable(name = "n_specimens", event = specimens, reducer = omop_reduce("count", window = omop_window(-30, 365)))),
  outputs = list(omop_output("episodes", rows = "onc", format = "wide", grain = "episode")))
plan <- recipe_to_plan(rec)
''', '''
Rows: one episode per `episode` row of concept 32531 (index = episode start, end =
episode end). Members: the rows linked through episode_event, mapped by
episode_event_field_concept_id (1147094 drug_exposure, 1147082 procedure_occurrence)
to their tables; an episode-indexed consumer joins members by the parent episode
identity and person, so `n_members` counts the links of THAT episode (no window), and
the same drug row linked to two episodes counts once in each. `n_member_drugs`
restricts the member tables to drug_exposure. `first_member_days` = days from the
index to the earliest member row (start date of the member event). `n_specimens` =
tumour specimen rows (4001225) in [index-30, index+365]. Fixture: 1015 has episodes 1
(3 chemotherapy fills + 1 procedure, 2019-03-01..09-30) and 2 (the 2019-10-01 fill
plus the 2019-04-12 fill linked a second time), 1016 episode 3 (one fill), base persons
1..8 one episode each (2019-12-01..2020-02-28) linking their 2020-02-01 metformin fill.
''')

# ---------------------------------------------------------------------------
# 15 costs and adherence
# ---------------------------------------------------------------------------
p15 = plan(
    events={"metf": metf_fill, "visits": ev("visit_occurrence"),
            "visit_cost": {"cost": {"event": {"event": "visits"}, "field": "total_cost", "currency": 44818568}},
            "drug_cost": {"cost": {"event": {"event": "metf"}, "field": "total_cost", "currency": 44818568}}},
    intervals={"metf_stock": {"source": {"event": "metf"}, "gap": 30, "fallback": "days_supply", "stockpile": True},
               "metf_era": {"source": {"event": "metf"}, "gap": 30, "fallback": "days_supply"}},
    populations={"users": {"kind": "index", "index": {"source": {"cohort_definition_id": 1001}, "select": "all", "end": {"strategy": "event_end"}},
                           "coverage": {"policy": "union", "on_ambiguous": "drop", "on_missing": "drop", "gap": 30}}},
    variables={"visit_cost_60": var({"event": "visit_cost"}, red("sum", value="value", window=win(1, 60))),
               "drug_cost_60": var({"event": "drug_cost"}, red("sum", value="value", window=win(1, 60))),
               "supply_90": var({"event": "metf"}, red("supply_days", window=win(1, 90))),
               "observed_90": {"derived": {"kind": "observed_days", "window": win(1, 90)}},
               "pdc_num_eff_90": var({"interval": "metf_stock"}, red("covered_days", window=win(1, 90), coverage="effective_supply", observed_only=True)),
               "pdc_num_rec_90": var({"interval": "metf_era"}, red("covered_days", window=win(1, 90), observed_only=True)),
               "covered_eff_90": var({"interval": "metf_stock"}, red("covered_days", window=win(1, 90), coverage="effective_supply")),
               "covered_rec_90": var({"interval": "metf_era"}, red("covered_days", window=win(1, 90))),
               "observed_10": {"derived": {"kind": "observed_days", "window": win(1, 10)}},
               "pdc_num_eff_10": var({"interval": "metf_stock"}, red("covered_days", window=win(1, 10), coverage="effective_supply", observed_only=True)),
               "covered_eff_10": var({"interval": "metf_stock"}, red("covered_days", window=win(1, 10), coverage="effective_supply"))},
    outputs={"adherence": out("users", "wide", grain="episode", result_symbol="D_adherence")})
add(15, "costs-adherence", p15, r'''
# Acceptance recipe 15 - costs and adherence: MPR and observed-clipped PDC (design 10.7 item 15)
library(dsOMOPClient)
metf <- omop_event("drug_exposure", 1503297, descendants = TRUE, end = list(fallback = "days_supply"), name = "metf")
visits <- omop_event("visit_occurrence", name = "visits")
visit_cost <- omop_cost(visits, field = "total_cost", currency = 44818568, name = "visit_cost")
drug_cost <- omop_cost(metf, field = "total_cost", currency = 44818568, name = "drug_cost")
metf_stock <- omop_era(metf, gap = 30, fallback = "days_supply", stockpile = TRUE, name = "metf_stock")
metf_era <- omop_era(metf, gap = 30, fallback = "days_supply", name = "metf_era")
w90 <- omop_window(1, 90); w10 <- omop_window(1, 10); w60 <- omop_window(1, 60)
rec <- omop_recipe(
  events = list(metf, visits, visit_cost, drug_cost), intervals = list(metf_stock, metf_era),
  populations = list(omop_population("users", index = omop_index(list(cohort_definition_id = 1001), select = "all", end = omop_end("event_end")),
                                     coverage = omop_coverage("union", gap = 30))),
  variables = list(
    visit_cost_60 = omop_variable(name = "visit_cost_60", event = visit_cost, reducer = omop_reduce("sum", window = w60)),
    drug_cost_60 = omop_variable(name = "drug_cost_60", event = drug_cost, reducer = omop_reduce("sum", window = w60)),
    supply_90 = omop_variable(name = "supply_90", event = metf, reducer = omop_reduce("supply_days", window = w90)),
    observed_90 = omop_observed_days(w90),
    pdc_num_eff_90 = omop_variable(name = "pdc_num_eff_90", event = metf_stock, reducer = omop_reduce("covered_days", window = w90, coverage = "effective_supply", observed_only = TRUE)),
    pdc_num_rec_90 = omop_variable(name = "pdc_num_rec_90", event = metf_era, reducer = omop_reduce("covered_days", window = w90, observed_only = TRUE)),
    covered_eff_90 = omop_variable(name = "covered_eff_90", event = metf_stock, reducer = omop_reduce("covered_days", window = w90, coverage = "effective_supply")),
    covered_rec_90 = omop_variable(name = "covered_rec_90", event = metf_era, reducer = omop_reduce("covered_days", window = w90)),
    observed_10 = omop_observed_days(w10),
    pdc_num_eff_10 = omop_variable(name = "pdc_num_eff_10", event = metf_stock, reducer = omop_reduce("covered_days", window = w10, coverage = "effective_supply", observed_only = TRUE)),
    covered_eff_10 = omop_variable(name = "covered_eff_10", event = metf_stock, reducer = omop_reduce("covered_days", window = w10, coverage = "effective_supply"))),
  outputs = list(omop_output("adherence", rows = "users", format = "wide", grain = "episode")))
plan <- recipe_to_plan(rec)
# MPR = supply_90 / observed_90 and PDC = pdc_num_eff_90 / observed_90 are computed in dsBase or ds.omop.mutate()
''', '''
Rows: cohort 1001 (index 2019-12-31) under the union-coverage policy with gap 30: the
eligibility component is the coalesced observation span containing an ACTUAL recorded
period that covers the index; actual observed days are the union of that component's
recorded periods. Costs: `visit_cost_60` sums total_cost of USD (44818568) cost rows
whose cost_event_id is a visit of the person (cost_domain_id "Visit") dated in
[index+1, index+60]; `drug_cost_60` likewise for metformin fills ("Drug"); EUR rows are
excluded by the currency filter; persons without cost rows get 0. Adherence over
[1, 90] = 2020-01-01..2020-03-30: `supply_90` (MPR numerator) = sum of days_supply of
fills starting in the window; `observed_90` = observed days in the window;
`pdc_num_eff_90` = union of the stockpiled effective-supply intervals intersected with
the window AND the same observed days; `pdc_num_rec_90` the same over recorded
intervals; `covered_*_90` without the observation clip. The [1, 10] variables pin the
design's fixture on person 1003: observed [1,2] U [9,10] = 4 days, effective coverage
4 of 10 -> pdc_num_eff_10 = 4, observed_10 = 4 (4/4, never 10/4); person 1004 pins
recorded 44 vs effective 60 covered days.
''')

# ---------------------------------------------------------------------------
# write and validate
# ---------------------------------------------------------------------------
os.makedirs(OUT, exist_ok=True)
index_lines = ["# Acceptance corpus (design-2.10.3 section 10.7)", "",
               "| # | directory | output(s) | rows |", "|---|---|---|---|"]
for num, name, p, recipe_r, readme in recipes:
    errs = sorted(V.iter_errors(p), key=lambda e: list(e.path))
    if errs:
        for e in errs[:5]:
            print(num, name, "/".join(map(str, e.path)), e.message[:160])
        raise SystemExit("plan %02d does not validate" % num)
    d = os.path.join(OUT, "%02d-%s" % (num, name))
    os.makedirs(d, exist_ok=True)
    with open(os.path.join(d, "plan.json"), "w") as f:
        json.dump(p, f, indent=2); f.write("\n")
    with open(os.path.join(d, "recipe.R"), "w") as f:
        f.write(recipe_r.lstrip("\n"))
    with open(os.path.join(d, "README.md"), "w") as f:
        f.write("# %02d %s\n\nStatus: plan validated against plan-v2.json; expected.json computed by "
                "tools/ws0/acceptance_expected.R on the extended reach fixture "
                "(tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R "
                "computation.\n\n## Semantics of the expected results\n%s" % (num, name, readme.lstrip("\n")))
    if num == 12:
        with open(os.path.join(d, "circe.json"), "w") as f:
            json.dump(CIRCE, f, indent=2); f.write("\n")
    rows = ", ".join(sorted(set(o["rows"] for o in p["outputs"].values())))
    index_lines.append("| %02d | %02d-%s | %s | %s |" % (num, num, name, ", ".join(p["outputs"].keys()), rows))
with open(os.path.join(OUT, "README.md"), "w") as f:
    f.write("\n".join(index_lines) + "\n\nEach directory holds `recipe.R` (public-client recipe in the frozen "
            "2.10.3 API; runs once WS-8 lands), `plan.json` (the validated v2 plan, the normative target "
            "the client must emit after normalisation), `README.md` (the semantics the expected results "
            "follow) and `expected.json` (hand-computed results on the extended reach fixture at "
            "nfilter.subset = 5: every output has at least five persons).\n")
print("wrote", len(recipes), "recipes under", OUT)
