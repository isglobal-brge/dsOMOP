# 02 active-comparator

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Arms are eras (gap 30) over the recorded drug_era rows of ACE inhibitors (1308216) and
ARBs (1367500); the index is the first candidate (by start date, then declared arm
order ACEI before ARB, then key) whose criteria hold: >= 365 observed days before it
under the unique-coverage policy, and no era of either arm starting in [index-365,
index-1] (washout; an era "occurrence" in a window is its start date). `index_end`
is the selected era's end (course end). Baseline: arm (index_source = "acei" or "arb"),
age, sex, prior MI (any MI on or before the index). Survival (competing risk, first of
mi/death): entry = index + 1 for both TARs; ITT exit = observation end; on-treatment
exit = course end + 30; censoring also at the first start of the OTHER arm's era after
entry, expressed with `when_index_source` (amendment A-7): the ARB censor applies to
ACEI-index episodes only and the ACEI censor to ARB-index episodes only, so an own-arm
restart never censors; observation end also censors; for the competing-risk format
death is an outcome, not a censor. `prior_outcome` applies to every declared outcome:
an episode with an MI OR a death in [entry-365, entry) is excluded. Reach fixture: ACEI
eras 2019-06-01..2020-06-01 for persons 20..45, ARB eras 2020-01-01..2020-08-01 for
15..40, MI 2019-07-01 for 1..25, deaths 2020-06-15 for persons divisible by 4;
extension 1031 (a first ACEI era with < 365 observed days before it, then an eligible
one on 2020-05-01, more than a year after the first era's start) and 1032 (ACEI 2019-06-01..08-31, own-arm restart 2019-11-01
never censoring, ARB 2020-02-15 censoring the ACEI index at day 259).
Released components (A-5): `baseline` is a wide episode frame (row_id, cohort_row_id,
person_id, arm, age, sex, prior_mi); `tte.itt` and `tte.on_treatment` carry row_id,
cohort_row_id, person_id, outcome_name, event, entry_days_from_index,
exit_days_from_index, follow_up_days (= exit - entry).
