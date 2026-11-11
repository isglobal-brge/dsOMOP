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
entry (the own arm's eras never censor), observation end, and for the competing-risk
format death is an outcome, not a censor; episodes with an MI in [entry-365, entry)
are excluded (prior_outcome). Reach fixture: ACEI eras 2019-06-01..2020-06-01 for
persons 20..45, ARB eras 2020-01-01..2020-08-01 for 15..40, MI 2019-07-01 for 1..25,
deaths 2020-06-15 for persons divisible by 4.
