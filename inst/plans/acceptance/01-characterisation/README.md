# 01 characterisation

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Rows: every episode of results cohort 1 (index 2020-01-01, end 2025-12-31; the reach
persons 1..100 plus the extension members of cohort 1) whose index date lies in exactly
one observation period (the default unique-coverage policy: person 1001, with two
covering periods, is dropped from every cohort-1 output). Columns: exact annual age
(2020 - year_of_birth; NA for a NULL or future birth year), sex (M/F), forty binary
covariates (any row of the concept, descendants included, with start date in
[index-365, index] = [2019-01-01, 2020-01-01]; `cond_covar2` is present in exactly
persons 1013 and 1014), two unit-normalised last values in the same window (HbA1c in %
with mmol/mol converted as 0.0915 x + 2.15; glucose in mg/dL with the fixture's 8753
rows converted x 18.0182; unknown units give NA; the last row by date then id), and
the Charlson index (sum of category weights over condition rows with start date on
or before the index date; categories and weights as `.computeComorbidityScore`,
descendants from the fixture's concept_ancestor). Amendment A-2: the native v2 score
counts conditions on or before the reference date (extension person 1011, whose MI is
dated after the index, scores 1 = diabetes only; the v1 adapter with
`legacy_all_time_conditions` would score 2). Released component: row_id, cohort_row_id,
person_id and the 45 variables.
