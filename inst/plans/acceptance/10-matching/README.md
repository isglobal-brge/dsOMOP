# 10 matching

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
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
