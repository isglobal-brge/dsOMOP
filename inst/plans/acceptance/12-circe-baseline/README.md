# 12 circe-baseline

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
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
