# 11 incidence-years

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Rows: one row per cohort-1 episode and calendar year overlapping [index, index+730]
= [2020-01-01, 2021-12-31] intersected with the observation period (2020 and 2021 for
every episode). Columns: period_start_days, period_end_days, days (observed days:
366 for 2020, 365 for 2021 when fully observed), calendar_year, n_resp (respiratory
infection rows dated inside the period and inside an observed segment). Fixture: 320136
on 2020-04-01 for persons 1..30; extension 1018 (2020-01-02, 2020-03-02), 1027 (three
rows in Jan/Feb 2020).
