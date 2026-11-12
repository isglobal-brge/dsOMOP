# 08 sccs-matrix

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Rows: cohort 1002 (index 2019-07-01, end 2020-06-30; base aspirin users 1..30 and the
SCCS persons 1019..1022). Effective window [L, U] = [2019-07-01, 2020-06-30]
intersected with the eligibility span. Boundaries: L and U+1; the bin starts L + 365k
inside the window (width 365: L + 365 = 2020-06-30 = U, so the last day is its own
period); starts and ends+1 of the actual observed segments; starts and ends+1 of each
aspirin era (gap 30, supply fallback); the 1 January of every year (age bands of 5
years change on 1 January and calendar years start there). Periods are
[b_i, b_{i+1}-1] intersected with the observed segments; empty periods are dropped.
Columns: period_start_days, period_end_days, days, age (annual: year of period start
- year_of_birth), age_band (floor(age/5)), calendar_year, exposed_aspirin (days of
the period inside the union of aspirin eras), active_aspirin (the union covers the
period start), n_mi (MI rows dated inside the period and inside an observed segment).
Fixture: base aspirin 2019-07-01 (end = start, supply 30: one-day exposure), base MI
2019-07-01 (persons 1..25); extension exposures and MI straddle 1 January 2020. The bin
boundary adds the one-day period [365, 365] to every episode whose last day was not
already a boundary: 139 rows in all. Released component: row_id, cohort_row_id,
person_id, period_start_days, period_end_days, days, age, age_band, calendar_year,
exposed_aspirin, active_aspirin, n_mi.
