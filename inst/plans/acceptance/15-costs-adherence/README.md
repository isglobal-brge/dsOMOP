# 15 costs-adherence

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Rows: cohort 1001 (index 2019-12-31) under the union-coverage policy with gap 30: the
eligibility component is the coalesced observation span containing an ACTUAL recorded
period that covers the index; actual observed days are the union of that component's
recorded periods. Costs: `visit_cost_60` sums total_cost of USD (44818668, Currency vocabulary code USD) cost rows
whose cost_event_id is a visit of the person (cost_domain_id "Visit") dated in
[index+1, index+60]; `drug_cost_60` likewise for metformin fills ("Drug"); the EUR
(44818568) row is excluded by the currency filter; persons without cost rows get 0.
Released component: row_id, cohort_row_id, person_id and the thirteen variables. Adherence over
[1, 90] = 2020-01-01..2020-03-30: `supply_90` (MPR numerator) = sum of days_supply of
fills starting in the window; `observed_90` = observed days in the window;
`pdc_num_eff_90` = union of the stockpiled effective-supply intervals intersected with
the window AND the same observed days; `pdc_num_rec_90` the same over recorded
intervals; `covered_*_90` without the observation clip. The [1, 10] variables pin the
design's fixture on person 1003: observed [1,2] U [9,10] = 4 days, effective coverage
4 of 10 -> pdc_num_eff_10 = 4, observed_10 = 4 (4/4, never 10/4); person 1004 pins
recorded 44 vs effective 60 covered days.
