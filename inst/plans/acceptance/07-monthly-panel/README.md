# 07 monthly-panel

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Rows: one row per cohort-1 episode (unique coverage; 1001 dropped) and calendar month
overlapping the window [index-92, index+59] = [2019-10-01, 2020-02-29] intersected with
the episode's observation period: five months for every episode except 1002, whose
covering period starts 2020-01-01 (January and February only). Columns (amendment A-3):
period_start_days/period_end_days (relative to the index), days (observed days in the
period); for the single-identity source `weight`: weight_count, weight_last (normalised
value of the last row in the month by date then id; NA when none), weight_carry (value
at the period start: the last non-missing normalised value dated <= period start and >=
period start - 180 days; NA when none; no expiry); for the two-identity source `vitals`
(weight 3025315 and systolic blood pressure 3004249, no unit conversion): vitals_3025315_count,
vitals_3025315_last, vitals_3004249_count, vitals_3004249_last (concepts are never
pooled); for the available but whole-site-empty source `bmi` (3038553, no row at this
site): bmi_count = 0, bmi_last = NA and bmi_carry = NA on every row of the dense roster.
Weights are in kg; the fixture's pound rows (unit 8739) are converted x 0.45359237 for
`weight` only. Fixture: persons 1..40 weigh 80 kg on 2019-12-01 and 1..20 have 176 lb on
2020-02-01, persons 20..70 have systolic pressures on 2019-12-01 and 2020-03-01 (the
latter outside the window); extension 1023 (Oct 80, Dec 82, Feb 85), 1024 (no rows),
1025 (70 kg on 2019-09-01, before the window: carried), 1026 (90 and 91 in Jan).
Released component: row_id, cohort_row_id, person_id, period_start_days, period_end_days,
days, weight_count, weight_last, weight_carry, vitals_3025315_count, vitals_3025315_last,
vitals_3004249_count, vitals_3004249_last, bmi_count, bmi_last, bmi_carry.
