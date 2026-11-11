# 07 monthly-panel

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
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
