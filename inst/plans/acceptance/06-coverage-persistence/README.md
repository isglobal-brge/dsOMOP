# 06 coverage-persistence

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Rows: cohort 1001 (index 2019-12-31; base metformin users 1..40, the adherence
fixtures 1003/1004/1005 and the cost person 1017). Metformin fills are normalised to
closed intervals: recorded end when present, else start + days_supply - 1 when the
supply is a positive integer, else the start day alone; a row whose end precedes its
start contributes nothing. `metf_era`: eras with a 30-day gap (a fill joins the era
when start <= previous max end + 31). Window [1, 90] = 2020-01-01..2020-03-30.
`covered_rec` = union length of the recorded fill intervals clipped to the window;
`covered_eff` = the same over the stockpiled relation (carry = min(90, max(0,
previous effective end - start + 1)), effective end = start + supply - 1 + carry,
supply precedence: positive days_supply, else recorded length, else one day);
`era_count` = eras overlapping the window; `first_era_days` = envelope length of the
first era overlapping the window, clipped to it; `supply_start` = sum of days_supply
of fills starting in the window (invalid supply contributes 0); `supply_clipped` =
per-fill supplied days inside the window, overlaps not unioned. Survival: outcome MI,
TAR [1, 365], censoring at observation end, death, and 30 days after the end of the
first metformin era ending after entry (persons without such an era are not
censored by it); exit = min(outcome, censor); `event` = 1 when the outcome comes
first (ties: outcome first). Fixture: base fills 2019-07-01/2019-10-15/2020-02-01
(end = start, supply 90 -> one-day intervals), 1003 one 10-day fill, 1004 two
overlapping 30-day fills (recorded 44, effective 60), 1005 data-quality rows.
