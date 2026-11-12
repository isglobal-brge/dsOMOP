# Acceptance corpus (design-2.10.3 section 10.7)

| # | directory | output(s) | rows |
|---|---|---|---|
| 01 | 01-characterisation | table1 | c1 |
| 02 | 02-active-comparator | baseline, tte | new_users |
| 03 | 03-distinct-days | two_days | dm2 |
| 04 | 04-units-censored | labs | with_a1c |
| 05 | 05-same-day-same-visit | episodes | mi_all |
| 06 | 06-coverage-persistence | coverage, persistence | users |
| 07 | 07-monthly-panel | panel | c1 |
| 08 | 08-sccs-matrix | matrix | sccs |
| 09 | 09-recurrent-events | recurrent | c1 |
| 10 | 10-matching | cc_table | cc |
| 11 | 11-incidence-years | years | c1 |
| 12 | 12-circe-baseline | baseline | t2dm_first |
| 13 | 13-parametrised | two_days | dm2 |
| 14 | 14-oncology-episodes | episodes, first_line | first_line, onc |
| 15 | 15-costs-adherence | adherence | users |

Each directory holds `recipe.R` (public-client recipe in the frozen 2.10.3 API; runs once WS-8 lands), `plan.json` (the validated v2 plan, the normative target the client must emit after normalisation), `README.md` (the semantics the expected results follow) and `expected.json` (hand-computed results on the extended reach fixture at nfilter.subset = 5: every output has at least five persons).

## Paired score reference (CR-2 / amendment A-2)

`score-reference.json` (written by `tools/ws0/acceptance_expected.R` from
`tests/fixtures/score_reference.R`, plain SQL + base R, no engine) freezes the
Charlson, CHADS2 and CHA2DS2-VASc scores of every member of cohort 1 (index
2020-01-01) and cohort 1002 (index 2019-07-01) under both semantics:

- `legacy`: v1 adapter (IR flag `legacy_all_time_conditions`): every condition
  row counts, age at the explicit `reference_date` (v1 has no default);
- `native`: v2 (A-2): condition rows dated on or before the reference date,
  `reference_date = NULL` resolving to the episode index date, age at that date.

Cases: cohort 1 with fixed reference dates 2020-01-01 and 2030-01-01 (the age
points move: `pins.age_reference_shift`), cohort 1002 with the native
index-date resolution (heart failure dated 2019-07-10 drops out:
`pins.index_resolution`) and with a fixed 2020-01-01. `pins.charlson_history`
records person 1011 (legacy 2, native 1). `test-ws0-score-reference.R`
recomputes the file from the fixture and checks that the current v1 engine
produces the `legacy` columns and still refuses a missing age reference; the
`native` columns are the WS-4 target.
