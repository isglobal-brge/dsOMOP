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
| 14 | 14-oncology-episodes | episodes | onc |
| 15 | 15-costs-adherence | adherence | users |

Each directory holds `recipe.R` (public-client recipe in the frozen 2.10.3 API; runs once WS-8 lands), `plan.json` (the validated v2 plan, the normative target the client must emit after normalisation), `README.md` (the semantics the expected results follow) and `expected.json` (hand-computed results on the extended reach fixture at nfilter.subset = 5: every output has at least five persons).
