# 03 distinct-days

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
`dm2` inherits the episodes of cohort 1 (parent; unique coverage, 1001 dropped) and keeps those with diabetes
(201820 + descendants) rows on at least two distinct days in [index-365, index].
Columns: distinct diagnosis days and raw row count in the window. Reach fixture:
persons 1..25 have rows on 2019-06-01, 2019-06-05 and 2019-06-08 (3 days), 26..50 one
day; extension person 1008 has 2019-01-01 (the window's first day) and 2020-01-01
(2 days; the 2018-12-31 row is outside).
