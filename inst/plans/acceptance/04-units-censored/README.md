# 04 units-censored

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Rows: cohort-1 episodes (unique coverage) with at least one HbA1c row (any unit, any operator) in
[index-180, index] = [2019-07-05, 2020-01-01]. Normalisation happens before every
predicate and reducer: `value` = the % value when the unit is 8554, 0.0915 x + 2.15 when
the unit is 8753 (mmol/mol), NA when the unit is NULL or anything else (unknown =
missing keeps the row). `a1c_closest`: the row nearest to the index on or before it
whose normalised value is not NULL (missing = skip); among rows at the minimum distance
the FIRST under ascending (date, source namespace, event key) wins, as the reducer
contract orders rows (extension person 1006 has two rows on 2019-12-20: 52 mmol/mol,
key 500001, and 7.9 %, key 500002, so 6.908 wins).
`a1c_n_ge65`: rows whose normalised value >= 6.5 (NULL values never satisfy).
`a1c_n_uncensored`: rows whose operator is not a reviewed inequality (4171756, 4172704,
4171754, 4171755); NULL/0/= operators count. `a1c_n_known_unit`: rows whose unit is 8554
or 8753. `a1c_max`: maximum normalised value (NULLs ignored). Extension person 1006 is
the adversarial case (mmol/mol, NULL unit, < and > operators, NULL value).
