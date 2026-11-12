# 13 parametrised

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Recipe 03 written with three parameters (the concept set, the cohort id and the day
threshold) and bound on the client before emission; its bound plan equals plan.json of
recipe 03 exactly (this directory's plan.json is a copy) and its expected results are
recipe 03's. The directory `variants/` holds one parametrised public recipe for EACH of
recipes 01..12 (`recipe-NN.R`, a bound parameter per recipe) with the bound plan target
`plan-NN.json` (byte-identical to that recipe's plan.json) and `variants.json`, the
manifest mapping every variant to the directory whose expected.json it must reproduce.
