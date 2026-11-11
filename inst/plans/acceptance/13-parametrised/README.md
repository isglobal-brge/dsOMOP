# 13 parametrised

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
The same study as recipe 03, written with three parameters (the concept set, the
cohort id and the day threshold) and bound on the client before emission. The bound
plan equals plan.json of recipe 03 exactly (this directory's plan.json is a copy) and
the expected results are recipe 03's.
