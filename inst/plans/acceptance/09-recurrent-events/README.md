# 09 recurrent-events

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Rows: every cohort-1 episode; TAR [index+1, index+90] = [2020-01-02, 2020-03-31];
exit = min(TAR end, observation end, death date). Event rows: every respiratory
infection (320136) dated in [entry, exit], numbered per episode by date then key
(`event = 1`, event_days_from_index); `complete = TRUE` adds exactly one sentinel
row (`event = 0`, event_number = last + 1, event_days_from_index = NA) for every
episode without a retained event, so the dense frame has one row per episode at
least. Fixture: the base rows of 320136 are dated 2020-04-01 (day 91, outside the
TAR) so only extension person 1027 (days 9, 19, 45) has events; everyone else gets a
sentinel; persons divisible by 4 die on 2020-06-15 (after the TAR, no effect).
