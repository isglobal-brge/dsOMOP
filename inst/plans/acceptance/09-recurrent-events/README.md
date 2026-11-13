# 09 recurrent-events

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Rows: every cohort-1 episode (unique coverage) whose exit is not before its entry:
TAR [index+1, index+90] = [2020-01-02, 2020-03-31]; exit = min(TAR end, observation
end, death date); an episode with exit < entry (person 1011, dead on 2019-06-01) is
ineligible and produces neither a sentinel nor a risk-set row — `complete = TRUE`
never revives an ineligible episode. Components (amendment A-5): `recurrent.events`
(row_id, cohort_row_id, person_id, outcome_name, event, event_number,
outcome_event_number, event_days_from_index, entry_days_from_index,
exit_days_from_index) and `recurrent.risk_sets` (row_id, cohort_row_id, person_id,
entry_days_from_index, exit_days_from_index, follow_up_days). Event rows: every respiratory
infection (320136) dated in [entry, exit], numbered per episode by date then key
(`event = 1`, event_days_from_index); `complete = TRUE` adds exactly one sentinel
row (`event = 0`, event_number = last + 1, event_days_from_index = NA) for every
episode without a retained event, so the dense frame has one row per episode at
least. Fixture: the base rows of 320136 are dated 2020-04-01 (day 91, outside the
TAR) so only extension persons 1027 (days 9, 19, 45) and 1018 (days 1 and 61) have
events; every other eligible episode gets a sentinel (119 sentinels, 124 event rows,
121 persons); persons divisible by 4 die on 2020-06-15 (after the TAR, no effect).
