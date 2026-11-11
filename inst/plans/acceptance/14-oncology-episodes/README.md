# 14 oncology-episodes

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
Rows: one episode per `episode` row of concept 32531 (index = episode start, end =
episode end). Members: the rows linked through episode_event, mapped by
episode_event_field_concept_id (1147094 drug_exposure, 1147082 procedure_occurrence)
to their tables; an episode-indexed consumer joins members by the parent episode
identity and person, so `n_members` counts the links of THAT episode (no window), and
the same drug row linked to two episodes counts once in each. `n_member_drugs`
restricts the member tables to drug_exposure. `first_member_days` = days from the
index to the earliest member row (start date of the member event). `n_specimens` =
tumour specimen rows (4001225) in [index-30, index+365]. Fixture: 1015 has episodes 1
(3 chemotherapy fills + 1 procedure, 2019-03-01..09-30) and 2 (the 2019-10-01 fill
plus the 2019-04-12 fill linked a second time), 1016 episode 3 (one fill), base persons
1..8 one episode each (2019-12-01..2020-02-28) linking their 2020-02-01 metformin fill.
