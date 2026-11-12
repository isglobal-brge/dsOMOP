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
tumour specimen rows (4001225) in [index-30, index+365]. `first_line` is the
chronologically first treatment episode per person (same coverage rule); its
`line_days` is the INCLUSIVE coverage of the episode itself (the `episode_course` era
over the episode rows with gap 0 and no supply fallback, `covered_days` over the window
[index, index_end]): 214 for 1015 (2019-03-01..2019-09-30), 123 for 1016, 90 for
persons 1..8; `n_member_drugs` is repeated there for the first line. Fixture: 1015 has
episodes 1 (3 chemotherapy fills + 1 procedure, 2019-03-01..09-30) and 2 (the
2019-10-01 fill plus the 2019-04-12 fill linked a second time), 1016 episode 3 (one
fill), base persons 1..8 one episode each (2019-12-01..2020-02-28) linking their
2020-02-01 metformin fill. Released components: `episodes` (row_id, cohort_row_id,
person_id, n_members, n_member_drugs, first_member_days, n_specimens) and `first_line`
(row_id, cohort_row_id, person_id, line_days, n_member_drugs).
