# 05 same-day-same-visit

Status: plan validated against plan-v2.json; expected.json computed by tools/ws0/acceptance_expected.R on the extended reach fixture (tests/fixtures/create_reach_db.R + extend_reach_db.R), an independent SQL/base-R computation.

## Semantics of the expected results
One episode per MI row (4329847, descendants included; the alternative code 312327 is a
different concept) whose date lies in exactly one observation period; two rows on the
same day give two episodes ordered by (person, date, end, source, key). Columns:
MI rows on the index day (window [0, 0], counts the index row itself), a PCI procedure
or a chest-pain condition on the index row's visit (`visit_occurrence_id` equal to the
index row's), and distinct inpatient visits (9201) starting in [index, index+30].
Reach fixture: MI 2019-07-01 for persons 1..25 whose condition rows carry
visit_occurrence_id = person id (the person's 2019-05-01 outpatient visit); the base
procedure rows have no visit. Extension: 1007 (two MI rows and a same-visit PCI and
chest pain on visit 700701), 1011 (MI after death, still an episode), 1012, 1019
(two episodes), 1020, 1021, 1022.
