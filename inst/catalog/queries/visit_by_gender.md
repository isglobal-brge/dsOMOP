---
Group: Visit
Name: Visit type by gender
ID: visit.type_by_gender
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns visit counts cross-tabulated by visit type and gender. Shows both
distinct person counts and total record counts for each combination.

## Output

| Field | Description |
|-------|-------------|
| visit_concept_id | Visit concept ID |
| visit_name | Visit type name |
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| n_persons | Number of distinct persons |
| n_records | Total number of visit records |

## Query

```sql
SELECT vo.visit_concept_id,
       vc.concept_name AS visit_name,
       p.gender_concept_id,
       gc.concept_name AS gender_name,
       COUNT(DISTINCT vo.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.visit_occurrence vo
JOIN @cdm.person p ON p.person_id = vo.person_id
JOIN @vocab.concept vc ON vc.concept_id = vo.visit_concept_id
JOIN @vocab.concept gc ON gc.concept_id = p.gender_concept_id
WHERE vo.visit_concept_id != 0
GROUP BY vo.visit_concept_id, vc.concept_name,
         p.gender_concept_id, gc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons, n_records
