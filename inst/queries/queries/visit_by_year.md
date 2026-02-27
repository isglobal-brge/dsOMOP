---
Group: Visit
Name: Visit type by year
ID: visit.type_by_year
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns visit counts cross-tabulated by visit type and calendar year of the
visit start date. Shows both distinct person counts and total record counts
for each combination.

## Output

| Field | Description |
|-------|-------------|
| visit_concept_id | Visit concept ID |
| visit_name | Visit type name |
| visit_year | Calendar year of visit start date |
| n_persons | Number of distinct persons |
| n_records | Total number of visit records |

## Query

```sql
SELECT vo.visit_concept_id,
       c.concept_name AS visit_name,
       EXTRACT(YEAR FROM vo.visit_start_date) AS visit_year,
       COUNT(DISTINCT vo.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.visit_occurrence vo
JOIN @vocab.concept c ON c.concept_id = vo.visit_concept_id
WHERE vo.visit_concept_id != 0
GROUP BY vo.visit_concept_id, c.concept_name,
         EXTRACT(YEAR FROM vo.visit_start_date)
ORDER BY visit_year, n_persons DESC
```

## Sensitive Fields

n_persons, n_records
