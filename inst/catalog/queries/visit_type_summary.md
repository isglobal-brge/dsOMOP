---
Group: Visit
Name: Visit type summary
ID: visit.type_summary
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns a summary of visits by visit concept (inpatient, outpatient, ER, etc.),
showing the number of persons and records for each type.

## Output

| Field | Description |
|-------|-------------|
| visit_concept_id | Visit concept ID |
| visit_name | Visit type name |
| n_persons | Number of distinct persons |
| n_records | Total number of visit records |

## Query

```sql
SELECT vo.visit_concept_id,
       c.concept_name AS visit_name,
       COUNT(DISTINCT vo.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.visit_occurrence vo
JOIN @vocab.concept c ON c.concept_id = vo.visit_concept_id
WHERE vo.visit_concept_id != 0
GROUP BY vo.visit_concept_id, c.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons, n_records
