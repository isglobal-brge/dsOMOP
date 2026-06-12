---
Group: Payer Plan
Name: Persons by plan concept
ID: payer_plan.by_plan_concept
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons covered under each payer plan concept,
joined to the vocabulary for human-readable plan names.

## Output

| Field | Description |
|-------|-------------|
| plan_concept_id | Payer plan concept ID |
| concept_name | Plan concept name |
| n_persons | Number of distinct persons covered |

## Query

```sql
SELECT ppp.plan_concept_id,
       c.concept_name,
       COUNT(DISTINCT ppp.person_id) AS n_persons
FROM @cdm.payer_plan_period ppp
JOIN @vocab.concept c ON c.concept_id = ppp.plan_concept_id
GROUP BY ppp.plan_concept_id, c.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
