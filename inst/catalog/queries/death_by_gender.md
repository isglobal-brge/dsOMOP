---
Group: Death
Name: Death by gender
ID: death.by_gender
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns death counts cross-tabulated by gender. Shows the number of distinct
persons who died for each gender category.

## Output

| Field | Description |
|-------|-------------|
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT p.gender_concept_id,
       gc.concept_name AS gender_name,
       COUNT(DISTINCT d.person_id) AS n_persons
FROM @cdm.death d
JOIN @cdm.person p ON p.person_id = d.person_id
JOIN @vocab.concept gc ON gc.concept_id = p.gender_concept_id
GROUP BY p.gender_concept_id, gc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
