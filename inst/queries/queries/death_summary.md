---
Group: Death
Name: Death summary statistics
ID: death.summary
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns a summary of deaths by cause of death concept, showing the number of
distinct persons for each cause. Useful for understanding the mortality
landscape in the CDM population.

## Output

| Field | Description |
|-------|-------------|
| cause_concept_id | Cause of death concept ID |
| cause_name | Cause of death concept name |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT d.cause_concept_id,
       c.concept_name AS cause_name,
       COUNT(DISTINCT d.person_id) AS n_persons
FROM @cdm.death d
JOIN @vocab.concept c ON c.concept_id = d.cause_concept_id
GROUP BY d.cause_concept_id, c.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
