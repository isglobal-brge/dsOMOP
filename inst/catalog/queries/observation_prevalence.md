---
Group: Observation
Name: Observation prevalence by concept
ID: observation.prevalence_by_concept
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the top observations ranked by number of distinct persons, with concept
names from the vocabulary. Useful for understanding the observation landscape
in the CDM population.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| top_n     | 50      | No        | Number of top observations (default 50) |

## Output

| Field | Description |
|-------|-------------|
| concept_id | Observation concept ID |
| concept_name | Standard concept name |
| n_persons | Number of distinct persons with this observation |
| n_records | Total number of observation records |

## Query

```sql
SELECT o.observation_concept_id AS concept_id,
       c.concept_name,
       COUNT(DISTINCT o.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.observation o
JOIN @vocab.concept c ON c.concept_id = o.observation_concept_id
WHERE o.observation_concept_id != 0
GROUP BY o.observation_concept_id, c.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
