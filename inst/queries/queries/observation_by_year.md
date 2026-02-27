---
Group: Observation
Name: Observation prevalence by year
ID: observation.prevalence_by_year
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns observation prevalence by calendar year of the observation date.
Optionally filters to a specific observation concept ID; when concept_id is 0
all observations are included.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 4214956 | No | Observation concept ID (0 = all observations) |
| top_n     | 50      | No | Maximum rows returned (default 50) |

## Output

| Field | Description |
|-------|-------------|
| observation_concept_id | Observation concept ID |
| concept_name | Standard concept name |
| observation_year | Calendar year of observation date |
| n_persons | Number of distinct persons |
| n_records | Total number of observation records |

## Query

```sql
SELECT o.observation_concept_id,
       c.concept_name,
       EXTRACT(YEAR FROM o.observation_date) AS observation_year,
       COUNT(DISTINCT o.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.observation o
JOIN @vocab.concept c ON c.concept_id = o.observation_concept_id
WHERE o.observation_concept_id != 0
  AND (@concept_id = 0 OR o.observation_concept_id = @concept_id)
GROUP BY o.observation_concept_id, c.concept_name,
         EXTRACT(YEAR FROM o.observation_date)
ORDER BY observation_year DESC, n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
