---
Group: Condition Era
Name: Condition era records by season
ID: condition_era.by_season
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of condition era records by meteorological season of the
condition era start date for a given condition concept ID. Seasons are derived
from the start month (Northern Hemisphere convention).

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | Yes | Condition concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| condition_concept_id | Condition concept ID |
| concept_name | Condition concept name |
| season | Season of the condition era start (Winter/Spring/Summer/Autumn) |
| n_persons | Number of distinct persons |
| n_records | Total number of condition era records |

## Query

```sql
SELECT ce.condition_concept_id,
       c.concept_name,
       CASE
         WHEN EXTRACT(MONTH FROM ce.condition_era_start_date) IN (12, 1, 2) THEN 'Winter'
         WHEN EXTRACT(MONTH FROM ce.condition_era_start_date) IN (3, 4, 5) THEN 'Spring'
         WHEN EXTRACT(MONTH FROM ce.condition_era_start_date) IN (6, 7, 8) THEN 'Summer'
         ELSE 'Autumn'
       END AS season,
       COUNT(DISTINCT ce.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.condition_era ce
JOIN @vocab.concept c ON c.concept_id = ce.condition_concept_id
WHERE ce.condition_concept_id = @concept_id
GROUP BY ce.condition_concept_id, c.concept_name,
         CASE
           WHEN EXTRACT(MONTH FROM ce.condition_era_start_date) IN (12, 1, 2) THEN 'Winter'
           WHEN EXTRACT(MONTH FROM ce.condition_era_start_date) IN (3, 4, 5) THEN 'Spring'
           WHEN EXTRACT(MONTH FROM ce.condition_era_start_date) IN (6, 7, 8) THEN 'Summer'
           ELSE 'Autumn'
         END
ORDER BY season
```

## Sensitive Fields

n_persons, n_records
