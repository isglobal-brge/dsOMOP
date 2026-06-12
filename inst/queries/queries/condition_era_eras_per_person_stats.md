---
Group: Condition Era
Name: Condition eras per person statistics
ID: condition_era.eras_per_person_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons together with the average and standard
deviation of the number of condition eras per person for a given condition
concept ID. No minimum or maximum values are returned.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | Yes | Condition concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| n_persons | Number of distinct persons with this condition era |
| avg_eras | Average number of condition eras per person |
| stddev_eras | Standard deviation of condition eras per person |

## Query

```sql
SELECT COUNT(*) AS n_persons,
       AVG(CAST(per.n_eras AS FLOAT)) AS avg_eras,
       STDDEV(CAST(per.n_eras AS FLOAT)) AS stddev_eras
FROM (
  SELECT ce.person_id,
         COUNT(*) AS n_eras
  FROM @cdm.condition_era ce
  WHERE ce.condition_concept_id = @concept_id
  GROUP BY ce.person_id
) per
```

## Sensitive Fields

n_persons
