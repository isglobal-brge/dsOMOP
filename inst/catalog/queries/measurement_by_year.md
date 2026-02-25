---
Group: Measurement
Name: Measurement prevalence by year
ID: measurement.prevalence_by_year
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns measurement prevalence by calendar year of measurement date.
Optionally filters to a specific measurement concept ID and limits to the top N measurements.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 0 | No | Measurement concept ID to filter (0 = all) |
| top_n | 50 | No | Number of top results to return (default 50) |

## Output

| Field | Description |
|-------|-------------|
| measurement_concept_id | Measurement concept ID |
| concept_name | Measurement concept name |
| measurement_year | Calendar year of measurement |
| n_persons | Number of distinct persons |
| n_records | Total number of measurement records |

## Query

```sql
SELECT m.measurement_concept_id,
       c.concept_name,
       EXTRACT(YEAR FROM m.measurement_date) AS measurement_year,
       COUNT(DISTINCT m.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.measurement m
JOIN @vocab.concept c ON c.concept_id = m.measurement_concept_id
WHERE m.measurement_concept_id != 0
  AND (@concept_id = 0 OR m.measurement_concept_id = @concept_id)
GROUP BY m.measurement_concept_id, c.concept_name,
         EXTRACT(YEAR FROM m.measurement_date)
ORDER BY measurement_year DESC, n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
