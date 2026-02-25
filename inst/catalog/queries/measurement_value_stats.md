---
Group: Measurement
Name: Measurement value statistics
ID: measurement.value_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns summary statistics (mean, sd, min, max, count) for the numeric values
of a specific measurement concept. Useful for understanding value distributions
without revealing individual measurements.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 3025315 | Yes | Measurement concept ID |

## Output

| Field | Description |
|-------|-------------|
| measurement_concept_id | Measurement concept ID |
| concept_name | Concept name |
| n_persons | Number of distinct persons |
| n_values | Number of non-null numeric values |
| mean_value | Mean of value_as_number |
| sd_value | Standard deviation |
| min_value | Minimum value |
| max_value | Maximum value |

## Query

```sql
SELECT m.measurement_concept_id,
       c.concept_name,
       COUNT(DISTINCT m.person_id) AS n_persons,
       COUNT(m.value_as_number) AS n_values,
       AVG(m.value_as_number) AS mean_value,
       STDDEV(m.value_as_number) AS sd_value,
       MIN(m.value_as_number) AS min_value,
       MAX(m.value_as_number) AS max_value
FROM @cdm.measurement m
JOIN @vocab.concept c ON c.concept_id = m.measurement_concept_id
WHERE m.measurement_concept_id = @concept_id
  AND m.value_as_number IS NOT NULL
GROUP BY m.measurement_concept_id, c.concept_name
```

## Sensitive Fields

n_persons, n_values
