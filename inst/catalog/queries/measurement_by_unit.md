---
Group: Measurement
Name: Measurement prevalence by unit
ID: measurement.prevalence_by_unit
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns measurement counts cross-tabulated by unit of measurement. For a given
measurement concept ID, shows the number of persons and records by each unit
concept.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 3025315 | Yes | Measurement concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| measurement_concept_id | Measurement concept ID |
| measurement_name | Measurement concept name |
| unit_concept_id | Unit concept ID |
| unit_name | Unit concept name |
| n_persons | Number of distinct persons |
| n_records | Total number of measurement records |

## Query

```sql
SELECT m.measurement_concept_id,
       mc.concept_name AS measurement_name,
       m.unit_concept_id,
       uc.concept_name AS unit_name,
       COUNT(DISTINCT m.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.measurement m
JOIN @vocab.concept mc ON mc.concept_id = m.measurement_concept_id
JOIN @vocab.concept uc ON uc.concept_id = m.unit_concept_id
WHERE m.measurement_concept_id = @concept_id
GROUP BY m.measurement_concept_id, mc.concept_name,
         m.unit_concept_id, uc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons, n_records
