---
Group: Measurement
Name: Measurement prevalence by age group
ID: measurement.prevalence_by_age
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns measurement prevalence cross-tabulated by age decade for a given
measurement concept ID. Age is computed at the time of measurement.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 3025315 | Yes | Measurement concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| measurement_concept_id | Measurement concept ID |
| measurement_name | Measurement concept name |
| age_group | Age decade bucket (e.g. 0-9, 10-19) |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT m.measurement_concept_id,
       c.concept_name AS measurement_name,
       CASE
         WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth <  10 THEN '0-9'
         WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 20 THEN '10-19'
         WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 30 THEN '20-29'
         WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 40 THEN '30-39'
         WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 50 THEN '40-49'
         WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 60 THEN '50-59'
         WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 70 THEN '60-69'
         WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 80 THEN '70-79'
         ELSE '80+'
       END AS age_group,
       COUNT(DISTINCT m.person_id) AS n_persons
FROM @cdm.measurement m
JOIN @cdm.person p ON p.person_id = m.person_id
JOIN @vocab.concept c ON c.concept_id = m.measurement_concept_id
WHERE m.measurement_concept_id = @concept_id
GROUP BY m.measurement_concept_id, c.concept_name,
         CASE
           WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth <  10 THEN '0-9'
           WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 20 THEN '10-19'
           WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 30 THEN '20-29'
           WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 40 THEN '30-39'
           WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 50 THEN '40-49'
           WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 60 THEN '50-59'
           WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 70 THEN '60-69'
           WHEN EXTRACT(YEAR FROM m.measurement_date) - p.year_of_birth < 80 THEN '70-79'
           ELSE '80+'
         END
ORDER BY age_group
```

## Sensitive Fields

n_persons
