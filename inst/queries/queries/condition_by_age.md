---
Group: Condition
Name: Condition prevalence by age group
ID: condition.prevalence_by_age
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons with a specific condition, stratified
by age decade at the time of condition onset. Age is computed as the year of
the condition start date minus the year of birth, then binned into decades
(0-9, 10-19, ..., 80+).

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | Yes | Condition concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| condition_concept_id | Condition concept ID |
| condition_name | Condition concept name |
| age_group | Age decade label (e.g. 0-9, 10-19, ..., 80+) |
| n_persons | Number of distinct persons in this age group |

## Query

```sql
SELECT co.condition_concept_id,
       c.concept_name AS condition_name,
       CASE
         WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 10 THEN '0-9'
         WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 20 THEN '10-19'
         WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 30 THEN '20-29'
         WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 40 THEN '30-39'
         WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 50 THEN '40-49'
         WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 60 THEN '50-59'
         WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 70 THEN '60-69'
         WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 80 THEN '70-79'
         ELSE '80+'
       END AS age_group,
       COUNT(DISTINCT co.person_id) AS n_persons
FROM @cdm.condition_occurrence co
JOIN @cdm.person p ON p.person_id = co.person_id
JOIN @vocab.concept c ON c.concept_id = co.condition_concept_id
WHERE co.condition_concept_id = @concept_id
GROUP BY co.condition_concept_id, c.concept_name,
         CASE
           WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 10 THEN '0-9'
           WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 20 THEN '10-19'
           WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 30 THEN '20-29'
           WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 40 THEN '30-39'
           WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 50 THEN '40-49'
           WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 60 THEN '50-59'
           WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 70 THEN '60-69'
           WHEN EXTRACT(YEAR FROM co.condition_start_date) - p.year_of_birth < 80 THEN '70-79'
           ELSE '80+'
         END
ORDER BY age_group
```

## Sensitive Fields

n_persons
