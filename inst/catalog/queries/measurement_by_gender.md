---
Group: Measurement
Name: Measurement prevalence by gender
ID: measurement.prevalence_by_gender
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns measurement prevalence cross-tabulated by gender for a given measurement
concept ID. Shows the count of distinct persons by gender.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 3025315 | Yes | Measurement concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| measurement_concept_id | Measurement concept ID |
| measurement_name | Measurement concept name |
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT m.measurement_concept_id,
       mc.concept_name AS measurement_name,
       p.gender_concept_id,
       gc.concept_name AS gender_name,
       COUNT(DISTINCT m.person_id) AS n_persons
FROM @cdm.measurement m
JOIN @cdm.person p ON p.person_id = m.person_id
JOIN @vocab.concept mc ON mc.concept_id = m.measurement_concept_id
JOIN @vocab.concept gc ON gc.concept_id = p.gender_concept_id
WHERE m.measurement_concept_id = @concept_id
GROUP BY m.measurement_concept_id, mc.concept_name,
         p.gender_concept_id, gc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
