---
Group: Observation
Name: Observation prevalence by gender
ID: observation.prevalence_by_gender
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns observation prevalence cross-tabulated by gender for a given observation
concept ID. Shows the count of distinct persons by gender.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 4214956 | Yes | Observation concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| observation_concept_id | Observation concept ID |
| observation_name | Observation concept name |
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT o.observation_concept_id,
       oc.concept_name AS observation_name,
       p.gender_concept_id,
       gc.concept_name AS gender_name,
       COUNT(DISTINCT o.person_id) AS n_persons
FROM @cdm.observation o
JOIN @cdm.person p ON p.person_id = o.person_id
JOIN @vocab.concept oc ON oc.concept_id = o.observation_concept_id
JOIN @vocab.concept gc ON gc.concept_id = p.gender_concept_id
WHERE o.observation_concept_id = @concept_id
GROUP BY o.observation_concept_id, oc.concept_name,
         p.gender_concept_id, gc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
