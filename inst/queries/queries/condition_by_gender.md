---
Group: Condition
Name: Condition prevalence by gender
ID: condition.prevalence_by_gender
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns condition prevalence cross-tabulated by gender. For a given concept
ID (or all conditions), shows the count of persons by gender.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | Yes | Condition concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| condition_concept_id | Condition concept ID |
| condition_name | Condition concept name |
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT co.condition_concept_id,
       cc.concept_name AS condition_name,
       p.gender_concept_id,
       gc.concept_name AS gender_name,
       COUNT(DISTINCT co.person_id) AS n_persons
FROM @cdm.condition_occurrence co
JOIN @cdm.person p ON p.person_id = co.person_id
JOIN @vocab.concept cc ON cc.concept_id = co.condition_concept_id
JOIN @vocab.concept gc ON gc.concept_id = p.gender_concept_id
WHERE co.condition_concept_id = @concept_id
GROUP BY co.condition_concept_id, cc.concept_name,
         p.gender_concept_id, gc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
