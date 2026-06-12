---
Group: Condition Era
Name: Condition era prevalence by gender
ID: condition_era.by_gender
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns condition era prevalence cross-tabulated by gender for a given
condition concept ID, showing the count of distinct persons by gender concept.

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
SELECT ce.condition_concept_id,
       cc.concept_name AS condition_name,
       p.gender_concept_id,
       gc.concept_name AS gender_name,
       COUNT(DISTINCT ce.person_id) AS n_persons
FROM @cdm.condition_era ce
JOIN @cdm.person p ON p.person_id = ce.person_id
JOIN @vocab.concept cc ON cc.concept_id = ce.condition_concept_id
JOIN @vocab.concept gc ON gc.concept_id = p.gender_concept_id
WHERE ce.condition_concept_id = @concept_id
GROUP BY ce.condition_concept_id, cc.concept_name,
         p.gender_concept_id, gc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
