---
Group: Condition
Name: Condition prevalence by provider specialty
ID: condition.prevalence_by_specialty
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns condition prevalence cross-tabulated by provider specialty. For a given
condition concept ID, shows the count of distinct persons by the specialty of
the provider who recorded the condition.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | Yes | Condition concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| condition_concept_id | Condition concept ID |
| condition_name | Condition concept name |
| specialty_concept_id | Provider specialty concept ID |
| specialty_name | Provider specialty concept name |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT co.condition_concept_id,
       cc.concept_name AS condition_name,
       pr.specialty_concept_id,
       sc.concept_name AS specialty_name,
       COUNT(DISTINCT co.person_id) AS n_persons
FROM @cdm.condition_occurrence co
JOIN @cdm.provider pr ON pr.provider_id = co.provider_id
JOIN @vocab.concept cc ON cc.concept_id = co.condition_concept_id
JOIN @vocab.concept sc ON sc.concept_id = pr.specialty_concept_id
WHERE co.condition_concept_id = @concept_id
GROUP BY co.condition_concept_id, cc.concept_name,
         pr.specialty_concept_id, sc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
