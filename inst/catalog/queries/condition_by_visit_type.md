---
Group: Condition
Name: Condition prevalence by visit type
ID: condition.prevalence_by_visit_type
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns condition prevalence cross-tabulated by visit type. For a given
condition concept ID, shows the count of persons and records by the type
of visit (e.g. inpatient, outpatient, emergency) in which the condition
was recorded.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | Yes | Condition concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| condition_concept_id | Condition concept ID |
| condition_name | Condition concept name |
| visit_concept_id | Visit type concept ID |
| visit_type_name | Visit type concept name |
| n_persons | Number of distinct persons |
| n_records | Total number of condition records |

## Query

```sql
SELECT co.condition_concept_id,
       cc.concept_name AS condition_name,
       vo.visit_concept_id,
       vc.concept_name AS visit_type_name,
       COUNT(DISTINCT co.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.condition_occurrence co
JOIN @cdm.visit_occurrence vo ON vo.visit_occurrence_id = co.visit_occurrence_id
JOIN @vocab.concept cc ON cc.concept_id = co.condition_concept_id
JOIN @vocab.concept vc ON vc.concept_id = vo.visit_concept_id
WHERE co.condition_concept_id = @concept_id
GROUP BY co.condition_concept_id, cc.concept_name,
         vo.visit_concept_id, vc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons, n_records
