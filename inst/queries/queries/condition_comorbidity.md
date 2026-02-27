---
Group: Condition
Name: Comorbidities of a condition
ID: condition.comorbidity
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

For persons who have a specific condition, finds the top co-occurring
conditions ranked by number of distinct persons. Useful for understanding
comorbidity patterns and disease clustering.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | Yes | Target condition concept ID |
| top_n     | 50      | No | Maximum comorbidities returned (default 50) |

## Output

| Field | Description |
|-------|-------------|
| comorbid_concept_id | Comorbid condition concept ID |
| comorbid_name | Comorbid condition concept name |
| n_persons | Number of distinct persons with both conditions |

## Query

```sql
SELECT co2.condition_concept_id AS comorbid_concept_id,
       c.concept_name AS comorbid_name,
       COUNT(DISTINCT co2.person_id) AS n_persons
FROM @cdm.condition_occurrence co1
JOIN @cdm.condition_occurrence co2 ON co2.person_id = co1.person_id
JOIN @vocab.concept c ON c.concept_id = co2.condition_concept_id
WHERE co1.condition_concept_id = @concept_id
  AND co2.condition_concept_id != @concept_id
  AND co2.condition_concept_id != 0
GROUP BY co2.condition_concept_id, c.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons
