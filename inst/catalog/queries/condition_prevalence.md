---
Group: Condition
Name: Condition prevalence by concept
ID: condition.prevalence_by_concept
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the top conditions ranked by number of distinct persons, with concept
names from the vocabulary. Useful for understanding the disease burden in
the CDM population.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| top_n     | 50      | No        | Number of top conditions (default 50) |

## Output

| Field | Description |
|-------|-------------|
| concept_id | Condition concept ID |
| concept_name | Standard concept name |
| n_persons | Number of distinct persons with this condition |
| n_records | Total number of condition records |

## Query

```sql
SELECT co.condition_concept_id AS concept_id,
       c.concept_name,
       COUNT(DISTINCT co.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.condition_occurrence co
JOIN @vocab.concept c ON c.concept_id = co.condition_concept_id
WHERE co.condition_concept_id != 0
GROUP BY co.condition_concept_id, c.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
