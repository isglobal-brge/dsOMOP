---
Group: Condition
Name: Condition type distribution
ID: condition_occurrence.type_distribution
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of records and distinct persons grouped by condition type
concept ID, with the standard concept name. Useful for understanding the
provenance of condition records (e.g. EHR, claim, registry).

## Output

| Field | Description |
|-------|-------------|
| condition_type_concept_id | Condition type concept ID |
| concept_name | Standard concept name of the condition type |
| n_persons | Number of distinct persons |
| n_records | Total number of condition records |

## Query

```sql
SELECT co.condition_type_concept_id,
       c.concept_name,
       COUNT(DISTINCT co.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.condition_occurrence co
JOIN @vocab.concept c ON c.concept_id = co.condition_type_concept_id
GROUP BY co.condition_type_concept_id, c.concept_name
ORDER BY n_records DESC
```

## Sensitive Fields

n_persons, n_records
