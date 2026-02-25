---
Group: Procedure
Name: Procedure prevalence by concept
ID: procedure.prevalence_by_concept
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the top procedures ranked by number of distinct persons, with concept
names from the vocabulary.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| top_n     | 50      | No        | Number of top procedures (default 50) |

## Output

| Field | Description |
|-------|-------------|
| concept_id | Procedure concept ID |
| concept_name | Standard concept name |
| n_persons | Number of distinct persons with this procedure |
| n_records | Total number of procedure records |

## Query

```sql
SELECT po.procedure_concept_id AS concept_id,
       c.concept_name,
       COUNT(DISTINCT po.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.procedure_occurrence po
JOIN @vocab.concept c ON c.concept_id = po.procedure_concept_id
WHERE po.procedure_concept_id != 0
GROUP BY po.procedure_concept_id, c.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
