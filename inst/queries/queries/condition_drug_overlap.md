---
Group: Condition
Name: Drugs used by condition patients
ID: condition.drug_overlap
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

For persons who have a specific condition, finds the top drugs they are exposed
to, ranked by number of distinct persons. Useful for understanding treatment
patterns and medication use within a disease population.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | Yes | Target condition concept ID |
| top_n     | 50      | No | Maximum drugs returned (default 50) |

## Output

| Field | Description |
|-------|-------------|
| condition_concept_id | Condition concept ID |
| condition_name | Condition concept name |
| drug_concept_id | Drug concept ID |
| drug_name | Drug concept name |
| n_persons | Number of distinct persons with the condition exposed to the drug |

## Query

```sql
SELECT co.condition_concept_id,
       cc.concept_name AS condition_name,
       de.drug_concept_id,
       dc.concept_name AS drug_name,
       COUNT(DISTINCT de.person_id) AS n_persons
FROM @cdm.condition_occurrence co
JOIN @cdm.drug_exposure de ON de.person_id = co.person_id
JOIN @vocab.concept cc ON cc.concept_id = co.condition_concept_id
JOIN @vocab.concept dc ON dc.concept_id = de.drug_concept_id
WHERE co.condition_concept_id = @concept_id
  AND de.drug_concept_id != 0
GROUP BY co.condition_concept_id, cc.concept_name,
         de.drug_concept_id, dc.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons
