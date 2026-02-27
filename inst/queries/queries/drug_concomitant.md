---
Group: Drug
Name: Concomitant medications
ID: drug.concomitant_medications
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

For persons exposed to a specific drug, finds the top concomitant medications
ranked by number of distinct persons. Useful for understanding polypharmacy
patterns and co-prescribing behavior.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 1127433 | Yes | Target drug concept ID |
| top_n     | 50      | No | Maximum concomitant drugs returned (default 50) |

## Output

| Field | Description |
|-------|-------------|
| concomitant_concept_id | Concomitant drug concept ID |
| concomitant_name | Concomitant drug concept name |
| n_persons | Number of distinct persons exposed to both drugs |

## Query

```sql
SELECT de2.drug_concept_id AS concomitant_concept_id,
       c.concept_name AS concomitant_name,
       COUNT(DISTINCT de2.person_id) AS n_persons
FROM @cdm.drug_exposure de1
JOIN @cdm.drug_exposure de2 ON de2.person_id = de1.person_id
JOIN @vocab.concept c ON c.concept_id = de2.drug_concept_id
WHERE de1.drug_concept_id = @concept_id
  AND de2.drug_concept_id != @concept_id
  AND de2.drug_concept_id != 0
GROUP BY de2.drug_concept_id, c.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons
