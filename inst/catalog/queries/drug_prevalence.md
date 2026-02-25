---
Group: Drug
Name: Drug exposure prevalence by concept
ID: drug.prevalence_by_concept
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the top drug exposures ranked by number of distinct persons, with
concept names from the vocabulary.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| top_n     | 50      | No        | Number of top drugs (default 50) |

## Output

| Field | Description |
|-------|-------------|
| concept_id | Drug concept ID |
| concept_name | Standard concept name |
| n_persons | Number of distinct persons with this drug |
| n_records | Total number of drug exposure records |

## Query

```sql
SELECT de.drug_concept_id AS concept_id,
       c.concept_name,
       COUNT(DISTINCT de.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.drug_exposure de
JOIN @vocab.concept c ON c.concept_id = de.drug_concept_id
WHERE de.drug_concept_id != 0
GROUP BY de.drug_concept_id, c.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
