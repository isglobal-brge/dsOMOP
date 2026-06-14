---
Group: Drug Era
Name: Drug era prevalence by concept
ID: drug_era.prevalence_by_concept
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
Scope Column: dre.person_id
---

## Description

Returns drug eras ranked by number of distinct persons, with concept names from
the vocabulary. Each drug era aggregates contiguous drug exposures of the same
ingredient.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| top_n | 50 | No | Number of top drug eras (default 50) |

## Output

| Field | Description |
|-------|-------------|
| drug_concept_id | Drug (ingredient) concept ID |
| concept_name | Standard concept name |
| n_persons | Number of distinct persons with this drug era |
| n_records | Total number of drug era records |

## Query

```sql
SELECT dre.drug_concept_id,
       c.concept_name,
       COUNT(DISTINCT dre.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.drug_era dre
JOIN @vocab.concept c ON c.concept_id = dre.drug_concept_id
WHERE dre.drug_concept_id != 0 @cohort
GROUP BY dre.drug_concept_id, c.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
