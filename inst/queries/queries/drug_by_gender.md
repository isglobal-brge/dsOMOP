---
Group: Drug
Name: Drug exposure prevalence by gender
ID: drug.prevalence_by_gender
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns drug exposure prevalence cross-tabulated by gender for a given drug concept ID.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 1127433 | Yes | Drug concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| drug_concept_id | Drug concept ID |
| drug_name | Drug concept name |
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT de.drug_concept_id,
       dc.concept_name AS drug_name,
       p.gender_concept_id,
       gc.concept_name AS gender_name,
       COUNT(DISTINCT de.person_id) AS n_persons
FROM @cdm.drug_exposure de
JOIN @cdm.person p ON p.person_id = de.person_id
JOIN @vocab.concept dc ON dc.concept_id = de.drug_concept_id
JOIN @vocab.concept gc ON gc.concept_id = p.gender_concept_id
WHERE de.drug_concept_id = @concept_id
GROUP BY de.drug_concept_id, dc.concept_name,
         p.gender_concept_id, gc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
