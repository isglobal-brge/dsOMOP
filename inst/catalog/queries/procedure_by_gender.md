---
Group: Procedure
Name: Procedure prevalence by gender
ID: procedure.prevalence_by_gender
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns procedure prevalence cross-tabulated by gender for a given procedure
concept ID. Shows the count of distinct persons by gender.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 4141909 | Yes | Procedure concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| procedure_concept_id | Procedure concept ID |
| procedure_name | Procedure concept name |
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT po.procedure_concept_id,
       pc.concept_name AS procedure_name,
       p.gender_concept_id,
       gc.concept_name AS gender_name,
       COUNT(DISTINCT po.person_id) AS n_persons
FROM @cdm.procedure_occurrence po
JOIN @cdm.person p ON p.person_id = po.person_id
JOIN @vocab.concept pc ON pc.concept_id = po.procedure_concept_id
JOIN @vocab.concept gc ON gc.concept_id = p.gender_concept_id
WHERE po.procedure_concept_id = @concept_id
GROUP BY po.procedure_concept_id, pc.concept_name,
         p.gender_concept_id, gc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
