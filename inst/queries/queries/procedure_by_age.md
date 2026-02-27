---
Group: Procedure
Name: Procedure prevalence by age group
ID: procedure.prevalence_by_age
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns procedure prevalence cross-tabulated by age decade for a given
procedure concept ID. Age is computed at the time of the procedure.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 4141909 | Yes | Procedure concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| procedure_concept_id | Procedure concept ID |
| procedure_name | Procedure concept name |
| age_group | Age decade bucket (e.g. 0-9, 10-19) |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT po.procedure_concept_id,
       c.concept_name AS procedure_name,
       CASE
         WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth <  10 THEN '0-9'
         WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 20 THEN '10-19'
         WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 30 THEN '20-29'
         WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 40 THEN '30-39'
         WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 50 THEN '40-49'
         WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 60 THEN '50-59'
         WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 70 THEN '60-69'
         WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 80 THEN '70-79'
         ELSE '80+'
       END AS age_group,
       COUNT(DISTINCT po.person_id) AS n_persons
FROM @cdm.procedure_occurrence po
JOIN @cdm.person p ON p.person_id = po.person_id
JOIN @vocab.concept c ON c.concept_id = po.procedure_concept_id
WHERE po.procedure_concept_id = @concept_id
GROUP BY po.procedure_concept_id, c.concept_name,
         CASE
           WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth <  10 THEN '0-9'
           WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 20 THEN '10-19'
           WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 30 THEN '20-29'
           WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 40 THEN '30-39'
           WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 50 THEN '40-49'
           WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 60 THEN '50-59'
           WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 70 THEN '60-69'
           WHEN EXTRACT(YEAR FROM po.procedure_date) - p.year_of_birth < 80 THEN '70-79'
           ELSE '80+'
         END
ORDER BY age_group
```

## Sensitive Fields

n_persons
