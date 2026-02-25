---
Group: Visit
Name: Visit type by age group
ID: visit.type_by_age
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns visit counts cross-tabulated by visit type and age decade. Age is
computed at the time of the visit start date. Optionally filters to a specific
visit concept ID.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| visit_concept_id | 9201 | No | Visit concept ID to filter (0 = all visits) |

## Output

| Field | Description |
|-------|-------------|
| visit_concept_id | Visit concept ID |
| visit_name | Visit type name |
| age_group | Age decade bucket (e.g. 0-9, 10-19) |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT vo.visit_concept_id,
       vc.concept_name AS visit_name,
       CASE
         WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth <  10 THEN '0-9'
         WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 20 THEN '10-19'
         WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 30 THEN '20-29'
         WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 40 THEN '30-39'
         WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 50 THEN '40-49'
         WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 60 THEN '50-59'
         WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 70 THEN '60-69'
         WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 80 THEN '70-79'
         WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 90 THEN '80-89'
         ELSE '90+'
       END AS age_group,
       COUNT(DISTINCT vo.person_id) AS n_persons
FROM @cdm.visit_occurrence vo
JOIN @cdm.person p ON p.person_id = vo.person_id
JOIN @vocab.concept vc ON vc.concept_id = vo.visit_concept_id
WHERE vo.visit_concept_id != 0
  AND (@visit_concept_id = 0 OR vo.visit_concept_id = @visit_concept_id)
GROUP BY vo.visit_concept_id, vc.concept_name,
         CASE
           WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth <  10 THEN '0-9'
           WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 20 THEN '10-19'
           WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 30 THEN '20-29'
           WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 40 THEN '30-39'
           WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 50 THEN '40-49'
           WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 60 THEN '50-59'
           WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 70 THEN '60-69'
           WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 80 THEN '70-79'
           WHEN EXTRACT(YEAR FROM vo.visit_start_date) - p.year_of_birth < 90 THEN '80-89'
           ELSE '90+'
         END
ORDER BY visit_name, age_group
```

## Sensitive Fields

n_persons
