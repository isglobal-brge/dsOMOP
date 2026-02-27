---
Group: Person
Name: Gender by age decade
ID: person.gender_by_age_decade
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns a cross-tabulation of gender and age decade. Age is computed as the
difference between the current year and year of birth, then grouped into
decade bins. Useful for understanding demographic structure.

## Output

| Field | Description |
|-------|-------------|
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| age_group | Age decade group label |
| n_persons | Number of persons |

## Query

```sql
SELECT p.gender_concept_id,
       c.concept_name AS gender_name,
       CASE
         WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 10 THEN '0-9'
         WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 20 THEN '10-19'
         WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 30 THEN '20-29'
         WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 40 THEN '30-39'
         WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 50 THEN '40-49'
         WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 60 THEN '50-59'
         WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 70 THEN '60-69'
         WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 80 THEN '70-79'
         WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 90 THEN '80-89'
         ELSE '90+'
       END AS age_group,
       COUNT(DISTINCT p.person_id) AS n_persons
FROM @cdm.person p
JOIN @vocab.concept c ON c.concept_id = p.gender_concept_id
GROUP BY p.gender_concept_id, c.concept_name,
         CASE
           WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 10 THEN '0-9'
           WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 20 THEN '10-19'
           WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 30 THEN '20-29'
           WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 40 THEN '30-39'
           WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 50 THEN '40-49'
           WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 60 THEN '50-59'
           WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 70 THEN '60-69'
           WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 80 THEN '70-79'
           WHEN EXTRACT(YEAR FROM CURRENT_DATE) - p.year_of_birth < 90 THEN '80-89'
           ELSE '90+'
         END
ORDER BY gender_name, age_group
```

## Sensitive Fields

n_persons
