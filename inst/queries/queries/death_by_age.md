---
Group: Death
Name: Death by age group
ID: death.by_age
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns death counts by age decade at time of death. Age is computed from the
death date and the person's year of birth.

## Output

| Field | Description |
|-------|-------------|
| age_group | Age decade bucket (e.g. 0-9, 10-19) |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT CASE
         WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth <  10 THEN '0-9'
         WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 20 THEN '10-19'
         WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 30 THEN '20-29'
         WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 40 THEN '30-39'
         WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 50 THEN '40-49'
         WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 60 THEN '50-59'
         WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 70 THEN '60-69'
         WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 80 THEN '70-79'
         WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 90 THEN '80-89'
         ELSE '90+'
       END AS age_group,
       COUNT(DISTINCT d.person_id) AS n_persons
FROM @cdm.death d
JOIN @cdm.person p ON p.person_id = d.person_id
GROUP BY CASE
           WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth <  10 THEN '0-9'
           WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 20 THEN '10-19'
           WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 30 THEN '20-29'
           WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 40 THEN '30-39'
           WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 50 THEN '40-49'
           WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 60 THEN '50-59'
           WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 70 THEN '60-69'
           WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 80 THEN '70-79'
           WHEN EXTRACT(YEAR FROM d.death_date) - p.year_of_birth < 90 THEN '80-89'
           ELSE '90+'
         END
ORDER BY age_group
```

## Sensitive Fields

n_persons
