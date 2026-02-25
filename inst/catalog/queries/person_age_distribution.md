---
Group: Person
Name: Age distribution by decade
ID: person.age_distribution_decade
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns persons grouped into 10-year age bins based on year of birth relative
to a reference year. Provides age distribution without revealing exact birth
years.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| reference_year | 2024 | No | Year for age calculation (default current year) |

## Output

| Field | Description |
|-------|-------------|
| age_group | Age group label (e.g., "40-49") |
| n_persons | Number of persons in this age group |

## Query

```sql
SELECT CASE
         WHEN (@reference_year - p.year_of_birth) < 10 THEN '0-9'
         WHEN (@reference_year - p.year_of_birth) < 20 THEN '10-19'
         WHEN (@reference_year - p.year_of_birth) < 30 THEN '20-29'
         WHEN (@reference_year - p.year_of_birth) < 40 THEN '30-39'
         WHEN (@reference_year - p.year_of_birth) < 50 THEN '40-49'
         WHEN (@reference_year - p.year_of_birth) < 60 THEN '50-59'
         WHEN (@reference_year - p.year_of_birth) < 70 THEN '60-69'
         WHEN (@reference_year - p.year_of_birth) < 80 THEN '70-79'
         WHEN (@reference_year - p.year_of_birth) < 90 THEN '80-89'
         ELSE '90+'
       END AS age_group,
       COUNT(DISTINCT p.person_id) AS n_persons
FROM @cdm.person p
GROUP BY CASE
         WHEN (@reference_year - p.year_of_birth) < 10 THEN '0-9'
         WHEN (@reference_year - p.year_of_birth) < 20 THEN '10-19'
         WHEN (@reference_year - p.year_of_birth) < 30 THEN '20-29'
         WHEN (@reference_year - p.year_of_birth) < 40 THEN '30-39'
         WHEN (@reference_year - p.year_of_birth) < 50 THEN '40-49'
         WHEN (@reference_year - p.year_of_birth) < 60 THEN '50-59'
         WHEN (@reference_year - p.year_of_birth) < 70 THEN '60-69'
         WHEN (@reference_year - p.year_of_birth) < 80 THEN '70-79'
         WHEN (@reference_year - p.year_of_birth) < 90 THEN '80-89'
         ELSE '90+'
       END
ORDER BY age_group
```

## Sensitive Fields

n_persons
