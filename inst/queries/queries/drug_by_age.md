---
Group: Drug
Name: Drug exposure prevalence by age group
ID: drug.prevalence_by_age
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns drug exposure prevalence cross-tabulated by age decade for a given
drug concept ID. Age is computed at the time of drug exposure start.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 1127433 | Yes | Drug concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| drug_concept_id | Drug concept ID |
| drug_name | Drug concept name |
| age_group | Age decade bucket (e.g. 0-9, 10-19) |
| n_persons | Number of distinct persons |

## Query

```sql
SELECT de.drug_concept_id,
       c.concept_name AS drug_name,
       CASE
         WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth <  10 THEN '0-9'
         WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 20 THEN '10-19'
         WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 30 THEN '20-29'
         WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 40 THEN '30-39'
         WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 50 THEN '40-49'
         WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 60 THEN '50-59'
         WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 70 THEN '60-69'
         WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 80 THEN '70-79'
         WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 90 THEN '80-89'
         ELSE '90+'
       END AS age_group,
       COUNT(DISTINCT de.person_id) AS n_persons
FROM @cdm.drug_exposure de
JOIN @cdm.person p ON p.person_id = de.person_id
JOIN @vocab.concept c ON c.concept_id = de.drug_concept_id
WHERE de.drug_concept_id = @concept_id
GROUP BY de.drug_concept_id, c.concept_name,
         CASE
           WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth <  10 THEN '0-9'
           WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 20 THEN '10-19'
           WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 30 THEN '20-29'
           WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 40 THEN '30-39'
           WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 50 THEN '40-49'
           WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 60 THEN '50-59'
           WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 70 THEN '60-69'
           WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 80 THEN '70-79'
           WHEN EXTRACT(YEAR FROM de.drug_exposure_start_date) - p.year_of_birth < 90 THEN '80-89'
           ELSE '90+'
         END
ORDER BY age_group
```

## Sensitive Fields

n_persons
