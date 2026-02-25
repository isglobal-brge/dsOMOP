---
Group: Drug
Name: Drug exposure prevalence by year
ID: drug.prevalence_by_year
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns drug exposure prevalence by calendar year of drug exposure start date.
Optionally filters to a specific drug concept ID and limits to the top N drugs.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 0 | No | Drug concept ID to filter (0 = all drugs) |
| top_n | 50 | No | Number of top results to return (default 50) |

## Output

| Field | Description |
|-------|-------------|
| drug_concept_id | Drug concept ID |
| concept_name | Drug concept name |
| exposure_year | Calendar year of drug exposure start |
| n_persons | Number of distinct persons |
| n_records | Total number of drug exposure records |

## Query

```sql
SELECT de.drug_concept_id,
       c.concept_name,
       EXTRACT(YEAR FROM de.drug_exposure_start_date) AS exposure_year,
       COUNT(DISTINCT de.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.drug_exposure de
JOIN @vocab.concept c ON c.concept_id = de.drug_concept_id
WHERE de.drug_concept_id != 0
  AND (@concept_id = 0 OR de.drug_concept_id = @concept_id)
GROUP BY de.drug_concept_id, c.concept_name,
         EXTRACT(YEAR FROM de.drug_exposure_start_date)
ORDER BY exposure_year DESC, n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
