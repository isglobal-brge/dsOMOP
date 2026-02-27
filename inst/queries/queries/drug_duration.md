---
Group: Drug
Name: Drug exposure duration statistics
ID: drug.duration_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns summary statistics for drug exposure duration in days for a given drug
concept ID. Duration is computed as the difference between drug_exposure_end_date
and drug_exposure_start_date.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 1127433 | Yes | Drug concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| drug_concept_id | Drug concept ID |
| concept_name | Drug concept name |
| n_persons | Number of distinct persons |
| n_records | Total number of drug exposure records |
| avg_days | Mean exposure duration in days |
| sd_days | Standard deviation of exposure duration |
| min_days | Minimum exposure duration in days |
| max_days | Maximum exposure duration in days |

## Query

```sql
SELECT de.drug_concept_id,
       c.concept_name,
       COUNT(DISTINCT de.person_id) AS n_persons,
       COUNT(*) AS n_records,
       AVG(CAST(de.drug_exposure_end_date - de.drug_exposure_start_date AS FLOAT)) AS avg_days,
       STDDEV(CAST(de.drug_exposure_end_date - de.drug_exposure_start_date AS FLOAT)) AS sd_days,
       MIN(CAST(de.drug_exposure_end_date - de.drug_exposure_start_date AS FLOAT)) AS min_days,
       MAX(CAST(de.drug_exposure_end_date - de.drug_exposure_start_date AS FLOAT)) AS max_days
FROM @cdm.drug_exposure de
JOIN @vocab.concept c ON c.concept_id = de.drug_concept_id
WHERE de.drug_concept_id = @concept_id
  AND de.drug_exposure_end_date IS NOT NULL
GROUP BY de.drug_concept_id, c.concept_name
```

## Sensitive Fields

n_persons, n_records
