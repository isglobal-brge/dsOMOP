---
Group: Drug Era
Name: Drug era length statistics
ID: drug_era.length_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns summary statistics for drug era length in days by drug concept. Length is
computed as the difference between drug_era_end_date and drug_era_start_date.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| top_n | 50 | No | Number of top drug eras (default 50) |

## Output

| Field | Description |
|-------|-------------|
| drug_concept_id | Drug (ingredient) concept ID |
| concept_name | Drug concept name |
| n_persons | Number of distinct persons |
| n_records | Total number of drug era records |
| avg_days | Mean drug era length in days |
| sd_days | Standard deviation of drug era length |

## Query

```sql
SELECT dre.drug_concept_id,
       c.concept_name,
       COUNT(DISTINCT dre.person_id) AS n_persons,
       COUNT(*) AS n_records,
       AVG(EXTRACT(EPOCH FROM (dre.drug_era_end_date - dre.drug_era_start_date)) / 86400.0) AS avg_days,
       STDDEV(EXTRACT(EPOCH FROM (dre.drug_era_end_date - dre.drug_era_start_date)) / 86400.0) AS sd_days
FROM @cdm.drug_era dre
JOIN @vocab.concept c ON c.concept_id = dre.drug_concept_id
WHERE dre.drug_concept_id != 0
  AND dre.drug_era_end_date IS NOT NULL
GROUP BY dre.drug_concept_id, c.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
