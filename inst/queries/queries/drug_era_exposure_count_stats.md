---
Group: Drug Era
Name: Drug era exposure count statistics
ID: drug_era.exposure_count_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns summary statistics for the drug_exposure_count field of drug eras for a
given drug concept ID. Only records with a non-null drug_exposure_count are
included.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 1127433 | Yes | Drug (ingredient) concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| drug_concept_id | Drug (ingredient) concept ID |
| concept_name | Drug concept name |
| n_persons | Number of distinct persons |
| n_records | Total number of drug era records |
| avg_exposure_count | Mean number of exposures per era |
| sd_exposure_count | Standard deviation of exposure count |

## Query

```sql
SELECT dre.drug_concept_id,
       c.concept_name,
       COUNT(DISTINCT dre.person_id) AS n_persons,
       COUNT(*) AS n_records,
       AVG(CAST(dre.drug_exposure_count AS FLOAT)) AS avg_exposure_count,
       STDDEV(CAST(dre.drug_exposure_count AS FLOAT)) AS sd_exposure_count
FROM @cdm.drug_era dre
JOIN @vocab.concept c ON c.concept_id = dre.drug_concept_id
WHERE dre.drug_concept_id = @concept_id
  AND dre.drug_exposure_count IS NOT NULL
GROUP BY dre.drug_concept_id, c.concept_name
```

## Sensitive Fields

n_persons, n_records
