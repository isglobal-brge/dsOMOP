---
Group: Drug
Name: Drug days supply statistics
ID: drug.days_supply_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns summary statistics for the days_supply field of drug exposures for a
given drug concept ID. Only records with a non-null days_supply are included.

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
| avg_days_supply | Mean days supply |
| sd_days_supply | Standard deviation of days supply |
| min_days_supply | Minimum days supply |
| max_days_supply | Maximum days supply |

## Query

```sql
SELECT de.drug_concept_id,
       c.concept_name,
       COUNT(DISTINCT de.person_id) AS n_persons,
       COUNT(*) AS n_records,
       AVG(CAST(de.days_supply AS FLOAT)) AS avg_days_supply,
       STDDEV(CAST(de.days_supply AS FLOAT)) AS sd_days_supply,
       MIN(de.days_supply) AS min_days_supply,
       MAX(de.days_supply) AS max_days_supply
FROM @cdm.drug_exposure de
JOIN @vocab.concept c ON c.concept_id = de.drug_concept_id
WHERE de.drug_concept_id = @concept_id
  AND de.days_supply IS NOT NULL
GROUP BY de.drug_concept_id, c.concept_name
```

## Sensitive Fields

n_persons, n_records
