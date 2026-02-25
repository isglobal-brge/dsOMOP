---
Group: Drug
Name: Drug quantity statistics
ID: drug.quantity_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns summary statistics for the quantity field of drug exposures for a given
drug concept ID. Only records with a non-null quantity are included.

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
| avg_quantity | Mean quantity |
| sd_quantity | Standard deviation of quantity |
| min_quantity | Minimum quantity |
| max_quantity | Maximum quantity |

## Query

```sql
SELECT de.drug_concept_id,
       c.concept_name,
       COUNT(DISTINCT de.person_id) AS n_persons,
       COUNT(*) AS n_records,
       AVG(de.quantity) AS avg_quantity,
       STDDEV(de.quantity) AS sd_quantity,
       MIN(de.quantity) AS min_quantity,
       MAX(de.quantity) AS max_quantity
FROM @cdm.drug_exposure de
JOIN @vocab.concept c ON c.concept_id = de.drug_concept_id
WHERE de.drug_concept_id = @concept_id
  AND de.quantity IS NOT NULL
GROUP BY de.drug_concept_id, c.concept_name
```

## Sensitive Fields

n_persons, n_records
