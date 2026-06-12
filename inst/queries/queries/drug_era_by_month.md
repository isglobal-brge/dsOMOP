---
Group: Drug Era
Name: Drug era records by month
ID: drug_era.by_month
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of drug era records by calendar month of the drug era start
date for a given drug concept ID.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 1127433 | Yes | Drug (ingredient) concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| drug_concept_id | Drug (ingredient) concept ID |
| concept_name | Drug concept name |
| start_month | Calendar month of drug era start (1-12) |
| n_persons | Number of distinct persons |
| n_records | Total number of drug era records |

## Query

```sql
SELECT dre.drug_concept_id,
       c.concept_name,
       EXTRACT(MONTH FROM dre.drug_era_start_date) AS start_month,
       COUNT(DISTINCT dre.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.drug_era dre
JOIN @vocab.concept c ON c.concept_id = dre.drug_concept_id
WHERE dre.drug_concept_id = @concept_id
GROUP BY dre.drug_concept_id, c.concept_name,
         EXTRACT(MONTH FROM dre.drug_era_start_date)
ORDER BY start_month
```

## Sensitive Fields

n_persons, n_records
