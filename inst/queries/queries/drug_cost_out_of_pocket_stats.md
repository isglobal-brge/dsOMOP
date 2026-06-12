---
Group: Drug Cost
Name: Out-of-pocket cost statistics
ID: drug_cost.out_of_pocket_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns mean and standard deviation of the amount paid by the patient
(paid_by_patient) for cost records linked to a given drug concept. No minimum or
maximum values are disclosed.

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
| n_records | Number of cost records contributing |
| avg_out_of_pocket | Mean of paid_by_patient |
| sd_out_of_pocket | Standard deviation of paid_by_patient |

## Query

```sql
SELECT de.drug_concept_id,
       c.concept_name,
       COUNT(DISTINCT de.person_id) AS n_persons,
       COUNT(*) AS n_records,
       AVG(co.paid_by_patient) AS avg_out_of_pocket,
       STDDEV(co.paid_by_patient) AS sd_out_of_pocket
FROM @cdm.cost co
JOIN @cdm.drug_exposure de ON de.drug_exposure_id = co.cost_event_id
JOIN @vocab.concept c ON c.concept_id = de.drug_concept_id
WHERE de.drug_concept_id = @concept_id
  AND co.paid_by_patient IS NOT NULL
GROUP BY de.drug_concept_id, c.concept_name
```

## Sensitive Fields

n_persons, n_records
