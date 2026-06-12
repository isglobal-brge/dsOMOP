---
Group: Drug Cost
Name: Cost per unit statistics
ID: drug_cost.cost_per_unit_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns mean and standard deviation of the total paid amount per dispensed unit
(total_paid / quantity) for a given drug concept, restricted to records with a
positive quantity. No minimum or maximum values are disclosed.

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
| avg_cost_per_unit | Mean of total_paid / quantity |
| sd_cost_per_unit | Standard deviation of total_paid / quantity |

## Query

```sql
SELECT de.drug_concept_id,
       c.concept_name,
       COUNT(DISTINCT de.person_id) AS n_persons,
       COUNT(*) AS n_records,
       AVG(co.total_paid / de.quantity) AS avg_cost_per_unit,
       STDDEV(co.total_paid / de.quantity) AS sd_cost_per_unit
FROM @cdm.cost co
JOIN @cdm.drug_exposure de ON de.drug_exposure_id = co.cost_event_id
JOIN @vocab.concept c ON c.concept_id = de.drug_concept_id
WHERE de.drug_concept_id = @concept_id
  AND de.quantity > 0
  AND co.total_paid IS NOT NULL
GROUP BY de.drug_concept_id, c.concept_name
```

## Sensitive Fields

n_persons, n_records
