---
Group: Condition
Name: Condition duration statistics
ID: condition.duration_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns summary statistics on condition duration in days for a specific
condition concept. Duration is computed as condition_end_date minus
condition_start_date. Only records with both dates non-null are included.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | Yes | Condition concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| condition_concept_id | Condition concept ID |
| concept_name | Standard concept name |
| n_persons | Number of distinct persons |
| n_records | Number of records with non-null duration |
| avg_days | Mean duration in days |
| sd_days | Standard deviation of duration in days |
| min_days | Minimum duration in days |
| max_days | Maximum duration in days |

## Query

```sql
SELECT co.condition_concept_id,
       c.concept_name,
       COUNT(DISTINCT co.person_id) AS n_persons,
       COUNT(*) AS n_records,
       AVG(CAST(co.condition_end_date - co.condition_start_date AS INTEGER)) AS avg_days,
       STDDEV(CAST(co.condition_end_date - co.condition_start_date AS INTEGER)) AS sd_days,
       MIN(CAST(co.condition_end_date - co.condition_start_date AS INTEGER)) AS min_days,
       MAX(CAST(co.condition_end_date - co.condition_start_date AS INTEGER)) AS max_days
FROM @cdm.condition_occurrence co
JOIN @vocab.concept c ON c.concept_id = co.condition_concept_id
WHERE co.condition_concept_id = @concept_id
  AND co.condition_start_date IS NOT NULL
  AND co.condition_end_date IS NOT NULL
GROUP BY co.condition_concept_id, c.concept_name
```

## Sensitive Fields

n_persons, n_records
