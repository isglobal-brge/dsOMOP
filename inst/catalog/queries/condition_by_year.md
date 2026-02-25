---
Group: Condition
Name: Condition prevalence by year
ID: condition.prevalence_by_year
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns condition prevalence by calendar year of the condition start date.
Optionally filters to a specific condition concept ID; when concept_id is 0
all conditions are included.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | No | Condition concept ID (0 = all conditions) |
| top_n     | 50      | No | Maximum rows returned (default 50) |

## Output

| Field | Description |
|-------|-------------|
| condition_concept_id | Condition concept ID |
| concept_name | Standard concept name |
| condition_year | Calendar year of condition start date |
| n_persons | Number of distinct persons |
| n_records | Total number of condition records |

## Query

```sql
SELECT co.condition_concept_id,
       c.concept_name,
       EXTRACT(YEAR FROM co.condition_start_date) AS condition_year,
       COUNT(DISTINCT co.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.condition_occurrence co
JOIN @vocab.concept c ON c.concept_id = co.condition_concept_id
WHERE co.condition_concept_id != 0
  AND (@concept_id = 0 OR co.condition_concept_id = @concept_id)
GROUP BY co.condition_concept_id, c.concept_name,
         EXTRACT(YEAR FROM co.condition_start_date)
ORDER BY condition_year DESC, n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
