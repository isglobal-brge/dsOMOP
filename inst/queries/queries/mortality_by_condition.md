---
Group: Condition
Name: Mortality rate for condition
ID: condition.mortality_rate
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns mortality statistics for a given condition. Counts the total number of
persons with the condition and the number of those who also appear in the death
table, allowing computation of a mortality rate.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 201820 | Yes | Condition concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| condition_concept_id | Condition concept ID |
| condition_name | Condition concept name |
| n_total | Total number of distinct persons with the condition |
| n_deaths | Number of distinct persons with the condition who died |

## Query

```sql
SELECT co.condition_concept_id,
       c.concept_name AS condition_name,
       COUNT(DISTINCT co.person_id) AS n_total,
       COUNT(DISTINCT d.person_id) AS n_deaths
FROM @cdm.condition_occurrence co
LEFT JOIN @cdm.death d ON d.person_id = co.person_id
JOIN @vocab.concept c ON c.concept_id = co.condition_concept_id
WHERE co.condition_concept_id = @concept_id
GROUP BY co.condition_concept_id, c.concept_name
```

## Sensitive Fields

n_total, n_deaths
