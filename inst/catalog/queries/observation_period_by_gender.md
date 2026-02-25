---
Group: Observation Period
Name: Observation period length by gender
ID: observation_period.length_by_gender
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns summary statistics for observation period length in days, grouped by
gender. Useful for assessing whether follow-up time differs across gender
categories.

## Output

| Field | Description |
|-------|-------------|
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| n_persons | Number of distinct persons |
| avg_days | Mean observation period length in days |
| min_days | Minimum observation period length in days |
| max_days | Maximum observation period length in days |

## Query

```sql
SELECT p.gender_concept_id,
       c.concept_name AS gender_name,
       COUNT(DISTINCT op.person_id) AS n_persons,
       AVG(op.observation_period_end_date - op.observation_period_start_date) AS avg_days,
       MIN(op.observation_period_end_date - op.observation_period_start_date) AS min_days,
       MAX(op.observation_period_end_date - op.observation_period_start_date) AS max_days
FROM @cdm.observation_period op
JOIN @cdm.person p ON p.person_id = op.person_id
JOIN @vocab.concept c ON c.concept_id = p.gender_concept_id
GROUP BY p.gender_concept_id, c.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
