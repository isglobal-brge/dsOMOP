---
Group: Observation Period
Name: Observation period length statistics
ID: observation_period.length_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns summary statistics for observation period lengths in days, grouped
by calendar year of the observation start. Useful for understanding data
coverage and follow-up time.

## Output

| Field | Description |
|-------|-------------|
| start_year | Calendar year of observation period start |
| n_persons | Number of persons starting observation in this year |
| avg_days | Average observation period length in days |

## Query

```sql
SELECT EXTRACT(YEAR FROM op.observation_period_start_date) AS start_year,
       COUNT(DISTINCT op.person_id) AS n_persons,
       AVG(op.observation_period_end_date - op.observation_period_start_date) AS avg_days
FROM @cdm.observation_period op
GROUP BY EXTRACT(YEAR FROM op.observation_period_start_date)
ORDER BY start_year
```

## Sensitive Fields

n_persons
