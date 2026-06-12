---
Group: Observation Period
Name: Observation period length in months statistics
ID: observation_period.length_months_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons together with the average and standard
deviation of observation period length expressed in months, grouped by calendar
year of the observation start. No minimum or maximum values are returned.

## Output

| Field | Description |
|-------|-------------|
| start_year | Calendar year of observation period start |
| n_persons | Number of distinct persons starting observation in this year |
| avg_months | Average observation period length in months |
| stddev_months | Standard deviation of observation period length in months |

## Query

```sql
SELECT EXTRACT(YEAR FROM op.observation_period_start_date) AS start_year,
       COUNT(DISTINCT op.person_id) AS n_persons,
       AVG((EXTRACT(YEAR FROM op.observation_period_end_date) * 12 + EXTRACT(MONTH FROM op.observation_period_end_date))
           - (EXTRACT(YEAR FROM op.observation_period_start_date) * 12 + EXTRACT(MONTH FROM op.observation_period_start_date))) AS avg_months,
       STDDEV((EXTRACT(YEAR FROM op.observation_period_end_date) * 12 + EXTRACT(MONTH FROM op.observation_period_end_date))
           - (EXTRACT(YEAR FROM op.observation_period_start_date) * 12 + EXTRACT(MONTH FROM op.observation_period_start_date))) AS stddev_months
FROM @cdm.observation_period op
GROUP BY EXTRACT(YEAR FROM op.observation_period_start_date)
ORDER BY start_year
```

## Sensitive Fields

n_persons
