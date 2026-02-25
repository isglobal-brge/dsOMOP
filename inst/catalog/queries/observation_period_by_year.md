---
Group: Observation Period
Name: Observation period coverage by year
ID: observation_period.coverage_by_year
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons and observation periods by calendar year
of the observation period start date. Useful for understanding temporal data
coverage across the study population.

## Output

| Field | Description |
|-------|-------------|
| start_year | Calendar year of observation period start date |
| n_persons | Number of distinct persons |
| n_periods | Total number of observation periods |

## Query

```sql
SELECT EXTRACT(YEAR FROM op.observation_period_start_date) AS start_year,
       COUNT(DISTINCT op.person_id) AS n_persons,
       COUNT(*) AS n_periods
FROM @cdm.observation_period op
GROUP BY EXTRACT(YEAR FROM op.observation_period_start_date)
ORDER BY start_year
```

## Sensitive Fields

n_persons, n_periods
