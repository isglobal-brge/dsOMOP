---
Group: Observation Period
Name: Long observation period count
ID: observation_period.long_period_count
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons whose observation period length is at
least a given number of days. Useful for assessing the size of the cohort with
sufficient follow-up time.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| min_days | 365 | No | Minimum observation period length in days (default 365) |

## Output

| Field | Description |
|-------|-------------|
| n_persons | Number of distinct persons with a period of at least min_days |

## Query

```sql
SELECT COUNT(DISTINCT person_id) AS n_persons
FROM @cdm.observation_period
WHERE (observation_period_end_date - observation_period_start_date) >= @min_days
```

## Sensitive Fields

n_persons
