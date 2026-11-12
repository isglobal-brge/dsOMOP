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

Aggregate release (dsOMOP 2.10.3): `min_days` snaps to the nearest point of the
public duration grid (0, 30, 90, 180, 365, 730, 1095, 1825, 3650, 7300 and
10957 days; ties to the smaller, out-of-range values saturate). In auto mode
the answer is the tail sum of the one protected histogram of each person's
longest observation period on that grid. The protection label reports the
requested and effective threshold.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| min_days | 365 | No | Minimum observation period length in days (default 365); snapped to the public duration grid in aggregate releases |

## Output

| Field | Description |
|-------|-------------|
| n_persons | Number of distinct persons with a period of at least min_days |

## Query

```sql
SELECT COUNT(DISTINCT person_id) AS n_persons
FROM @cdm.observation_period
WHERE DATEDIFF(day, observation_period_start_date, observation_period_end_date) >= @min_days
```

## Sensitive Fields

n_persons
