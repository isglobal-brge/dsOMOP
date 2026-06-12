---
Group: Observation Period
Name: Persons by number of observation periods
ID: observation_period.persons_with_n_periods
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons grouped by how many observation periods
each person has. Useful for understanding the distribution of fragmented versus
continuous observation in the database.

## Output

| Field | Description |
|-------|-------------|
| n_periods | Number of observation periods a person has |
| n_persons | Number of distinct persons with that many periods |

## Query

```sql
SELECT per.n_periods AS n_periods,
       COUNT(*) AS n_persons
FROM (
  SELECT op.person_id,
         COUNT(*) AS n_periods
  FROM @cdm.observation_period op
  GROUP BY op.person_id
) per
GROUP BY per.n_periods
ORDER BY n_periods
```

## Sensitive Fields

n_persons
