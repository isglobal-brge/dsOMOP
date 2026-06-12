---
Group: Person
Name: Birth month distribution
ID: person.birth_month_distribution
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons grouped by month of birth (1-12). Only
the birth month is used; the day of birth is never returned to preserve privacy.

## Output

| Field | Description |
|-------|-------------|
| month_of_birth | Calendar month of birth (1-12) |
| n_persons | Number of distinct persons born in that month |

## Query

```sql
SELECT p.month_of_birth AS month_of_birth,
       COUNT(DISTINCT p.person_id) AS n_persons
FROM @cdm.person p
WHERE p.month_of_birth IS NOT NULL
GROUP BY p.month_of_birth
ORDER BY month_of_birth
```

## Sensitive Fields

n_persons
