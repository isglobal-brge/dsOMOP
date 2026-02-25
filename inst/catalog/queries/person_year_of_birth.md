---
Group: Person
Name: Year of birth distribution
ID: person.year_of_birth_distribution
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the distribution of persons by year of birth. Provides a view of
the birth year composition of the study population.

## Output

| Field | Description |
|-------|-------------|
| year_of_birth | Year of birth |
| n_persons | Number of persons |

## Query

```sql
SELECT p.year_of_birth,
       COUNT(DISTINCT p.person_id) AS n_persons
FROM @cdm.person p
GROUP BY p.year_of_birth
ORDER BY p.year_of_birth
```

## Sensitive Fields

n_persons
