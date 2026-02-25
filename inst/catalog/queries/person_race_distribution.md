---
Group: Person
Name: Race distribution
ID: person.race_distribution
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the distribution of persons by race concept, with concept names.
Provides a breakdown of the study population by race category.

## Output

| Field | Description |
|-------|-------------|
| race_concept_id | Race concept ID |
| race_name | Race concept name |
| n_persons | Number of persons |

## Query

```sql
SELECT p.race_concept_id,
       c.concept_name AS race_name,
       COUNT(DISTINCT p.person_id) AS n_persons
FROM @cdm.person p
JOIN @vocab.concept c ON c.concept_id = p.race_concept_id
GROUP BY p.race_concept_id, c.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
