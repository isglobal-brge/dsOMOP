---
Group: Person
Name: Ethnicity distribution
ID: person.ethnicity_distribution
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the distribution of persons by ethnicity concept, with concept names.
Provides a breakdown of the study population by ethnicity category.

## Output

| Field | Description |
|-------|-------------|
| ethnicity_concept_id | Ethnicity concept ID |
| ethnicity_name | Ethnicity concept name |
| n_persons | Number of persons |

## Query

```sql
SELECT p.ethnicity_concept_id,
       c.concept_name AS ethnicity_name,
       COUNT(DISTINCT p.person_id) AS n_persons
FROM @cdm.person p
JOIN @vocab.concept c ON c.concept_id = p.ethnicity_concept_id
GROUP BY p.ethnicity_concept_id, c.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
