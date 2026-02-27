---
Group: Person
Name: Demographic summary
ID: person.demographic_summary
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns demographic summary statistics: counts of persons by gender concept,
with concept names. Provides a high-level view of the study population.

## Output

| Field | Description |
|-------|-------------|
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| n_persons | Number of persons |

## Query

```sql
SELECT p.gender_concept_id,
       c.concept_name AS gender_name,
       COUNT(DISTINCT p.person_id) AS n_persons
FROM @cdm.person p
JOIN @vocab.concept c ON c.concept_id = p.gender_concept_id
GROUP BY p.gender_concept_id, c.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons
