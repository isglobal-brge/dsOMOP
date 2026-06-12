---
Group: Person
Name: Load person table
ID: person.load
CDM Version: 5.3+
Mode: assign
Author: dsOMOP
---

## Description

Materializes a server-side data frame of the person table with gender, race,
and ethnicity concept names joined from the vocabulary. Data stays server-side
and is never returned to the client.

## Output

| Field | Description |
|-------|-------------|
| person_id | Person identifier |
| gender_concept_id | Gender concept ID |
| gender_name | Gender concept name |
| year_of_birth | Year of birth |
| month_of_birth | Month of birth |
| day_of_birth | Day of birth |
| race_concept_id | Race concept ID |
| race_name | Race concept name |
| ethnicity_concept_id | Ethnicity concept ID |
| ethnicity_name | Ethnicity concept name |
| location_id | Location identifier |
| provider_id | Provider identifier |
| care_site_id | Care site identifier |

## Query

```sql
SELECT p.person_id,
       p.gender_concept_id,
       g.concept_name AS gender_name,
       p.year_of_birth,
       p.month_of_birth,
       p.day_of_birth,
       p.race_concept_id,
       r.concept_name AS race_name,
       p.ethnicity_concept_id,
       e.concept_name AS ethnicity_name,
       p.location_id,
       p.provider_id,
       p.care_site_id
FROM @cdm.person p
LEFT JOIN @vocab.concept g ON g.concept_id = p.gender_concept_id
LEFT JOIN @vocab.concept r ON r.concept_id = p.race_concept_id
LEFT JOIN @vocab.concept e ON e.concept_id = p.ethnicity_concept_id
```

## Sensitive Fields

