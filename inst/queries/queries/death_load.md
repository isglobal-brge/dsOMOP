---
Group: Death
Name: Load death table
ID: death.load
CDM Version: 5.3+
Mode: assign
Author: dsOMOP
---

## Description

Materializes a server-side data frame of the death table with the cause and
death type concept names joined from the vocabulary. Source values are
excluded. Data stays server-side and is never returned to the client.

## Output

| Field | Description |
|-------|-------------|
| person_id | Person identifier |
| death_date | Death date |
| death_datetime | Death datetime |
| death_type_concept_id | Death type concept ID |
| death_type_name | Death type concept name |
| cause_concept_id | Cause concept ID |
| cause_name | Cause concept name |

## Query

```sql
SELECT d.person_id,
       d.death_date,
       d.death_datetime,
       d.death_type_concept_id,
       dt.concept_name AS death_type_name,
       d.cause_concept_id,
       cc.concept_name AS cause_name
FROM @cdm.death d
LEFT JOIN @vocab.concept dt ON dt.concept_id = d.death_type_concept_id
LEFT JOIN @vocab.concept cc ON cc.concept_id = d.cause_concept_id
```

## Sensitive Fields

