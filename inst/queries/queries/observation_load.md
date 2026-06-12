---
Group: Observation
Name: Load observation table
ID: observation.load
CDM Version: 5.3+
Mode: assign
Author: dsOMOP
---

## Description

Materializes a server-side data frame of the observation table with the
observation and unit concept names joined from the vocabulary. Source values
are excluded. Data stays server-side and is never returned to the client.

## Output

| Field | Description |
|-------|-------------|
| observation_id | Observation identifier |
| person_id | Person identifier |
| observation_concept_id | Observation concept ID |
| observation_name | Observation concept name |
| observation_date | Observation date |
| observation_datetime | Observation datetime |
| observation_type_concept_id | Observation type concept ID |
| value_as_number | Value as number |
| value_as_concept_id | Value as concept ID |
| qualifier_concept_id | Qualifier concept ID |
| unit_concept_id | Unit concept ID |
| unit_name | Unit concept name |
| visit_occurrence_id | Visit occurrence identifier |
| visit_detail_id | Visit detail identifier |
| provider_id | Provider identifier |

## Query

```sql
SELECT o.observation_id,
       o.person_id,
       o.observation_concept_id,
       oc.concept_name AS observation_name,
       o.observation_date,
       o.observation_datetime,
       o.observation_type_concept_id,
       o.value_as_number,
       o.value_as_concept_id,
       o.qualifier_concept_id,
       o.unit_concept_id,
       u.concept_name AS unit_name,
       o.visit_occurrence_id,
       o.visit_detail_id,
       o.provider_id
FROM @cdm.observation o
LEFT JOIN @vocab.concept oc ON oc.concept_id = o.observation_concept_id
LEFT JOIN @vocab.concept u ON u.concept_id = o.unit_concept_id
```

## Sensitive Fields

