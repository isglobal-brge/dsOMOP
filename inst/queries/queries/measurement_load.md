---
Group: Measurement
Name: Load measurement table
ID: measurement.load
CDM Version: 5.3+
Mode: assign
Author: dsOMOP
---

## Description

Materializes a server-side data frame of the measurement table with the
measurement, unit, and type concept names joined from the vocabulary. Source
values are excluded. Data stays server-side and is never returned to the client.

## Output

| Field | Description |
|-------|-------------|
| measurement_id | Measurement identifier |
| person_id | Person identifier |
| measurement_concept_id | Measurement concept ID |
| measurement_name | Measurement concept name |
| measurement_date | Measurement date |
| measurement_datetime | Measurement datetime |
| measurement_time | Measurement time |
| measurement_type_concept_id | Measurement type concept ID |
| measurement_type_name | Measurement type concept name |
| operator_concept_id | Operator concept ID |
| value_as_number | Value as number |
| value_as_concept_id | Value as concept ID |
| unit_concept_id | Unit concept ID |
| unit_name | Unit concept name |
| range_low | Range low |
| range_high | Range high |
| visit_occurrence_id | Visit occurrence identifier |
| visit_detail_id | Visit detail identifier |
| provider_id | Provider identifier |

## Query

```sql
SELECT m.measurement_id,
       m.person_id,
       m.measurement_concept_id,
       mc.concept_name AS measurement_name,
       m.measurement_date,
       m.measurement_datetime,
       m.measurement_time,
       m.measurement_type_concept_id,
       mt.concept_name AS measurement_type_name,
       m.operator_concept_id,
       m.value_as_number,
       m.value_as_concept_id,
       m.unit_concept_id,
       u.concept_name AS unit_name,
       m.range_low,
       m.range_high,
       m.visit_occurrence_id,
       m.visit_detail_id,
       m.provider_id
FROM @cdm.measurement m
LEFT JOIN @vocab.concept mc ON mc.concept_id = m.measurement_concept_id
LEFT JOIN @vocab.concept mt ON mt.concept_id = m.measurement_type_concept_id
LEFT JOIN @vocab.concept u ON u.concept_id = m.unit_concept_id
```

## Sensitive Fields

