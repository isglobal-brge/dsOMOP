---
Group: Condition
Name: Load condition_occurrence table
ID: condition_occurrence.load
CDM Version: 5.3+
Mode: assign
Author: dsOMOP
---

## Description

Materializes a server-side data frame of the condition_occurrence table with
the condition concept name joined from the vocabulary. Data stays server-side
and is never returned to the client.

## Output

| Field | Description |
|-------|-------------|
| condition_occurrence_id | Condition occurrence identifier |
| person_id | Person identifier |
| condition_concept_id | Condition concept ID |
| condition_name | Condition concept name |
| condition_start_date | Condition start date |
| condition_start_datetime | Condition start datetime |
| condition_end_date | Condition end date |
| condition_end_datetime | Condition end datetime |
| condition_type_concept_id | Condition type concept ID |
| condition_status_concept_id | Condition status concept ID |
| visit_occurrence_id | Visit occurrence identifier |
| visit_detail_id | Visit detail identifier |
| provider_id | Provider identifier |

## Query

```sql
SELECT co.condition_occurrence_id,
       co.person_id,
       co.condition_concept_id,
       c.concept_name AS condition_name,
       co.condition_start_date,
       co.condition_start_datetime,
       co.condition_end_date,
       co.condition_end_datetime,
       co.condition_type_concept_id,
       co.condition_status_concept_id,
       co.visit_occurrence_id,
       co.visit_detail_id,
       co.provider_id
FROM @cdm.condition_occurrence co
LEFT JOIN @vocab.concept c ON c.concept_id = co.condition_concept_id
```

## Sensitive Fields

