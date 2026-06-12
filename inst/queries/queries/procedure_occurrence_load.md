---
Group: Procedure
Name: Load procedure_occurrence table
ID: procedure_occurrence.load
CDM Version: 5.3+
Mode: assign
Author: dsOMOP
---

## Description

Materializes a server-side data frame of the procedure_occurrence table with
the procedure and type concept names joined from the vocabulary. Source values
are excluded. Data stays server-side and is never returned to the client.

## Output

| Field | Description |
|-------|-------------|
| procedure_occurrence_id | Procedure occurrence identifier |
| person_id | Person identifier |
| procedure_concept_id | Procedure concept ID |
| procedure_name | Procedure concept name |
| procedure_date | Procedure date |
| procedure_datetime | Procedure datetime |
| procedure_type_concept_id | Procedure type concept ID |
| procedure_type_name | Procedure type concept name |
| modifier_concept_id | Modifier concept ID |
| quantity | Quantity |
| visit_occurrence_id | Visit occurrence identifier |
| visit_detail_id | Visit detail identifier |
| provider_id | Provider identifier |

## Query

```sql
SELECT po.procedure_occurrence_id,
       po.person_id,
       po.procedure_concept_id,
       pc.concept_name AS procedure_name,
       po.procedure_date,
       po.procedure_datetime,
       po.procedure_type_concept_id,
       pt.concept_name AS procedure_type_name,
       po.modifier_concept_id,
       po.quantity,
       po.visit_occurrence_id,
       po.visit_detail_id,
       po.provider_id
FROM @cdm.procedure_occurrence po
LEFT JOIN @vocab.concept pc ON pc.concept_id = po.procedure_concept_id
LEFT JOIN @vocab.concept pt ON pt.concept_id = po.procedure_type_concept_id
```

## Sensitive Fields

