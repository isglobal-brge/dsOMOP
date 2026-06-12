---
Group: General
Name: Record count by table
ID: general.record_count_by_table
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the total number of records and distinct persons for each major CDM
clinical domain table. Provides a quick overview of the volume of data held in
each domain.

## Output

| Field | Description |
|-------|-------------|
| domain | Clinical domain table name |
| n_persons | Number of distinct persons |
| n_records | Total number of records |

## Query

```sql
SELECT 'condition_occurrence' AS domain,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.condition_occurrence
UNION ALL
SELECT 'drug_exposure' AS domain,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.drug_exposure
UNION ALL
SELECT 'procedure_occurrence' AS domain,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.procedure_occurrence
UNION ALL
SELECT 'measurement' AS domain,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.measurement
UNION ALL
SELECT 'observation' AS domain,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.observation
UNION ALL
SELECT 'visit_occurrence' AS domain,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.visit_occurrence
```

## Sensitive Fields

n_persons, n_records
