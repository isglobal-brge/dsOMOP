---
Group: General
Name: Domain coverage summary
ID: general.domain_coverage
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the total number of distinct persons and records across major CDM
clinical tables. Provides a quick overview of data coverage across domains.

## Output

| Field | Description |
|-------|-------------|
| domain | Clinical domain name |
| n_persons | Number of distinct persons |
| n_records | Total number of records |

## Query

```sql
SELECT 'condition' AS domain,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.condition_occurrence
UNION ALL
SELECT 'drug' AS domain,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.drug_exposure
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
SELECT 'procedure' AS domain,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.procedure_occurrence
UNION ALL
SELECT 'visit' AS domain,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.visit_occurrence
```

## Sensitive Fields

n_persons, n_records
