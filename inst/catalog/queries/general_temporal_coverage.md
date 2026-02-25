---
Group: General
Name: Temporal coverage by domain
ID: general.temporal_coverage
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns record and person counts by calendar year across major clinical domains
(condition, drug, measurement, procedure, observation, visit). Useful for
understanding data availability and temporal trends across the CDM.

## Output

| Field | Description |
|-------|-------------|
| domain | Clinical domain name |
| record_year | Calendar year of the record |
| n_persons | Number of distinct persons |
| n_records | Total number of records |

## Query

```sql
SELECT 'condition' AS domain,
       EXTRACT(YEAR FROM condition_start_date) AS record_year,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.condition_occurrence
GROUP BY EXTRACT(YEAR FROM condition_start_date)
UNION ALL
SELECT 'drug' AS domain,
       EXTRACT(YEAR FROM drug_exposure_start_date) AS record_year,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.drug_exposure
GROUP BY EXTRACT(YEAR FROM drug_exposure_start_date)
UNION ALL
SELECT 'measurement' AS domain,
       EXTRACT(YEAR FROM measurement_date) AS record_year,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.measurement
GROUP BY EXTRACT(YEAR FROM measurement_date)
UNION ALL
SELECT 'procedure' AS domain,
       EXTRACT(YEAR FROM procedure_date) AS record_year,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.procedure_occurrence
GROUP BY EXTRACT(YEAR FROM procedure_date)
UNION ALL
SELECT 'observation' AS domain,
       EXTRACT(YEAR FROM observation_date) AS record_year,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.observation
GROUP BY EXTRACT(YEAR FROM observation_date)
UNION ALL
SELECT 'visit' AS domain,
       EXTRACT(YEAR FROM visit_start_date) AS record_year,
       COUNT(DISTINCT person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.visit_occurrence
GROUP BY EXTRACT(YEAR FROM visit_start_date)
ORDER BY domain, record_year
```

## Sensitive Fields

n_persons, n_records
