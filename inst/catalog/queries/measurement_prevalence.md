---
Group: Measurement
Name: Measurement prevalence by concept
ID: measurement.prevalence_by_concept
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the top measurements ranked by number of distinct persons, with
concept names from the vocabulary.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| top_n     | 50      | No        | Number of top measurements (default 50) |

## Output

| Field | Description |
|-------|-------------|
| concept_id | Measurement concept ID |
| concept_name | Standard concept name |
| n_persons | Number of distinct persons with this measurement |
| n_records | Total number of measurement records |

## Query

```sql
SELECT m.measurement_concept_id AS concept_id,
       c.concept_name,
       COUNT(DISTINCT m.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.measurement m
JOIN @vocab.concept c ON c.concept_id = m.measurement_concept_id
WHERE m.measurement_concept_id != 0
GROUP BY m.measurement_concept_id, c.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
