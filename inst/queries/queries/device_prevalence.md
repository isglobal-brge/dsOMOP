---
Group: Device
Name: Device exposure prevalence by concept
ID: device.prevalence_by_concept
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the top device exposures ranked by number of distinct persons, with
concept names from the vocabulary. Useful for understanding device utilization
in the CDM population.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| top_n     | 50      | No        | Number of top device exposures (default 50) |

## Output

| Field | Description |
|-------|-------------|
| concept_id | Device concept ID |
| concept_name | Standard concept name |
| n_persons | Number of distinct persons with this device exposure |
| n_records | Total number of device exposure records |

## Query

```sql
SELECT de.device_concept_id AS concept_id,
       c.concept_name,
       COUNT(DISTINCT de.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.device_exposure de
JOIN @vocab.concept c ON c.concept_id = de.device_concept_id
WHERE de.device_concept_id != 0
GROUP BY de.device_concept_id, c.concept_name
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
