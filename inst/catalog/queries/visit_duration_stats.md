---
Group: Visit
Name: Visit duration statistics
ID: visit.duration_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns summary statistics for visit duration in days, computed as the
difference between visit_end_date and visit_start_date. If a visit concept ID
is provided, results are filtered to that type; otherwise results are grouped
by visit type.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| visit_concept_id | 0 | No | Visit type concept ID (0 = all types) |

## Output

| Field | Description |
|-------|-------------|
| visit_concept_id | Visit concept ID |
| visit_name | Visit type name |
| n_persons | Number of distinct persons |
| n_records | Total number of visit records |
| avg_days | Mean visit duration in days |
| sd_days | Standard deviation of visit duration |
| min_days | Minimum visit duration in days |
| max_days | Maximum visit duration in days |

## Query

```sql
SELECT vo.visit_concept_id,
       c.concept_name AS visit_name,
       COUNT(DISTINCT vo.person_id) AS n_persons,
       COUNT(*) AS n_records,
       AVG(CAST(vo.visit_end_date - vo.visit_start_date AS FLOAT)) AS avg_days,
       STDDEV(CAST(vo.visit_end_date - vo.visit_start_date AS FLOAT)) AS sd_days,
       MIN(CAST(vo.visit_end_date - vo.visit_start_date AS FLOAT)) AS min_days,
       MAX(CAST(vo.visit_end_date - vo.visit_start_date AS FLOAT)) AS max_days
FROM @cdm.visit_occurrence vo
JOIN @vocab.concept c ON c.concept_id = vo.visit_concept_id
WHERE vo.visit_end_date IS NOT NULL
  AND (@visit_concept_id = 0 OR vo.visit_concept_id = @visit_concept_id)
GROUP BY vo.visit_concept_id, c.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons, n_records
