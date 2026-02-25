---
Group: Procedure
Name: Procedure prevalence by year
ID: procedure.prevalence_by_year
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns procedure prevalence by calendar year of procedure date.
Optionally filters to a specific procedure concept ID and limits to the top N procedures.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 0 | No | Procedure concept ID to filter (0 = all) |
| top_n | 50 | No | Number of top results to return (default 50) |

## Output

| Field | Description |
|-------|-------------|
| procedure_concept_id | Procedure concept ID |
| concept_name | Procedure concept name |
| procedure_year | Calendar year of procedure |
| n_persons | Number of distinct persons |
| n_records | Total number of procedure records |

## Query

```sql
SELECT po.procedure_concept_id,
       c.concept_name,
       EXTRACT(YEAR FROM po.procedure_date) AS procedure_year,
       COUNT(DISTINCT po.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.procedure_occurrence po
JOIN @vocab.concept c ON c.concept_id = po.procedure_concept_id
WHERE po.procedure_concept_id != 0
  AND (@concept_id = 0 OR po.procedure_concept_id = @concept_id)
GROUP BY po.procedure_concept_id, c.concept_name,
         EXTRACT(YEAR FROM po.procedure_date)
ORDER BY procedure_year DESC, n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons, n_records
