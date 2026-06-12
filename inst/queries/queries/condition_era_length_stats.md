---
Group: Condition Era
Name: Condition era length statistics
ID: condition_era.length_stats
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons together with the average and standard
deviation of condition era length in days, grouped by condition concept. Length
is computed from the era start and end dates. No minimum or maximum values are
returned.

## Output

| Field | Description |
|-------|-------------|
| condition_concept_id | Condition concept ID |
| concept_name | Condition concept name |
| n_persons | Number of distinct persons |
| n_records | Number of condition era records with non-null dates |
| avg_days | Average condition era length in days |
| stddev_days | Standard deviation of condition era length in days |

## Query

```sql
SELECT ce.condition_concept_id,
       c.concept_name,
       COUNT(DISTINCT ce.person_id) AS n_persons,
       COUNT(*) AS n_records,
       AVG((EXTRACT(YEAR FROM ce.condition_era_end_date) * 365 + EXTRACT(MONTH FROM ce.condition_era_end_date) * 30 + EXTRACT(DAY FROM ce.condition_era_end_date))
           - (EXTRACT(YEAR FROM ce.condition_era_start_date) * 365 + EXTRACT(MONTH FROM ce.condition_era_start_date) * 30 + EXTRACT(DAY FROM ce.condition_era_start_date))) AS avg_days,
       STDDEV((EXTRACT(YEAR FROM ce.condition_era_end_date) * 365 + EXTRACT(MONTH FROM ce.condition_era_end_date) * 30 + EXTRACT(DAY FROM ce.condition_era_end_date))
           - (EXTRACT(YEAR FROM ce.condition_era_start_date) * 365 + EXTRACT(MONTH FROM ce.condition_era_start_date) * 30 + EXTRACT(DAY FROM ce.condition_era_start_date))) AS stddev_days
FROM @cdm.condition_era ce
JOIN @vocab.concept c ON c.concept_id = ce.condition_concept_id
WHERE ce.condition_era_start_date IS NOT NULL
  AND ce.condition_era_end_date IS NOT NULL
GROUP BY ce.condition_concept_id, c.concept_name
```

## Sensitive Fields

n_persons, n_records
