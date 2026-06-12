---
Group: Payer Plan
Name: Plan period length distribution
ID: payer_plan.length_distribution
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons whose payer plan period length falls into
each whole-year bin, computed from the difference between the plan period start
and end dates. Dates are binned, not disclosed.

## Output

| Field | Description |
|-------|-------------|
| years_covered | Whole years of payer plan coverage (floored) |
| n_persons | Number of distinct persons in this length bin |

## Query

```sql
SELECT FLOOR((EXTRACT(YEAR FROM ppp.payer_plan_period_end_date) -
              EXTRACT(YEAR FROM ppp.payer_plan_period_start_date))) AS years_covered,
       COUNT(DISTINCT ppp.person_id) AS n_persons
FROM @cdm.payer_plan_period ppp
WHERE ppp.payer_plan_period_start_date IS NOT NULL
  AND ppp.payer_plan_period_end_date IS NOT NULL
GROUP BY FLOOR((EXTRACT(YEAR FROM ppp.payer_plan_period_end_date) -
                EXTRACT(YEAR FROM ppp.payer_plan_period_start_date)))
ORDER BY years_covered
```

## Sensitive Fields

n_persons
