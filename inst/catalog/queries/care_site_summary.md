---
Group: Care Site
Name: Place of service summary
ID: care_site.place_of_service_summary
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of distinct persons and visits by place of service. Links
visit_occurrence to care_site and resolves the place of service concept name
from the vocabulary.

## Output

| Field | Description |
|-------|-------------|
| place_of_service_concept_id | Place of service concept ID |
| place_of_service_name | Place of service concept name |
| n_persons | Number of distinct persons |
| n_visits | Total number of visit records |

## Query

```sql
SELECT cs.place_of_service_concept_id,
       c.concept_name AS place_of_service_name,
       COUNT(DISTINCT vo.person_id) AS n_persons,
       COUNT(*) AS n_visits
FROM @cdm.visit_occurrence vo
JOIN @cdm.care_site cs ON cs.care_site_id = vo.care_site_id
JOIN @vocab.concept c ON c.concept_id = cs.place_of_service_concept_id
GROUP BY cs.place_of_service_concept_id, c.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons, n_visits
