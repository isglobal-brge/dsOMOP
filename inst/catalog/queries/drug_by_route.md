---
Group: Drug
Name: Drug exposure by route of administration
ID: drug.prevalence_by_route
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns drug exposure counts cross-tabulated by route of administration. For a
given drug concept ID, shows the number of persons and records by each route
concept.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| concept_id | 1127433 | Yes | Drug concept ID to analyze |

## Output

| Field | Description |
|-------|-------------|
| drug_concept_id | Drug concept ID |
| drug_name | Drug concept name |
| route_concept_id | Route of administration concept ID |
| route_name | Route of administration concept name |
| n_persons | Number of distinct persons |
| n_records | Total number of drug exposure records |

## Query

```sql
SELECT de.drug_concept_id,
       dc.concept_name AS drug_name,
       de.route_concept_id,
       rc.concept_name AS route_name,
       COUNT(DISTINCT de.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.drug_exposure de
JOIN @vocab.concept dc ON dc.concept_id = de.drug_concept_id
JOIN @vocab.concept rc ON rc.concept_id = de.route_concept_id
WHERE de.drug_concept_id = @concept_id
GROUP BY de.drug_concept_id, dc.concept_name,
         de.route_concept_id, rc.concept_name
ORDER BY n_persons DESC
```

## Sensitive Fields

n_persons, n_records
