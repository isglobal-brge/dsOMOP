---
Group: Drug
Name: Drug type distribution
ID: drug_exposure.type_distribution
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns the number of records and distinct persons grouped by drug type concept
ID, with the standard concept name. Useful for understanding the provenance of
drug exposure records (e.g. prescription, dispensing, administration).

## Output

| Field | Description |
|-------|-------------|
| drug_type_concept_id | Drug type concept ID |
| concept_name | Standard concept name of the drug type |
| n_persons | Number of distinct persons |
| n_records | Total number of drug exposure records |

## Query

```sql
SELECT de.drug_type_concept_id,
       c.concept_name,
       COUNT(DISTINCT de.person_id) AS n_persons,
       COUNT(*) AS n_records
FROM @cdm.drug_exposure de
JOIN @vocab.concept c ON c.concept_id = de.drug_type_concept_id
GROUP BY de.drug_type_concept_id, c.concept_name
ORDER BY n_records DESC
```

## Sensitive Fields

n_persons, n_records
