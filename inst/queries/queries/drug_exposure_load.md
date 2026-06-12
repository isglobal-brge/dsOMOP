---
Group: Drug
Name: Load drug_exposure table
ID: drug_exposure.load
CDM Version: 5.3+
Mode: assign
Author: dsOMOP
---

## Description

Materializes a server-side data frame of the drug_exposure table with the drug
and drug type concept names joined from the vocabulary. Free-text fields
(stop_reason, sig) and source values are excluded. Data stays server-side and
is never returned to the client.

## Output

| Field | Description |
|-------|-------------|
| drug_exposure_id | Drug exposure identifier |
| person_id | Person identifier |
| drug_concept_id | Drug concept ID |
| drug_name | Drug concept name |
| drug_exposure_start_date | Drug exposure start date |
| drug_exposure_start_datetime | Drug exposure start datetime |
| drug_exposure_end_date | Drug exposure end date |
| drug_exposure_end_datetime | Drug exposure end datetime |
| verbatim_end_date | Verbatim end date |
| drug_type_concept_id | Drug type concept ID |
| drug_type_name | Drug type concept name |
| refills | Number of refills |
| quantity | Quantity |
| days_supply | Days supply |
| route_concept_id | Route concept ID |
| lot_number | Lot number |
| visit_occurrence_id | Visit occurrence identifier |
| visit_detail_id | Visit detail identifier |
| provider_id | Provider identifier |

## Query

```sql
SELECT de.drug_exposure_id,
       de.person_id,
       de.drug_concept_id,
       d.concept_name AS drug_name,
       de.drug_exposure_start_date,
       de.drug_exposure_start_datetime,
       de.drug_exposure_end_date,
       de.drug_exposure_end_datetime,
       de.verbatim_end_date,
       de.drug_type_concept_id,
       dt.concept_name AS drug_type_name,
       de.refills,
       de.quantity,
       de.days_supply,
       de.route_concept_id,
       de.lot_number,
       de.visit_occurrence_id,
       de.visit_detail_id,
       de.provider_id
FROM @cdm.drug_exposure de
LEFT JOIN @vocab.concept d ON d.concept_id = de.drug_concept_id
LEFT JOIN @vocab.concept dt ON dt.concept_id = de.drug_type_concept_id
```

## Sensitive Fields

