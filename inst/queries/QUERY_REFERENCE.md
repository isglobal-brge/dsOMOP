# dsOMOP Catalog Query Reference

## Sources

- OHDSI QueryLibrary: https://github.com/OHDSI/QueryLibrary (162 queries across 15 domains)
- dsQueryLibraryServer (SIB Swiss): https://github.com/sib-swiss/dsQueryLibraryServer
- dsQueryLibrary client: https://github.com/sib-swiss/dsQueryLibrary

## Safety Classification

All catalog queries in dsOMOP are classified as:

- **SAFE_AGGREGATE**: Returns aggregate/summary statistics only. Safe to return to client.
  Applied disclosure controls: cell counts < nfilter.tab are set to NA.
- **SAFE_ASSIGN**: Data stays server-side. Not returned to client.
- **BLOCKED**: Cannot be executed (returns identifiers, free-text, or unbinned dates).

## Implemented Queries

### Condition Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| condition.prevalence_by_concept | Condition prevalence by concept | top_n | Yes |
| condition.prevalence_by_gender | Condition prevalence by gender | concept_id | Yes |
| condition.prevalence_by_age | Condition prevalence by age group | concept_id | Yes |
| condition.prevalence_by_year | Condition prevalence by year | concept_id, top_n | Yes |
| condition.comorbidity | Comorbidities of a condition | concept_id, top_n | Yes |
| condition.duration_stats | Condition duration statistics | concept_id | Yes |
| condition.prevalence_by_visit_type | Condition prevalence by visit type | concept_id | Yes |
| condition.prevalence_by_specialty | Condition prevalence by provider specialty | concept_id | Yes |
| condition.drug_overlap | Drugs used by condition patients | concept_id, top_n | Yes |
| condition.mortality_rate | Mortality rate for condition | concept_id | Yes |

### Drug Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| drug.prevalence_by_concept | Drug exposure prevalence by concept | top_n | Yes |
| drug.prevalence_by_gender | Drug exposure prevalence by gender | concept_id | Yes |
| drug.prevalence_by_age | Drug exposure prevalence by age group | concept_id | Yes |
| drug.prevalence_by_year | Drug exposure prevalence by year | concept_id, top_n | Yes |
| drug.duration_stats | Drug exposure duration statistics | concept_id | Yes |
| drug.quantity_stats | Drug quantity statistics | concept_id | Yes |
| drug.days_supply_stats | Drug days supply statistics | concept_id | Yes |
| drug.concomitant_medications | Concomitant medications | concept_id, top_n | Yes |
| drug.prevalence_by_route | Drug exposure by route | concept_id | Yes |

### Measurement Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| measurement.prevalence_by_concept | Measurement prevalence by concept | top_n | Yes |
| measurement.prevalence_by_gender | Measurement prevalence by gender | concept_id | Yes |
| measurement.prevalence_by_age | Measurement prevalence by age group | concept_id | Yes |
| measurement.prevalence_by_year | Measurement prevalence by year | concept_id, top_n | Yes |
| measurement.value_stats | Measurement value statistics | concept_id | Yes |
| measurement.prevalence_by_unit | Measurement prevalence by unit | concept_id | Yes |

### Procedure Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| procedure.prevalence_by_concept | Procedure prevalence by concept | top_n | Yes |
| procedure.prevalence_by_gender | Procedure prevalence by gender | concept_id | Yes |
| procedure.prevalence_by_age | Procedure prevalence by age group | concept_id | Yes |
| procedure.prevalence_by_year | Procedure prevalence by year | concept_id, top_n | Yes |

### Observation Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| observation.prevalence_by_concept | Observation prevalence by concept | top_n | Yes |
| observation.prevalence_by_gender | Observation prevalence by gender | concept_id | Yes |
| observation.prevalence_by_year | Observation prevalence by year | concept_id, top_n | Yes |

### Person Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| person.demographic_summary | Demographic summary | (none) | Yes |
| person.age_distribution_decade | Age distribution by decade | reference_year | Yes |
| person.race_distribution | Race distribution | (none) | Yes |
| person.ethnicity_distribution | Ethnicity distribution | (none) | Yes |
| person.year_of_birth_distribution | Year of birth distribution | (none) | Yes |
| person.gender_by_age_decade | Gender by age decade | (none) | Yes |

### Visit Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| visit.type_summary | Visit type summary | (none) | Yes |
| visit.type_by_gender | Visit type by gender | (none) | Yes |
| visit.type_by_year | Visit type by year | (none) | Yes |
| visit.type_by_age | Visit type by age group | visit_concept_id | Yes |
| visit.duration_stats | Visit duration statistics | visit_concept_id | Yes |

### Death Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| death.summary | Death summary statistics | (none) | Yes |
| death.by_gender | Death by gender | (none) | Yes |
| death.by_age | Death by age group | (none) | Yes |

### Observation Period Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| observation_period.length_stats | Observation period length statistics | (none) | Yes |
| observation_period.coverage_by_year | Observation period coverage by year | (none) | Yes |
| observation_period.length_by_gender | Observation period length by gender | (none) | Yes |

### Device Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| device.prevalence_by_concept | Device exposure prevalence by concept | top_n | Yes |

### Care Site Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| care_site.place_of_service_summary | Place of service summary | (none) | Yes |

### General / Cross-Domain
| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| general.domain_coverage | Domain coverage summary | (none) | Yes |
| general.temporal_coverage | Temporal coverage by domain | (none) | Yes |

## Queries NOT Implemented (and Why)

### Individual-Level (Return person_id)
- CO06, COC07, COC08, DER01, DER04, DER14, DER16, OP09

### Free-Text Fields
- CO20, DEX17, DEX38 (return stop_reason)

### Vocabulary-Only Queries
- C01-C11, D01-D27, G01-G17, O01, PO2
  Already covered by dsOMOP vocabulary endpoints: ds.omop.concept.search(),
  ds.omop.concept.lookup(), ds.omop.concept.descendants()

### Too Granular / Potentially Disclosive
- PE08 (zip code level), PE10 (day-of-year birth)
- CO02 (single MIN date for condition onset)
