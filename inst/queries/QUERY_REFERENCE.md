# dsOMOP Catalog Query Reference

## Sources and provenance

- OHDSI QueryLibrary: https://github.com/OHDSI/QueryLibrary (201 Markdown
  queries across 15 domains at commit
  `df8a21074b08519e581ca1afb7510468538117a4`, audited 2026-08-01)
- dsQueryLibraryServer (SIB Swiss): https://github.com/sib-swiss/dsQueryLibraryServer
- dsQueryLibrary client: https://github.com/sib-swiss/dsQueryLibrary

The reviewed upstream snapshot is recorded in the machine-readable
`upstream_querylibrary_audit.json`: it pins the source commit, query/domain
counts and a reproducible SHA-256 manifest over all 201 Markdown files. It also
records, for every query, the published upstream ID, filename ID/path, file
SHA-256, detectable schema
dependencies and tables, documented output fields/type, disclosure and SQL
portability signals, triage class and any DP-redesign rationale. The generator
is `tools/build-querylibrary-audit.R`; it refuses a different commit, file count
or corpus digest. This allows upstream drift to be detected without treating an
unreviewed new query as executable.

The local catalog is a curated set of 80 independently maintained dsOMOP
templates plus the `omopCrossTabDS` allow-list entry. It is not a vendored copy
of QueryLibrary and local entries do not yet carry per-template `upstream_id`
lineage. The snapshot digest therefore proves which upstream corpus was
reviewed, not that a local template is a verbatim or semantically equivalent
port. Per-entry lineage, CDM dependencies and tested DBMS metadata remain a
release requirement before claiming a particular upstream query is ported.

Runtime catalog discovery is dialect-aware. A local template is listed only
when its SQL constructs have known semantics for the handle's target dialect.
In particular, SQLite does not advertise templates that require `EXTRACT`,
`STDDEV` or plain date subtraction, and SQL Server/Oracle do not advertise
templates that retain an untranslated `LIMIT`. The complete announced catalog
is executed against empty OMOP 5.4 schemas in SQLite and, when installed,
DuckDB tests. PostgreSQL 16, MySQL 8.4 and MariaDB 11.4 also run live CI
contracts with separate CDM, vocabulary and results namespaces. Other network
engines remain SQL-contract-tested; each site must still validate its exact
driver, privileges and server version before claiming full cross-vendor
QueryLibrary parity.

## Safety Classification

All catalog queries in dsOMOP are classified as:

- **SAFE_AGGREGATE**: Returns aggregate/summary statistics only through the
  unified dsOMOP disclosure gate. Depending on the declared result unit, that
  gate applies distinct-person support, small-cell suppression, count banding
  and distribution masking. The label alone is never sufficient to make a SQL
  template safe.
- **SAFE_ASSIGN**: Data is assigned only to a dsOMOP-protected server object;
  rows are not returned directly to the client. This classification is safe
  only with a reviewed downstream DataSHIELD method allowlist: an unrelated
  server method that accepts the object could otherwise bypass dsOMOP's class
  controls.
- **BLOCKED**: Cannot be executed (returns identifiers, free-text, or unbinned dates).

`nfilter.noise` is not a general-purpose noise or differential-privacy layer.
In dsBase it is used by a small number of plotting endpoints and provides no
person-level contribution bounding or semantic stickiness. It therefore must
not be used to reclassify an otherwise unsafe QueryLibrary query. A protected
noise-backed aggregate requires a query-specific sensitivity contract, public
clipping bounds, server-controlled parameters and deterministic sticky noise.

## Upstream coverage inventory and executable redesigns

The 201 upstream queries were triaged by the result they expose, not merely by
SQL syntax:

| Class | Count | Required treatment |
|---|---:|---|
| Vocabulary/reference metadata | 54 | Serve through bounded vocabulary/metadata APIs |
| Rewritable patient aggregates | 56 | Add distinct-person support, public groups, contribution bounds, suppression and banding |
| Statistical queries needing redesign | 73 | Bin dates/ages and numeric supports, remove raw extremes, clip contributions and use an appropriate protected mechanism |
| Patient/event or single-subject results | 13 | Assignment-only; never return rows or a selected subject's result directly |
| Unsafe as written | 5 | Keep blocked or replace with a coarser derived result |
| **Total** | **201** | |

These counts are generated from the per-query evidence; the previous summary
`52/65/67/12/5` was not retained as a target. Two concrete corrections are
visible directly in the pinned SQL: `DEX12` reads only vocabulary tables despite
living under `drug_exposure`; `CS01` counts non-patient care-site metadata; and
`DER02` selects one unique `drug_era_id` and returns that episode's cost. Output
semantics also move raw means/maxima and unbounded numeric histograms into
statistical redesign even where their output column names look like ordinary
counts. The exact membership and reasons are in the JSON rather than being
implicit in this summary table.

The inventory marks 130 entries as `dp_candidate`. One of those entries is
the exact-ZIP query `PE08`, which the executable policy still blocks. The 129
aggregate questions in the rewritable/statistical triage classes are the set
now mapped to executable sticky redesigns. A candidate flag means
only that finite sensitivity is plausible after the recorded public
clipping/binning and per-person contribution bounds, with server-owned sticky
noise under a fixed per-release contract. It does **not** mean that the upstream
SQL is safe as written, that `nfilter.noise` supplies the sticky mechanism, or that any of
these literal upstream queries is enabled. The inventory authorizes zero
literal upstream SQL. `query_allowlist.json` remains the independent policy for
the older curated SQL catalog, while the sticky endpoint accepts only the typed
primitive mappings recorded below.

`dp_redesign_registry.json` retains the first 14 individually documented
questions and now also contains an executable semantic catalog of 129 mappings:
the original 14 plus 115 additional questions expressible through seven
bounded sticky primitives. The server joins every mapping to the pinned audit,
validates its upstream ID/commit/path/hash and exposes public catalog metadata;
the client constructs the fixed `omop_privacy` specification and verifies the
same mapping on every selected server before release. Neither path renders or
executes upstream SQL.

The 129 questions are re-expressed through dedicated
person-bounded sticky-noise primitives: a distinct-person count, a bounded
record count, a fixed-public-domain person or record histogram, a public-bin
numeric histogram, bounded distinct-category cardinality, a bounded person
mean, or a binary rate. Numeric and categorical record histograms use the
`records` reducer with an explicit order and finite per-person record cap;
scalar record counts and distinct cardinality also require an explicit cap.
Its `mapped_to_bounded_sticky_primitive` status
records that semantic mapping only;
it never authorizes the pinned upstream SQL or changes the runtime allowlist.
The registry states each family's contribution rule and sensitivity and pins
each entry to the audited path and SHA-256. Longitudinal records are collapsed
to person predicates, de-duplicated person/category pairs, or deterministically
retained up to the declared cap before aggregation. Domains, concept sets,
thresholds, category order and time intervals must be public and fixed before
data access—observed output levels and data-dependent top-N selection are not
allowed. The 18 formerly held aggregate questions now target explicit capped
estimands; they do not claim to reproduce their unbounded upstream estimands.

The 13 row/patient/event questions, four uncontrolled-source label questions
and exact-ZIP `PE08` remain blocked from client release.

Source/free-text frequency rows (`CO20`, `DEX17`, `DEX38`, `PP02`), exact ZIP
output (`PE08`) and the 13 patient/event or selected-subject assignment rows
remain explicitly blocked from DP release. Adding noise to counts cannot make
their labels, geography or row-level payload safe.

This upstream coverage is deliberately separate from the Recipe DSL. QueryLibrary should
remain a curated catalog of reviewed analysis patterns; maximum query
flexibility belongs in the typed Recipe/Plan compiler, where table relations,
cardinality, temporal semantics and disclosure contracts can be validated.

### Semantics of bounded upstream redesigns

The following representative upstream families now have bounded sticky
mappings, but are **not safe merely by adding the current `nfilter.noise`
value**. The local SQL catalog also contains some similarly named deterministic
summaries (for example drug quantity/days-supply, condition duration and
drug-cost statistics). Those local routes remove extrema, require both person
and value/record support, mask small distributions and band counts; they are
not proof that the corresponding upstream query was ported verbatim:

- Costs: DRC01, DRC03, DRC07.
- Drug-exposure distributions: DEX23 (days supply), DEX31 (records/person),
  DEX34 (quantity), DEX36 (refills).
- Observation-time distributions: OP05, OP06, OP12, OP20.
- Condition-duration/count distributions: CE01, CE03, CE16.

Each executable sticky specification therefore declares its contribution unit
(normally one bounded contribution per person, or a separately declared record
cap), public clipping bounds, removal of raw minima/maxima, and a server-side
privacy mechanism with fixed per-release sticky noise.
Means should be derived from protected bounded sums and counts; quantiles need
a protected histogram/quantile mechanism rather than noise added to the
published upstream statistic. A sticky variant must emit its full fixed
public cell domain and may threshold or band only the noisy result; it must not
branch first on an exact distinct-person or small-cell gate.

## Longitudinal contract and remaining boundaries

QueryLibrary expansion and longitudinal Recipe work must be reviewed together,
because many repeated records can still represent very few people. The
distinct-person gate therefore remains the principal support check even when
the requested estimand or output grain is an episode or an event.

The current longitudinal contract is explicit:

- Recurrent index-event cohorts can select `primary_limit = "first"`,
  `"last"` or `"all"`; episode-bearing results use a stable `cohort_row_id`.
- Wide/features output declares `grain = "person"` (the default) or
  `grain = "episode"`. An `index_window` requires episode grain and retains
  `cohort_row_id`; temporal Recipe features likewise require
  `options = list(grain = "episode")`.
- External index-dependent population filters on recurrent cohorts require one
  of `any_episode`, `all_episodes`, `first_episode` or `last_episode`. These
  policies decide person membership; they are not a hidden choice of which
  episode rows to emit.
- Long event/episode, sparse and temporal-covariate output retain episode
  identity. Sparse output carries a complete `personRef` at person or episode
  grain; a roster member with no qualifying event has no covariate row,
  representing an implicit zero.
- Basic `person_period` output crosses the complete episode roster with regular
  index-relative bins. Covariates remain sparse, so a missing
  `(rowId, timeId, covariateId)` tuple means zero; absolute dates and source
  event identifiers are not released.
- `min_gap` deterministically chains adjacent events within person/episode and,
  by default, concept. The policy declares positive `days`, `by` (`concept` or
  `grain`) and the retained representative (`first` or `last`), with the OMOP
  event primary key breaking same-date ties.
- Server-side shape expansion is bounded by configurable defaults of 1,000
  feature specifications, 1,000 pivoted concepts, 5,000 output columns and
  10,000 temporal bins. Federated requests must honor the minimum compatible
  cap across sites; these are operational limits, not disclosure guarantees.

Boundaries that remain before claiming unrestricted longitudinal analysis
support include:

1. Named counting-process, recurrent-event and competing-risk outputs have
   explicit contracts; arbitrary general multi-state transition models remain
   outside the reviewed output surface.
2. Add an explicit cross-table joined-long contract. Today a multi-table Recipe
   long request is split into separate table outputs rather than a relational
   join chosen implicitly.
3. Extend selection beyond the current grain-wide and concept-partitioned
   first/last-N modes when an estimand needs a different partition key; named
   feature reducers remain the preferred route for concept-set-specific values.
4. Promote any future QueryLibrary families only after recording upstream ID and commit,
   CDM/table dependencies, supported DBMS, contribution bounds and disclosure
   unit (person, episode or record).
5. Keep sticky releases on the dedicated bounded privacy path with canonical
   identity and a fixed per-release contract; `nfilter.noise` alone is not a
   substitute.

## Local Catalog Index

The bundled query directory contains 80 templates; the JSON allowlist also
contains `omopCrossTabDS`, for 81 entries in total. `query_allowlist.json` is the
authoritative runtime classification. A bundled template marked `BLOCKED` is
documented here but cannot execute. `Poolable` means that a reviewed client
pooling strategy is declared; it is not a claim that suppressed or banded
values recover the exact cross-site statistic.

For executable templates with `top_n`, dsOMOP removes the upstream raw
`TOP`/`LIMIT`, evaluates every grouped cell, applies the common disclosure gate
and then orders by the released support band plus public grouping keys. Varying
`top_n` therefore cannot reveal the exact ordering of cells inside one band.

### Condition Domain

| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| condition.prevalence_by_concept | Condition prevalence by concept | top_n | Yes |
| condition.prevalence_by_gender | Condition prevalence by gender | concept_id | Yes |
| condition.prevalence_by_age | Condition prevalence by age group | concept_id | Yes |
| condition.prevalence_by_year | Condition prevalence by year | concept_id, top_n | Yes |
| condition.comorbidity | Comorbidities of a condition | concept_id, top_n | Yes |
| condition.duration_stats | Condition duration statistics (**BLOCKED; use sticky bounded statistics**) | concept_id | No |
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
| drug.duration_stats | Drug exposure duration statistics (**BLOCKED; use sticky bounded statistics**) | concept_id | No |
| drug.quantity_stats | Drug quantity statistics (**BLOCKED; use sticky bounded statistics**) | concept_id | No |
| drug.days_supply_stats | Drug days supply statistics (**BLOCKED; use sticky bounded statistics**) | concept_id | No |
| drug.concomitant_medications | Concomitant medications | concept_id, top_n | Yes |
| drug.prevalence_by_route | Drug exposure by route | concept_id | Yes |

### Measurement Domain

| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| measurement.prevalence_by_concept | Measurement prevalence by concept | top_n | Yes |
| measurement.prevalence_by_gender | Measurement prevalence by gender | concept_id | Yes |
| measurement.prevalence_by_age | Measurement prevalence by age group | concept_id | Yes |
| measurement.prevalence_by_year | Measurement prevalence by year | concept_id, top_n | Yes |
| measurement.value_stats | Measurement value statistics (**BLOCKED; use sticky bounded statistics**) | concept_id | No |
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
| person.year_of_birth_distribution | Year of birth distribution (**BLOCKED**) | (none) | No |
| person.gender_by_age_decade | Gender by age decade (**BLOCKED**) | (none) | No |

### Visit Domain

| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| visit.type_summary | Visit type summary | (none) | Yes |
| visit.type_by_gender | Visit type by gender | (none) | Yes |
| visit.type_by_year | Visit type by year | (none) | Yes |
| visit.type_by_age | Visit type by age group | visit_concept_id | Yes |
| visit.duration_stats | Visit duration statistics (**BLOCKED; use sticky bounded statistics**) | visit_concept_id | No |

### Death Domain

| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| death.summary | Death summary statistics | (none) | Yes |
| death.by_gender | Death by gender | (none) | Yes |
| death.by_age | Death by age group | (none) | Yes |

### Observation Period Domain

| ID | Name | Inputs | Poolable |
|----|------|--------|----------|
| observation_period.length_stats | Observation period length statistics (**BLOCKED; use sticky bounded statistics**) | (none) | No |
| observation_period.coverage_by_year | Observation period coverage by year | (none) | Yes |
| observation_period.length_by_gender | Observation period length by gender (**BLOCKED; use sticky bounded statistics**) | (none) | No |

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

### Additional catalog entries

| ID | Class | Poolable |
|---|---|---:|
| condition_era.prevalence_by_concept | SAFE_AGGREGATE | Yes |
| condition_era.by_gender | SAFE_AGGREGATE | Yes |
| condition_era.by_season | SAFE_AGGREGATE | Yes |
| condition_era.eras_per_person_stats | BLOCKED | No |
| condition_era.length_stats | BLOCKED | No |
| condition_occurrence.load | SAFE_ASSIGN | No |
| condition_occurrence.type_distribution | SAFE_AGGREGATE | Yes |
| death.load | SAFE_ASSIGN | No |
| drug_cost.cost_per_unit_stats | BLOCKED | No |
| drug_cost.out_of_pocket_stats | BLOCKED | No |
| drug_era.by_month | SAFE_AGGREGATE | Yes |
| drug_era.exposure_count_stats | BLOCKED | No |
| drug_era.length_stats | BLOCKED | No |
| drug_era.prevalence_by_concept | SAFE_AGGREGATE | Yes |
| drug_exposure.load | SAFE_ASSIGN | No |
| drug_exposure.type_distribution | SAFE_AGGREGATE | Yes |
| general.record_count_by_table | SAFE_AGGREGATE | Yes |
| measurement.load | SAFE_ASSIGN | No |
| observation.load | SAFE_ASSIGN | No |
| observation_period.length_months_stats | BLOCKED | No |
| observation_period.long_period_count | SAFE_AGGREGATE | Yes |
| observation_period.persons_with_n_periods | SAFE_AGGREGATE | Yes |
| payer_plan.by_plan_concept | SAFE_AGGREGATE | Yes |
| payer_plan.length_distribution | SAFE_AGGREGATE | Yes |
| person.birth_month_distribution | BLOCKED | No |
| person.load | BLOCKED | No |
| procedure_occurrence.load | SAFE_ASSIGN | No |
| omopCrossTabDS | SAFE_AGGREGATE | Yes |

## Selected Upstream Queries Not Promoted (and Why)

### Individual-Level (Return person_id)

- CO06, CO16, COC07, COC08, DER01, DER04, DER14, DER16, DEX30, DEX32,
  DEX43, OP09

### Free-Text Fields

- CO20, DEX17, DEX38 (return stop_reason)

### Vocabulary-Only Queries

- C01-C11, D01-D27, G01-G17, O01, PO2
  Already covered by dsOMOP vocabulary endpoints: ds.omop.concept.search(),
  ds.omop.concept.lookup(), ds.omop.concept.descendants()

### Too Granular / Potentially Disclosive

- PE08 (zip code level), PE10 (day-of-year birth)
- CO02 (single MIN date for condition onset)
