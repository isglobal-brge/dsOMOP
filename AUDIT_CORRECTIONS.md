# Audit corrections in the 2.10.5 development branch

This document describes the implemented import, setting and identity corrections.
It does not certify all A-31 acceptance obligations or an upstream HADES adapter.

## Fix 4: scoped formula and daily additions

The 14 November coordinator decision narrows this release to the released
corrections above plus Q14/Q15. All four formula constructors and the closed
`number_daily` route are now present. The client expands formulas into existing
operations; no formula opcode, arbitrary evaluator or remote definition lookup
is introduced. The eGFR formula uses completed calendar age at the explicitly
selected creatinine event, a declared IDMS assay/unit interpretation and an
explicit disjoint mapping to its two equation branches. Missing/unknown sex,
underage people and nonpositive creatinine produce private NA.

Daily collapse retains source/concept/coordinate/date and consumer-episode
identity. It strips selected-row range/visit metadata, uses genuine person
lineage at publication, and cannot turn many days from one person into release
support. Multi-concept input needs an explicit common-quantity declaration and
still produces concept-day observations. Input and final presentation are
separate. Old servers without `number.daily` fail capability preflight.

Historical statements below that formula/day routes are absent describe the
pre-fix-4 tree. Deferred bundle/interoperability/teaching/export/consumer work
is tracked in [the post-2.10.5 roadmap](POST_2.10.5_ROADMAP.md). No endorsement
of those deferred routes or deployed Opal behavior is implied.

## Importing public OHDSI concept definitions

Both `recipe_import_circe()` and `recipe_import_concept_sets()` require an
integral JSON number in 0..2147483647 for CONCEPT_ID. For example, 201826 and
201826.0 are accepted; "201826" and 201826.9 are refused. Zero retains the
unknown-concept meaning and is not a wildcard. In a direct list, factors,
Dates and other classed numbers are invalid IDs.

Every present isExcluded/includeDescendants/includeMapped must be true or
false. Omission means false; null does not. Duplicate object keys and unknown
semantic item fields refuse. The documented descriptive concept fields in
NEWS never alter membership. The complete set of items is checked before
exclusion, deduplication or translation, including unreferenced cohort codesets.
Circe cohort import still refuses excluded items and nonuniform flags; registry
import retains its separate uniform included/excluded expressions. Upstream
Circe has a Death criterion; this bridge does not implement it.

## Imported vocabulary semantics and inspection

`recipe_circe_support()` lists the directional supported subset;
`recipe_circe_check()` returns bounded structural diagnostics before importing
or exporting. Native populations without a Circe adapter remain native recipes.
The fixture at `tests/fixtures/phenotype-library` pins supported phenotype 304
and (on the client) refused phenotype 10. The server compares phenotype 304's
membership and dates with its unmodified upstream generated SQL on the same
private synthetic PostgreSQL fixture using SqlRender 1.19.7. This is one tested
phenotype, not general PhenotypeLibrary or HADES equivalence.

Both import paths now attach `validity_policy = "ohdsi-import-1"` to executable
concept definitions, including registry exclusions. All explicit concepts must
resolve to valid public vocabulary entries; concept 0 needs its actual entry.
Expansion removes invalid descendants and invalid mapped source concepts. A
valid Maps-to relationship selects source concept_id_1 when target concept_id_2
is selected; it does not map patient facts. Classification C is accepted.
Native sets omit this marker and retain their existing behavior. Re-import old
saved definitions to obtain the strict semantics; provenance alone does not
change execution.

The paired wire and IR schemas, legacy lowering, save/load, binding, merging,
code generation and semantic digests preserve this policy. Execution requires
`vocabulary.ohdsi_import_validity`; an incapable server refuses before patient
access. Invalid explicit references receive a definition-only compatibility
refusal without IDs, observed counts or patient examples. These changes do not
complete A-31's separate codebook/declaration and full acceptance obligations.

## Effective scientific settings

`dsomop:incidence.rate` and its CohortDiagnostics/CohortIncidence live aliases
accept only an omitted or zero `clean_window`. Nonzero integers produce
`unsupported_clean_window`. The retained calculation selects each person's
first target-cohort episode, forms its inclusive requested risk window, and
intersects that with the observation island containing its index. Event
eligibility uses the same half-open internal interval as person-time. It counts
people with an event separately from recurrent outcome records; the rate's
numerator is people with an event, and its denominator is full-window person-days.
It does not subtract a clean period or stop time at the first outcome. A second
target episode does not add time to this first-episode calculation.

The ordinary compatibility route retains its existing time/rate masking. The
protected route reports its own admitted components and contribution caps:
the protected time horizon is 3,650 days, separate from the declared scientific
window. The rate uses days. A display conversion, if made by the analyst from
admitted coupled components, must name its days-per-year constant: for 365.25
days/year, per-100-person-years = per-person-day rate * 365.25 * 100. This is
unit conversion, not a new estimand or a recurrence correction.

`dsomop:fe.prevalence` accepts only omitted/zero `time_windows`; a nonzero
integer produces `unsupported_fe_window`. Its source is the selected domain's
occurrence table. It includes all-history events among scoped cohort members,
without a predicate restricting events to their cohort dates. A bounded native
feature recipe is a separate analysis with an explicit window. FE naming does
not imply compatibility with an upstream FeatureExtraction settings object.

Both parameters remain discoverable, with only zero advertised. Typed catalogue
transport can use decimal integer text. Null, fractions, vectors, nonfinite
values, booleans and arbitrary text produce `invalid_setting`. Omitted and
explicit zero normalize to the same request. Validation precedes snapshots,
patient computation and protection-key work for the qualified built-in routes;
server canonical resolution enforces the same contract for aliases/shorthands.
Dropping a clinically required clean period/window is not an equivalent migration.

## Physical result identities

Public physical readers and pooling-contract discovery require the identities
listed in `R/ohdsi_schema.R` before completing optional columns. Integer identity
columns must use an exact integer database type; reference names use declared
text types. Rows with missing/invalid required identities refuse with a generic
`results_schema_integrity` message, including when the caller projects only an
effect estimate. Empty tables still undergo structural/type checks. SQLite
values are checked against physical integer storage before DBI can coerce mixed
values. Exact signed BIGINT identities, including values above 2^53, remain
canonical decimal text rather than passing through a double. Optional missing
columns use only their declared types; missing required keys are never padded.

CohortMethod's target_comparator_id layout is recognized and refused as
`unsupported_results_schema`: the reader has no versioned cm_target_comparator
join. It cannot construct target_id/comparator_id by padding them with NA. The
bundled 6.0.3 fixture records the immutable upstream source commit and SHA-256;
it is schema evidence, not an actual upstream patient-result export or a
certification of the full schema/version recognition obligation.

Historical inline layouts have no verified upstream package version. Their
existing disclosure contracts, statistic coupling and public column allowlists
remain in force. Unrecognized negative count encodings refuse with
`unsupported_results_suppression`; no sentinel is treated as zero. An explicitly
requested live-analysis alias is a separate method, never an automatic fallback.

No changes add state, privacy budgets, call limits, TLS requirements, owner
privacy configuration or a new release gate. Development uses targeted tests;
complete A-31 implementation and integration/release acceptance remain outstanding.

## Third audit repair: explicit edits and closed physical columns

An explicit `population$coverage` overrides the imported Circe translation's
coverage default. For example, after import, assigning
`r$populations$p$coverage <- omop_coverage("earliest")` selects the earliest
covering observation period, including after JSON/YAML save/load and generated
R code. Removing the override restores the translated policy. Index/filter
edits do not reset a separate coverage choice. Original bytes and hashes stay
in provenance; the edited declaration has its own derivative digest. A Circe
export that cannot express the policy refuses with a native save/code remedy.
The checker inspects each independent clause inside nested groups, while still
refusing execution of the unsupported group.

Physical layout matching now rejects uncontracted columns before result reads,
including when the caller projects those columns away. Known private legacy
incidence measures and SCCS event/time counts have explicit numeric contracts;
they remain excluded from release and cannot replace person support. Unknown
semantic dimensions produce unknown layout status and a bounded schema refusal.
Version-evidence reconciliation, reference associations, and actual old/current
physical-export acceptance still require further work.

The audit pair now includes pinned A-29/A-30 and 24-entry library prerequisites.
Integration retains the imported vocabulary policy in category domains and
corrects prospective generated-date factor ordering under category defaults.
These integrations do not supply the missing A-31 formula builders, daily
aggregation, study bundle, complete interoperability declarations, teaching or
deployment/consumer acceptance. This branch is not certified as complete A-31.
