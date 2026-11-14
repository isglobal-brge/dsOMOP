# Audit corrections in the 2.10.5 development branch

This document describes the implemented import, setting and identity corrections.
It does not certify all A-31 acceptance obligations or an upstream HADES adapter.

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
effect estimate. Empty tables still undergo structural/type checks.

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
