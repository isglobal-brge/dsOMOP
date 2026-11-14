# Study routes in 2.10.5

This guide describes the integrated 2.10.5 APIs. The run-specific evidence and
remaining acceptance obligations are in `tools/audit-fix4/REPORT.md` in the
server repository. An installed constructor, a rendered SQL statement and an
executed backend are different evidence. No general HADES equivalence is claimed.

## Choose a route from the study question

| Question | Available route | Meaning and boundary |
|---|---|---|
| A reusable baseline, outcome, drug, survival or panel recipe | `recipe_library_list()`, `recipe_library_describe(id)`, `recipe_library_instantiate(id, ...)` | 24 installed immutable templates; inspect their version, parameters and exact expansion. Edits are derivatives. |
| “ICD E11” | `ds.omop.concept.search()`, then inspect `lookup`, `synonyms` and `relationships` | A source code is not an OMOP standard concept ID. Keep code `001` as text. Qualified multi-vocabulary search is separate future work. |
| Timed features | Explicit `omop_window(-365,0)`, `(-180,0)`, `(-30,0)` and separately named pre-index versions ending -1 | Inclusive occurrence-start windows; not unbounded history, temporal bins or era-overlap queries. `inst/examples/audit-numeric-studies.R` supplies these named declarations. |
| Feature prevalence | `ds.omop.analysis.get("dsomop:fe.prevalence", ...)` and `.run()` | Its supported `time_windows=0` is all-history event prevalence among cohort members. A nonzero setting refuses; use a native bounded feature if timing is scientifically required. |
| Incidence | Inspect `dsomop:incidence.rate` and the native `panel.incidence` template | Catalogue route: first target episode, persons with events over full-window person-days, separate recurrent counts. `clean_window` supports zero only. A needed clean interval cannot be discarded as an equivalent remedy. |
| Adherence | `drug.adherence_components`, explicit drug source/end/supply policy | PDC/MPR summarize duration/supply, not ingested ingredient dose. Start-based washout does not exclude exposure spanning the washout. |
| BMI, MAP, ratio, adult creatinine eGFR | `omop_formula_bmi`, `omop_formula_map`, `omop_formula_ratio`, `omop_formula_egfr_ckdepi2021` | Exact installed version 1.0.0, explicit units/alignment/eligibility and public provenance. No arbitrary R/power operator. |
| Equal weighting of recorded days | `omop_number_daily(rows, omop_reduce("mean"))` | Private day collapse, then an independent outer reducer. It is not a visit key, missing-day imputation or duplicate-record adjudication. |
| What differs across sites? | `recipe_codebook()` and public vocabulary/reference declarations | Compare source, unit, instrument, missing/unavailable policy, coverage and semantic identities. Equal labels and equal plan digests do not prove equal ETL or measurement universes. |
| Save study provenance | `recipe_study_bundle()` | Local public artifact; no execution, connection discovery, automatic snapshot or complete Strategus-module claim. |
| DQD | Existing schema/profiling/plausibility routes | Not a run of DQD. Not-run, unsupported, unavailable and suppressed never mean pass. |

Calls needing connections retain their documented `conns`/`symbol` arguments.
The route table names installed functions; it does not create a new search or
analysis API. Use each installed help page for its full signature.

## Identity, vocabulary and time

Never join features solely on a matching integer. Native plan-local covariate
IDs, legacy concept-plus-suffix IDs, catalogue raw concepts and upstream FE
BIGINT covariates have different namespaces. Keep the full semantic definition,
scope digest and exact decimal text/integer64 for external identities above
2^53. A binary64 number cannot recover a rounded identity.

Imported OHDSI expressions carry executable `ohdsi-import-1` validity semantics.
Absent/invalid explicit included or excluded references refuse, invalid
descendants are excluded, and classification concepts remain allowed. Maps-to
expansion follows valid source concept_id_1 toward the selected target
concept_id_2. It does not rewrite patient facts or automatically select a source
code column. Native expressions retain their own policy. Original Circe bytes,
hash and origin remain client provenance; edits retain origin and become
derivatives. Unsupported clauses refuse, including unsupported native export.

Recorded `condition_era`/`drug_era` are CDM records. Native derived eras use the
separate `dsomop.native_era.v2` namespace: default gap 30, persistence 0, grouping
by event, closed endpoints. End day 0 and next start day 31 merge; start day 32
splits. Persistence extends ends independently of tolerated gaps. The declared
recorded-end/fallback policy has precedence; days-supply fallback is
start + days_supply - 1. OHDSI's persistence window must not be confused with
native end extension. No era default changed in this repair.

For comparative studies, scope order is target then comparator where that route
accepts a pair. A concept-filter outcome is not a cohort-definition outcome.
Preserve TAR anchor, offset, day-zero inclusion, recurrence, prior-outcome and
coverage/death/study-end policies. Contribution caps belong to protection and
are not scientific risk-window limits. Relative date columns are relative,
and native cohort membership shape is labelled native.

## CDM 5.3 / 5.4 interpretation

The small upstream lane pins **Eunomia 2.2.0, GiBleed CDM 5.3** and its archive
checksum. It explicitly requests 5.3; the package's moving 5.5 default is not
5.3/5.4 acceptance. Local GiBleed-shaped generators are separately labelled
synthetic package fixtures. They are not actual Eunomia exports.

`EPISODE` and `EPISODE_EVENT` in 5.4 describe recorded OMOP episode membership;
a study cohort episode is a different analysis unit. `omop_episode_members()`
uses the bounded, installed discriminator-to-domain mapping and validates person
association. Unsupported discriminators refuse; an event belonging to another
person cannot be attached merely because its event ID matches. Inspect the
installed blueprint/role contract rather than treating arbitrary fields as joins.

`DEATH.person_id`, date, cause concept and type concept have distinct roles.
An absent death table is unavailable; an available table with no row is not
proof of life; a recorded death is a separate state. Requested death censoring
uses the actual supported date role. `visit_detail` has its recorded FK and
version-specific parent field; A-30 visit ranking uses `visit_occurrence`.
Matching calendar dates do not create an interview or sub-visit association.
Tolerated observation continuity gaps contribute zero actually observed days.

## Current method labels

* CM outcome HR/RR fits are crude arm-only Cox/Poisson fits. Propensity
  diagnostics do not make them propensity-adjusted.
* The released band-derived survival display is not a fitted Kaplan–Meier
  curve. Competing-risk assignment is not Aalen–Johansen.
* PLP performance/calibration/threshold output is **in_sample/resubstitution**:
  fit and predict on the same data, not held-out or external validation.
* SCCS partition preparation, outcome-rate-per-month summary (including the
  legacy `sccs_result` alias), and a conditional exposed/unexposed estimate are
  distinct methods. A partition table alone does not estimate an SCCS effect.
* One-point component burden is an approximation. It is separate from the
  traced extraction score; inspect actual weights, reference and window.
* CohortDiagnostics standard-concept/overlap/temporal summaries are not complete
  source-mapping diagnostics or inclusion-rule attrition.
* Curated QueryLibrary/Achilles routes are route-specific supported questions,
  not the full upstream catalogues. SQL rendering does not establish execution
  on PostgreSQL, MariaDB, MySQL or SQLite.
* Physical results readers are not live analyses. Registered/admin-only tables
  are not automatically readable. CM 6.0.3's target-comparator lookup layout is
  recognized and refused; it never silently falls back to a live fit.
* No dedicated validated negative-control/calibration workflow or complete
  PhenotypeLibrary/Strategus execution is claimed. A software negative test is
  not a causal negative control.

## Study examples and consumer boundaries

`inst/book/audit-native-v2.Rmd` is the executed native lesson; the server evidence
archive includes its rendered HTML. `inst/examples/audit-numeric-studies.R`
contains the baseline/follow-up, eligible-observation fraction, previous-day
minimum/current-day maximum, cleaning-order and integer-scale compositions.
The independent server oracles execute their plans through normal publication.

The scalar fallback example gives measurement priority over observation with
an explicit common unit and alignment. Ordinary available NA may fall back;
unavailable input propagates. This does not create a unified, deduplicated
quantity history or permit foreign event-row operands.

The VA example in `inst/examples/audit-va-interviews.R` requires an ETL contract
that a real recorded visit identifies one interview. Every answer selects within
that visit FK, including when two interviews share a date. Preserve form revision
and response meanings: no, not asked, don't know, refused, ordinary missing and
unavailable are not synonyms. An old form missing a question is not “no.” A
recorded death cause remains recorded; no validated cause-assignment algorithm
is supplied.

`dsBase`/`dsBaseClient` 6.3.5 are the pinned downstream versions. Run the recorded
matrix for the advertised layout/type. Do not assume every published numeric
vector, ordered factor, exact integer, Date, repeated table or staged descriptor
works with every dsBase endpoint. Never strip protection or widen a generic
allowlist to make a method succeed. Reduce duplicate ID/time cells explicitly
before reshape; first-row selection is not lossless reshaping.

For growth, the pinned official WHO weight-for-age medians exercise the actual
published `ds.getWGSR`. Inputs are exact age in days, explicit male=1/female=2,
kg and the route's stature convention. Unknown sex, unavailable/unmapped units,
missing measures and out-of-range ages must follow the tested contract. This is
not a full WHO 2007 reference implementation. See the acceptance report for
observed deviations and the exact supported examples before using the workflow.

Pooled means, mean site estimates, mean personal rates and inverse-variance
SLMA estimate different quantities. With n=(2,8), means=(10,20), the pooled mean
is 18 and mean of means 15. Suppressed sites are not zero. A pooled GLM is not
SLMA; report component-specific retained sites, denominators, missing handling,
unit/contrast, approximation/protection and overlap assumptions. Repeated
episodes do not increase distinct-person support, and resource validation does
not deduplicate people across hospitals.

For same-currency costs, exact integer minor units can represent 10+20=30 cents
within the supported integer range. Keep the declared currency and minor-unit
coordinate. Do not round a floating amount and claim recovery of exact original
cents. Binary64 0.1+0.2 is not arbitrary-precision decimal; values above 2^53
must not silently become doubles. A currency conversion is not a price-year
adjustment. The exact-integer numeric tests define the permitted arithmetic
boundary; arbitrary decimal arithmetic remains outside this contract.

## Errors, deployment and disclosure

A malformed declaration, unsupported setting/schema, unavailable capability,
stale published profile, type/unit/alignment error and insufficient final support
are different failures. Structural errors give bounded public paths/remedies;
support errors give no hidden counts. Use a server advertising `number.daily`
and the emitted numeric/category/date primitives. On profile/version drift,
install the matching pair and republish the supported profile; a new package
file alone does not refresh an old endpoint allowlist.

Final accessible patient components gate on genuine distinct people. Private
intermediates may be small. Protected statistics retain their own support,
uncertainty and contribution limits. Heuristic sharing and deterministic noise
are not lifetime adaptive/composition guarantees, particularly as source data
evolves. No TLS mandate, privacy budget, answer-changing user/session state,
owner privacy tuning or persistent patient cache is introduced.

A supported restart can reuse existing noise and pseudonym roots only if the
infrastructure preserves them. Replacing the home/storage with empty roots
breaks continuity. Ordinary installation credentials are infrastructure, not
privacy controls. Actual fresh/upgrade/profile/restart and deployed boundary
acceptance must be recorded on the deployed service; DSLite is not that evidence.
