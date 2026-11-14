# Audit fix 4 — 2.10.5

Status: implemented and validated within the binding 2.10.5 scope; ready for independent review. R2/R3's broader obligations are explicitly deferred or assigned to integration by that decision.

## Authority and scope

Applied `/workspace/dsomop/design/astra-dev/COMMON.md`, its later targeted-test-only rule,
`brief-ws-common.md`, CLEANUP.md, and review 4 including its final **Coordinator scope
decision for A-31**. That decision says to implement/accept ONLY the released-defect
fixes, Q14's four formula constructors and Q15's daily collapse for 2.10.5. It defers
the broader amendment to 2.10.6 and assigns real Opal acceptance to integration e2e.
This report does not claim completion of the superseded all-Q01–Q35 release scope.

Both named audit worktrees remain on `feat/2.10.5-audit`, with package versions 2.10.5.
Starting commits: server `7d5812c266bed2cfa30abc0b722a2325a75605a1`, client
`0b5a49d9a8679d07f895b8849fc2061262aa8ce0`.

## Finding-to-fix and acceptance mapping

| Finding / scoped obligation | Final implementation and evidence |
|---|---|
| R2: Q14 missing constructors | Client `R/formulas.R` exports BMI, arithmetic MAP, ratio and race-free CKD-EPI 2021 creatinine eGFR, exact version 1.0.0. They expand solely into typed numeric/category/date operations. Explicit unit conversions, disjoint stable sex mappings, matching episode selection and unchanged completed calendar age are required. No source, unit, sex interpretation or assay calibration is guessed. |
| R2/R3: Q14 acceptance | `test-audit-formulas.R` publishes all four builders across three DSLite sites, memory and staged. eGFR uses an independent power equation and six pinned age-40 values, below/equal/above each sex-specific k. BMI=20, MAP=280/3, ratio=5, zero denominator=NA. `test-audit-formula-edges.R` covers ages 18/80, underage/unknown sex/zero/negative/NA creatinine, unavailable source, 88.4 micromol/L equivalence, incompatible event alignment and class-2 publication metadata. |
| R2/R3: Q14 portability | `test-audit-formula-portability.R` checks unit/version/mapping/calendar-anchor refusals, presented dependency stages, JSON/YAML/generated-R semantic digests, immutable formula definition and derivative provenance. `R/recipe.R` uses lossless binary64 YAML formatting and `digits17` generated literals: earlier decimal formatting rounded the small log-age coefficient. Formula provenance is client metadata; codebooks retain it with the actual expanded declaration. |
| R2: Q15 missing day collapse | Paired shared semantics and plan/IR schema support closed `number_daily`; client `omop_number_daily`; server `R/compile_number_daily.R`. Selection and input presentation precede private grouping by consumer episode/person, source, concept, normalized coordinate and recorded calendar date. Outer reduction precedes final presentation. Mean/max/first and skip/propagate are explicit; missing days are not filled. Group identity is private. Range/visit provenance is discarded, including for first. Multi-concept input requires a declared common coordinate and still counts concept-days. |
| R2/R3: Q15 acceptance | Paired `test-audit-number-daily.R`: raw record mean 7.5 versus equal-day mean 10; mean/max/first, missing stages, input conversion versus output presentation, all-NA/no-day/unavailable, reordered ties, exact relative/episode windows, range refusal, separate concepts, foreign event refusal, memory/staged agreement and class-2 lineage. One person's repeated episodes cannot satisfy nfilter. Injected assignment failures roll back memory/staged publication. Client checks JSON/YAML/generated R, merge dependencies and missing capability before assignment. |
| R1/R2/R4 released-defect closures retained | The strict Circe/registry ID/Boolean parser, closed result-column/type/identity checks, bounded nested support diagnostics and imported-vocabulary policy remain intact. Final tests include `audit-concept-import`, `audit-circe-support`, `audit-results-identity`, `audit-vocabulary-policy` and `ohdsi-results`; no broad upstream compatibility is inferred. |
| Q04/Q05 ignored settings | Existing effective-setting validation remains authoritative on ordinary/protected/alias routes. Nonzero incidence clean_window and FE time_windows refuse before patient computation. Both `test-audit-settings.R` files are rerun. |
| Q03 support/Death/provenance and R5 imported population edits | Existing support matrix, truthful Death comment, sanitized provenance and derivative reconciliation retained. Fresh client support tests include coverage edits across JSON/YAML/generated R, loss-checked export and all independent unsupported clauses. Server `audit-circe-coverage` and `audit-circe-oracle` freshly execute membership/start/end expectations, including the pinned upstream PhenotypeLibrary 304 SQL on PostgreSQL. |
| Q06 results schema detection/refusal | Existing structural fingerprints and closed historical inline contracts retained; the modern target_comparator_id layout is recognized and explicitly refused before row reads. Unknown/extra-column layouts refuse. No package version is inferred from a schema pattern; upstream_version stays unknown when producer metadata is absent. Older physical-export acceptance and expanded reference/version reconciliation are deferred by the binding decision. |
| R2/R3 broader amendment obligations | `POST_2.10.5_ROADMAP.md` in both packages records all remaining declarations, bundle, teaching, broader workflow/library/CDM/export/consumer/book acceptance for 2.10.6. Real Opal fresh/upgrade/profile/restart/deployed privacy acceptance is explicitly owned by 2.10.5 integration e2e. These are not silently claimed as tested on this branch. |

## Final targeted validation

All R tests use `/workspace/dsomop/tools/rtest.sh`, matching sibling `RTEST_WITH`,
and `RTEST_ENV='R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps'`; the first
server run also sets `DSOMOP_TEST_POSTGRES_HOST=omopdb`. No full suite was run.

| Run | Exact source commits (server/client) | Exact test-file filters | Result |
|---|---|---|---|
| final-server-1, timeout 1800 | fb79d70 / 60df5a8 | `^(audit-number-daily\|audit-formula-edges\|audit-settings\|audit-circe-coverage\|audit-circe-oracle\|audit-results-identity\|audit-vocabulary-policy\|ohdsi-results\|number-system-contract\|number-system-values\|number-system-lineage\|number-system-walks\|number-system-wire\|number-system-exact-expressions)$` | 1,146 passed, 0 failures/errors/skips |
| final-server-2, timeout 900 | f3e3588 / b6418a0 | `^(audit-number-daily\|audit-formula-edges)$` | 50 passed, 0 failures/errors/skips |
| final-client-1, timeout 1800 | f3e3588 / b6418a0 | `^(audit-concept-import\|audit-circe-support\|audit-settings\|audit-vocabulary-policy\|audit-category-defaults\|audit-formula-portability\|audit-formulas\|audit-number-daily\|number-system-api\|number-system-wire\|v2-composition-review)$` | 2,171 passed, 0 failures/errors/skips |

Each filter selects `tests/testthat/test-<name>.R`; no other test files were requested.
Final-server-2 reruns only new daily cases and formula dependents of the tightened age
validation. The broader successful final-server-1 run is not needlessly repeated.
`run-inventory.json` lists test files and observed counts from every retained full log.

Independent Python checker: 3 schemas, 2 examples, 224 probes, 15 acceptance plans,
12 digest vectors, 18 normalized fixtures, 34 numeric vectors, 11,043 corpus doubles;
all pass. Paired shared semantics/schema consistency and SHA256SUMS are checked.
`git diff --check` passes for both source deltas (raw evidence logs are preserved verbatim). Both DESCRIPTION files say 2.10.5
and RoxygenNote 7.3.3. The formula and daily help pages were generated on the pod
with roxygen2 7.3.3; only changed Rd/NAMESPACE content was pulled back.
A final 7.3.3 regeneration of the tested source produced zero Rd/NAMESPACE/DESCRIPTION
changes (`final-docs.log`). No new change request was needed: CR-32/A-31 already
authorizes the formula and daily interfaces.

## Scope adjustment and exploratory evidence

The initial work followed review 4's broad retained R2/R3 inventory. When the binding
scope decision at its end was read during this turn, the branch was reduced to the
specified 2.10.5 deliverables. Intermediate commits preserve the deferred work; the
final tree removes its new exports, schemas, metadata hooks, fixtures and teaching
deliverables. Existing released behavior/prerequisite implementations are retained.
The roadmap identifies the recovery points for 2.10.6.

Earlier logs in `tools/audit-fix4` are exploratory development records, **not final
acceptance**. Some ran uncommitted intermediate sources and cannot be assigned an
exact clean commit pair; do not substitute them for the pinned final runs above.
They include initial failures subsequently fixed (daily partial matching, formula
metadata hash class, YAML/codegen precision), as well as out-of-scope work. The latter
includes actual CM export, Eunomia, a rendered two-site lesson, and all 24 library
workflows. The last exploratory library selection passed 1,940 assertions but failed
three growth-consumer assertions: dsBase's accepted age/reference range differed
from the pinned WHO expectation. Eunomia's initial run had two metadata assertions
fail; its later untested pending changes are only preserved in history. No closure
of those deferred lanes is claimed. The exploratory dependency bootstrap initially
failed for missing DatabaseConnector/JVM and succeeded after installing Java 11 and
the pinned upstream packages in a task-specific pod library. These are test-only
environment dependencies, not new runtime/owner requirements.

## Commits

Server commits after the reviewed starting point:

```
ba64345 Implement closed private calendar-day numeric reduction
9961608 Preserve daily selection and lineage and add native interoperability records
a5ec319 Declare the actual method scope in catalogue discovery and admitted results
a7e2d9d Correct public model labels and exercise native study recipes through publication
9281502 Validate pinned physical CM exports and their private reference associations
520988d Retain upstream ATE and ATT exports and add pinned actual Eunomia acceptance
6e0811b Preserve pending audit work before applying narrowed release scope
fb79d70 Limit release to approved defect fixes, formulas and daily reduction
f3e3588 Exercise daily episode selection, unit conversion and publication rollback
```

Client commits after the reviewed starting point:

```
975c1ef Declare daily numeric reduction and preserve portable dependencies
44d8487 Add versioned formula expansions and public client study bundles
f4d68de Export and document the formula daily and study-bundle APIs
4b40114 Preserve small formula coefficients exactly through YAML serialization
f2c79a5 Add executable native study declarations and constrain bundle receipt metadata
b01d955 Distinguish covariate namespaces and make teaching clipping bounds explicit
b867203 Add an executable two-site native study lesson and consumer interface evidence
e3afc2b Pin WHO growth-reference medians and exercise the published under-five consumer
1fe72d4 Preserve pending study deliverables before applying narrowed release scope
60df5a8 Limit release to approved defect fixes, formulas and daily reduction
b6418a0 Require unchanged calendar age and matching episode scope for eGFR
```

The final evidence commit contains this report, run logs and their hash manifest.
`final-source-manifest.json` records hashes for every changed file in the tested pair.
`evidence-manifest.json` records artifact hashes and the decompressed HTML hash.
Its runtime source is the last tested source above. The requested external report
is byte-identical to the committed `tools/audit-fix4/REPORT.md`; the external design
directory is not a Git repository.

## Rules, cleanup and integration handoff

No Claude CLI, subagents, push, PR, tag, stash or released/other-job worktree changes.
No new TLS mandate, owner privacy setting, budget, accountant, per-user answer state
or persistent runtime server state. Formula and daily intermediates remain private
and permissive; existing final distinct-person gates and staging cleanup apply.
The arithmetic/reference tests use private synthetic fixtures, not analyst patient
row endpoints. This is package-level evidence, not deployed privacy certification.

Local free space remained about 9.6 GB. CLEANUP.md was read; no own local disposable
package copies or large downloads were retained. The shared disk is still below
its desired 15 GB reserve; no uncertain or other-job material was deleted.
After saving final evidence and confirming no active R process used these task
resources, cleanup removed only the paths below (`cleanup.log`).

| Deleted pod path | File bytes |
|---|---:|
| `/root/rt/wt/audit/dsOMOP` | 25,444,795 |
| `/root/rt/wt/audit/dsOMOPClient` | 19,813,529 |
| `/root/rt/lib/wt_audit_dsOMOP` | 6,179,508 |
| `/root/rt/lib/wt_audit_dsOMOPClient` | 11,789,060 |
| `/root/rt/audit-fix4-fixtures` | 42,690,823 |
| `/root/rt/audit-fix4-deps` | 21,969,496 |
| `/root/rt/audit-fix4-artifacts` | 634,518 |

Shared numeric/label dependencies and other jobs were left alone. Java 11 installed
for the deferred upstream experiment remains available to other pod jobs.

Integration must retain both package versions, the paired schema/semantics changes,
and the `number.daily` capability. Formulas require only their emitted existing
primitive capabilities. Run real-Opal e2e and the once-only release full-suite lane
there, per the coordinator decision and David's test-economy rule. An independent
Astra review remains the endorsement authority; this developer report requests
review of the scoped deliverable rather than claiming its own endorsement.
