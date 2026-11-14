# A-31 audit implementation report — incomplete

14 November 2026. **The requested complete A-31 implementation is NOT finished.
Do not endorse this branch as satisfying CR-32/A-31 or release 2.10.5.**
The independent parser/setting corrections and a subset of the physical-result
identity corrections are implemented, tested and committed. The acceptance
ledger below explicitly retains every other obligation.

## Scope and source state

Only `/workspace/dsomop/wt/audit/dsOMOP` and
`/workspace/dsomop/wt/audit/dsOMOPClient` were changed, on `feat/2.10.5-audit`.
Starting heads were server `18b096f` and client `397a71b` (released 2.10.4).
Both trees were clean; there were no job-local WIP commits to retain/revert.
The requested design report/evidence paths were also written.

Governing documents: COMMON.md (including the later targeted-test-only rule),
CR-32 and A-31, with the independent review's exact edits. A-31 still calls
coordinator disposition pending in its text; the user's explicit implementation
instruction was treated as authorization to implement its reviewed corrections,
not as implementation/release certification. No approval was requested for the
reversible fixes already within that instruction.

The audit branches do not contain A-29 numeric primitives, A-30 codebooks and
categories, or the 24-entry library implementation. I asked for approved
prerequisite revisions; no answer arrived during this run. Available A-29 and
library reports have endorsements, but the latest A-30 review is BLOCKED:
`design/2.10.3/astra-cats-review-2.md`, R4 (output-local codebook selection/order)
and R5 (category-reduction copies). No moving prerequisite branches were merged,
and no other job's worktree was modified. This prevents claiming acceptance of
those dependent A-31 routes. It does **not** explain away the independent
unfinished obligations: those also remain unimplemented, as listed below.

## Commits

Server implementation head: `abe6e6e1c7ee03c3af160a3f01e4e90ed50245d9`.

- `bdbc5be`: authoritative setting validation before scientific execution,
  normalized omitted/explicit zero, catalogue descriptions/choices, tests,
  NEWS and version 2.10.5.
- `20f9eb6`: mandatory physical identity columns/types, explicit CM lookup
  refusal, private row identity and negative-count checks, tests and pinned
  upstream structural fixture.
- `abe6e6e`: reject qualified invalid settings before catalogue discovery,
  strict scalar transport, independent retained-estimand fixture and public
  `AUDIT_CORRECTIONS.md`.

Client tested implementation head: `b980ebec5224075547b4261d00901886c048c8fd`.
Final client head (documentation only afterward): `64947ecd806efaf91e2ad6e047a10bc000a86d3f`.

- `7bd3fc6`: shared strict OHDSI item parser, both public importer paths,
  duplicate-key checks, original-type validation, tests and corrected Death comment.
- `a04d2e2`: matching client setting validation in execution/generated calls,
  tests, NEWS and version 2.10.5.
- `b980ebe`: explicit scalar/array boundary tests, matching integer-wire
  normalization, corrected Base64 test decoding and public correction notes.

- `64947ec`: document parsed-list input and strict ID/flag semantics in source
  help and the two Rd files regenerated with roxygen2 7.3.3 on the pod.

A following documentation-only server commit archives this report and evidence;
its hash is recorded in the requested external report after committing.
All commits use explicit pathspecs. No attribution trailers, push, PR, tag,
stash, sub-agent or Claude CLI invocation occurred.

## Implementation and acceptance ledger

“Implemented” below describes the named correction, not a release endorsement.
A partial row is not closed by adjacent source tests or documentation.

| ID | Status / fix and remaining obligation | Test / evidence |
|---|---|---|
| Q01 | Implemented strict shared ID validation before conversion, deduplication or importer subset rules. Both importers reject malformed originals and duplicate object keys; 0/201826/2147483647 survive exactly. Unreferenced cohort definitions are checked too. | `test-audit-concept-import.R`, `test-recipe-circe.R`, `test-v2-plan.R`, `test-v2-review.R` |
| Q02 | Implemented shared Boolean presence validation: absent=false; present null/vector/NA/text/numeric/array invalid. Registry uniform include/exclude groups and Circe refusal are preserved. | Same importer tests; malformed excluded item is tested before partitioning. |
| Q03 | Partial: corrected Death comment and added list entry to strict cohort import. Support/check APIs, complete multi-issue diagnostics, provenance lifecycle and pinned PhenotypeLibrary/upstream cohort oracles are **not implemented**. | Comment plus parser tests only; no claim of upstream cohort parity. |
| Q04 | Runtime correction implemented: only omitted/zero clean_window, same effective request, authoritative validation before snapshot/key/patient work, canonical aliases and client generated-call validation. Labels and a private first-episode oracle added. Full pinned protected-result/published-route acceptance remains outstanding. | `test-audit-settings.R` in both packages, server `test-analysis-catalog.R`, client `test-analysis-client.R`; oracle: 40 people, 400 days, 20 people with 40 first-episode events, later episodes excluded. |
| Q05 | Runtime correction implemented: only omitted/zero time_windows and explicit all-history meaning. Private fixture proves outside-period events remain included. The required actual bounded-native control/published protected-workflow comparison is outstanding. | Same setting tests; December-only concept among 40 cohort members has retained prevalence 1 despite January/February cohort dates. |
| Q06 | Partial: required identity columns/types before optional completion and pooling discovery; generic invalid/missing row-key refusal; CM lookup identity refuses before result reads; unknown negative count encoding refuses. Full version/fingerprint/status manifest, reference-association/uniqueness checks, typed optional completion and actual old/current upstream export acceptance remain outstanding. | `test-audit-results-identity.R`, `test-ohdsi-results.R`, `test-ohdsi-small-support.R`, `test-analysis-pooling-contract.R`. Pinned CM 6.0.3 **schema**, not an actual exported result. |
| Q07 | Not implemented: covariate namespaces, semantic joins and external BIGINT codebook lifecycle. | None added. |
| Q08 | Not implemented: recorded/native era codebook declarations and boundary acceptance. | None added. |
| Q09 | Not implemented: study roles and full TAR codebook contract. | Incidence description only does not close this row. |
| Q10 | Not implemented: named long_term/medium_term/short_term inclusive and pre_index native windows, observation clipping and repeated-episode acceptance. | None added. |
| Q11 | Not implemented: versioned executable imported-vocabulary validity policy, capability negotiation and lifecycle tests. Imports still use existing expansion semantics. Strict input parsing does not certify OHDSI expansion parity. | None added. |
| Q12 | Not implemented: full 5.3/5.4 roles/episode/death/coverage declaration and acceptance lane. | None added. |
| Q13 | Partial: incidence/FE discovery descriptions corrected. Full per-route fidelity contracts and result/codebook metadata for the other named families are absent. | Existing catalogue tests and setting discovery assertions only. |
| Q14 | Not implemented: four formula builders and their actual-plan numerical/lineage oracles. Requires integrated A-29/A-30 primitives. | None added. |
| Q15 | Not implemented: number_daily wire/schema/IR/compiler/capability and acceptance. | None added. |
| Q16 | Not implemented: runnable baseline/six-month comparison. | None added. |
| Q17 | Not implemented: runnable observation-fraction/count example. | None added. |
| Q18 | Not implemented: executable episode-scoped previous-day/current-day rule. | None added. |
| Q19 | Not implemented: pre-reduction plausibility variants. | None added. |
| Q20 | Not implemented: integer-scale/category example. | None added. |
| Q21 | Not implemented: published ds.getWGSR/reference workflow. | None added. |
| Q22 | Not implemented: executable cross-source scalar fallback example. | None added. |
| Q23 | Not implemented: pinned actual published downstream-consumer workflow matrix. | None added. |
| Q24 | Not implemented: study codebook comparison example. | None added. |
| Q25 | Not implemented: pooling lesson and its independent acceptance. | None added. |
| Q26 | Not implemented: study-bundle constructor/schema/serializer/privacy acceptance. | None added. |
| Q27 | Not implemented: versioned real-interview VA capstone. | None added. |
| Q28 | Not implemented: integrated 24-entry library reconciliation/execution. | No library branch merged. |
| Q29 | Not implemented: full question-to-route guide and executable call checks. | `AUDIT_CORRECTIONS.md` is limited to implemented corrections. |
| Q30 | Partial: bounded new parser/setting/schema reasons and remedies, no offending value echoed; complete new-route explain/codegen and all refusal classes remain absent. | Import sentinel, malformed setting tests, schema/no-result-read tests; generated-call normalization. |
| Q31 | Not implemented: native-v2 book/render/exercise/two-site acceptance. | None added. |
| Q32 | Partial: both package versions now 2.10.5. Fresh/upgrade/profile/restart smoke remains outstanding. | DESCRIPTION comparison only. |
| Q33 | Not certified: changes add no answer-changing state, privacy configuration, TLS requirement or support threshold; the requested deployed-boundary/consumer acceptance remains outstanding. | Source review and targeted regressions only. |
| Q34 | Not implemented: exact minor-unit cost example/acceptance. | None added. |
| Q35 | Partial supporting artifact: CM 6.0.3 structural fixture pinned. Actual Eunomia/dataset/CDM-version lane and expanded 5.4 edge cases are absent. | Schema fixture manifest only; does not close Q35. |

## Targeted validation and exact snapshots

All R runs used `/workspace/dsomop/tools/rtest.sh` on the pod with a nonempty
filter and `RTEST_WITH` set to the matching audit sibling. No local R test run
or full suite was executed. Import help was regenerated on the pod with an
explicit roxygen2 == 7.3.3 assertion, using load_source. Only the two changed
Rd files were pulled back; NAMESPACE/DESCRIPTION had no generated changes.
Evidence: `audit-impl-roxygen.log`. No 8.x invocation occurred. The client final runs used
`RTEST_ENV='R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps'` to make the
already-installed plot dependencies available. These are SQLite/local synthetic
and client tests; no new vendor/Opal/Parquet execution is claimed.

| Run | Source snapshot | Exact files selected | Outcome |
|---|---|---|---|
| Client import | Precommit parser tree recorded by `7bd3fc6`, sibling precommit `bdbc5be` | `test-audit-concept-import.R`, `test-recipe-circe.R`, `test-v2-plan.R`, `test-v2-review.R` | 475 pass, 0 failures/errors/skips |
| Server settings/catalogue | Precommit tree recorded by `bdbc5be`, client parser/setting tree later recorded by `7bd3fc6`/`a04d2e2` | `test-audit-settings.R`, `test-analysis-catalog.R` | 498 pass, 0 failures/errors/skips |
| Client settings | Precommit tree recorded by `a04d2e2`, server `bdbc5be` | `test-audit-settings.R` | 100 pass, 0 failures/errors/skips |
| Client analysis development | `a04d2e2`, server `bdbc5be` | `test-analysis-client.R` | 133 pass, 1 error, 7 missing-ggplot2 skips. Error was test-only unpadded Base64 decoding after adding normalized time_windows. |
| Client analysis repair | Tree later recorded by `b980ebe`, server result-fix working tree | `test-analysis-client.R` | 154 pass, 0 failures/errors/skips; test restores Base64 padding and asserts time_windows=0. |
| Server results development | Tree later recorded by `20f9eb6`, before physical-type checks, client `a04d2e2` | `test-audit-results-identity.R`, `test-ohdsi-results.R`, `test-ohdsi-small-support.R`, `test-analysis-pooling-contract.R` | 1817 pass, 1 error, 0 skips. Existing missing-key test expected the later pooling error; updated to assert the new earlier schema refusal. |
| Server results final | Product tree recorded by `20f9eb6` plus early-discovery guard/tests later recorded by `abe6e6e` (before added scientific/scalar cases) | `test-audit-results-identity.R`, `test-ohdsi-results.R`, `test-ohdsi-small-support.R`, `test-analysis-pooling-contract.R`, `test-audit-settings.R` | 1933 pass, 0 failures/errors/skips |
| Server estimand | `20f9eb6` plus working tree later recorded by `abe6e6e`, before added decimal-wire/array cases | `test-audit-settings.R` | 122 pass, 0 failures/errors/skips |
| Server decimal transport | Same working tree, before final matrix case | `test-audit-settings.R` | 130 pass, 0 failures/errors/skips |
| **Client final** | **`b980ebe` / server `abe6e6e` source trees** (commit made during immutable run) | **`test-audit-concept-import.R`, `test-audit-settings.R`, `test-recipe-circe.R`, `test-v2-plan.R`, `test-v2-review.R`, `test-analysis-client.R`** | **749 pass, 0 failures/errors/skips** |
| **Server final scalar/estimand** | **`abe6e6e` / client `b980ebe`** | **`test-audit-settings.R`** | **134 pass, 0 failures/errors/skips** |

Final client filter:
`^(audit-concept-import|audit-settings|recipe-circe|v2-plan|v2-review|analysis-client)$`.
Final results filter:
`^(audit-results-identity|ohdsi-results|ohdsi-small-support|analysis-pooling-contract|audit-settings)$`.
Final server scalar/estimand filter: `^audit-settings$`.
Earlier catalogue filter also contained names with no matching files; only the
two exact files listed above ran. Counts overlap; do not sum them as unique tests.
The earlier passing catalogue/result dependents were not repeated after changes
limited to scalar validation/tests/documentation. There are no unresolved test
failures in the final applicable runs; that is not complete A-31 acceptance.

Summary and full logs are `audit-impl-*.log` alongside this report. They are
also archived in the server's `tools/audit-impl/evidence/`. `git diff --check`
passed for both packages. The two `R/audit_settings.R` files are byte-identical.

## Fixture provenance

`tests/fixtures/ohdsi-audit/manifest.json` pins CohortMethod 6.0.3 to
`dd1a2a856ef608547a99d3db2d60d5c872f80dc6`, with the original and derived SHA-256.
Source: https://raw.githubusercontent.com/OHDSI/CohortMethod/dd1a2a856ef608547a99d3db2d60d5c872f80dc6/inst/csv/resultsDataModelSpecification.csv
Only structural fields for cm_result/cm_target_comparator were retained. The
test creates empty SQLite tables from those actual upstream field definitions
and proves refusal before result reads. It is deliberately not labelled an
actual upstream exported patient-result fixture.

## Deviations, handoff and disk

The major deviation is **incomplete implementation of the requested amendment**.
The available audit base predates its prerequisites and A-30 currently has
blocking review findings. An integrated, corrected prerequisite pair is needed
for dependent work; independent unfinished Q03/Q06/Q11/Q13 work must also be
completed. There is no claim that only integration or a final full suite remains.
Do not treat this report's passing tests as authorization to ship the missing APIs.

Another job must preserve the shared strict parser and effective-setting checks
when integrating numeric/category/library changes, including NEWS/DESCRIPTION
conflicts. Do not drop the Q11 executable validity policy merely because Q01/Q02
are corrected. Retain the strict physical-reader identity checks when adding
schema/version manifests and adapters. `AUDIT_CORRECTIONS.md` describes the
actual implemented scope and migration, not all proposed 2.10.5 functionality.

Both packages are version 2.10.5. Full suites, fresh/paired upgrade and actual
published-method acceptance remain integration/release work, after implementation
is complete. No protected feature was replaced with a raw-output workaround.

The local shared disk was 9.7 GB free at start/end. CLEANUP.md was read; no large
local package copy/full-suite run occurred and no unrelated files were deleted.
After preserving all evidence, the following **own pod mirrors/private libraries**
were deleted (bytes measured immediately beforehand):

| Deleted path | Bytes |
|---|---:|
| `/root/rt/wt/audit/dsOMOP` | 24,202,719 |
| `/root/rt/wt/audit/dsOMOPClient` | 15,094,107 |
| `/root/rt/lib/wt_audit_dsOMOP` | 5,153,337 |
| `/root/rt/lib/wt_audit_dsOMOPClient` | 11,171,525 |
| `/root/rt/wt/audit/dsOMOPClient` (recreated for documentation, removed again) | 15,095,242 |

Shared dependency libraries, runner logs and every other job's paths were kept.
No local scratch package copies were created. Cleanup evidence:
`audit-impl-cleanup.log`.
