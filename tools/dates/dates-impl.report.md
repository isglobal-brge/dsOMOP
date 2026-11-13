# A-28 / CR-29 date implementation report

**Implemented and committed**, including the colleague's native verbal-autopsy recipe end to end on DSLite. Final targeted checks are green after the fixes recorded below. Work is on `feat/2.10.3-dates` in the two requested worktrees. Both packages remain **2.10.3**. No main worktree, other job worktree or reference repository was edited; no push, PR, tag or claude CLI invocation. The approved amendment supersedes earlier inventory requests where they differ (notably implicit `today`: bind a fixed date instead).

## Commits

Server (base `766f33d`):
- `f6732a4` — Implement A28 native calendar and anchored age core with closed schema.
- `1d3393d` — Complete date output presentation, federation, staging and acceptance coverage.
- `f25f59b` — Pin native age units and mixed date policy compatibility.
- `56fec28` — Update approved assignment oracle and use production unindexed date fixture.
- `5419910` — Accept lossless bigint transport for validated integer date components.
- `3e67b41` — Exercise integer component transport through real backend compilation.
- `e3e27cd` — Refuse unsupported date slots before compiling source-less outputs.
- `1c9147b` — Keep endpoint presentation structural and apply joined policies once.
- `2ad449e` — Use observed period dates for nonmissing mixed-policy regression values.

Client (base `a5918c6`):
- `ffe1ee6` — Add A28 date constructors and verbal autopsy DSLite acceptance.
- `ad91403` — Preserve native date recipes through composition and controlled analysis.
- `064162b` — Verify custom date factor disclosure refusal and exact Date fallback.
- `6a279ca` — Pin date federation order and effective-policy digest invariants.
- `a44d77a` — Align native date capability preflight with normalized server operations.
- `bfbed85` — Compare capability inventories as sets using the established anchor operation.

## Implementation and findings

| Requirement / finding | Implementation | Executable evidence |
|---|---|---|
| D01–D14: fourteen precisions, Gregorian 0001–9999, all supported representations | Shared `R/date_semantics.R`; server private evaluation avoids SQL helpers' 1000-year lower bound; fixed English, explicit epi/fiscal profiles, signed period indices and decimal coordinates | Server `test-dates-system.R`; `test-date-system-outputs.R` low-year literals and elapsed millennium boundary on all five databases |
| D15–D19: all 24 parts, names and cyclic terms | Client bundle expansion, reserved/collision checks; integer and fixed-domain ordered factors; public phase conventions | Calendar/part fixtures, API tests, long output part broadcast, merge/code/JSON/YAML tests |
| D20–D23: safe custom patterns, labels, relative dates | Closed tokenizer/renderer; key validation, calendar ordering, public levels; independent typed source anchors | Every permitted directive, unsafe patterns and misleading ISO year refusal; numeric origins; event-relative membership test |
| D24: first/last-date migration | Explicit variable/recipe/empty spec selects the original Date; omission preserves legacy offsets and anchor rules | Existing reducer regression tests; native first/last against anchor results; unindexed native first_date |
| D25–D31: elapsed time and anchored exact ages | Signed completed anniversaries, original-day month clamps, February-29/March-1 rule, anniversary fractions, rounding and age-before-birth NA | Independent numeric/boundary fixtures; every age unit; VA death ages 43/33 and first-visit ages 39/29 |
| D32–D37: bands, incomplete/invalid/future dates | Public/custom ordered band domains; explicit no-imputation/start/mid/end; annual approximation; invalid values silently typed NA; fixed as_of | Public grids with translate_concepts=FALSE; partial fixtures; annual VA ages 44/34/24; malformed declarations and invalid component/source refusals |
| D38/D41: final presentation over typed output values | Wide/long/membership/date endpoints, survival/panel/interval covariates and joined metadata; endpoint dates added alongside unchanged duration coordinates; structurally refuse unsupported roster variables in sparse/temporal/dictionary slots | Five-backend long/membership/interval/person-period precision matrix; survival/joins; baseline/features DSLite aliases; unsupported-slot tests |
| D39: omission is distinct from explicit legacy policy | Wire omits unsupplied date_handling; normalize native policies before legacy defaults; preserve unrelated date columns; explicit overlap refuses | Hand-authored and client plans, normalization fixed points, mixed native/raw-date regression, disjoint policy list |
| D40: controlled federation | Distinct date variant in existing factor protocol; full public calendar identity including empty factors; observed metadata remains gated; shared ordered levels survive model-frame subsetting | Real two-site pooled ds.table (6,0,0,18,12), locally absent March and globally absent April/May, matching contrasts; protected ds.glm coefficients against pooled synthetic reference; high-cardinality refusal and rollback |
| D41: staged type parity | CSV reads restore closed manifest-bound primitive/Date types before reconciliation; date factors remain ordered in staged contracts | Staged/native Date, integer age and quarter parity; existing A-27 staged/federation regression tests |
| D42: closed schema, capabilities, metadata privacy | `date.presentation` and `date.elapsed`; v1 refusal; effective-policy explain/lint; typed rejection reasons; source/reference rewrites through merges and round trips | Python independent contract checker plus R WS-0 tests; v2 composition regressions and new date recipe round trips |
| D43: colleague table | `dsOMOPClient/tools/dates/va-native.R` builds and executes the complete native recipe, including translated education/fever/cause, exact ages and Date/month/quarter/year/parts | DSLite runs this exact file: 24 distinct pseudonyms, retained missing persons, expected values/types, threshold 25 refusal with no surviving output |
| Assignment date permission was inconsistently enforced | Removed only assignment-path owner checks in extraction, v1 plan execution, native output and analysis-catalog finalization. Class-1 source checks and aggregate firewall/admission code are unchanged | Updated approved before/after fixtures in `test-extraction.R`, `test-output-contracts.R`, `test-plan.R`, `test-compile-output.R`; analysis-catalog regression tests |
| A component source could be fetched as a sibling's memoized extra column | Validate the entire requested person-component column union before any fetch; event components reuse the registered typed/private catalogue | Identifier component refusals; source selection tests |
| Serialization lost date-part identity / recipe defaults | Restore expanded single-part variables, retain exact part names in generated code, emit recipe-wide date_spec | JSON/YAML/code equality and event namespace merge assertions |
| Invalid missing-event components could construct a date from constants | Mark a missing selected event as missing before component construction | Event-component NA retention case |
| Lossless bigint transport can be decimal text | Source type/privacy validation precedes private conversion; accept the DBI representation without accepting free-text sources | `test-date-component-transport.R`: real five-backend compilation with only the private integer transport representation changed |
| Unsupported date variables could fail incidentally inside dictionary compilation | Reject unsupported roster date slots before any output compilation | Format-specific typed refusals in `test-date-system-policies.R` |
| Numeric names and joined outputs could trigger inappropriate endpoint processing | Only structural interval/panel/survival formats add endpoint dates; resolve role anchors only for pending date columns | Numeric-name invariance, interval-to-wide join and multi-episode person-collapse regression |
| Client capability inventory used supplied rather than effective specs and missed elapsed-time anchors | Use the normalized variables and include from/to source anchors; compare inventory sets with the server | Date negotiation parity plus existing `test-v2-execution.R` |

The normalizer and calendar implementation are vendored identically where shared. Controlled schema generation adds the native date example, valid/refused probes, independent Python normalization and checksums. Existing non-date semantic digest vectors are unchanged relative to the base; a new native vector is added. No new DataSHIELD endpoint or patient-row transport exists.

## Validation

Only targeted files were run through `/workspace/dsomop/tools/rtest.sh`, with the matching sibling installed via `RTEST_WITH`. Vendor runs used PostgreSQL `omopdb`, MariaDB localhost and MySQL localhost:3307, plus SQLite and DuckDB. Real dsBase/dsBaseClient 6.3.5 dependencies were reused read-only from the pod's `/root/rt/labels-deps` library.

Completed development checkpoints (worktree snapshots based on `f6732a4`/`ffe1ee6` with uncommitted changes; the clean committed checks are listed separately below):
- Server `^(date-system-outputs|dates-system|ws0-schema|compile-reduce|compile-anchor)$`: **2,645 passed, 0 failed, 0 errors, 1 skipped**.
- Server `^(date-system-outputs|dates-system|ws0-schema)$`: **1,527 passed, 0 failed, 0 errors, 1 skipped** before the last additional cases.
- Client `^dates-system$`: **62 passed, 0 failed, 0 errors, 0 skipped** before the final custom-factor disclosure case.
- `python3 tools/ws0/check_contracts.py . ../dsOMOPClient`: all checks passed — 3 schemas, 2 canonical examples, 224 probes, 15 acceptance plans, 12 digest vectors, 18 normalized fixtures, 34 numbers and 11,043 corpus doubles.
- Schema generator: 213 plan/IR probes and 8 mutate probes validated.
- Documentation generated on the pod with **roxygen2 7.3.3**. Only relevant changed Rd files and NAMESPACE were pulled back. An unrelated regenerated comorbidity Rd file was not imported. Versions stayed 2.10.3.

Committed regression checkpoints:

| Checkpoint / exact files | Result |
|---|---|
| Client `ad91403` with server `1d3393d`: `test-dates-system.R`, `test-concept-label-federation.R`, `test-concept-label-analysis.R`, `test-v2-composition-review.R`, `test-ws0-schema.R` | **1,483 passed; 0 failures, 0 errors, 0 skips** |
| Client `064162b` with server `56fec28`: `test-dates-system.R` including the new custom-factor refusal | **66 passed; 0 failures, 0 errors, 0 skips** |
| Server `56fec28` with client `064162b`: `test-date-system-outputs.R`, `test-dates-system.R`, `test-compile-output.R`, `test-ws0-schema.R` | **2,333 passed**, two new test-fixture errors subsequently fixed and rerun in the policy file; one optional dependency skip |
| Server `e3e27cd` with client `064162b`: `test-date-system-policies.R`, `test-date-component-transport.R` | **40 passed; 0 failures, 0 errors, 0 skips** |
| Final server `2ad449e` / client `bfbed85`: `test-date-system-policies.R`, `test-date-component-transport.R` | **46 passed; 0 failures, 0 errors, 0 skips** |
| Final client `bfbed85` / server `1c9147b`: `test-dates-system.R`, `test-v2-execution.R` | **110 passed; 0 failures, 0 errors, 0 skips** |

The broader server development run covered exactly `test-date-system-outputs.R`, `test-dates-system.R`, `test-compile-output.R`, `test-extraction.R`, `test-output-contracts.R`, `test-plan.R`, `test-staging-csv.R`, `test-staging-contract.R`, `test-analysis-catalog.R` and `test-ws7f-membership.R`. It ran on an uncommitted development snapshot based on `f6732a4`/`ffe1ee6`, so it is recorded as development evidence, not attributed retroactively to a clean commit. It reported **5,943 passed**, one obsolete absolute-date expectation and two new fixture errors. Those failures were corrected and rerun; no product-runtime failure from that run is left outstanding. Its Parquet failure-cleanup test skipped because Arrow is unavailable on the pod. Staged CSV parity is exercised; Parquet runtime parity is not claimed.

Development failures were retained as findings (staged CSV types, recipe code generation, capability inventory and structural endpoint presentation). The report distinguishes failed intermediate runs from their passing targeted replacements. Logs and a source hash manifest are committed in `tools/dates/evidence/`. Full detailed R logs remain on the pod; local retained summaries include failed intermediate checkpoints rather than concealing them.

## Scope, deviations and handoff

- Test organization is consolidated into server `test-dates-system.R`, server `test-date-system-outputs.R`, `test-date-system-policies.R`, `test-date-component-transport.R` and client `test-dates-system.R`, with D01–D43 labels, rather than the eight suggested filenames. The existing direct-dependency tests supply anchor, reducer, composition, publication, staging and federation regressions.
- The test-economy instruction in COMMON.md overrides its older full-suite sentence. No full suite was run; integration/release owns that pass.
- No implicit site clock was added. Fixed reference dates cover census/mid-year/today-bound-in-plan use cases under the approved amendment.
- Formats with no roster variable slot produce a typed refusal naming the format and the wide typed companion route. Dates/factors are not encoded as undocumented sparse numeric cells. Existing numeric sparse/reference components remain intact.
- Direct inspection of synthetic server frames occurs only in test harness assertions and the local reference GLM. The colleague snippet returns server symbols; analyses use the existing protected DataSHIELD routes.
- This is developer implementation evidence, not an independent implementation endorsement or integration authorization. No independent reviewer session, coordinator disposition, merge or release was fabricated.
- Local disk started and remained at approximately 9.7 GiB free. No large local scratch package copies were created; tests and documentation ran on the pod. No safe multi-gigabyte cleanup in this job's authorized paths was found. Removed six generated `.pyc` files from this job's `dsOMOP/tools/ws0/__pycache__/` (70,173 bytes; 84 KiB allocated), then its empty directory. Exact paths and sizes are in `tools/dates/evidence/cleanup.json`. Automatic review rejected a force-recursive removal command; cleanup instead checked the exact directory and each regular `.pyc` file before individually unlinking it. Evidence files are retained.

The requested report is `/workspace/dsomop/design/astra-dev/dates-impl.report.md`; a committed copy lives in server `tools/dates/dates-impl.report.md`. Design/astra-dev is outside either package's Git worktree.

## Final handoff

All requested implementation changes, documentation, schema artifacts, tests, the executable VA snippet and this report/evidence copy are committed. The final report commit changes documentation/evidence only; its parent is server `2ad449e`. Client implementation/test HEAD is `bfbed85`. Both worktrees are checked clean after the report commit. The next process step is independent implementation review and the integration/release job's full pass; neither was substituted for or claimed by this development job.
