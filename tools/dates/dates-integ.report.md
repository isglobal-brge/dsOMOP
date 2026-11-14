# Dates integration report

13 November 2026. **Completed:** merged `feat/2.10.3-dates` into `feat/2.10.3` in both requested integration worktrees. All selected test files are green after focused expectation repairs. Both packages remain **2.10.3**. No runtime changes were needed beyond the endorsed dates implementation.

## Commits and merge resolution

| Package | Commit | Purpose |
|---|---|---|
| dsOMOP | `0649f1a6a6a2871e570f23df19ce7e034c8a2c73` | Merge endorsed dates tip `a620dd74b71d0471a5938492374a82faba6f4fd8` into integration base `766f33dda88dbf4298809069790b70a678fb1659` |
| dsOMOPClient | `7188cde5d0c7d553bf07f3da6bbd9fea5eac7044` | Merge endorsed dates tip `da95946096120854836191806c82f91db7528f17` into integration base `a5918c60985418eb4e50fb01a08ddb6bd5ce5264` |
| dsOMOP | `ee92a528cc8e07884f4fe79aa38f8d09109f0419` | Update capability inventory and staged-type regression expectations |
| dsOMOPClient | `30a9794b4fe30c7b04f30390686157bf67ff4a63` | Update A-27 constructor argument-order assertions for appended `date_spec` |

Both integration branches were clean direct ancestors of their dates branches. Explicit two-parent merges completed without conflicts; their trees exactly matched the endorsed source tips. There were no incoming WIP commits. Round-three endorsement was checked in `design/2.10.3/astra-dates-review-3.md`. The later integration corrections change only four test files. Shared `R/date_semantics.R` files remain byte-identical (SHA-256 `2dbfa538e46949774baad7a4f56c41ff00cb1cebfbc47f163c3254642e47db12`).

This report and evidence are also retained in the dsOMOP commit titled **Record dates integration results and targeted test evidence**, under `tools/dates/dates-integ.report.md` and `tools/dates/integ-evidence/`.

## Findings mapped to fixes and tests

| Finding | Fix | Validation |
|---|---|---|
| Server capability-count assertion still expected 44 operations; the independent SQL-operation fixture omitted the two approved date operations. | Expect 46 operations, explicitly require `date.presentation` and `date.elapsed` in advertised capabilities, and extend the independent operation inventory. Runtime capability code is unchanged. | `test-explain.R`, `test-sql-expr.R`; final server rerun passed. |
| Three staged-publication assertions expected CSV character keys even though A-28 now restores manifest-bound source types. | Assert the original numeric/source type and retain explicit canonical decimal lineage comparisons for both frame/SQL paths and DuckDB BIGINT keys. Existing support, publication, and descriptor checks remain. | `test-publication-compiled.R`; final server rerun passed. |
| Two A-27 client assertions treated the label arguments as the final three arguments, preceding A-28's appended `date_spec`. | Pin the final four arguments, preserving the existing positions of `broadcast`/`publish`, `labels`, and `column_names`. | `test-concept-label-api.R`; final client rerun passed. |
| Prior date review blockers B1–B4 must survive integration. | Merged the endorsed future-cutoff, chronological custom-calendar, escaped-percent, full-range and empty-rendering fixes unchanged. | Server/client `test-date-review-fixes.R` and `test-date-range.R` passed in the initial targeted runs. |
| Native date behavior must retain surrounding publication, staging, composition and legacy behavior. | Merged both packages together; no integration runtime edits. | All six server date files, three client date files, and the selected direct-dependent files below passed, except the stale expectations repaired above. Includes full VA recipe, threshold refusal/rollback, two-site factors, staging and capability negotiation. |

## Executed validation

All R tests used `/workspace/dsomop/tools/rtest.sh` on the test pod, with the matching sibling installed through `RTEST_WITH`. Both runs used `R_LIBS_USER=/root/rt/labels-deps`. Server runs enabled PostgreSQL `omopdb`, MariaDB `127.0.0.1`, and MySQL `127.0.0.1:3307`, alongside SQLite and DuckDB fixtures. Initial timeouts were 1,500 seconds; focused reruns used 900 seconds. Exact filters, environments, source commits and filenames are in `integ-evidence/run-config.json`.

| Run | Server / client source commits | Result |
|---|---|---|
| Initial server, 29 files listed below | `0649f1a` / `7188cde` | 9,603 passed; 5 failed assertions; 0 errors; 2 skips. All failures were the stale expectations mapped above. |
| Initial client, 24 files listed below | `0649f1a` / `7188cde` | 3,922 passed; 2 failed assertions; 0 errors; 0 skips. Both failures were the argument-order expectations mapped above. |
| Final server: `test-explain.R`, `test-publication-compiled.R`, `test-sql-expr.R` | `ee92a52` / `30a9794` | **2,135 passed; 0 failures, errors or skips** |
| Final client: `test-concept-label-api.R` | `ee92a52` / `30a9794` | **36 passed; 0 failures, errors or skips** |
| `PYTHONDONTWRITEBYTECODE=1 python3 tools/ws0/check_contracts.py . ../dsOMOPClient` | `0649f1a` / `7188cde` | **All checks passed:** 3 schemas, 2 examples, 224 probes, 15 acceptance plans, 12 digest vectors, 18 normalized fixtures, 34 numbers, 11,043 corpus doubles |

Only the four test files changed after the initial runs, so reruns were limited to those files. Runtime and schema artifacts remain identical to the initial tested merges. These are targeted results, not a full-suite claim.

The two initial server skips were `test-plan.R`'s Parquet failure-cleanup case (Arrow unavailable) and `test-ws0-schema.R`'s Python oracle (Python unavailable on the pod). The independent Python contract checker passed locally. Parquet runtime coverage is not claimed.

The initial client run emitted 27 warnings: 16 synthetic GLM nonconvergence warnings and 11 absent-symbol cleanup warnings in deliberate refusal/rollback tests. Their assertions passed; these warnings are retained in the detailed log. No rerun warnings were reported.

### Exact initial server files

- `test-analysis-catalog.R`
- `test-assignment-policy.R`
- `test-compile-anchor.R`
- `test-compile-interval.R`
- `test-compile-output.R`
- `test-compile-reduce.R`
- `test-concept-label-contract.R`
- `test-concept-label-presentation.R`
- `test-date-component-transport.R`
- `test-date-range.R`
- `test-date-review-fixes.R`
- `test-date-system-outputs.R`
- `test-date-system-policies.R`
- `test-dates-system.R`
- `test-disclosure.R`
- `test-explain.R`
- `test-extraction.R`
- `test-factor-harmonization.R`
- `test-ir.R`
- `test-output-contracts.R`
- `test-plan.R`
- `test-publication-compiled.R`
- `test-publication-integration.R`
- `test-publication.R`
- `test-sql-expr.R`
- `test-staging-contract.R`
- `test-staging-csv.R`
- `test-ws0-normalize.R`
- `test-ws0-schema.R`

### Exact initial client files

- `test-concept-label-analysis.R`
- `test-concept-label-api.R`
- `test-concept-label-contract.R`
- `test-concept-label-federation.R`
- `test-concept-label-legacy.R`
- `test-date-range.R`
- `test-date-review-fixes.R`
- `test-dates-system.R`
- `test-factor-harmonization-client.R`
- `test-plan-dsl.R`
- `test-recipe-declarative.R`
- `test-recipe-schema.R`
- `test-recipe.R`
- `test-staged-harmonization-regression.R`
- `test-v2-composition-review.R`
- `test-v2-constructors.R`
- `test-v2-editor-roundtrip.R`
- `test-v2-execution.R`
- `test-v2-explain-lint.R`
- `test-v2-lossless-transport.R`
- `test-v2-merge.R`
- `test-v2-plan.R`
- `test-ws0-normalize.R`
- `test-ws0-schema.R`

## Evidence, deviations and handoff

Evidence includes initial and rerun summary/detail logs, contract results, exact run configuration, merge parents/tree IDs, and final tested source state. The initial state proves the merge trees matched the endorsed branches; final state proves that only the named tests changed afterward. `SHA256SUMS` covers the evidence files.

- The user's explicit targeted-only instruction takes precedence over COMMON.md's integration/full-suite wording. No full suite was run; any final release-wide full pass remains outstanding outside this task.
- Git does not allow a pathspec-limited partial commit during a merge. The two merge commits therefore committed their complete, conflict-free merge indexes without pathspecs. The repair and evidence commits use explicit pathspecs.
- Merge whitespace checking identified pre-existing trailing whitespace in verbatim evidence logs and one blank EOF line in `test-date-system-outputs.R`. These were retained to preserve endorsed source and historical evidence. New test changes pass `git diff --check`.
- Free space was 9.7 GiB before and after the work. No large local copy, full suite or scratch package was created. CLEANUP.md was read; nothing was deleted, and other jobs' material was untouched. Retained integration evidence is small (about 0.3 MiB).
- No documentation regeneration, version change, attribution trailer, push, PR, tag, stash, sub-agent or claude CLI invocation occurred. Writes were confined to the two named integration worktrees and the requested design report.

Downstream work should take both integration heads together and retain the updated regression expectations. The dates source branches are unchanged; the four expectation repairs live on `feat/2.10.3`. There are no unresolved integration findings. Release-wide validation and unavailable Parquet coverage are not implied by this report.
