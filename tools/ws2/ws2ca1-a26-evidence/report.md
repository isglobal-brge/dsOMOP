# WS2ca1 fix report — A-26 implementation, 12 November 2026

This report supersedes the previous prerequisite-only WS2ca1 report. Review-a3 B1 is implemented under approved amendment A-26 (CR-27). N1/N2 remain non-blocking and outside the requested blocker-only scope.

## Authorization and scope

Read COMMON.md, the common workstream brief, the WS-2 review/re-review and WS-2c review chain through a3, CR-27, A-26 and preceding fix reports. CR-27 now records independent Astra review plus coordinator approval. A-26 expressly replaces the old proposed non-null/nonnegative contract with signed R integers or present JSON null. The old pending patch was reviewed and revised to that approved contract; it was not applied unchanged.

Worked in the assigned `feat/2.10.3-ws2c` pair. Entry heads were server `cdd133a4498410afa5a9be0ca24437fb09861357` and client `ed8f1e261e81c0e0a2d8ffc20865821cafd30f56`. Integration heads server `c7e541e4bac659cdc32358d14b1b4791c2e4203e` and client `c8b337b77b09d5ed219215ddf205652193354220` were already ancestors; no merge was necessary. Existing WS-2c WIP result fixtures were retained after inspecting their provenance and subsequent independent review evidence; no new WIP implementation was adopted.

## Finding → fix → test

| Finding | Fix | Verification |
|---|---|---|
| B1: membership labels missing from semantic IR | Lower every membership output's effective root label to closed IR-only `options.legacy_cohort_definition_id`, independent of its rows population. Distinguish integer, present null (failed scalar coercion), and absence. | `test-ws2f-membership-results.R`: distinct complete IR/digest/explain/closures for IDs 1/2; signed/zero/null/absence distinctions; coercion-equivalent forms; independent female population and unchanged selection. |
| B1/A-26: historical scalar success/refusal and diagnostics | Restrict inline-spec creation checks to its actual dispatch; retain results-cohort selector rules. The membership adapter consumes the IR value, restores present null to integer NA and replays failed raw coercion for its original warning at the extraction point. | Same file compares complete result frames and validation/execution diagnostics with 14 records from the independently approved pristine `30b7905` probe. Existing pinned WS-2f baseline records remain unchanged and are also replayed. |
| B1: frozen shape and native-wire closure | Regenerate IR schema, positive/negative probes and checksums. Accept only R-range integers or null on membership; native wire remains byte-identical. Vendor shared probes/checksums, excluding the compatibility schema. | Independent Python oracle; server/client `test-ws0-schema.R`; wrong type/range/format/native-wire checks in the membership test. |
| B1: corpus consistency and dependent regressions | Regenerate and audit affected IR golden records; preserve earlier repaired behavior. | Targeted nine-record membership subset of the corpus golden check, `test-ir-review.R`, `test-legacy-execution-call.R`. |
| Earlier B2/B3 | No repair needed; a3 independently confirms them repaired. | Relevant dependent lowerer/validation controls in `test-ir-review.R`; no claim of a new independent review of all previous findings. |
| a3 N1/N2 | Unchanged, non-blocking. | No tests or repair claimed for those findings. |

The new baseline text fixture is extracted verbatim from the approved independent CR-27 log, including row order, attributes, names, types and coercion warnings. Its provenance records the original baseline commit and source-log SHA-256. It is not a newly generated expectation from the implementation being tested.

## Validation

Implementation commits:

| Package | Commit | Change |
|---|---|---|
| Server | `5c6fd9a` | A-26 lowerer, incoming validation, membership adapter, schema/generator/probes and baseline/semantic tests. |
| Server | `62fd418` | Audited membership IR golden updates and NEWS. |
| Client | `4f1ef7b` | Shared probes/checksums, without compatibility schema. |
| Server | Commit titled `Record completed A-26 membership fix and targeted verification` | This report, exact selections, static checks and retained test evidence. |

All R tests ran through `/workspace/dsomop/tools/rtest.sh` with the matching sibling installed through `RTEST_WITH`. No full suite ran.

| Package / tested commit | Exact test files | Passed | Failed/errors | Skipped |
|---|---|---:|---:|---:|
| Server `5c6fd9a`, client `4f1ef7b` | `test-ir-review.R`, `test-legacy-execution-call.R`, `test-ws0-schema.R`, `test-ws2f-membership-results.R` | 1,152 | 0 / 0 | 1 |
| Client `4f1ef7b`, server `62fd418` | `test-ws0-schema.R` | 150 | 0 / 0 | 0 |
| Server `62fd418`, client `4f1ef7b` | `test-ws2ca1-membership-golden.R` (temporary targeted probe retained here) | 37 | 0 / 0 | 0 |

Total: **1,339 passed, zero failed/errors, one skipped**. The skip is the optional in-R Python oracle (Python is unavailable on the pod); the standalone local oracle passed: three schemas, two examples, 214 probes, 15 acceptance plans, 12 digest vectors, 17 normalized fixtures, 34 numbers and 11,043 corpus doubles. The oracle ran against implementation server `5c6fd9a` / client `4f1ef7b`; subsequent server changes affect only the audited golden fixture, NEWS and evidence. R results, not the launcher's exit status alone, determine success.

The initial server and client runs supplied `DSOMOP_TEST_POSTGRES_HOST=omopdb DSOMOP_TEST_MARIADB_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_PORT=3307`. The pure golden probe needed no database environment. Filters, exact commit hashes and selections are recorded in `test-selection.json`; summary and complete logs are retained beside it.

The pod regenerated all 477 golden entries. A structural audit found only five changed records (`v1-027`, `v1-028`, `v1-364`, `v1-365`, `v1-366`): each adds the effective membership label 1 and updates its digest. All outcomes, other IR fields and annexes are identical. The targeted probe verifies all nine membership records (37 assertions); the entire unrelated corpus test file was not rerun. See `golden-audit.json`.

Final static checks confirm both integration heads remain ancestors, equal package versions, byte-identical shared normalizers/serializers/native wire/probes, unique generated probe IDs, and no client compatibility schema or checksum entry. `git diff --check` passes in both worktrees.

## Handoff and deviations

- A-26 supersedes review-a3's older nonnegative/non-null prescription; signed integers and present null are intentional, approved changes.
- The historical membership path now consumes the IR label; this is not acceptance of a complete future native/compatibility population-output compiler. WS-7 must preserve the option strictly as a released label, never as a population selector. WS-0 should verify regeneration at integration.
- Shared normalizers are unchanged. No default/null normalization workaround is needed: the existing normalizer preserves explicit null properties and the new schema has no default. Native wire SHA-256 remains `b762bed6aa93b64ee174e17a852bc6b873b8b939bb95dd3a73e65b0cdad529aa`. Both package versions remain 2.10.2.
- Full suites and the vendor acceptance matrix were not run; integration/release owns them. Vendor environment settings do not turn SQLite fixture regressions into vendor acceptance.
- Disk was checked (9.8 GB free); CLEANUP.md was consulted. No large copy, disposable package snapshot or unrelated cleanup was performed. The temporary golden probe was removed from the local/pod test directories after its identical evidence copy and logs were saved; paths and byte counts are in `cleanup.json`. Tests used the assigned worktree mirrors and private pod libraries managed by rtest.sh.
- No Claude CLI, push, PR, tag, stash, new TLS requirement, quotas, privacy accountant or persistent answer-changing state was introduced.
- The requested design report is outside Git; an identical copy is committed here. Historical prerequisite-only reports and unapplied patch evidence remain historical records.
