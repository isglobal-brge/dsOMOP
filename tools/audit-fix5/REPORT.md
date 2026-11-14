# Audit fix 5 — 2.10.5

Status: R6 implemented and targeted validation passed; ready for independent review.

## Authority and scope

Applied `/workspace/dsomop/design/astra-dev/COMMON.md`, its later targeted-test-only
rule, `brief-ws-common.md`, and `CLEANUP.md`. This job addresses R6, the sole blocking
finding in `design/2.10.3/astra-audit-review-5.md`. The accepted prior findings and
the coordinator's deferred/integration scope remain unchanged.

Both named audit worktrees are on `feat/2.10.5-audit`; both packages remain 2.10.5.
Starting commits: server `a19c6369710b7ccdd0da41de942abae52eb68a8e`, client
`b6418a0263b66dfe53e59716beeafbaa716e290d`. Both worktrees were clean at entry;
there were no new WIP commits after those independently reviewed heads.

## Finding, fix and tests

| Finding | Repair | Regression evidence |
|---|---|---|
| R6: an input-local creatinine source-unit map can change the selected measurement while age still uses the original event | Client `.formulaAgeCheck()` now checks effective `event_unit` pipeline steps against the age anchor event's unit map, in addition to existing event/selector/scope/window checks. A mismatched local map raises `omop_formula_error`, reason `formula_age_anchor`, path `/age`, with an actionable instruction to move source normalization onto the shared event and keep numeric coordinate conversion on the creatinine input. | Client `test-audit-formula-portability.R` checks refusals for first/last/nth and the structured error/remedy; accepts shared-event normalization, a redundant identical local map, and safe day-zero/index age inputs. |
| R6: birthday and adult eligibility must follow the selected normalized measurement | No server execution semantics change. Both NEWS files classify the corrected/newly refused behavior and give the migration path. Client roxygen help documents the shared-map requirement. | Server `test-audit-formula-edges.R` compiles and executes the selection-changing two-date fixture for first/last/nth. It demonstrates the original age fork, asserts constructor refusal, and executes shared-event positive controls with and without a redundant local map. Selected ages are 40, 18 and 17: independent power-equation expectations are `142 * .9938^40`, `142 * .9938^18`, and NA. The last-selector case explicitly prevents erroneously admitting a selected underage measurement using a later unnormalized anchor. |
| R6: preserve conversions and correctly aligned formulas | Fixed numeric coordinate conversions remain admitted; identical shared source maps remain admitted despite normalized event-ID forks. Day-zero/index age remains valid because exclusion cannot change the selected calendar date. | Existing server tests retain explicit 88.4 micromol/L conversion equivalence and eligibility cases. Existing client formula tests publish all four formulas in memory and staged modes; portability tests retain JSON/YAML/generated-R digest and provenance checks. |

This implements the review's explicitly permitted refusal approach. It conservatively
requires shared source-map equality for measurement anchors rather than inferring
whether a particular local map happens to preserve row selection on particular data.
The supported migration retains the analysis. No formula equation/version, numeric
normalizer, shared schema, or wire interface changed; no change request was needed.

## Commits

- Client `cd3a371423e79a544eb4f079a0d7a652c607e02c` — reject mismatched eGFR source maps;
  add client regression tests, NEWS migration and generated help.
- Server `257f9d4f0deac91f931c8c0f2b792b0bf48240fd` — actual-plan birthday/adult
  eligibility regressions and matching NEWS.
- An evidence-only server commit archives this report and the logs under
  `tools/audit-fix5/`; its runtime/test sources are the exact pair above.

## Targeted validation

Both runs use `/workspace/dsomop/tools/rtest.sh`, timeout 1200 seconds, matching
sibling `RTEST_WITH`, and
`RTEST_ENV='R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps'`.
The exact source pair for both runs is server `257f9d4` / client `cd3a371`;
`tested-source.json` records full commit IDs and changed-file SHA-256 hashes.

| Package | Filter | Exact test files | Result |
|---|---|---|---|
| Server | `^(audit-formula-edges)$` | `tests/testthat/test-audit-formula-edges.R` | 33 passed; zero failures/errors/skips |
| Client | `^(audit-formula-portability\|audit-formulas)$` | `tests/testthat/test-audit-formula-portability.R`, `tests/testthat/test-audit-formulas.R` | 104 passed; zero failures/errors/skips |

Summary and complete logs are archived as `server.log`, `server.full.log`,
`client.log`, and `client.full.log`. No other package test files or full suites ran.
The later test-economy rule supersedes COMMON.md's earlier full-suite instruction;
integration/release owns the full pass. No vendor or real-Opal execution is claimed.

Client help was regenerated on the pod with roxygen2 7.3.3 (`docs.log`); only the
changed `man/omop_formula_bmi.Rd` was pulled back. Both DESCRIPTION files retain
version 2.10.5 / RoxygenNote 7.3.3. Source-delta `git diff --check` passes in both
packages. The requested external report is also archived byte-for-byte as
`tools/audit-fix5/REPORT.md` because the external design directory is not a Git
repository. `SHA256SUMS` covers the archived report and evidence.

## Cleanup and integration handoff

Local free space was 9.6 GB. CLEANUP.md was read; inspected cache/build paths had no
disposable task-owned local material. No large local copies/downloads or uncertain
or other-job deletions were made. After both runs and archiving their complete logs,
cleanup confirmed no active R process used these resources and removed only this
job's disposable pod mirrors/private libraries (`cleanup.log`):

| Deleted pod path | File bytes |
|---|---:|
| `/root/rt/wt/audit/dsOMOP` | 25,560,684 |
| `/root/rt/wt/audit/dsOMOPClient` | 19,816,730 |
| `/root/rt/lib/wt_audit_dsOMOP` | 6,180,923 |
| `/root/rt/lib/wt_audit_dsOMOPClient` | 11,789,558 |

Shared numeric/label dependency libraries and other jobs were left intact.

No Claude CLI, subagents, stash, push, PR, tag, attribution trailers, or other
worktree edits. No runtime state, owner configuration, TLS requirement, budgets,
quotas, or per-user/session answer behavior were introduced. Existing private
intermediates and final release gates remain unchanged.

Integration must take the client validation fix together with the server regression
coverage. Independent review remains responsible for endorsement; this developer
report does not claim deployed privacy certification or the deferred integration
acceptance. No other job needs to change its implementation for this repair.
