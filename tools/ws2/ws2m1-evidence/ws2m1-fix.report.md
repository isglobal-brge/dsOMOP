# WS2m1 fix report — 12 November 2026

**Status: incomplete. B1 remains blocked by CR-27's explicit dual-review endorsement prerequisite.** Integration is merged and the affected targeted checks pass after one fixture correction. This report does not claim that the membership IR/digest collision is fixed.

Work is on `feat/2.10.3-ws2c` in the assigned server/client worktrees. Read COMMON.md, the stream's original review, re-review, WS-2c review and review-2, the WS-2f report, CR-27, and the relevant endorsed design/amendments. No Claude CLI, push, PR, tag, stash or full suite was invoked.

## Commits and integration

| Package | Commit | Change |
|---|---|---|
| dsOMOP | `7f536fe5e074fa9c9c02a0962cb29bdc1bc6e94a` | Merge integration `bf446cbd24c30abdaaac2889e9fc442bf4ba1d92`; resolve overlapping A-21/A-25 changes and regenerate schema probes/checksums. |
| dsOMOPClient | `d83ec34` | Merge integration `fd0c2d8199e58e1e462c40cdd6f4cb9b8cc3121b`; vendor merged probes. |
| dsOMOPClient | `acdfc75b5b5558fc6eda66cd84c91f8819182385` | Exclude server-only compatibility schema from the client checksum inventory. |
| dsOMOP | `0ae44f8` | Align the matching-template anchor regression with the integrated WS-6 case-time strata rule. |
| dsOMOP | Evidence/report commit, subject `Record WS2m1 integration checks and pending CR-27 prerequisite` | This report and targeted-run evidence. |

The integration heads above were checked again before reporting; both are ancestors of the respective workstream heads. Merge conflicts retained the current A-25 named-cohort shape and incorporated integration's A-21 probes and native population validation. The native wire schema remains SHA-256 `b762bed6aa93b64ee174e17a852bc6b873b8b939bb95dd3a73e65b0cdad529aa`. Neither the client package nor its checksum inventory contains `ir-v2.json`. Both versions remain 2.10.2.

The inherited `1f23cb4` WIP contains baseline/result-capture work already discussed in the subsequent independent reviews. Its result-comparison and capture changes were inspected and retained; no historical fixture was regenerated in this job.

## Finding → disposition → test

| Finding | Disposition | Evidence |
|---|---|---|
| Review-2 B1, membership identifier absent from semantic IR | **Unresolved.** The ledger still says CR-27 awaits both designated reviewers. Requested those endorsements/their location from the user; none were supplied during this turn. The existing concrete patch applies cleanly after the integration merge but remains unapplied. | Existing `test-ws2f-membership-results.R` passes its complete pristine-baseline comparisons (IDs 1/2/0, string coercions, independent population, absent/null dispositions). This verifies preserved execution only. The pending IR/digest/explain/closure tests remain inactive and are not claimed passed. |
| Earlier B2/B3 compatibility protections | Preserved during merge. No new changes to their implementations. | `test-ws2f-base-seed.R`, `test-ir-owned-cohort.R`, `test-ir-review.R` and `test-ir-legacy-empty.R` in the targeted selection. This is selected regression coverage, not a fresh re-review of every earlier finding. |
| Review-2 N1/N2 | Unchanged. The requested scope is blocking findings. | No claim of repair or new verification. |
| Integration fixture mismatch | The older test used a source anchor in matching strata, which the merged WS-6 validator refuses under its interpretation of §6.10's case-time strata rule. The corrected test explicitly checks that typed refusal and puts the source anchor in template criteria for the positive candidate-inventory assertion. Runtime validation/compiler behavior is unchanged. | `test-ir-review.R` rerun: 361 passed, no failures/errors/skips. This changes an earlier test's acceptance expectation and is explicitly disclosed for integration review. |

B1's exact instruction is: **“obtain the required endorsements for the narrow CR-27 amendment, then add”** the compatibility field (`design/2.10.3/astra-ws2c-review-2.md`). CR-27's final status in `design/2.10.3/change-requests.md` remains **“awaiting endorsement from both designated reviewers before adding the frozen shape.”** The request to implement the review exactly supplies no evidence that those endorsements have occurred. No endorsement or amendment approval was invented.

The prepared files remain `tools/ws2/ws2f-evidence/pending-b1-implementation.patch` and `pending-b1-tests.R`. After endorsement, apply the patch, activate those tests, regenerate affected server schema/probes/checksums, vendor shared artifacts with the compatibility-schema exclusion, audit the expected membership golden changes, and run only the oracle and affected targeted regressions. The WS-2f report's old full-suite instruction is superseded by current COMMON.md.

## Exact test record

Every R run used `/workspace/dsomop/tools/rtest.sh`, the matching sibling via `RTEST_WITH`, and:

```text
DSOMOP_TEST_POSTGRES_HOST=omopdb DSOMOP_TEST_MARIADB_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_PORT=3307
```

| Selection and tested commits | Passed | Failed | Errors | Skipped |
|---|---:|---:|---:|---:|
| Server `7f536fe`, client sibling `acdfc75`: `test-ir-legacy-empty.R`, `test-ir-owned-cohort.R`, `test-ir-review.R`, `test-ws2f-base-seed.R`, `test-ws2f-membership-results.R`, `test-ws0-schema.R` | 1,326 | 0 | 1 | 1 |
| Server `0ae44f8`, client sibling `acdfc75`: rerun only `test-ir-review.R` after the fixture edit | 361 | 0 | 0 | 0 |
| Client `acdfc75`, server sibling `7f536fe`: only `test-ws0-schema.R` | 145 | 0 | 0 | 0 |

The initial error is the disclosed strata fixture, superseded by its green rerun. The skip is the optional in-R Python oracle; the oracle ran separately and passed on final source: three schemas, two examples, 195 probes, 15 acceptance plans, 12 digest vectors, 17 normalized fixtures, 34 number vectors and 11,043 corpus doubles. These selections overlap; do not add their totals. No full package or full vendor acceptance pass is claimed.

Evidence is committed under `dsOMOP/tools/ws2/ws2m1-evidence/`: raw and summary logs, `test-selections.json`, `integration-verification.json`, `final-source-hashes.json`, oracle logs, README and this report. The initial missing local Python dependency and corrected client checksum inventory are disclosed in the README. Final oracle/client checks pass.

## Deviations and handoff

- The primary requested fix is unfinished solely because its explicit prerequisite remains unsatisfied. Green integration checks do not waive B1.
- Normal commits use explicit pathspecs. Conflicted merge commits used `git merge --continue`, which records the complete merge rather than a partial path commit.
- The requested design report lives outside a Git repository; an identical copy is committed with server evidence.
- No disposable package copies or full suites were created. Disk was checked (9.9 GB free); no large copy or cleanup deletion was performed.
- WS-7 must eventually consume `options.legacy_cohort_definition_id` only as the released membership label, preserving root conversion and omission versus zero. It must not use it to select persons or derive it from output-population identity. That option is still unavailable until CR-27 is endorsed and applied.
- Integration should note the explicit matching-strata fixture reconciliation above. Final full vendor suites remain integration/release work.
