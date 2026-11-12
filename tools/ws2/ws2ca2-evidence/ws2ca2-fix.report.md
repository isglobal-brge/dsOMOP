# WS2ca2 fix report — 12 November 2026

**Incomplete: B1 still requires CR-27's designated reviewer endorsements or an explicit user waiver. No implementation fix is claimed.**

Read COMMON.md, the common stream brief, the original WS-2 review and re-review, WS-2c review, review-2, review-m1, review-a1, CR-27, the relevant amendment text, and the previous fix report. Inspected the prepared B1 implementation and inactive tests against the current lowerer, validator and membership executor.

## Repository and integration state

Both assigned worktrees are on `feat/2.10.3-ws2c` and were clean on entry.

| Package | Entry HEAD | Integration head, already an ancestor |
|---|---|---|
| dsOMOP | `b6944d4fa531a7726d3c725c3cfefb47fe67e579` | `c7e541e4bac659cdc32358d14b1b4791c2e4203e` |
| dsOMOPClient | `ed8f1e261e81c0e0a2d8ffc20865821cafd30f56` | `c8b337b77b09d5ed219215ddf205652193354220` |

Neither integration head has moved beyond the already merged heads. No merge or client commit is necessary. The only new commit is the server report commit titled **Record WS2ca2 blocker prerequisite and patch readiness**; an identical report is committed at `tools/ws2/ws2ca2-evidence/ws2ca2-fix.report.md`. No package implementation, schema, fixture or version changed.

## Findings, fixes and verification

| Finding | Disposition | Verification this round |
|---|---|---|
| Review-a1 B1: membership identifier missing from semantic IR | Unresolved at the explicit endorsement prerequisite. The existing concrete patch remains unapplied. | `git apply --check tools/ws2/ws2f-evidence/pending-b1-implementation.patch` passed. Static inspection confirms it adds the root-derived membership label and IR-only schema/probes, without changing population selection or the native schema. |
| Earlier repaired blockers B2/B3 | Retained; no affected implementation changed. | Read the latest review's independent regression results; no new runtime verification claimed. |
| Review-a1 N1/N2 | Remain open and non-blocking; outside this blocking-findings request. | No edits or new tests. |

The exact prerequisite is review-a1 B1: **“obtain the CR-27 endorsements, then add”** the closed compatibility option. CR-27 still states **“awaiting endorsement from both designated reviewers before adding the frozen shape.”** The amendment ledger contains no CR-27 approval. The implementation review explicitly supplies no amendment endorsement or waiver.

Requested the endorsement location or explicit authorization to apply the specified fix without endorsements through the asynchronous user-input tool. No answer was received before this report. No reviewer approval has been inferred or invented.

## Tests and checks

**R test files run: none. Contract oracle run: none.** Only documentation changed. In accordance with the user's instruction to test only changes, no unchanged runtime tests or full suites were repeated. Earlier passing evidence is not presented as a new test result or as B1 acceptance.

Static checks on the entry heads above:
- Both integration ancestry checks passed.
- Both worktrees were clean; `git diff --check` passed.
- Prepared B1 patch application check passed.
- Searched the design review/change-request/amendment records for CR-27 endorsement; status remains pending.

Prepared patch SHA-256: `eaeb4d0e2540f5a82562435925c695d3a1f0313796883c9d782c6db8f4a76d0a`.
Inactive B1 tests SHA-256: `2de0f4887d7f66e5891ca527836e5cd8318a7e875e44399091a839fd3a793ca1`.

## Deviation and handoff

The requested blocker repair could not be completed consistently with its explicit prerequisite. The pending patch and inactive tests are reviewable preparation, not a repair.

After endorsements or an explicit user waiver: apply the prepared patch; activate its membership tests alongside the pinned complete-result fixture; regenerate the affected schema, probes and checksums; vendor only shared artifacts into the client; audit membership golden changes; run the affected tests and independent contract oracle. Preserve absence/null versus zero and historical conversion/refusal behavior. WS-0 verifies regeneration; WS-7 consumes the option solely as the membership column label, never as a population selector.

Full suites remain integration/release work. No Claude CLI, push, PR, tag, stash, large copy or scratch package was used. Disk had 9.8 GB free; CLEANUP.md was read, and no files were deleted.
