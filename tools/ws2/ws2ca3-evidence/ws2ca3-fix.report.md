# WS2ca3 fix report — 12 November 2026

**Incomplete: B1 remains unresolved. The required CR-27 endorsements are still pending; no implementation fix is claimed.**

Read COMMON.md, the common workstream brief, original WS-2 review and re-review, WS-2c review, review-2, review-m1, review-a1 and review-a2, CR-27 and the preceding fix reports. Inspected the prepared patch and inactive tests against the current lowering, shape conversion and membership executor. Worked only in the assigned worktrees and this requested report location. No Claude CLI was invoked.

## Integration and commits

Both worktrees are on `feat/2.10.3-ws2c` and were clean on entry.

| Package | Entry HEAD | Integration head, already an ancestor |
|---|---|---|
| Server | `01e5f531e5589b299bc0cf7af03642612b87bd57` | `c7e541e4bac659cdc32358d14b1b4791c2e4203e` |
| Client | `ed8f1e261e81c0e0a2d8ffc20865821cafd30f56` | `c8b337b77b09d5ed219215ddf205652193354220` |

Neither integration head needs merging. Both package versions remain 2.10.2. The sole new server commit is titled **Record WS2ca3 review prerequisite and patch verification**, containing this report and `tools/ws2/ws2ca3-evidence/checks.json`. The client is unchanged; no empty client commit was created. The design report is outside Git; its copy under the server evidence directory is committed.

## Finding → disposition → verification

| Finding | Disposition | Verification this round |
|---|---|---|
| Review-a2 B1: membership identifier absent from semantic IR | Unresolved at the explicit CR-27 endorsement prerequisite. Prepared patch remains unapplied and semantic tests remain inactive. | `git apply --check tools/ws2/ws2f-evidence/pending-b1-implementation.patch` passes. Inspected the root-derived ID, historical conversion helper, membership extractor, IR-only schema/probes and pending tests. No population-selection or native-wire schema change is proposed. |
| Earlier B2/B3 and repaired non-blocking findings | Retained without implementation edits. | Read their fresh passing evidence in review-a2; no new runtime verification claimed. |
| Review-a2 N1/N2 | Still open and non-blocking; outside the requested blocking-findings scope. | No edits or new tests. |

Review-a2 B1 explicitly says **“obtain the CR-27 endorsements, then add”** the compatibility option. CR-27 still says **“awaiting endorsement from both designated reviewers before adding the frozen shape.”** The review also explicitly supplies no amendment endorsement or waiver. Searched the design records and relevant Git history; no approval was located. Asked the user for the endorsement location or explicit authorization to apply the specified fix without it. That question remains unanswered and pending; silence is not authorization.

The existing concrete proposal is `tools/ws2/ws2f-evidence/pending-b1-implementation.patch`, with semantic tests in `pending-b1-tests.R` and pinned complete-result tests in `tests/testthat/test-ws2f-membership-results.R`. The patch uses `.v1CohortInteger()` on the root field for each membership output and adds only the closed compatibility option. The tests cover independent output populations, IDs 1/2, zero, missing/null, coercions, IR/digest/explain/output closure, schema type/format refusal and native-wire refusal. This inspection is preparation, not evidence that the unapplied repair works.

## Tests and checks

**R test files run: none. Contract oracle run: none.** Only report/evidence files changed, so the user's targeted-test instruction does not warrant rerunning unchanged implementation tests. No full suite was run. Previous test totals are not claimed as fresh results or B1 acceptance.

Static checks ran against the exact entry commits above: both integration ancestry checks, both `git diff --check` checks and the pending patch application check passed. `checks.json` records the heads, versions and patch/test SHA-256 hashes. No implementation, schema, fixture or package version changed.

## Deviation and handoff

The requested blocker repair remains incomplete solely at its explicit endorsement prerequisite. Once endorsements or a user waiver are available, apply the prepared patch, activate the pending membership tests, regenerate the affected schema/probes/checksums, vendor shared artifacts without the client compatibility schema, audit the affected membership golden records, and run the changed-code tests plus independent contract oracle. Keep absence/null distinct from zero and preserve historical execution/refusal and independent-population behavior.

WS-0 verifies schema regeneration at integration. WS-7 must consume the proposed option solely as the membership label, never as a population selector. The option is not currently an available contract. Full suites remain integration/release work. No push, PR, tag, stash, large copy or scratch package was used; no cleanup is required.
