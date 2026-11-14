# WS2ca1 fix report — 12 November 2026

**Incomplete: B1 remains unresolved because its required CR-27 endorsements have not been supplied.** Both current integration heads are merged, a resulting client corpus inconsistency is fixed, and targeted checks pass. These checks do not repair or waive the membership IR/digest collision.

Read COMMON.md, the original WS-2 review, re-review, WS-2c review, review-2 and review-m1, CR-27, the relevant amendment text and prior fix evidence. Worked only in the assigned `feat/2.10.3-ws2c` package pair and the requested report location. No full suite, Claude CLI, push, PR, tag or stash was invoked.

## Commits

| Package | Commit | Change |
|---|---|---|
| Server | `764d0a80c42845da4a15b39be3b97e81d090e327` | Merge integration `c7e541e4bac659cdc32358d14b1b4791c2e4203e`. |
| Client | `70ca070884255173bdc50d15ff7ef64eb946f2f7` | Merge integration `c8b337b77b09d5ed219215ddf205652193354220`. |
| Client | `ed8f1e261e81c0e0a2d8ffc20865821cafd30f56` | Vendor the merged server corpus classifications, retaining approved A-19 decisions alongside A-24. |
| Server | Commit titled `Record WS2ca1 targeted integration checks and outstanding CR-27 prerequisite` | This report and test evidence under `tools/ws2/ws2ca1-evidence/`. |

Both merges conflicted only in NEWS; both sides' entries were retained. Runtime files merged automatically. Both integration heads were verified as ancestors after testing. Both package versions remain 2.10.2.

## Finding → fix → verification

| Finding | Disposition and verification |
|---|---|
| Review-m1 B1: executed membership identifier omitted from semantic IR | **Unresolved.** CR-27 still explicitly awaits both designated reviewers. Asked the user for the endorsement location or explicit authorization to proceed without that prerequisite; no answer arrived before this report. The prepared `tools/ws2/ws2f-evidence/pending-b1-implementation.patch` still passes `git apply --check` after integration. Its desired-contract tests remain inactive. `test-ws2f-membership-results.R` passes complete pinned baseline comparisons, including IDs 1/2/0, string coercions, independent female population, and absent/null execution dispositions. This proves preserved execution only, not repaired semantic IR. |
| Merge-related client corpus mismatch | Fixed in `ed8f1e2`. Integration introduced the client corpus copy, which lacked this stream's previously approved A-19 classification edits. Copied the merged server fixture byte-for-byte. The independent contract oracle validates it against the pinned baseline inventory and reviewed decisions; it now passes. No new classification policy was introduced. |
| Earlier repaired findings | No implementation edits. Selected `test-ir-review.R` and legacy endpoint regressions pass after the automatic runtime merge. This is targeted integration coverage, not a new independent review of every earlier finding. |
| Review-m1 N1/N2 | Unchanged: the user requested blocking findings. No claim of repair or new verification. |

The explicit prerequisite is in review-2 B1: **“obtain the required endorsements for the narrow CR-27 amendment, then add”** the compatibility option. CR-27 concludes **“awaiting endorsement from both designated reviewers before adding the frozen shape.”** Review-m1 explicitly reiterates that its implementation review supplies no amendment approval. The request to fix this exactly does not establish that the endorsements occurred; none were invented.

After endorsement or an explicit user override: apply the prepared lowerer/generator patch, activate the pending membership tests, regenerate affected IR schema/probes/checksums, vendor shared artifacts while excluding the compatibility schema and its checksum entry, audit membership golden changes, and run only affected tests plus the oracle. Preserve the root field's effective historical integer conversion, absent/null versus zero, and independent population behavior. The option labels membership rows; it must never select persons.

## Exact verification record

One R invocation used `/workspace/dsomop/tools/rtest.sh` with timeout 600, server commit `764d0a8`, and matching client commit `70ca070` installed via `RTEST_WITH`. Filter:

```text
^(ws2f-membership-results|ir-review|legacy-execution-call|ws0-contract-tables)$
```

Exactly four files ran:

- `test-ws2f-membership-results.R`
- `test-ir-review.R`
- `test-legacy-execution-call.R`
- `test-ws0-contract-tables.R`

**760 passed, 0 failed, 0 errors, 0 skipped.** Environment:

```text
DSOMOP_TEST_POSTGRES_HOST=omopdb DSOMOP_TEST_MARIADB_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_PORT=3307
```

The standalone Python oracle passed on server `764d0a8` and client `ed8f1e2`: three schemas, two examples, 195 probes, 15 acceptance plans, 12 digest vectors, 17 normalized fixtures, 34 number vectors and 11,043 corpus doubles. No client R suite was run: the only client correction is the corpus CSV, directly checked by this oracle. The first invocation lacked local `jsonschema`; rerunning with `uv run --with jsonschema` exposed the corpus mismatch. Both diagnostics and the final passing log are retained.

Evidence includes raw/summary R logs, oracle diagnostics/final log, exact test selection, integration heads and static checks. Native wire SHA-256 stays `b762bed6aa93b64ee174e17a852bc6b873b8b939bb95dd3a73e65b0cdad529aa`; shared normalizers and corpus copies match. The client contains neither `ir-v2.json` nor its checksum entry. No full vendor acceptance pass is claimed.

## Deviations and handoff

- The requested blocker fix remains unfinished solely at its explicit endorsement prerequisite. Green integration checks are not B1 acceptance.
- Merge commits used `git merge --continue`; the subsequent normal commit used explicit pathspecs.
- The design report is outside Git; an identical report is committed with server evidence.
- Disk was checked (9.8 GB free). No large source copy or disposable package snapshot was created.
- WS-0 must verify the eventual approved schema regeneration; WS-7 must consume the eventual option only as the membership label. Until approval and application, the pending patch is not an available contract.
- Integration/release retains responsibility for full suites. No TLS requirement, query budget, privacy accounting or persistent answer-changing state was added.
