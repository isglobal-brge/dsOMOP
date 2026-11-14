# e2e4-fix2 — real-Opal round-two repairs

13 November 2026. **Run 43464880783 is fully classified and its product defect is repaired. Run 43464871866 cannot be classified from the supplied clone: its result refs and artifacts are absent.** The missing results were requested while independent repairs continued. No O1–O12 outcome is inferred from the e2e run or from source inspection.

## Commits and scope

- Server `/workspace/dsomop/wt/int/dsOMOP`, `feat/2.10.3`: **860c31f**, explain unavailable protected MDRR on direct, alias, diagnostics and standard-mode routes; regression test and NEWS.
- Client `/workspace/dsomop/wt/int/dsOMOPClient`, `feat/2.10.3`: unchanged at **30a9794**. No client defect is established by these results. Its protection-label reader preserves and validates the server label, including additional explanation fields.
- Requested harness clone, `main`: **efb5be5**, require an explicit nonempty G10 availability reason; **4107fd6**, vendor the exact three changed server files from 860c31f. The client already matches 30a9794.
- Evidence/report commits follow these implementation commits. Identical report copies are tracked in server `tools/e2e4-fix2/report.md` and harness `docs/e2e4-fix2/report.md`; the requested report path is outside a git repository.

Both packages remain **2.10.3**, the coordinated version of the integrated prospective 2.10.4 work. No frozen interface, version number, dependency, privacy mechanism, state provision or owner configuration changed. No WIP commits occur in the current relevant repair history; the preceding exact-hash fix remains intact and now passes real-Opal latency acceptance.

## Available run: 43464880783

Immutable input refs in the supplied clone:

- `res/43464880783-fresh-candidate`: `9b94610535d24022d4102621bde698744ed7d987`.
- `res/43464880783-upgrade-candidate`: `bf0b009bb9ab0103e6dcd64589f52e8800698104`.

Fresh G: **88 PASS / 0 FAIL / 12 INFO / 0 ERROR**. Upgrade G: **89 PASS / 1 FAIL / 15 INFO / 0 ERROR**, including the copied V checks. Each old-client run: **10 PASS / 0 FAIL / 0 ERROR**. Standalone upgrade V: **3 PASS / 2 INFO**. D: **67 PASS / 1 expected REFUSED / 0 FAIL / 5 NOT-EXERCISED**. C, E and F have no failed captured checks.

The extractor audits all top-level result files and all four archived server log bundles: **62 files, 24,386,238 bytes**. The final error-heading inventory includes ordinary Docker log ERROR lines as well as bracketed server logs and structured audit errors: **579 headings: 1 product defect, 568 expected-refusal diagnostics and 10 stack-lifecycle diagnostics; zero unclassified headings**. Counts include duplicate wrappers/log copies, not independent defects. Additional scans for execution halts, fatal errors, tracebacks and segmentation faults found no additional failure cause. The classifier is specific to the reviewed inputs and exits unsuccessfully if a heading remains unclassified.

| Finding | Classification | Evidence, repair and validation |
|---|---|---|
| Upgrade **G10 mdrr** | **Product** | `upgrade-scenario-G.txt:1342` displays target/comparator persons 1867/845, protected person-days 634618/0, outcomes 377/114 and MDRR NA/NA. The protected comparator time is zero, so the unavailable MDRR is correct; the protection label lacks a reason. 860c31f adds `unavailable_reason`, computed solely from the already protected result, and propagates it through both CohortMethod aliases, diagnostics and standard mode. `test-e2e4-mdrr.R` reproduces failure on the original code and passes after repair. |
| G10 accepted a reason-column name even with NA/blank content, or any unrelated metadata mentioning “unavailable” | **Harness**, latent false-pass defect found while tracing G10; not the cause of the observed FAIL | efb5be5 replaces the broad text/name search with `g_has_unavailable_reason()`. It requires a nonempty textual reason in explicit label/frame fields. The harness regression replays the recorded MDRR frame, rejects absent/empty reasons and unrelated metadata, and accepts a real explanation. The failed MDRR verdict is preserved as a product finding, not downgraded. |
| Fresh **G16b big** | **Passing regression**, no new defect | Cold **14.4 s**, warm **9.4 s**, warm other **8.3 s**, four concurrent cold **42.5 s**; limits remain 60/15/120 s. Native hash acceleration is TRUE. The previous performance defect is resolved on this real stack. Upgrade intentionally does not run the million-person fixture. |
| Upgrade **V2**, and G's copied V2 | **Passing regression**, no new defect | Numeric old-server preparation now succeeds; the expected legacy result/refusal is correctly labelled. The prior harness translation/default defect no longer occurs. |
| D missing cohort definition, absent physical OHDSI result table, protected birth-column aggregate probes | **Harness negative controls**, expected refusal | Missing definition is honestly REFUSED; the physical-results probe cannot pool an absent fixture; aggregate birth protections stay in force. Both birth-column assignment probes now succeed. No product relaxation is justified. |
| D's five NOT-EXERCISED methods | **Harness coverage gaps**, not observed product failures | `omopPlanStatusDS`, `omopStagedRevokeDS`, `omopPlanExplainDS`, `omopAsFactorColumnsDS`, `omopMutateDS`. No method is falsely credited PASS without an Opal call. Missing O-run evidence cannot be used to fill these gaps. |
| Duplicate-source pooling, injected generation/epoch/seed controls, all-null numeric support, DP-only and disabled routes, revoked reads, deliberately killed connections, old-client typed requests | **Harness negative controls**, expected refusal | Their positive security/recovery verdicts pass. Preserve fail-closed behavior. The old-server status call's `unused argument` diagnostic is the expected negotiation fallback, not a new candidate failure. |
| Authentication/resource-provider readiness and Rock connection refusals | **Harness stack lifecycle**, not product failures | Fresh Rock closes R at 16:43:24; the three connection refusals occur during that interval; R restarts successfully at 16:43:51. Subsequent install, publication, analyses and restart-replay checks pass. Duplicate archived/container copies account for the heading count. |

`docs/e2e4-fix2/classification.csv` maps the sole failed verdict to its product commit/test. `all-errors.csv` retains file, line, classification and evidence for every indexed heading. `manifest.json`, `SHA256SUMS`, `extract.py`, summaries, verdict tables and the method audit make the classification reproducible without altering result refs.

## Unavailable run: 43464871866

No local or remote-tracking ref containing this run ID exists in this clone, including under `res/` or `origin/results/`. The nearby scratch `e2e-results` and named run-file search also found no corresponding artifact. `git ls-remote origin 'refs/heads/*43464871866*'` fails authentication; `gh auth status` reports no authenticated GitHub hosts. Commands and results are retained in `availability.log` and `run-availability.json`.

Unlike `e2e.yml`, `opal-contract.yml` uploads artifacts but has no result-branch writer. Thus the user's expected `res/43464871866-*` branches cannot be assumed to have been produced. Candidate preflight now succeeds locally; the previous run's version-preflight defect does not establish the cause of this run's failure.

**All four contract jobs — Opal 6.1.3/5.7.6 × fresh/upgrade — remain unclassified and their O1–O12 results unverified.** Required follow-up: supply those actual job logs/artifacts or local result refs. No unsupported product or harness repair was guessed from that missing run.

## Product behavior and privacy

The repair uses the existing optional protection-label explanation field. It neither modifies the frame's closed columns nor changes the MDRR formula, displayed values, signed components, sampler, noise framing, support gates or pooling. The reason is the same for unavailable protected values and contains no raw counts, exact support, generation, person identifiers or private fingerprint. The diagnostics overlay forwards the same explanation rather than dropping it; the existing standard compatibility path already forwards extras.

NEWS classifies the change as corrected and tells users to rerun the analysis to obtain the label; no recipe changes are necessary. No new persistent state, cache, TLS requirement, call limit, quota, privacy accountant or user/session-dependent answer was introduced. No frozen-interface change request was needed.

## Targeted validation

All substantive R regression runs use the existing pod via `/workspace/dsomop/tools/rtest.sh`. Only the changed area and direct dependents are selected. No full suites, live CI runs, push, PR, tag, stash, sub-agent or claude CLI were used.

- Before repair, `test-e2e4-mdrr.R` on server 4c23bc9 plus the new test reproduces the missing field: 3 assertions pass, then `expect_match()` errors because `unavailable_reason` is NULL. The exact pre-fix log is retained.
- After repair, the same file passes **53 assertions, zero failures/errors/skips**. It uses a real SQLite CDM and full catalogue release paths with deterministic sampler witnesses for zero protected time and low protected persons; checks aliases, diagnostics, standard mode, stable repeats, attribute serialization and the finite-result case without a spurious reason. The repaired source is committed as 860c31f.
- Final server targeted run on **860c31f**, with sibling client **30a9794** installed through `RTEST_WITH`: **3,035 assertions passed, zero failures/errors/skips**. Files: `test-e2e4-mdrr.R`, `test-tools-release.R`, `test-tools-release-survival.R`, `test-tools-release-sweeps.R`, `test-tools-release-pooling.R`, `test-tools-release-client-survival.R`. Exact filter: `^e2e4-mdrr$|^tools-release$|^tools-release-survival$|^tools-release-sweeps$|^tools-release-pooling$|^tools-release-client-survival$`. The last two files exercise the matching client directly; no separate client suite was needed for this server-label-only change.
- Harness `scripts/tests/g-contract.R`: base-R smoke probe passes; the same script passes on the pod in an isolated package with **1 wrapper assertion, zero failures/errors/skips**. That wrapper executes all embedded `stopifnot` assertions, including the new G10 replay and negative controls. Source is efb5be5; vendoring does not change those functions.
- Harness Python `scripts/tests/o-candidate.py`: **2 tests passed**. `scripts/o/check_candidate.py` passes against the vendored coordinated pair. Changed R scripts parse and source/report `git diff --check` passes. Captured logs and original summaries retain their original trailing whitespace.

Full test logs and the exact selected-file filter are committed under server `tools/e2e4-fix2/evidence/`; harness-specific logs are also retained under harness `docs/e2e4-fix2/`. These tests validate the repair locally/on the pod, not a rerun of either real-Opal workflow.

## Cleanup, deviations and handoff

Disk remained about 9.7 GiB free. CLEANUP.md was read; no full suite or large package copy occurred. Only three changed vendored source files were copied, **151,919 bytes**. The job's own temporary harness probe under `tools/e2e4-fix2/probe` was removed after preserving its log: **332,021 file bytes**. Its pod mirror (**340,642 bytes**, `du -sb`) and empty private test-library directory (**10 bytes**, `du -sb`) were also removed; `cleanup.json` records every path and size. Raw extracted inputs remain as evidence at `/workspace/dsomop/design/astra-dev/e2e4-fix2-inputs`, reproducible from the immutable refs; no shared repository, other worktree, primary checkout or credential file was modified.

The only scope limitation is missing contract-run evidence. The available run's product repair, harness validation improvement and source vendoring are committed locally. The coordinator should provide the four missing contract artifacts and rerun the updated candidate to verify G10 and the O matrix. Do not present this report as complete classification of run 43464871866 or as a real-Opal pass of the new MDRR label.
