E2E fix1 — run 43314476432, 12–13 November 2026

The run contains 63 FAIL/ERROR verdicts: 19 on fresh (18 FAIL, one ERROR), and 44 on upgrade (38 FAIL plus one ERROR in G; five FAIL in G-old). Every verdict is classified below and in the harness's `docs/e2e-fix1/classification.csv`, which retains its original evidence. Two fresh findings are product defects: DP-only loader admission and large-histogram latency. The remaining 61 verdicts are harness defects, including invalid candidate testing against the still-installed 2.10.2 server. This is a repair report, not release acceptance: a real Opal rerun, including its unchanged latency limits, remains required.

Starting product heads were dsOMOP `e6d2f4c9f323897404abf4c2445fde40c6abc376` and dsOMOPClient `51a6032349d966a03dfcb97d60358c9a860248f8`, both on `fix/2.10.3-ohdsi-p0`. The result branches resolve to fresh `34cd2fa13d5a41d7a76cd1cb32b1c4ef0e458729` and upgrade `ff42c6dadedb7e5dd406df496ac5971cbdbd129c`. Both summaries, verdict CSVs, scenario logs, container logs, the before/after Rserve logs, and the extracted Opal/Rock `*-srv-logs.tgz` contents were inspected. No result branch was modified.

**Commits and scope.**

- Server `b850917`: route-aware private loader admission, histogram framing/indexing optimization, NEWS, targeted regressions and the reproducible synthetic profiling script.
- Server `b93a3c6`: directly index the request-local framed contributions instead of repeatedly matching their tokens.
- Client: unchanged at `51a6032`; the suspected effect-pooling failure was in the harness. Its matching targeted tests passed.
- Harness `77b0b18`: exact installation verification/fallback and round-three contract/fixture corrections.
- Harness `5f61d86`: exhaustive 63-row classification and numeric-measurement source evidence.
- Harness `47b0536`: administrator resolver-ordering regression.
- Harness `238abcb`: vendor all five changed committed server paths from `b93a3c6`, retaining client `51a6032`. A full tracked-file comparison found all 1,682 server and 1,309 client files identical to those source heads.
- Final evidence/report commit ID is recorded in the completion message and repository history. The requested report is also committed as `dsOMOP/tools/e2e-fix1/report.md`; `/workspace/dsomop/design` itself is not a git repository.

**Installation: H-install, not a dsOMOP dependency defect.**

`upgrade-rock-srv-logs/Rserve.log` and its before-restart counterpart show the successful filelock installation followed by `dsOMOP` **2.10.2** (`* DONE (dsOMOP)`, before-restart lines 134–167). There is no candidate 2.10.3 install attempt/error there. Opal's archived `rest.log:644` records the local candidate upload/install request returning HTTP 201, while `upgrade-path.txt:133–138` immediately reports Rock still at 2.10.2. The same local API was ineffective on fresh, but that path ran the Rock-session fallback and installed 2.10.3 successfully (`fresh-path.txt:14–17`; `fresh-rock-srv-logs/Rserve.log:129–141`). Thus the evidence does not support adding a dependency or changing DESCRIPTION/Imports. The upstream local-operation failure is not explained by a captured R error; it must not be invented as a missing package.

The decisive harness defect is `install_server()` accepting **any** `^2\.` installed version when deciding whether to invoke the fallback. Upgrading from 2.10.2 therefore skipped the very fallback that made fresh succeed. It now requires the exact version in the candidate DESCRIPTION, runs `R CMD INSTALL` in Rock's own service R/library when needed, captures the full install output and exit status, and refuses to proceed if the resulting version differs. G and G-old also enforce that prerequisite. The regression simulates the observed successful HTTP/no-op upgrade, confirms fallback, and checks nonzero install status/wrong-version refusal. This fixes candidate installation without owner setup or changing the package dependency surface.

**Noise root: H-status/H-restart, not a permission defect.**

The Rock administrator R session already recorded user `rock`, HOME `/var/lib/rock`, state `/var/lib/rock/.dsomop`, and `secrets/dp_noise_root | 32 | 600` before G4 (`fresh-scenario-G.txt:5–17`). G4 explicitly moved that valid root aside. Its subsequent `omopDpStatusDS()` omitted the negotiated `dsomop-protected-release-v1` argument, so the endpoint correctly refused it before bootstrap. Later valid requests automatically created the replacement root, with no owner action. The fresh before/after restart root retained its 32-byte size, mode 0600 and mtime; all ten reference answers replayed. The restart check incorrectly counted two historical creation lines in the accumulated log as new creations, and its key-ID read had the same protocol error.

Raw status and concurrent bootstrap calls now negotiate the protocol. The expected generation policy is `person_membership_digest`, not the removed `automatic_session_fingerprint`. Restart checks inspect only new log lines (or the new rotated log). State diagnostics now include directory owner/mode/writability from Rock's R session. Docker mount-format quoting was also corrected: the old diagnostic literally returned `template parsing error: ... unclosed action`, hence its NA volume result. The code retains the existing automatic HOME-based state path and owner-only protections; it adds no configuration requirement. Persistence across container **replacement** is not established by the original restart test, and the corrected mount diagnostic must be observed on the rerun.

**Measurement and H: H-measurement.**

The loaded GiBleed CSV has 44,053 measurement rows and **zero** nonempty `value_as_number` entries. The harness source archive was read independently and those counts recorded in `docs/e2e-fix1/measurement-data.json`. Rock's generic disclosure refusal is correct for an empty numeric eligible population; changing that gate to force a passing H result would violate the privacy contract. The existing protected measurement/H implementation works on a numeric population in targeted tests.

G10 now checks the original all-null refusal. G10/G17 then add 500 explicitly synthetic numeric measurements of an unregistered unit using reserved, collision-checked IDs, register cleanup, and remove those exact rows on section exit. They change no source persons, scales, thresholds, secrets or product options. G17 now inspects actual finite signed `h` components rather than accepting an allocation declaration as proof of a histogram. Positive record/H, cap, quantile, replay and empty-population targets are included in validation.

**Other harness corrections.**

| Finding | Cause and fix | Targeted evidence |
|---|---|---|
| H-generation | Load the dsOMOP resolver before assigning the administrator resource. Replace removed `GenerationSnapshot` with the current membership hash/generation inside `.dsomopWithReadSnapshot`. Only booleans/timings/method metadata leave that admin check; no generation/digest is printed. | Pure harness resolver-order regression; `test-dp-generation.R`; syntax parsing of the edited scripts. |
| H-route | `condition_era.length_stats` is realised-sticky, even unscoped, because it enumerates data strata. Remove it from the fixed set, retain it in ordinary replay checks, and require its DP-only refusal. | Existing protected-route/generation targets and corrected G3a/G21 assertions. |
| H-alias | Two arm rows versus one diagnostics row repeated the same MDRR. Compare canonical distinct values, preserving failure for different values. | Original values agree to nine decimals; existing tools-release targets. |
| H-pooling | `estimate` is deliberately nonpoolable. Inspect `estimate_fixed` and `estimate_random` from inverse-variance meta-analysis. | `test-privacy-protected-client.R` explicitly checks one finite/one NA site, `estimate_fixed == 1.5`, and the named dropped-site warning; client targets passed. |
| H-audit | The flagged 64-zero string was the analyst's own injection, not a private generation. Exclude only that exact `data_generation` field literal, and continue checking all other content, including other hex on the same line. | Harness positive/negative audit cases; original fresh audit had 78 released and four refused typed releases and no other private-looking values. |
| H-restart | Historical creation lines were counted again after restart. Compare log suffixes or a newly rotated log. | Harness append/unchanged/rotation cases; original file metadata and reference answers were unchanged. |

**Product changes.**

P-loader changes only `omopAnalysisRunAssignDS` admission to `.dsomopRequireCommunityTool(dp_routes = TRUE)`. Assignment prepares server-side data; the aggregate/typed endpoints still decide whether a final release is permitted. The tools-disabled check remains before argument evaluation. The normal catalog assignment firewall, pseudonymization, authenticated sealed provenance, publication checks, current-source generation and final support/DP-only checks remain intact. The new endpoint regression actually creates and releases the pinned loader in both modes and compares complete payloads; it also checks disabled lazy arguments and aggregate/assign confusion. NEWS documents the corrected behavior.

P-latency is real: fresh warm latency was **24.6s**, beyond the harness's **15s** limit. The old large histogram rebuilt unit contribution strings for root/stratum/cell maps and scanned a full stratum once per public cell. The implementation now sorts the capped substrate by token once, frames unit contributions once within the function call, uses direct indices for subsets, and forms the same fixed-size fingerprint chunks without factor splitting. Non-unit moment contributions still use canonical validation/framing. This trades transient request memory for speed; the unit-framed string vector is not retained after the builder returns. Empty public cells, duplicate-person rejection, whole-person caps, root/owner identity, public basis, chunk size, signed components, exact sampler and projection remain unchanged.

The reproducible `tools/e2e-fix1/profile.R` runs production cryptography on one million synthetic persons. On the shared pod, the original builder took **21.083s** elapsed; the final builder took **15.153s** with R profiling enabled (about 28% faster). `identical()` held for the **entire** built result, not just displayed counts. A separate independent 20,003-record framing oracle exercises both chunk boundaries, permutation invariance and duplicate refusal. These are mechanism-profile timings, not end-to-end Opal timings, and include neither a real database request nor DSI transport. The real G16 limit remains unchanged and **not yet demonstrated to pass**. Do not call the latency acceptance item closed until the repaired candidate is rerun; the small helper improvement alone cannot establish an end-to-end SLO.

**Validation.**

Both packages remain version 2.10.3. Targeted package tests run on the pod through `tools/rtest.sh`; no full suite ran. Client targets installed server `b93a3c6` first; server targets installed matching client `51a6032` first.

| Package / exact files | Commit | Result |
|---|---|---|
| Server: `test-e2e-fix1.R`, `test-tools-release.R`, `test-tools-release-astra.R`, `test-tools-release-hierarchy.R`, `test-tools-release-pooling.R`, `test-tools-release-record-histogram.R`, `test-dp-generation.R`, `test-state-secrets.R`, `test-analysis-catalog.R` | `b93a3c6` | 3,223 passed; zero failures/errors; one macOS-only skip on Linux |
| Client: `test-privacy-protected-client.R`, `test-analysis-pooling-contract.R` | `51a6032`, matching server `b93a3c6` | 2,971 passed; zero failures, errors or skips |
| Harness: `scripts/tests/g-contract.R`, wrapped in an isolated probe package and run through `rtest.sh` with filter `^harness$`; all-script syntax parsing | `238abcb` (test source unchanged from `47b0536`) | One wrapper assertion passed on the pod; all internal contract assertions passed. This is not a live Opal rerun |
| Million-person profile and complete-result comparison | baseline pre-optimization vs `b93a3c6` | Identical complete built result; times above |

The server skip is the macOS canonicalization case on Linux. The minimal loader fixture reports its expected missing optional CDM tables; an existing SQLite fixture also emits a connection-cleanup warning. Neither is a release failure.

Detailed final logs and profile evidence are committed under `tools/e2e-fix1/evidence/`. The first small regression run failed because its fixture lacked a pseudonymization provider and a mock resource close method; fixing the fixture produced ten passes before the optimization. Two intermediate broader runs were inadvertently overlapped on the same runner mirror/log and were stopped; their outputs are discarded, not counted as passes. The final server run was restarted cleanly on the committed source. This is a testing-procedure deviation, not product evidence.

**Privacy and handoff.**

No privacy accountant, quota, call limit, analyst/session partition, retained answer cache, persistent state beyond the existing roots, TLS requirement, custodian setting or weakened support gate was introduced. Contribution frames used for the speedup are local variables of one builder invocation. Generation remains request-local, recomputed from person membership in its database snapshot. The round-three realised/fixed distinctions, clinical-edit/equality residuals, immutable public domains, signed pooling and existing scope/uncertainty qualifications remain as in `design/2.10.5/`. No unrelated compatibility work (including the independent review's N06) is claimed fixed.

Only the requested P0 product worktrees and the authorized harness were edited. The requested report is the sole deliverable under design. Existing integrated WIP ancestry was not treated as validated by assumption: the affected catalog/publication and mechanism paths were inspected and targeted. No sub-agents, claude CLI, stash, PR, tag or push were used. Disk had about 9.8 GB free; no full suite or large package copy was made, and no unrelated cleanup was attempted. Extracted evidence was small, the dataset archive was read in memory, and final vendoring copies only changed committed paths. The own isolated harness probe (46,900 source bytes) and its pod mirror/library were removed after evidence capture; paths are recorded in `evidence/cleanup.json`. The three `/tmp/e2e-fix1-before.rds`, `/tmp/e2e-fix1-after.rds`, `/tmp/e2e-fix1-profile.out` synthetic profile scratch files were removed after comparison evidence was saved. No other package copies were made for this job. Normal tool-managed P0 test mirrors remain in place.

The next release run must test the exact newly vendored server and unchanged matching client, observe successful old-to-new installation and publication, recheck the real shared-Rock bootstrap and restart, exercise the numeric H fixture, verify current generation secrecy and all three actual fixed routes, and retest **G16 on the real stack without relaxing its timing thresholds**. No live Opal acceptance or full-suite release pass is asserted here.

**Every FAIL/ERROR verdict.** Detailed reasons and original evidence are in the committed classification CSV. Upgrade verdicts are harness-invalid candidate evidence because the installation prerequisite failed; the fresh P-loader finding independently establishes that actual product defect.

| Path | Check | Verdict | Class | Finding |
|---|---|---|---|---|
| fresh | G4a | FAIL | harness defect | H-status |
| fresh | G2a | FAIL | harness defect | H-status |
| fresh | G3d1 | ERROR | harness defect | H-generation |
| fresh | G10 measurement_value | FAIL | harness defect | H-measurement |
| fresh | G17b measurement.value_stats | FAIL | harness defect | H-measurement |
| fresh | G15a mdrr_aliases | FAIL | harness defect | H-alias |
| fresh | G19b label | FAIL | harness defect | H-status |
| fresh | G3a1 | FAIL | harness defect | H-status |
| fresh | G3a2 | FAIL | harness defect | H-route |
| fresh | G3a3 | FAIL | harness defect | H-route |
| fresh | G3a7 | FAIL | product defect | P-loader |
| fresh | G21 st4 | FAIL | harness defect | H-route |
| fresh | G21 st6 | FAIL | harness defect | H-route |
| fresh | G21 st8 | FAIL | harness defect | H-route |
| fresh | G14b pooled_only_ok | FAIL | harness defect | H-pooling |
| fresh | G16b big | FAIL | product defect | P-latency |
| fresh | G3c4 root | FAIL | harness defect | H-restart |
| fresh | G8 | FAIL | harness defect | H-audit |
| fresh | G18 | FAIL | harness defect | H-audit |
| upgrade | O3 typed | FAIL | harness defect | H-install |
| upgrade | O5 record_summary | FAIL | harness defect | H-install |
| upgrade | O5 effect_strict | FAIL | harness defect | H-install |
| upgrade | O5 effect_pooled_only_ok | FAIL | harness defect | H-install |
| upgrade | O6 | FAIL | harness defect | H-install |
| upgrade | G1a | FAIL | harness defect | H-install |
| upgrade | G6a achilles.3 | FAIL | harness defect | H-install |
| upgrade | G6a condition_era.length_stats | FAIL | harness defect | H-install |
| upgrade | G2a | FAIL | harness defect | H-install + H-status |
| upgrade | G2c loader_count | FAIL | harness defect | H-install |
| upgrade | G2c prepared_hist | FAIL | harness defect | H-install |
| upgrade | G2d achilles.3 | FAIL | harness defect | H-install |
| upgrade | G2d condition_era.length_stats | FAIL | harness defect | H-install |
| upgrade | G2d incidence.rate | FAIL | harness defect | H-install |
| upgrade | G3d1 | ERROR | harness defect | H-generation |
| upgrade | G10 cox | FAIL | harness defect | H-install |
| upgrade | G10 measurement_value | FAIL | harness defect | H-install + H-measurement |
| upgrade | G17a interaction_cox | FAIL | harness defect | H-install |
| upgrade | G17b drug.quantity_stats | FAIL | harness defect | H-install |
| upgrade | G17b measurement.value_stats | FAIL | harness defect | H-install + H-measurement |
| upgrade | G17c recurrent_counts | FAIL | harness defect | H-install |
| upgrade | G17d event_time_H | FAIL | harness defect | H-install |
| upgrade | G15a mdrr_aliases | FAIL | harness defect | H-alias |
| upgrade | G15c weighted_histogram | FAIL | harness defect | H-install |
| upgrade | G19b label | FAIL | harness defect | H-install + H-status |
| upgrade | G19b | FAIL | harness defect | H-install |
| upgrade | G19b routes | FAIL | harness defect | H-install |
| upgrade | G19b2 | FAIL | harness defect | H-install |
| upgrade | G19c | FAIL | harness defect | H-install |
| upgrade | G3a2 | FAIL | harness defect | H-install + H-route |
| upgrade | G3a3 | FAIL | harness defect | H-install + H-route |
| upgrade | G3a5 | FAIL | harness defect | H-install |
| upgrade | G3a6 | FAIL | harness defect | H-install |
| upgrade | G3a7 | FAIL | harness defect | H-install + P-loader |
| upgrade | G9 | FAIL | harness defect | H-install |
| upgrade | G21 st1 | FAIL | harness defect | H-install |
| upgrade | G21 st2 | FAIL | harness defect | H-install |
| upgrade | G21 st4 | FAIL | harness defect | H-install + H-route |
| upgrade | G21 st6 | FAIL | harness defect | H-install + H-route |
| upgrade | G21 st8 | FAIL | harness defect | H-install + H-route |
| upgrade | G20c | FAIL | harness defect | H-install |
| upgrade | G3c4 root | FAIL | harness defect | H-restart |
| upgrade | G8 | FAIL | harness defect | H-install + H-audit |
| upgrade | G18 | FAIL | harness defect | H-audit |
