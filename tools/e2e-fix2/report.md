# e2e-fix2 — run 43326334391

13 November 2026. Functional repairs and harness corrections are committed. **The large-stack G16 latency acceptance remains open**: the measured optimization is too small to establish the 15-second warm limit. This report does not claim a passing new Opal run or a complete release acceptance.

## Commits and scope

Product branch: `fix/2.10.3-ohdsi-p0`, only `/workspace/dsomop/wt/p0/{dsOMOP,dsOMOPClient}`. Both packages remain **2.10.3**.

- Server **87bc430**: announce DP-root creation only after the winning no-clobber write and durable sync; preserve HMAC bytes while avoiding a raw-vector conversion; add actual endpoint, concurrent bootstrap and HMAC equivalence regressions.
- Client **7907881**, **398f759**: recognize the publication protocol backported to 2.10.3; correct upgrade advice and its legacy-version assertions.
- Harness **f0e61fb**: correct measurement parameters and signed-component redraw oracle; repair duplicate IDs in the large synthetic fixture; commit complete verdict classification and diagnostic indexes.
- Harness **95bfa27**: require the exact candidate archive to install successfully through Rock's service R session, including same-version rebuilds; fail on missing dependencies or a failed build.
- Harness **091e103**: vendor the repaired server and matching client. No push.

Validation/report commits follow these implementation commits; the repository logs identify them. A tracked copy of this report and replay/evidence files is in server `tools/e2e-fix2/`.

## Evidence and every FAIL/ERROR verdict

Read both summaries, all scenario outputs, package/path/install output, Opal/Rock console logs, and **all four `*-srv-logs.tgz` archives**, including their Rserve, Opal and REST logs. Immutable results:

- `res/43326334391-fresh-candidate`: `412b030b09caac1d9525945da042e04d2253b75f`.
- `res/43326334391-upgrade-candidate`: `d1e0fa717e238b5b6b70a764b461fe4c3f03c922`.

The harness has `docs/e2e-fix2/classification.csv` with the original evidence for **all 13 FAILs**. There are **zero ERROR verdicts**: fresh G is 81 PASS / 7 FAIL / 12 INFO; upgrade G is 84 PASS / 6 FAIL / 15 INFO. Each old-client path has ten passes; the five upgrade old-server V checks also pass.

| Path | Check | Classification | Finding / repair |
|---|---|---|---|
| fresh | G4a | Product defect | P-root-log: two workers logged one actual root creation |
| upgrade | G4a | Product defect | P-root-log |
| fresh | G10 measurement_value | Harness defect | H-measurement: fixture concept not requested |
| upgrade | G10 measurement_value | Harness defect | H-measurement |
| fresh | G17b measurement.value_stats | Harness defect | H-measurement |
| upgrade | G17b measurement.value_stats | Harness defect | H-measurement |
| fresh | G21 st4 | Harness defect | H-redraw: public numeric metadata counted as noise cells |
| upgrade | G21 st4 | Harness defect | H-redraw |
| fresh | G21 st6 | Harness defect | H-redraw |
| upgrade | G21 st6 | Harness defect | H-redraw |
| fresh | G21 st8 | Harness defect | H-redraw |
| upgrade | G21 st8 | Harness defect | H-redraw |
| fresh | G16b big | Product performance defect; fixture also defective | P-latency remains open; H-big-fixture repaired |

`command-errors.csv` additionally indexes every one of the **60 scenario-level ERROR diagnostics** by original file/line. These are not additional failed verdicts: most deliberately test duplicate-node pooling, old-client protocol refusal, analyst injection, all-null support, DP-only restrictions, tools-disabled ordering, revoked source permission or connection loss. Correct refusals must remain refusals. The ten positive measurement command errors are H-measurement. Scenario D's pooled `achilles_results` request has no registered physical result table, and its `condition.duration_stats` request lacks eligible support; those are harness fixture limitations with correct fail-closed product behavior, not evidence to relax either gate.

`server-errors.csv` indexes **177 server error headings**, including repeated Opal wrappers of the same refusals. It also identifies D's blocked birth-year profiling, unregistered Achilles result table, invalid `tool_id='achilles'`, and absent cohort definition 1 as negative/invalid fixture calls. The transient startup errors (profile initialization, unavailable default R service/sqldf, pool size and duplicate administrator ACL) belong to the external harness stack; later installation and ready-service checks succeeded. They are not dsOMOP installation errors. D's incomplete positive method coverage is retained as a limitation; this job does not call those missing fixtures product defects or claim they were exercised positively.

An additional real harness setup error appears in `fresh-steps.log:3441`: duplicate `drug_exposure_id=62142` prevents a primary key from being created, aborting the remaining large-fixture indexes and VACUUM/ANALYZE. The new setup rekeys **only the copied synthetic database's** drug surrogate IDs, preserving every clinical row, then builds its indexes. The dump/restore pipe also propagates failures. A PostgreSQL probe with duplicate IDs verifies row count, distinct IDs, quantity total and successful primary-key creation.

## Upgrade: what actually happened

The premise that 2.10.3 never installed is stale for this run. `upgrade-path.txt:133–155` shows the local Opal call returned success while Rock still had **2.10.2**, followed by the existing harness fallback installing **2.10.3**, exit status **0**, library **`/var/lib/rock/R/library`**, and **`* DONE (dsOMOP)`**, including both package-load checks. The new package was subsequently published correctly and G ran on 2.10.3.

The archived Rock Rserve log records `filelock` installation successfully at lines 98–100, then successful 2.10.2 installation at 114–126. The fallback's full 2.10.3 install transcript is in `upgrade-path.txt`. There is **no missing-import, missing-DESCRIPTION-dependency, compilation or lazy-load error** in this run to fix. Accordingly, no speculative dependency was added.

The archived REST log records the local request at 23:17:08 with manager `local`, an uploaded archive path, HTTP **201**, and a **167 ms** duration. No corresponding 2.10.3 install appears in the archived Rserve log. This establishes a local-upload/install-path failure, not a dsOMOP load failure. The precise lower-level reason for that no-op is **not present in these artifacts**. Opal's [6.2.0 installation helper](https://github.com/obiba/opal/blob/6.2.0/opal-r/src/main/java/org/obiba/opal/web/r/RPackageResourceHelper.java) catches installation exceptions, so HTTP success alone is not proof of package installation; this source behavior is not claimed as proof of a particular missing exception in these logs.

Harness installation now always executes `R CMD INSTALL` on the exact freshly built archive as Rock's own R user, checks its status and final version, and records the install log. It cannot accept an older build merely because it also says 2.10.3. Imports are installed automatically and missing dependencies stop the run; failed builds cannot select a stale tarball. No data owner setup is introduced. Local uploads remain exercised, with explicit publication as before.

A separate product defect is visible at `upgrade-path.txt:337`: although 2.10.3 is installed, the client says publication cannot add the protocol and demands 2.10.4. The protocol was backported into this candidate. The client minimum is now 2.10.3, and the stale-2.10.2-profile test requires advice to **Publish** rather than upgrade again. Actual method/capability checks still control execution; a version number alone grants no capability.

## Root location, permissions and concurrency

Both G0a records show Rock's R identity **`rock`**, HOME **`/var/lib/rock`**, state **`/var/lib/rock/.dsomop`**, owner **rock**, directory mode **0700**, writable **TRUE**. The root exists as **`secrets/dp_noise_root`**, **32 bytes**, mode **0600**. Upgrade G1d passes: the 2.10.2 root, identity and mtime survive installation/publication. Both restart tests pass with identical file metadata/key identity and zero new creation messages. There is no missing-root or permission defect in this run.

G4 deliberately moves the root aside to test concurrent first use. Both workers return the same identity and there is one private file, but each logs creation because absence was checked **before** taking the bootstrap lock. The fix observes the winning atomic hard-link commit inside the locked creation path and announces creation only after durable synchronization. Losing/adopting workers remain silent. Secret bytes, paths, lock/no-clobber validation, corrupted-root refusal and automatic initialization are unchanged. A four-process test checks one identity, one creation message, file size/modes and quiet reuse.

The state directory is **not on a Docker volume** in this disposable harness. Restart persistence is proved; container-replacement persistence is not. No claim that an R package can create a host volume, and no new administrator configuration prerequisite, is made.

## Measurement and redraw checks

The original GiBleed numeric population is all-null and correctly fails final support. The harness then inserts 500 numeric records for **concept 3004410**, unit **999001**, but calls `measurement.value_stats` with **empty parameters**. That selects a different catalogue default concept. The positive G10 and G17 calls now explicitly request the fixture concept; G10 checks the actual schema (`n_persons`, `n_values`, `mean_value`). The original all-null refusal check remains.

`test-e2e-fix2.R` uses the actual exported endpoint with real SQL, cryptography, support and signed payloads. The omitted-concept call refuses; the explicit concept returns finite signed H components with the realised-sticky label and identical repeated results; replacing values with NULL refuses again. No bounds, nfilter, admission, unit separation, cap or protected fallback was changed to force a pass.

G21 previously counted all numeric frame columns, including invariant bin numbers, years and concept IDs. Hence the reported 107/234 or 113/240 changed cells could fail despite actual redraw and agreement across workers. Its oracle now compares only the complete signed N/S/Q/H components and canonical parent, aligned by owner/stratum/coordinate. Missing or mismatched component inventories fail. Every fixed family must supply evidence and more than half its signed components must change; display metadata cannot turn a stale basis into a pass. Pure regressions cover redraw, replay, changed metadata with stale components, and missing components. The round-three membership/snapshot logic itself is unchanged; the earlier removed-generation-helper issue was already fixed before this run.

## Performance boundary

G16's original times were cold **24.7 s**, warm **18.6 s**, another warm question **16.6 s**, four concurrent cold **86.5 s**. The warm limit is **15 s** and remains unchanged. Generation alone was **0.771 s**, so dropping coherent generation or its guarantees would not be an acceptable remedy.

The UTF-8 HMAC path now hashes the same bytes directly through OpenSSL, without copying a large character chunk into an R raw vector and formatting it back. Raw inputs keep their previous path. Independent raw-byte comparisons cover empty, ASCII, Unicode and a 700 KB input; existing canonical/sampler/crypto tests pass. On the shared pod the million-person builder with profiling took **15.552 s before**, **15.337 s after**, and **the complete built result is identical**. This is a small optimization, **not a solution demonstrated to meet the live SLO**. The broken large-fixture setup is independently repaired, but the latency miss remains conservatively classified as a product performance defect pending a valid live measurement and further optimization if needed. No cache, generation shortcut, cryptographic weakening, limit relaxation or feature removal was used.

## Targeted validation

All R executions used `/workspace/dsomop/tools/rtest.sh`; no full suites ran. Detailed/summary logs and replay sources are retained in server `tools/e2e-fix2/`. Matching sibling worktrees were installed through `RTEST_WITH` for product tests.

| Tests | Tested implementation | Result |
|---|---|---|
| Server `test-e2e-fix1.R`, `test-e2e-fix2.R`, `test-state-secrets.R`, `test-privacy-dp.R`, `test-privacy-bigint.R`, `test-tools-release-record-histogram.R`, `test-tools-release-records.R`, `test-dp-generation.R` | server 87bc430, client 7907881 | **6,287 passed, 0 failures/errors, 4 skips** |
| Client `test-publication-status.R`, `test-privacy-protected-client.R`, `test-versions.R` | client 7907881, server 87bc430 | 3,060 passed; two stale test assertions still expected the former 2.10.4 advice; zero errors/skips |
| Client `test-versions.R` after correcting those assertions | client 398f759, server 87bc430 | **47 passed, 0 failures/errors/skips**; the other two files were unchanged and already passed |
| Harness `scripts/tests/g-contract.R` plus SQL fixture and syntax probe via isolated `test-harness.R` | harness 95bfa27 | **3 wrapper assertions passed, 0 failures/errors/skips**, including all internal stopifnot contracts, same-version installation, failed build/dependency handling, and PostgreSQL duplicate-ID repair |
| Vendor record-summary target | server 87bc430, client 398f759 | **381 passed, 0 failures/errors/skips** on PostgreSQL 14, MariaDB 10.6 and MySQL 8.4 (the runner’s opt-in vendor environment) |
| Million-person mechanism profile and full-object comparison | before/after string HMAC | exact complete-result equality; timings above |

The initial server skips were macOS-only canonicalization plus the three opt-in vendor record-summary cases; the vendor run below addresses those database skips. The initial new endpoint/concurrency probe passed eleven assertions before the HMAC regression was added. The client first-run failure is retained and was not counted as a green run. The existing minimal loader fixture's missing-optional-table warning is expected.

## Privacy, deviations and handoff

David's rules and the 2.10.5 P0 design remain binding: no TLS requirement, call quota, privacy accountant, analyst/session-dependent answer, new persistent state, relaxed support gate, exact fallback, exposed secret/fingerprint or feature withdrawal. Persistent state remains the noise and pseudonym roots. Fixed and realised-sticky claims, coherent source snapshots, provenance, public scales, signed pooling and documented evolving-data limitations are unchanged. The optimization preserves even the existing mechanism bytes.

The requested “fix everything” outcome is **not fully achieved for P-latency**, and the exact upstream local-install no-op cause cannot be proved from the saved logs. Installation is nevertheless made concrete and checked through the successful service-user route. Scenario D's fixture-limited negative calls and its four unexercised methods are not claimed as positive coverage. A new real Opal run remains necessary for runtime acceptance of the repaired candidate. No push, PR, tag, stash, sub-agent, claude CLI or pod lifecycle operation was performed.

Disk stayed around 9.7 GB free. No full suite or large package copy was made. Only the job's own small isolated probe and duplicate downloaded/extracted evidence are cleaned after retaining their indexes, hashes, replay sources and logs; exact paths/sizes are in `evidence/cleanup.json`. The original result branches and other jobs' files are untouched. Normal runner-managed package mirrors remain.
