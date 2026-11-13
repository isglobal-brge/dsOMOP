# e2e-fix3 — run 43336373825

13 November 2026. **The run has one failed verdict: large-CDM latency.** The four earlier findings in the task description are already repaired in this candidate. This job corrects additional harness defects and reduces product overhead while preserving exact outputs. **G16's real-Opal 15-second warm acceptance remains open.** The measured improvement is insufficient to claim that defect resolved; no new real-Opal run was performed.

## Commits and scope

Product branch `fix/2.10.3-ohdsi-p0`, only `/workspace/dsomop/wt/p0/{dsOMOP,dsOMOPClient}`. Both packages remain **2.10.3**.

- Server **7932f62**: canonical person-token fast path, identical fixed-width SHA-256 framing, avoid unused public-stratum tabulation, bulk consumption of the exact sampler bitstream; regression and million-person comparison fixtures.
- Server **3d1db25**: exact two-digit rational-coin arithmetic with arbitrary-precision fallback; boundary/cursor comparisons and NEWS.
- Client remains **398f759**. No client repair is justified by this run; its prior publication/version fixes pass.
- Harness **70f1499**: scenario D fixture corrections, supported CLI version reporting, exhaustive error index.
- Harness **ff353fc**: vendor server 3d1db25 with the matching 2.10.3 client for the next candidate run.
- Harness **1740dab**: recognize client `ds_errors` in method audits; explicitly retain the absent persisted-definition negative control. This supersedes 70f1499's attempted use of temporary cohort 100: temporary creation writes membership, not a persisted definition.
- Harness **06ccc5f**: correlate generic Rock disclosure errors with the audited request.

Evidence/report commits follow these implementation commits. The final repository logs identify them. A tracked copy of this report is server `tools/e2e-fix3/report.md` and harness `docs/e2e-fix3/report.md`. Nothing was pushed, tagged, or published.

## Evidence and every FAIL / ERROR

Immutable result refs in the requested harness clone:

- `res/43336373825-fresh-candidate`: **af314a2ddb410767d2892f9cceb90698533185f6**.
- `res/43336373825-upgrade-candidate`: **580540c9d52d940b9bcb746ba9e614d78d939c79**.

Inspected the summaries, scenarios, installation/path logs, console logs and all four `*-srv-logs.tgz` archives. Their extracted Opal/Rock logs include Rserve, REST, DataSHIELD, Opal and Rock logs; SQL/polyglot logs are empty. Harness `docs/e2e-fix3/manifest.json` records hashes and sizes for every downloaded/extracted input. `classification.csv` records every failed verdict; `all-errors.csv` records **463 failure/error headings**, including duplicate wrappers and copies, with original file/line and a disposition. The audit script leaves **zero unclassified headings**. Structured DataSHIELD errors that omit the R reason are correlated with the nearest Opal error; bare Rock disclosure errors use the adjacent audited request.

Fresh G: **87 PASS / 1 FAIL / 12 INFO / 0 ERROR**. Upgrade G: **90 PASS / 0 FAIL / 15 INFO / 0 ERROR**. Each old-client path has **10 PASS / 0 FAIL / 0 ERROR**. There are **49 scenario-level ERROR captures**; these are not 49 failed security checks.

| Finding | Classification | Evidence and disposition |
|---|---|---|
| Fresh G16b big, the only FAIL | Product performance defect, still open | Warm and warm-other **18.7 s**, limit **15 s**. Cold **24.7 s** and four concurrent cold **86.5 s** pass their 60/120 s limits. Pure builder optimization below does not establish live acceptance. |
| D positive `condition.duration_stats` | Harness fixture defect | The requested fixture lacks sufficient eligible support; product refusal is correct. D now uses the known-supported protected `achilles.3` for positive aggregate-method coverage. Low-support controls remain. |
| D `ohdsi.summary("achilles")` | Harness input defect | `achilles` is not an OHDSI summary tool ID. Use the registered `cohort_diagnostics` ID. |
| D absent cohort definition reported PASS | Harness audit defect | `.ds_safe_aggregate` returns server errors in `ds_errors`, rather than always throwing. D now recognizes that attribute and records refusal, not success. GiBleed has no supported persisted definition fixture; the absent-definition call is explicitly a negative control. |
| Both stack logs: `No such option '--version'` | Harness CLI defect | Replace unsupported `easy-opal --version` with `uv tool list`, which reports the installed tool/version. |
| Sensitive birth-year profiling, absent physical Achilles result table | Expected negative controls / harness diagnostics, not product defects | Keep the sensitive-column and registered-table gates. The corrected method audit no longer treats returned `ds_errors` as PASS. Positive physical-result fixture coverage is not claimed. |
| G6 duplicate-source pooling, injected generation/epoch fields, all-null measurements, DP-only heuristic requests, tools-disabled requests, revoked read access, killed connections and old-client protected-contract requests | Expected negative controls / harness diagnostics, not product defects | Fail-closed refusals are required. Their corresponding G verdicts pass. No product gate was weakened. |
| Opal startup profile race/authentication and R-service errors around deliberate restart | External harness-stack lifecycle diagnostics | Opal startup and restart, not dsOMOP installation/lazy-load failures; subsequent readiness, publication and replay checks pass. |

The exhaustive index contains **1 product failure, 19 harness-defect headings, 433 expected-refusal headings and 10 stack-lifecycle headings**. Expected refusals are explicitly distinguished from defects rather than mislabeled as broken product behavior. D's original 68 PASS count was overstated by its `ds_errors` handling; its four NOT-EXERCISED methods and absent positive physical-result/definition fixtures are not claimed as covered by this job.

## Upgrade installation: no missing product dependency

`upgrade-path.txt:133–155` shows:

1. Opal's local-package call reports success, but the next service-session check still sees **2.10.2**.
2. The harness then installs the **exact candidate archive** through Rock's service R session into **`/var/lib/rock/R/library`**.
3. `R CMD INSTALL` returns **status 0**, passes both temporary/final package-load checks, prints **`* DONE (dsOMOP)`**, and the fresh check sees **2.10.3**.

The archived upgrade `Rserve.log:118–155` shows `filelock` installed successfully before the old 2.10.2 package. There is **no missing Import, missing DESCRIPTION dependency, compilation error or lazy-load error** to repair. No speculative dependency was added.

Archived `upgrade-opal-srv-logs/rest.log:690` records the local request at 00:30:36 with manager `local`, uploaded archive path, HTTP **201**, and **297 ms** duration. No corresponding 2.10.3 installation transcript appears in archived Rserve.log. The evidence establishes an Opal local-install no-op followed by a successful service-user installation. **Its lower-level upstream cause is not present in these artifacts**; it would be incorrect to invent a missing dependency as the cause. Existing exact-archive installation/status/version verification remains in place and is regression-tested, including same-version candidates and failed builds/dependencies.

Before publication the installed server is 2.10.3 while the profile still publishes 2.10.2. The client correctly advises Publish. After publication the declared **59 aggregate + 14 assign methods and all 36 options** match, with no missing or stale entries. This is the intentional upgrade-without-publish check, not installation failure.

## Noise root, permissions and plug-and-play behavior

Both G0a records identify R user **rock**, HOME **`/var/lib/rock`**, state **`/var/lib/rock/.dsomop`**, owner **rock**, mode **0700**, writable **TRUE**. The root is **`secrets/dp_noise_root`**, **32 bytes**, mode **0600**. No administrator provisioning was needed.

Upgrade G1d proves the 2.10.2 root/key identity/file timestamp survives installation and publication. Both G4a checks pass concurrent first use: one root, identical identities, exactly one creation message. Both restart checks pass with identical root metadata/key identity and no new creation message. Root-location/permission/missing-root changes are therefore unwarranted for this run.

The state is not on a Docker volume in this disposable harness. Process/container restart persistence is proved; container-replacement persistence is not. This job adds no state directory setting or administrator prerequisite and does not claim that an R package can provision host volumes.

## Measurement and generation: prior corrections confirmed

Both paths pass **G10 measurement_value**, **G17b measurement.value_stats**, **G3d1**, and the membership/clinical-edit G21 checks. The correct fixture concept **3004410** is now explicitly requested. G17 examines actual finite signed H components; its informational `histogram cells FALSE` field concerns older owner metadata, not absence of signed H. A protected `mean_value=NA` on an unregistered scale is intentional: the record H fallback remains available without inventing public moment bounds. The all-null negative control still refuses.

No stale generation helper remains in the exercised G3d1 path. G21 compares signed components rather than invariant public display coordinates, and insertion/deletion/balanced membership replacement redraws all the expected components consistently across handles/sessions. No product generation change or another speculative harness rewrite was needed here.

## Product optimization and the open latency limit

The hot path performs per-person HMACs, contribution framing and exact rational sampling. Repairs preserve the existing mechanism bytes and outputs:

- The builder already normalized person IDs; its internal token path avoids normalizing them again. The public/general helper retains canonicalization and validation, including exact integer64 IDs.
- SHA-256 tokens are 64 ASCII bytes, so unit records use the identical literal `64:` prefix without a million repeated length scans/conversions. Public strata no longer build an unused exact-count table.
- Uniform draws take consecutive bits in blocks from the same request-local HMAC cursor. Scalar draws and block draws share the cursor and unbounded block counter; no unused entropy is discarded.
- For rational coins with up to four 16-bit limbs, two base-2^32 digits perform the same exact decisions. Every intermediate digit is an integer of magnitude below 2^33, exactly representable in R doubles. Larger inputs keep the original arbitrary-precision path. No probability approximation, tail cutoff, cap or extra dependency is introduced.

The retained baseline builder/sampler are from server 29c960a. The comparison byte-compiles baseline functions and checks **identical complete built objects** for one million persons under both fixed and realised routes, using real cryptography. Warm results on the shared test pod for the final code:

| Route | Baseline | Repaired | Complete result |
|---|---:|---:|---|
| fixed_synopsis | 12.802 s | 12.258 s | identical |
| realised_sticky | 12.548 s | 11.416 s | identical |

These are builder timings, not end-to-end Opal timings. The first baseline call also paid process/string initialization costs; its larger first-call difference is not used as evidence of the warm improvement. The fixed-route improvement is about **0.54 s**, far short of demonstrating recovery of the observed **3.7 s** live overrun. **P-latency remains unresolved pending further performance work and a real-stack run.** The harness thresholds remain unchanged, and the next candidate is vendored locally.

## Targeted validation

All R work used `/workspace/dsomop/tools/rtest.sh` on the existing pod. No full suite ran. Detailed logs are retained in server `tools/e2e-fix3/evidence/`.

| Test files | Code tested | Result |
|---|---|---|
| Server `test-e2e-fix1.R`, `test-e2e-fix2.R`, `test-e2e-fix3.R`, `test-privacy-sampler.R`, `test-privacy-bigint.R`, `test-privacy-dp.R`, `test-tools-release.R`, `test-dp-generation.R`, `test-state-secrets.R`, `test-tools-release-record-histogram.R`, `test-tools-release-records.R` | **3d1db25**, sibling client **398f759** installed via RTEST_WITH | **10,377 passed, 0 failures/errors, 4 skips** |
| Client `test-privacy-protected-client.R`, `test-publication-status.R`, `test-versions.R` | **398f759**, sibling server **3d1db25** installed via RTEST_WITH | **3,062 passed, 0 failures/errors/skips** |
| Server `test-e2e-fix3-performance.R` plus `test-e2e-fix3.R`, opt-in million-person comparison | final implementation committed as **3d1db25** | **1,720 passed, 0 failures/errors/skips**, including both complete-object equality checks and exact bit-cursor comparisons |
| Harness `scripts/tests/g-contract.R`, changed R syntax, through isolated `test-harness.R` | final harness behavior **1740dab** | **1 wrapper assertion passed**, all embedded stopifnot checks passed; **0 failures/errors/skips** |
| Python audit and shell syntax | final audit script / `scripts/stack.sh` | 463 indexed headings, zero unclassified; `bash -n` passed |

The server skips are macOS-only state-path canonicalization and the three opt-in PostgreSQL/MySQL/MariaDB record-summary comparisons. There are no SQL/driver changes in this job. The existing minimal loader fixture emits its expected missing-optional-table warning. The sampler suite includes its distribution checks and exact-arithmetic/reference comparisons; the new probes also compare bit consumption across 256-bit boundaries, huge block counters, 32/64-bit arithmetic boundaries and larger fallback inputs. The harness was rerun once after its detailed pod log was removed by the later normal server mirror; its final detailed log is retained.

## Privacy, deviations and handoff

All David rules remain: no TLS requirement; automatic private roots; no accountant, call budget/quota, user/session-dependent answer, cross-request answer cache, additional persistent state, exposed fingerprint, weaker support/provenance/snapshot check, exact fallback or removed protected feature. Protocol identifiers, keys, signed components, fixed/heuristic labels, masks and pooling semantics are unchanged. Persistent product state remains the two secrets.

The requested fully-fixed outcome is **not achieved for latency**. The named earlier functional failures are proved repaired by this run, and the remaining harness defects are fixed, but the new optimization alone does not justify closing G16. The lower-level Opal local-upload no-op cause also cannot be proved from the saved logs; the exact candidate nevertheless installs and loads successfully through the verified service-user path. No new live run was launched, and no live acceptance is implied by unit/pod results.

Disk remained approximately **9.7 GB free**. No full suite or large package copy was performed. Cleanup is limited to this job's small harness probe and duplicate downloaded/extracted evidence after retaining summaries, hashes, indexes, replay sources and test logs. Exact owned paths and byte sizes removed are in `tools/e2e-fix3/evidence/cleanup.json`, `pod-cleanup.json`, and harness `docs/e2e-fix3/cleanup.json`. The immutable inputs can be recreated with harness `docs/e2e-fix3/extract.py`. Other worktrees, the shared primary checkouts, refs and original result branches remain untouched. No claude CLI, sub-agent, stash, push, PR, tag or pod lifecycle action was used.
