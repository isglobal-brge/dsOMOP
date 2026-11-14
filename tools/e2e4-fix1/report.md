# e2e4-fix1 — real-Opal run repairs

13 November 2026. Product and harness repairs are committed locally. **Run 43452232732 is fully classified; run 43452223095 cannot be fully audited because its result refs/logs are absent and this environment has no authenticated GitHub read access.** Its version preflight defect is reproduced from the committed harness. The live G16 latency acceptance still needs a rerun; pod timings are not an Opal pass.

## Scope and commits

- Server `/workspace/dsomop/wt/int/dsOMOP`, branch `feat/2.10.3`: **77c6e6c**, request-local OpenSSL acceleration for exact person HMACs and contribution chunks, automatic portable fallback, regression tests and NEWS.
- Client `/workspace/dsomop/wt/int/dsOMOPClient`, branch `feat/2.10.3`: unchanged at **30a9794**. No client defect was established by the supplied e2e evidence.
- Requested harness clone, branch `main`: **b19398a**, coordinated candidate version checks, numeric legacy probes, preparation failure handling, birth-column fixture repair and honest method coverage; **f7ca7e5**, vendor server 77c6e6c with matching client 30a9794; **a2617aa**, avoid generated Python bytecode; **6870e7c**, record native acceleration in G16 evidence.
- Evidence/report commits follow these implementation commits. Tracked copies of this report live in server `tools/e2e4-fix1/report.md` and harness `docs/e2e4-fix1/report.md`.

Both packages still declare **2.10.3**. The conceptual 2.10.4 work was integrated/backported under that version. The original harness incorrectly required the prospective 2.10.4 label, although the prior publication repair and the integrated sources already identify 2.10.3 as supporting the native/publication contracts. No product version bump or frozen-interface change was needed.

## Evidence and failure classification

Available immutable result refs:

- `res/43452232732-fresh-candidate`: `4bfbe4b29b36b24ce7e1b6ab381331ec96778129`.
- `res/43452232732-upgrade-candidate`: `29040504ac466e72714ec0c357fdaca6e005e2e9`.

Inspected all result files and all four archived Opal/Rock log bundles (62 extracted files). The harness report directory contains a reproducible extractor, SHA-256 input manifest, both summaries and verdict tables, `classification.csv` for the two distinct failed verdicts, and `all-errors.csv` for every retained failure/error heading. The audit contains **432 headings: 1 product failure, 18 harness-defect headings, 401 expected-refusal diagnostics, and 12 stack-lifecycle diagnostics; zero unclassified headings**. Repeated wrappers, archived log copies and the V2 row copied into G are not separate defects.

Fresh G: **87 PASS / 1 FAIL / 12 INFO / 0 ERROR**. Upgrade G: **87 PASS / 1 FAIL / 16 INFO / 0 ERROR**. Both old-client runs: **10 PASS / 0 FAIL / 0 ERROR**.

| Finding | Classification | Cause, repair, validation |
|---|---|---|
| Fresh **G16b big** | Product performance defect | Cold 28.8 s and four concurrent cold 84.5 s pass 60/120 s limits. Warm 18.7 s fails the unchanged 15 s limit; warm-other 20.7 s is also recorded (the existing predicate gates warm, not warm-other). Per-person keyed hashing and contribution framing dominate the builder. 77c6e6c reuses an OpenSSL HMAC context within each request and streams the identical contribution chunks. Exact oracle and full-object tests pass; live acceptance remains unverified. |
| Upgrade **V2**, repeated as **V V2** in G | Harness | `upgrade-path.txt:100` refuses preparation because default translation needs `output.concept_labels` on the old server. Line 101 then releases nonexistent `V_ages`. This is the documented A-27 refusal, not a product regression. The probe explicitly sets `translate_concepts = FALSE`, keeps a numeric v1 recipe, and aborts on failed preparation. The targeted harness replay checks both preparation outcomes; client protocol tests cover the real old-server contract. |
| D birth-column probes for both `block_sensitive` values | Harness | `factor_concepts = FALSE` conflicts with default translation, so neither probe reached its intended server operation. Explicitly disable translation as documented. These are diagnostic probes, not evidence of a product birth-column refusal. |
| D `omopAsFactorColumnsDS` credited PASS with zero logged calls | Harness coverage defect | A successful wrapper was credited to every optional method. A zero-call method now reports NOT-EXERCISED. The original four other NOT-EXERCISED methods remain coverage gaps, not product failures. |
| Contract candidate preflight | Harness, locally reproduced | `scripts/o/check_candidate.py` rejected both integrated 2.10.3 sources before provisioning. It now requires equal versions >=2.10.3, integrated native normalizer/label sources, fixtures and all 15 acceptance plans. O1 still requires actual published/running version equality and native operations. Install/setup/O10 use the coordinated source version. Python negative controls reject mismatched versions, old candidates, missing native sources and incomplete corpora. |
| O6's connection-time old-server refusal assertion | Latent harness defect found in source review; not a verified run outcome | Connecting for supported v1 work is permitted. The repaired probe connects, checks that a native request refuses without assigning outputs, then exercises numeric v1 execution. Runtime validation on real 2.10.1 remains for the next contract run. |
| D absent cohort definition, absent physical result tables, protected-column profiling; G duplicate-source pooling, injected seed/epoch/generation fields, all-null support, disabled/DP-only routes, revoked reads, killed connections and old-client protected calls | Harness negative controls / expected refusals | Preserve fail-closed product behavior. The absent-definition control is already honestly REFUSED, not a new defect. Corresponding positive security verdicts pass. |
| Opal initialization/authentication and R-service errors during startup/restart | Harness stack lifecycle diagnostics | Later readiness, publication, root replay and restart checks pass. No dsOMOP installation or namespace-load failure is established. |

### Missing contract-run inputs

No `res/43452223095-*` or equivalent origin refs exist in this clone. HTTPS fetch/listing fails authentication; `gh auth status` has no authenticated host; SSH read access also fails. The missing inputs were requested from the user while independent work continued. No reply or refs arrived during the work. The workflow uploads artifacts rather than publishing result branches, which explains why those refs cannot be assumed to exist.

The original preflight deterministically fails on the vendored source pair before stack provisioning; this supports the harness finding above. **It does not establish the exact failed steps or any O1–O12 runtime outcomes for Opal 6.1.3/5.7.6 × fresh/upgrade.** Those four jobs remain unverified pending their actual logs/artifacts. There is no claim that every failure of that inaccessible run has been classified.

## Product change and privacy invariants

The native helper uses OpenSSL's HMAC-SHA256 implementation, resetting a keyed context for each person or contribution chunk. It does not implement a new hash or sampler. Person input framing is unchanged. Contribution chunks retain the same 10,000-record boundaries, header, count, newline separators and absence of a trailing newline; final canonical metadata hashing remains unchanged.

Contexts are local to one call, freed on normal completion, errors and R interrupts. There is no cross-request cache, new persistent state, key exposure, change to support gates, sampler, deterministic answers, noise domain, allocation, signed components or pooling. No TLS requirement, limits, quotas, privacy accountant or user/session-dependent answer was added.

Configure detects usable system OpenSSL headers/library automatically. Without them, including the Windows build, the compiled dispatcher returns a sentinel and the existing R openssl implementation runs unchanged. Compilation does not download/install anything. The existing runtime-state setup and key-free installation behavior remain. Accelerated Linux builds are measured here; portable builds preserve correctness but do not claim the latency improvement. G16 now records which path is active.

## Targeted validation

All package tests use `/workspace/dsomop/tools/rtest.sh` on the existing pod. No full suite, live CI run, push, tag, PR, stash, sub-agent or claude CLI was used.

- `test-e2e4-hmac.R` compares direct native hashes to R openssl for two keys, empty/ASCII/Unicode/separator-bearing domains, repeated IDs, large exact integer text, invalid keys, NA recovery and vectors beyond the interrupt boundary. Contribution tests cover 0, 1, 9,999, 10,000, 10,001 and 20,001 records with empty and Unicode/newline-bearing records.
- `test-e2e4-profile.R` compares complete built releases with the unmodified b9cb9f8 hashing helpers under both protection routes, one million persons, two repetitions. **32 assertions passed across these two files, zero failures/errors/skips.** Code was committed as 77c6e6c; final targeted regression runs use that committed source.
- Warm builder results: fixed_synopsis **11.525 → 7.866 s**; realised_sticky **12.712 → 7.331 s**. Complete objects are identical in all four comparisons. These are shared-pod builder timings, not end-to-end Opal timings.
- The initial profiling-only run passed one assertion. An intermediate run of `test-e2e-fix3-performance.R`, `test-e2e4-hmac.R` and the initial profile passed 25 assertions. Its old performance fixture resolves some current helpers, so its timings are not used as the independent baseline; the final comparison explicitly loads the original hashing helpers.
- Portable native dispatcher compiled without OpenSSL linkage, returned the fallback sentinel for both entry points and rejected malformed keys. Roxygen **7.3.3** regenerated NAMESPACE successfully after native compilation. An initial attempt before the DLL existed failed and was superseded by the successful regeneration.
- Harness `scripts/tests/g-contract.R` passed its embedded assertions locally as a base-R probe and through a one-test isolated pod wrapper: **1 wrapper assertion, zero failures/errors/skips**. It covers successful/failed old-server preparation, candidate archive installation checks and method-audit error handling. Python preflight tests: **2 passed**. Changed R files parse; shell syntax checks pass.

Server final run on **77c6e6c**, with sibling client **30a9794** installed: **5,246 passed, zero failures/errors, four skips**. Exact filter:

```text
^e2e4-hmac$|^e2e-fix3$|^tools-release$|^tools-release-(hierarchy|records|record-histogram|survival|time-to-event)$|^dp-generation$|^state-secrets$
```

Files: `test-e2e4-hmac.R`, `test-e2e-fix3.R`, `test-tools-release.R`, `test-tools-release-hierarchy.R`, `test-tools-release-records.R`, `test-tools-release-record-histogram.R`, `test-tools-release-survival.R`, `test-tools-release-time-to-event.R`, `test-dp-generation.R`, `test-state-secrets.R`. Skips are the macOS-only path test and the three opt-in PostgreSQL/MySQL/MariaDB record-summary comparisons. This change does not modify SQL or database adapters.

Client final run on **30a9794**, with repaired server **77c6e6c** installed through `RTEST_WITH`: **3,680 passed, zero failures/errors/skips**. Files: `test-privacy-protected-client.R`, `test-v1-2.10.2-payloads.R`, `test-v2-protocol.R`, `test-publication-status.R`, `test-versions.R`. The source install log confirms automatic OpenSSL acceleration and successful namespace loading. No client source changes were made.

## Evidence, cleanup and handoff

Server evidence is in `tools/e2e4-fix1/evidence/`; harness evidence is in `docs/e2e4-fix1/`. Exact test commands, output logs, source commits and input hashes are retained. The source pair is vendored locally for the next run; 12 changed server files (152,079 bytes) were updated in place, and the already-matching client needed no copy.

Disk stayed near 9.7 GiB free. CLEANUP.md was read; no full test or large package copy occurred. Only this job's temporary harness probe, hash experiment and portable-build directory were removed after preserving logs. Paths and byte counts for the package probe and extracted inputs are recorded in cleanup evidence. The portable-build trap logged its directory path (`/tmp/dsomop-e2e4-portable-xsfK7q`) but did not capture its size before removal. Shared repositories, other worktrees, primary checkouts and refs were not modified. The requested external report is mirrored into both authorized git repositories so its contents are committed.

The coordinator must supply the missing 43452223095 logs/artifacts, then rerun the repaired candidate on both workflows to verify O1–O12 and G16. No frozen-interface change request was filed. No client repair or version bump is justified by the available evidence. The intended fully verified outcome remains incomplete until that missing evidence and real-stack acceptance are available.
