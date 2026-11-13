# S4 integration report

Status: complete. Both full suites ran exactly once with the vendor environment,
including MySQL. Every failing file has passed targeted rechecks after repair.
Final code commits: server `990973b`, client `a5918c6`. Both branches are `feat/2.10.3`.

## Scope and merge order

Worked only in `/workspace/dsomop/wt/int/{dsOMOP,dsOMOPClient}` on
`feat/2.10.3`. In each repository merged, in order, `feat/2.10.3-labels`,
`feat/2.10.3-ws10`, then local `main`. The release tag `2.10.3` resolves to
that local main in each repository and is an ancestor of the integrated head.
No main checkout, tag, remote branch, or other job worktree was changed.
No Claude CLI, subagent, push, or PR was used.

## Findings, fixes, and verification

- WS-10's notes called this release 2.10.4; security main carries 2.10.3.
  Consolidated the security and recipe notes under 2.10.3, preserving A-27
  classifications/migration and the protected-release guarantees and limits.
  Both DESCRIPTION versions remain 2.10.3.
- Server interface conflict: retained early closed-envelope validation and
  refusal contexts without dialect queries, together with security-main
  snapshot finalizers that revoke publications after a late failure.
  Verification: publication-snapshot, WS-2f validation, and live snapshot tests.
- MariaDB extractor: retained `.databaseVersionNumber()` byte-for-byte from
  56b983c; adapted its surrounding documentation. Verification: exact source
  comparison, mariadb-version, sql-expr, and live three-vendor SQL tests.
- Client transport conflict: retained the stricter deparse/parse B64 assertion.
  Removed the capability-negotiation override from the A-18 end-to-end test;
  fixtures requesting numeric output explicitly disable translation under A-27.
  Verification: phase-e-ergonomics and ws4-adapter-refusals-e2e.
- A-27 resealing called the pre-security signature without its source.
  Passed the already authenticated original object to preserve the release-person
  map and snapshot provenance. Verification: protected-factor seal and tamper
  checks in concept-label-presentation, plus real pooled table/GLM tests.
- Presentation metadata unnecessarily changed historical membership objects.
  Retain presentation-handle metadata only for concept/generated-name contracts,
  and omit false reference tags. Verification: complete WS-2f/A-26 baseline
  comparisons; the baseline data and classifications were not rewritten.
- The SQL operation inventory test omitted `output.concept_labels`; added the
  advertised operation to its expected complete set. Verification: sql-expr.
- A label fixture's `on.exit()` replaced withr's key cleanup, leaking a scoped
  provider into subsequent vendor tests. Preserve cleanup with `add = TRUE`;
  force helper cleanup environments and pass the snapshot test environment
  explicitly. Verification: combined label/publication/live snapshot run.

## Test environment and evidence

R tests use `/workspace/dsomop/tools/rtest.sh` on the existing pod, with the
matching sibling in `RTEST_WITH`. Vendor environment for all harness runs:

```
DSOMOP_TEST_POSTGRES_HOST=omopdb
DSOMOP_TEST_MARIADB_HOST=127.0.0.1
DSOMOP_TEST_MYSQL_HOST=127.0.0.1
DSOMOP_TEST_MYSQL_PORT=3307
```

The final runs additionally use `R_LIBS_USER=/root/rt/labels-deps` so real
`dsBaseClient`/`dsBase` analysis checks execute, and an R profile that increases
failure-report detail only. Full suites were each invoked exactly once with
`RTEST_FULL=1`; failures receive targeted repairs and rechecks.

- Independent contract checker: all 3 schemas, 219 probes, 15 acceptance plans,
  12 digest vectors, 17 normalized fixtures and numeric corpus pass.
- Roxygen 7.3.3 ran on the pod; pulled back only
  `man/dot-databaseVersionAtLeast.Rd`. Other generated changes were not imported.
- Client targeted run: 387 assertions, zero failures/errors; two analysis blocks
  initially skipped because dsBaseClient was outside the default library path.
- Repeated those analysis blocks with the existing labels library: 59 assertions,
  zero failures/errors/skips (real pooled tables and GLM).

Exact tested commits, file lists and results for every harness run are in
[s4-integ-test-manifest.json](s4-integ-test-manifest.json). Logs use the
`s4-integ-*` prefix in `/workspace/dsomop/design/astra-dev/`.

## Targeted test record

### server-targeted

Package `dsOMOP` at `761f7bc`, sibling at `b5aab2e`.

Files: `test-concept-label-contract.R`, `test-concept-label-presentation.R`, `test-mariadb-version.R`, `test-publication-snapshot.R`, `test-read-snapshot-generated.R`, `test-read-snapshot-views.R`, `test-read-snapshot.R`, `test-sql-expr-snapshots.R`, `test-sql-expr.R`, `test-staging-csv.R`, `test-vendor-db-integration-sql-expr.R`, `test-ws2f-base-seed.R`, `test-ws2f-explain-roles.R`, `test-ws2f-membership-results.R`, `test-ws2f-validation.R`.

`RESULT failed: 23 errors: 26 skipped: 0 passed: 3156`.

### client-targeted

Package `dsOMOPClient` at `d946bb2`, sibling at `250dc25`.

Files: `test-concept-label-analysis.R`, `test-concept-label-api.R`, `test-concept-label-contract.R`, `test-concept-label-federation.R`, `test-concept-label-legacy.R`, `test-phase-e-ergonomics.R`, `test-ws4-adapter-refusals-e2e.R`.

`RESULT failed: 0 errors: 0 skipped: 2 passed: 387`.

### server-targeted2

Package `dsOMOP` at `6141379`, sibling at `d946bb2`.

Files: `test-concept-label-contract.R`, `test-concept-label-presentation.R`, `test-mariadb-version.R`, `test-publication-snapshot.R`, `test-read-snapshot-generated.R`, `test-read-snapshot-views.R`, `test-read-snapshot.R`, `test-sql-expr-snapshots.R`, `test-sql-expr.R`, `test-staging-csv.R`, `test-vendor-db-integration-sql-expr.R`, `test-ws2f-membership-results.R`.

`RESULT failed: 22 errors: 25 skipped: 0 passed: 2693`.

### client-analysis

Package `dsOMOPClient` at `d946bb2`, sibling at `6141379`.

Files: `test-concept-label-analysis.R`.

`RESULT failed: 0 errors: 0 skipped: 0 passed: 59`.

### server-targeted3

Package `dsOMOP` at `45841d6`, sibling at `d946bb2`.

Files: `test-concept-label-presentation.R`, `test-publication-snapshot.R`, `test-read-snapshot-generated.R`, `test-read-snapshot-views.R`, `test-read-snapshot.R`, `test-ws2f-membership-results.R`.

`RESULT failed: 0 errors: 0 skipped: 0 passed: 743`.

## Full-suite findings and targeted repairs

The one full client invocation on `d946bb2` with server `45841d6` completed:
17,910 passed, 7 failed assertions, 17 errors, zero skips. Commit `3d8ad38`
updates nine affected test files for A-27 (no runtime behavior changed):
current-session mocks advertise `output.concept_labels`; numeric protocol/scope
fixtures disable translation explicitly; old servers refuse translated plans
before assignment; DP factor-method preflight depends on translation; staged
execution retains its translation and revalidates its binding; positional
formals include the appended maps. Presentation-only vocabulary differences
are allowed while descendant expansion still requires matching semantics.

The WS-7 recipe-07 test applies an explicit before/after assertion for its four
A-27 fallback column names (`vitals_<id>_<analysis>` to
`vitals_concept_<id>_<analysis>`). Frozen plans, expected values and baseline
classification files are unchanged; every column and value is still compared.

The first client repair run (`3d8ad38`, server `45841d6`) passed 6,500
assertions, leaving one mock-column error and the parameterized recipe-07
naming assertion (two backend failures). Commit `a5918c6` completed those two
fixtures. The final focused recheck of `test-plan-dsl.R` and
`test-ws7-client-acceptance.R` passed **5,417 assertions, zero failures/errors/
skips**, including all fifteen recipes and twelve parameterized variants on
SQLite and PostgreSQL, memory and staged. All nine files that failed in the
single full client run have now passed targeted rechecks. Full-suite and repair
logs are preserved separately; the full client suite was not rerun.

The full server run exposed pre-A27 IR snapshot assertions. An independent
read-only probe compared all 431 changed documents against an explicit overlay
of empty `labels` and `column_names` maps on options and variables: **zero
unexplained differences**. Commit `ee807cf` preserves the historical JSON and
its original digest assertion, then checks the exact amended document and
its digest. No golden file or classification was regenerated.

The server replay also exposed added frame metadata on untranslated private
v1 results. Probes spanning all eight representative output families reproduce
the exact historical hashes after removing only `omop_concept_cols`,
`omop_concept_reference`, and `omop_column_contract` from frames. Commit
`5f03cd6` limits that restoration to the legacy executor's private numeric
return, retaining outer-list concept tags. Published assignments, translated
outputs, native execution, publication gates, and provenance retain their full
metadata. The immutable compatibility fixtures remain unchanged. This
supersedes the narrower initial metadata repair in `3677714`.

The one full server run (`45841d6`, client `d946bb2`) completed all **186 test
files**: 71,738 passed assertions, 1,436 failed assertions, two errors and 12
skips. Failures were confined to these seven files:

| File | Failed assertions | Errors |
| --- | ---: | ---: |
| `test-ir-corpus.R` | 869 | 0 |
| `test-ir-v1-compat.R` | 475 | 0 |
| `test-ir-results.R` | 41 | 0 |
| `test-ir-results-a19.R` | 32 | 0 |
| `test-publication-v1-reference.R` | 18 | 0 |
| `test-publication-integration.R` | 0 | 2 |
| `test-ir.R` | 1 | 0 |

The focused diagnostic at `5f03cd6` confirmed that the private-return fix
eliminated 17 of 18 reference failures. Commit `458ce14` addresses the remaining
findings:

- `ir-corpus` and `ir`: assert empty A-27 maps explicitly. Four exact frozen
  corpus IDs (`v1-125`, `v1-126`, `v1-244`, `v1-246`) now assert the documented
  translation/factor conflict refusal, including its original contradictory
  inputs. Historical class rows, plans, IR snapshots and digests stay intact.
- `ir-results` and `ir-results-a19`: assert CSV descriptors advertise
  `backslash-v1`, then compare every historical descriptor field, fixture byte
  and content value to the frozen capture. Only that explicit new descriptor
  field is excluded from the historical comparison.
- `ir-results-a19`: expect v3 provenance's six additional fields and release-
  person map; verify its digest, row identities and snapshot, authenticate the
  complete capsule, and reject a tampered map. Original data, tokens and old
  policy fields still compare exactly to the v2 baseline. No v3 field was
  removed from production objects to satisfy a historical assertion.
- `publication-integration` and `publication-v1-reference`: numeric-only
  publication skips unused label schema work and reference presentation tags.
  Minimal numeric publication fixtures declare translation off. Translation
  remains enabled and fully checked by the label presentation tests.

The server's 12 skips are: CommonDataModel unavailable (2), SqlRender unable to
load (4), opt-in million-person profile (1), Arrow unavailable (2), Node.js
unavailable on the pod (1), a macOS-only case on Linux (1), and Python unavailable
on the pod (1). The independent Python contract checker passed locally. No live
vendor test was skipped. Five warnings were fixture catalog notices or already-
disconnected vendor cleanup connections; full details are preserved in
`s4-integ-server-full-skips.txt`.

The first server repair run (`458ce14`, client `a5918c6`) passed 14,672
assertions with no errors or skips; only five assertions in `test-ir-results.R`
remained. All other full-failing server files passed. Commit `990973b` adds an
explicit assertion of the new unordered gender factor, its vocabulary identity
and exact levels for frozen pin `r006`, then compares the complete historical
text view. It also removes the fourth internal presentation attribute,
`omop_reference_support`, from untranslated private v1 returns (the sparse
`r009` pin); public support evidence is retained. No frozen fixture changed.

Final client dependent checks against `458ce14` passed 206 assertions with
zero failures, errors or skips: `test-concept-label-analysis.R`,
`test-concept-label-federation.R`, and `test-concept-label-legacy.R`.
The subsequent `990973b` runtime change affects only private untranslated v1
returns; it does not alter public client execution.

## Deviations and operational notes

The single server full run exceeded its original one-hour wrapper limit. At
54:46 elapsed, suspended only the `timeout` watchdog (PID 306820), while R
(PID 306821) continued uninterrupted. The watcher resumed the watchdog after
R exited; both processes are now gone. No test process, source, or invocation was restarted. Evidence:
`s4-integ-watchdog.log`.

A state-leak diagnostic on the pod used `testthat::test_file()` directly for
`test-concept-label-presentation.R`, `test-publication-snapshot.R`, and
`test-read-snapshot.R` from the `6141379` mirror, retaining one R environment to
print provider settings before/after each file. Unlike harness verification,
that diagnostic did not configure vendor hosts; its five vendor skips are not
validation evidence. The later combined harness run and full suite exercised
them with all hosts configured. Read-only IR and result probes inspected the
recorded source snapshots without modifying the running full suite.

Git merge commits cannot accept partial pathspecs; merge commits therefore
commit the resolved index. Ordinary commits use explicit pathspecs.

The local workspace began below 10 GiB free. Followed CLEANUP.md, deleting only
old eligible Codex rollouts and its cache (231,772,254 bytes; every path/size in
`s4-integ-cleanup.json`). Review package copies have `.git` entries and were
left intact under the prohibition on deleting repositories. Approximately
9.7 GiB remains, below the cleanup target; no further known-safe deletions
would meet 15 GiB. Test execution uses the pod, with 27 GiB free; no large local
package copies were created. Local test-tool setup installed jsonschema without
a download cache to run the independent contract checker.

The requested report path is outside a Git repository. An identical copy and
the verification manifests are committed under server `tools/integration/`.

## Final verification

The final server recheck (`990973b`, client `a5918c6`) passed **467 assertions,
zero failures, errors or skips** in `test-ir-results.R`. All seven full-failing
server files and all nine full-failing client files have now passed targeted
rechecks. The full suites were not repeated. The original failing full logs
remain preserved separately from the successful repair logs.

The combined server repair run also passed label presentation, publication
snapshots, CSV staging, F-26 compatibility, and WS-2f membership dependencies.
The final client dependent run passed real pooled analysis, federation and
legacy label checks. No historical fixture or classification file was rewritten.

The exact source delta after the full runs is recorded in
[s4-integ-source-delta.json](s4-integ-source-delta.json). The server runtime delta
is limited to `R/plan.R` (private numeric v1 metadata) and `R/publish.R`
(avoid label work for numeric publication). The client runtime is unchanged
after its full run; its `R/plan.R` delta is a comment correction.

## Complete harness run ledger

| Run | Package commit | Sibling commit | Result |
| --- | --- | --- | --- |
| `server-targeted` | `761f7bc` | `b5aab2e` | failed: 23 errors: 26 skipped: 0 passed: 3156 |
| `client-targeted` | `d946bb2` | `250dc25` | failed: 0 errors: 0 skipped: 2 passed: 387 |
| `server-targeted2` | `6141379` | `d946bb2` | failed: 22 errors: 25 skipped: 0 passed: 2693 |
| `client-analysis` | `d946bb2` | `6141379` | failed: 0 errors: 0 skipped: 0 passed: 59 |
| `server-targeted3` | `45841d6` | `d946bb2` | failed: 0 errors: 0 skipped: 0 passed: 743 |
| `client-full` | `d946bb2` | `45841d6` | failed: 7 errors: 17 skipped: 0 passed: 17910 |
| `client-repair` | `3d8ad38` | `45841d6` | failed: 2 errors: 1 skipped: 0 passed: 6500 |
| `client-repair2` | `a5918c6` | `45841d6` | failed: 0 errors: 0 skipped: 0 passed: 5417 |
| `server-full` | `45841d6` | `d946bb2` | failed: 1436 errors: 2 skipped: 12 passed: 71738 |
| `server-repair-probe` | `5f03cd6` | `a5918c6` | failed: 2 errors: 2 skipped: 0 passed: 983 |
| `server-repair` | `458ce14` | `a5918c6` | failed: 5 errors: 0 skipped: 0 passed: 14672 |
| `client-final-dependents` | `a5918c6` | `458ce14` | failed: 0 errors: 0 skipped: 0 passed: 206 |
| `server-repair2` | `990973b` | `a5918c6` | failed: 0 errors: 0 skipped: 0 passed: 467 |

Exact files for each row are enumerated in the linked test manifest.

## Commits

### dsOMOP

- `35551cd` — Merge concept labels and A27 presentation contract into integration
- `52ef925` — Merge WS10 documentation while retaining A27 migration notes
- `761f7bc` — Integrate 2.10.3 security release with amended publication and label contracts
- `250dc25` — Consolidate security and recipe notes under the 2.10.3 release
- `6141379` — Preserve authenticated label lineage and isolate publication test key scopes
- `1018294` — Retain membership metadata compatibility and document the integrated extractor
- `45841d6` — Keep scoped key restoration when label fixtures close publication contexts
- `ee807cf` — Assert the exact A27 normalization delta against frozen IR snapshots
- `3677714` — Preserve v1 private result metadata while tagging public frames
- `5f03cd6` — Keep presentation bookkeeping out of untranslated private v1 returns
- `458ce14` — Reconcile frozen integration assertions with A27 and security provenance
- `990973b` — Verify the translated baseline factor and retain private sparse result fidelity

### dsOMOPClient

- `950b926` — Merge concept labels and A27 presentation contract into integration
- `419dc25` — Merge WS10 documentation while retaining A27 migration notes
- `b5aab2e` — Integrate 2.10.3 security client and exercise real capability negotiation
- `d946bb2` — Consolidate release notes and declare numeric output in transport fixtures
- `3d8ad38` — Align integration regressions with the amended concept presentation contract
- `a5918c6` — Complete descendant fixtures and parameterized A27 naming assertions

The final report-only commit contains this report and its three manifests.

## Notes for subsequent jobs

- Keep the advertised `output.concept_labels` operation and the real client
  capability negotiation; do not restore the removed adapter-test stub.
- Numeric fixtures that set `factor_concepts = FALSE` must also disable
  translation. A-27's corrected factor and generated-name behavior is intentional.
- Keep v3 provenance, snapshot finalizers and authenticated source resealing.
  The complete security-main disclosure-control text is retained verbatim.
- Preserve the `56b983c` MariaDB extractor; the exact function comparison passed.
- Allow more than two hours for the complete server suite on this pod. Its
  exhaustive incidence release test is expensive; it was not skipped or shortened.
- No push, PR, tag change, Claude CLI invocation, or other worktree edit occurred.
