# Date review round 2 fixes — A-28 / CR-29

13 November 2026. DATES-B4, the sole blocking finding in `astra-dates-review-2.md`, is implemented in both requested worktrees on `feat/2.10.3-dates`. Both packages remain 2.10.3.

## Commits

| Package | Commit | Change |
|---|---|---|
| dsOMOP | `c9a976a` | Include every declared range year in custom ordering; add range regressions and NEWS |
| dsOMOP | `a1450ae` | Preserve zero-length rendering for custom patterns with separators |
| dsOMOPClient | `8e03cab` | Mirror the calendar fix; add independent formatter and two-site publication regressions and NEWS |
| dsOMOPClient | `6cc536d` | Mirror the empty-input rendering fix |
| dsOMOPClient | `da95946` | Give the synthetic month/week publication fixture sufficient public-level capacity |

This report and retained evidence are committed separately in dsOMOP under `tools/dates/`. The report is also written to `/workspace/dsomop/design/astra-dev/dates-fix2.report.md`.

## Finding, fix and regression coverage

**DATES-B4 (P2):** both copies of `R/date_semantics.R` now union every year from the inclusive public range into the year-bearing ordering calendar, before adding and sorting adjacent years for ISO week boundaries. Public middle-year keys no longer depend on observed source years. The exact 2000–2399 calendar for yearless patterns and token-based directive handling are unchanged.

The requested empty-observation coverage also exposed `paste0()` creating separator-only keys (`/` or `-W`) from zero dates. `.dateRender()` now returns `character()` for empty input. This is part of satisfying B4's empty-observation requirement, not a change to the declared calendar contract. Both NEWS files classify the corrections and instruct users to re-execute affected recipes; no declaration change or amendment is needed.

Both packages' new `test-date-range.R` independently test `%Y`, `%m/%Y` and `%G-W%V` over 2000-01-01 through 2010-12-31 with zero rows, all-missing dates, one endpoint observation, one middle-year observation, and reversed/disjoint observations in 1980 and 2030. Expectations use base R date formatting, independently of the shared implementation. Assertions cover values, ordered levels, public unused levels, monotonic ranks, chronological month/year order, the preceding ISO year and an intervening week 53.

The client file additionally executes public recipes on two synthetic DSLite/SQLite sites, checks all eleven year levels and value `2000` against the built-in year control, and verifies month/year and ISO-week factors with endpoint, disjoint first-visit and all-missing sources. The large synthetic fixture explicitly uses a level cap of 1,000 for its hundreds of declared categories; default-cap fixtures are unchanged. The independent eleven-year reproduction passes at the default cap. Every site retains the expected shared unused levels. Reversing site order preserves them. Inspection of synthetic frames occurs only in the test harness.

## Targeted validation

All R runs use `/workspace/dsomop/tools/rtest.sh`. Package runs install the matching sibling with `RTEST_WITH` and set `RTEST_ENV` to include `R_LIBS_USER=/root/rt/labels-deps`. Server runs additionally enable PostgreSQL (`omopdb`), MariaDB (`127.0.0.1`) and MySQL (`127.0.0.1:3307`) through the documented environment variables; existing fixtures also exercise SQLite and DuckDB.

| Run | Exact files | Server / client source commits | Result |
|---|---|---|---|
| Initial server, 900 s | `test-date-range.R`, `test-date-review-fixes.R`, `test-dates-system.R`, `test-date-system-outputs.R`, `test-date-system-policies.R`, `test-date-component-transport.R` | `c9a976a` / `8e03cab` | 1,242 passed; 0 failed, 1 error, 0 skipped. The sole error exposed empty month/year rendering; fixed in `a1450ae` / `6cc536d`. All existing tests passed. |
| Final server, 900 s | `test-date-range.R`, `test-date-review-fixes.R` | `a1450ae` / `6cc536d` | **213 passed; 0 failed, 0 errors, 0 skipped** |
| Initial client, 900 s | `test-date-range.R`, `test-date-review-fixes.R`, `test-dates-system.R`, `test-v2-execution.R` | `a1450ae` / `6cc536d` | 279 passed; 0 failed, 1 error, 0 skipped. All existing tests and 95 formatter assertions passed; the publication fixture exceeded the existing 40-level cap. Fixed the fixture in `da95946`, without changing runtime policy. |
| Final client, 900 s | `test-date-range.R` | `a1450ae` / `da95946` | **197 passed; 0 failed, 0 errors, 0 skipped** |
| Unmodified independent review harness, 600 s | `test-review-dates.R`, `test-review-date-range.R` | `a1450ae` / `6cc536d` | **37 passed; 0 failed, 0 errors, 0 skipped**. All three B4 failures are resolved, and the original 27 review assertions remain green, including complete VA publication and threshold refusal. |
| Contract checker | `PYTHONDONTWRITEBYTECODE=1 python3 tools/ws0/check_contracts.py . ../dsOMOPClient` | `c9a976a` / `8e03cab` | All passed: 3 schemas, 2 examples, 224 probes, 15 acceptance plans, 12 digest vectors, 18 normalised fixtures, 34 numbers, 11,043 corpus doubles |

The server rerun covers the final empty-rendering change and preserves B1–B3. The other server files passed before that one-line empty-input guard; they were not redundantly rerun. The final client commit changes only test-fixture capacity for the new publication test, so only `test-date-range.R` is rerun after it. Existing helper callers retain their previous settings. No full suite ran, following the user's instruction and COMMON.md's later Test economy rule. Integration/release retains the final full pass.

The independent harness is copied byte-for-byte from `design/2.10.3/astra-dates-review-2-evidence/datesReview`, with SHA-256 evidence in `probe-integrity.json`. It reuses the final package installations through `R_LIBS_USER=/root/rt/lib/wt_dates_dsOMOPClient:/root/rt/lib/wt_dates_dsOMOP:/root/rt/labels-deps`. The original reviewer files are untouched. Existing client warnings concern deliberate-refusal cleanup and the synthetic GLM fit; the independent probe warns during cleanup of its deliberate threshold refusal. Those refusal assertions pass.

Retained evidence is in `tools/dates/fix2-evidence/`: initial/final server summaries and details, initial/final client summaries and details, independent-probe summary and detail, contract results, final tested source hashes, probe integrity and scratch cleanup records.

## Scope, cleanup and handoff

No design deviation, API/schema/version/dependency change or documentation regeneration was needed. The shared semantics files are byte-identical. Integrate both packages together. The repaired B1–B3 behavior and complete verbal-autopsy route are covered by the focused regressions and independent probes; this report does not substitute for independent endorsement or integration/release testing.

Disk checks showed 9.7 GiB free on `/workspace`; no large local package copy or full-suite run was made. After retaining the evidence, this job deleted only its scratch copies and private probe library (sizes from `du -sb`):

- `/workspace/dsomop/wt/dates/dsOMOP/tools/dates/fix2-probes`: 19,458 bytes.
- `/root/rt/wt/dates/dsOMOP/tools/dates/fix2-probes`: 7,348 bytes.
- `/root/rt/lib/wt_dates_dsOMOP_tools_dates_fix2-probes`: 10 bytes.

Shared free space remained below CLEANUP.md's 15 GiB target; no material belonging to another workstream was deleted.

No full suite, claude CLI invocation, push, PR, tag or attribution trailer was used. Work is confined to the requested worktrees and report path.
