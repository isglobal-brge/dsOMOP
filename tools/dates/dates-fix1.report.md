# Date review round 1 fixes — A-28 / CR-29

13 November 2026. All three blocking findings in `astra-dates-review-1.md` are implemented on `feat/2.10.3-dates` in the requested worktree pair. Both packages remain 2.10.3. All required targeted checks are green, including all 27 unchanged independent review assertions. The final client fixture rerun passed all 74 assertions.

## Commits

| Package | Commit | Change |
|---|---|---|
| dsOMOP | `cc22077` | Apply selected typed-date policy consistently; tokenize custom patterns; fix reference-calendar order; add server regressions and NEWS |
| dsOMOP | `0bf57c4` | Strengthen the chronological-order regression for noncontiguous year-bearing patterns |
| dsOMOPClient | `37dc927` | Mirror the shared custom-date implementation; add client/publication regressions and NEWS |
| dsOMOPClient | `02e178b` | Correct the weekday fixture's case-insensitive column-name collision |
| dsOMOPClient | `cba4f62` | Disconnect the staged fixture before removing its staging directory |

The report and retained evidence are committed separately in dsOMOP under `tools/dates/`. The requested report is also written to `/workspace/dsomop/design/astra-dev/dates-fix1.report.md`.

## Findings mapped to fixes and tests

| Finding | Fix | Regression evidence |
|---|---|---|
| DATES-B1 (P1) | `R/compile_dates.R` uses `.dateTypedValues()` for selected reducer/raw values, date helpers and raw output roles. Invalid/nonfinite/fractional/out-of-domain typed dates become NA. The effective future cutoff is applied after selection and before presentation. A future selected event becomes NA without substituting an earlier event. | Server `test-date-review-fixes.R`: variable and recipe policies over `first_date`, `last_date`, date-valued `first_value` and `last_value`, raw observation-period dates and helpers; exact cutoff, missing rows, existing earlier events, Date/month/relative representations, domain endpoints and invalid typed dates. Client file of the same name: real public recipe assignment and exact expected VA fixture vectors for both policy scopes. Original review probes now pass all four failing B1 assertions. Existing policy/output tests and reducer tests also pass. |
| DATES-B2 (P2) | Both copies of `R/date_semantics.R` use exactly 2000-01-01 through 2399-12-31 for yearless patterns. Adjacent years are generated only for actual year-bearing patterns and are sorted chronologically. | Server and client regressions independently pin `%a` to Sat/Sun/Mon/Tue/Wed/Thu/Fri, `%A` to the corresponding full names, and `%u` to 6/7/1/2/3/4/5. Server pins reference dates/ranks and noncontiguous year-bearing ordering. Client checks final two-site publication, ordered contrasts and staged CSV parity with reversed site order, including escaped `%%Y %a`. The unchanged review reproduction passes. |
| DATES-B3 (P2) | One shared tokenizer distinguishes literal characters and actual directives. Dependency validation, rendering, year detection and label-key validation consume those tokens; presentation tokenizes once and reuses the result throughout rendering/reference/key validation. | Client declarations and hand-authored server plans render `%%Y %G-W%V` as `%Y 2020-W01` and `%%V %Y` as `%V 2020` for 2020-01-01. Adjacent percent cases `%%%Y`, `%%%%%Y`, `%Y%%`, `%%G%%V%Y` and escaped yearless patterns are covered. Valid and malformed label keys are checked in both implementations; active `%Y-W%V`, escaped-but-unpaired ISO directives and dangling percent signs still refuse with `date_pattern_invalid`. Both original B3 probes pass. |

Both NEWS files classify the corrections and tell users to re-execute affected recipes to refresh values/contrasts. No declaration change or new amendment is needed. The shared date semantics files are byte-identical.

## Executed targeted validation

Every R run used `/workspace/dsomop/tools/rtest.sh` on the pod with its matching sibling installed through `RTEST_WITH`, and `RTEST_ENV` included `R_LIBS_USER=/root/rt/labels-deps`. Server runs also enabled `DSOMOP_TEST_POSTGRES_HOST=omopdb DSOMOP_TEST_MARIADB_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_PORT=3307`. These existing fixtures exercise SQLite, DuckDB, PostgreSQL, MariaDB and MySQL.

| Run | Exact files / command | Tested source commits (server / client) | Result |
|---|---|---|---|
| Server date tests, 900 s | `test-date-review-fixes.R`, `test-dates-system.R`, `test-date-system-outputs.R`, `test-date-system-policies.R`, `test-date-component-transport.R`; filter `^(date-review-fixes\|dates-system\|date-system-outputs\|date-system-policies\|date-component-transport)$` | `cc22077` / `37dc927` | **1,212 passed, 0 failed, 0 errors, 0 skipped** |
| Client first run, 900 s | `test-date-review-fixes.R`, `test-dates-system.R`, `test-v2-execution.R`; filter `^(date-review-fixes\|dates-system\|v2-execution)$` | `cc22077` / `37dc927` | 146 passed; 1 error in the new weekday fixture due to columns `a` and `A`. Existing files passed. Fixed in `02e178b`. |
| Server final reducer/regression run, 900 s | `test-date-review-fixes.R`, `test-compile-reduce.R`; filter `^(date-review-fixes\|compile-reduce)$` | `0bf57c4` / `02e178b` | **812 passed, 0 failed, 0 errors, 0 skipped** |
| Client fixture rerun, 900 s | `test-date-review-fixes.R`; filter `^date-review-fixes$` | `0bf57c4` / `02e178b` | 48 passed, including all weekday/staging assertions; 2 errors from fixture cleanup order (disconnect failed, causing the next fixture connection to fail). Fixed in `cba4f62`. |
| Client final focused run, 900 s | `test-date-review-fixes.R`; filter `^date-review-fixes$` | `0bf57c4` / `cba4f62` | **74 passed, 0 failed, 0 errors, 0 skipped** |
| Unchanged independent reviewer harness, 600 s | `test-review-dates.R`; filter `^review-dates$`; both matching packages installed | `0bf57c4` / `02e178b` | **27 passed, 0 failed, 0 errors, 0 skipped**; all seven original failures resolved, complete VA recipe retained, threshold refusal retained |
| Contract checker | `PYTHONDONTWRITEBYTECODE=1 python3 tools/ws0/check_contracts.py . ../dsOMOPClient` | `cc22077` / `37dc927` | All passed: 3 schemas, 2 examples, 224 probes, 15 acceptance plans, 12 digest vectors, 18 normalised fixtures, 34 numbers, 11,043 corpus doubles |

The runtime code did not change after `cc22077` / `37dc927`; subsequent commits only improve/correct regression tests. No full suite ran: the user's targeted-only instruction and COMMON.md's later Test economy rule supersede its earlier full-suite wording. Integration/release owns the final full pass.

Retained evidence is in `/workspace/dsomop/wt/dates/dsOMOP/tools/dates/fix1-evidence/`: `server-date-tests.log`, `server-reducer-tests.log`, `client-initial.log`, `client-fixture-rerun.log`, `client-final.log`, `review-probes.log`, `contracts.log`, `tested-source.json`, `review-probe-source.json`, and `scratch-cleanup.json`. The probe source hash verifies that the review harness was copied without changes. The original review files were not modified.

Existing client tests warn on deliberate threshold/disclosure refusal cleanup and the synthetic GLM fit. The independent probe has its original absent-execution-symbol warning after deliberate threshold refusal; its no-output assertion passes. The two failed development runs above were fixture errors, not changes to publication safeguards.

## Scope, cleanup and handoff

No design deviation, new API, schema change, version change, dependency change or documentation regeneration was needed. No full suite, claude CLI invocation, push, PR, tag or attribution trailer was used. Changes are confined to the requested worktrees and report path. Final publication gates, class-1 protections and the working verbal-autopsy route remain covered by the targeted tests.

Disk checks showed 9.7 GiB available on `/workspace`; no large local package copy was made. After retaining evidence, this job removed only its temporary review harness at `/workspace/dsomop/wt/dates/dsOMOP/tools/dates/fix1-probes` (4,809 file bytes), the pod copy at `/root/rt/wt/dates/dsOMOP/tools/dates/fix1-probes` (4,951 bytes by `du -sb`), and its private pod library `/root/rt/lib/wt_dates_dsOMOP_tools_dates_fix1-probes` (16,205,536 bytes). Paths and sizes are recorded in `scratch-cleanup.json`. Shared free space remained below CLEANUP.md's 15 GiB target; no large-copy or full-suite operation was undertaken, and this job avoided deleting material belonging to other active workstreams.

Integration must take both packages' shared semantic changes together. Existing recipes need re-execution to receive corrected NA values and factor contrasts. Independent review of this fix round and the integration/release full pass remain the next workflow steps; this report does not claim reviewer endorsement or full-suite validation.
