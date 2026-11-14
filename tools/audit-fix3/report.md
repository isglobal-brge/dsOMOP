# Audit fix 3 — implementation and validation record

14 November 2026. **The complete request remains unfinished. R2 and R3 are not
closed. Do not treat these commits as complete CR-32/A-31 acceptance or release
approval.** This repair addresses R5, the demonstrated uncontracted-column
admission in R2, and independent nested Circe diagnostics, and integrates the
numeric/category/library prerequisites needed for the remaining implementation.
Targeted validation results and unresolved obligations are recorded below.

## Scope and rules

Only the named audit package worktrees were changed, on `feat/2.10.5-audit`,
plus the requested report/evidence under `design/astra-dev`. Entry commits were
server `77ee8b63bacf469d13ffe5e6863028b9f89590e0` and client
`6dfbba218538e3003a0e9dd2e083a245462b0b20`; both trees were clean. Their recorded
repair commits contained no new WIP commits to disposition.

COMMON.md, review 3, the preceding implementation/repair reports and operative
CR-32/A-31 requirements were read. COMMON.md's later Test Economy instruction
supersedes its earlier full-suite requirement: only affected tests and direct
dependents run here. No full suite, Claude CLI, sub-agent, stash, push, PR, tag or
attribution trailer was used. Commits used explicit pathspecs. Both packages
remain version 2.10.5.

## Findings mapped to changes and tests

| Finding | Change / disposition | Evidence |
|---|---|---|
| R5: explicit imported coverage overwritten | `.v2w_population` prefers explicit population coverage over the translation default. Removing the override restores the imported default. Legacy index/filter retranslation preserves the orthogonal override. JSON/YAML and generated R retain the override and derivative identity without rewriting original provenance. An incompatible Circe export refuses with a native serialization remedy. | Client `test-audit-circe-support.R`; server `test-audit-circe-coverage.R`. The latter executes an imported recipe on overlapping periods: `unique` retains only person 2; `earliest` retains people 1 and 2 and selects ends 2020-01-31 / 2020-03-31. Saved and generated recipes must execute those same episodes. All dates/IDs are private synthetic test assertions, not demonstration releases. |
| R2: uncontracted physical dimension admitted | Closed physical column membership now gates schema support, discovery, reading and pooling before result reads, including projections. Existing private incidence measures and SCCS event/time counts have explicit numeric contracts and remain nonpublic; they never substitute for person support. | `test-audit-results-identity.R`, `test-ohdsi-results.R`, `test-ohdsi-small-support.R`, `test-analysis-pooling-contract.R`. Unknown extra dimension changes the fingerprint, produces `unknown`, removes availability and refuses projected reading/pooling. |
| R2: collective nested-group diagnostics | Recursive structural inspection reports independent nested domain, bound, occurrence, window and demographic issues at each path; malformed siblings do not erase the rest. No nested-group execution adapter is implied. | `test-audit-circe-support.R`, including multiple levels, malformed criteria and sentinel exclusion. |
| R2: prerequisite composition absent | Pinned A-29/A-30/library sources are integrated into this pair, retaining audit vocabulary, settings, schema and Circe fixes. Additional signed64 fixes from the endorsed numeric head are applied. | Exact pins below; paired contract checker; focused numeric/category/library and audit regressions. This is prerequisite integration, not completion of all residual Q requirements. |
| R2 / integration: category vocabulary domain validation | The shared category-domain validator accepts and validates the executable imported policy, including exclusions, rather than rejecting it after client lowering. | Client `test-audit-vocabulary-policy.R`: wire/server capability, JSON, generated code, and bounded invalid-policy refusal. |
| Prerequisite R8: generated-date category default | The independent category review 5 appeared during this job. Effective category ordering/recode semantics now also apply to generated date endpoints without a variable node; endpoint creation passes category defaults. | `test-audit-category-defaults.R`: exact unordered-default/no-default control, memory/staged frames, nested multi-TAR survival and joined dependents. |
| R3: wider required acceptance missing | **Open.** No actual old/current physical exports, published consumer matrix, paired deployed profile/restart lane or rendered book is delivered by this repair. | Targeted package checks and inherited fixture evidence cannot substitute for these named acceptance artifacts. |
| R1 / R4 | Prior closures retained; physical identity and vocabulary-policy regressions remain selected. | `test-audit-results-identity.R`, paired `test-audit-vocabulary-policy.R`. |

## Prerequisite integration

Squashed source integration uses these immutable commits, read from the named
repositories' object databases; no other job's worktree was edited:

| Prerequisite | Server | Client |
|---|---|---|
| A-30 categories, including A-29 baseline | `f5c88cecd6b441f30675f9bb14ca545b9297c0f0` | `416a9452045e771ad1fc185db429faf0e5f50390` |
| Additional endorsed A-29 numeric corrections | `01a2fafb2b289b133a9570b33fc59d7edadf1c95` | `ed73633cde501636b96803bf9c29615c7ca27edc` |
| Endorsed 24-entry library and empty-cohort repair | `2beed9c63a10573e7f6e855edd1598ad1b01a82e` | `29396d7a699654e124fc2dcdb8ebcd88f706dd61` |

The initial category conflict-resolution script incorrectly removed text between
separate conflict blocks. This was caught immediately by the diff statistics
and repaired in separate commits by reconstructing each three-way merge from
its original revisions. No R test or package installation ran on that temporary
state. The final shared schemas and eight shared helpers are byte-identical;
the contract checker passes. Existing library evidence files retain their
original bytes, including trailing spaces; they are inherited artifacts, not
fresh execution evidence.

The category prerequisite was under independent review when integrated. Review
5 then reported R8; its audit-local repair is listed above and requires
independent verification. Endorsed numeric/library reports are inherited
supporting evidence; they do not certify their composition with every A-31 route.

## Q01–Q35 acceptance ledger

| Obligation | Disposition after this repair |
|---|---|
| Q01–Q02 | Existing strict shared import implementation retained; no new parser claim beyond the focused Circe regressions. |
| Q03 | Nested independent diagnostics and coverage/edit/export lifecycle strengthened; pinned PhenotypeLibrary artifacts retained. Complete lifecycle acceptance is not certified. |
| Q04–Q05 | Existing ignored-setting refusals and incidence/FE estimand evidence retained. |
| Q06 | Demonstrated unknown-column admission repaired. Full version-evidence reconciliation, required reference association checks and actual old/current physical-export acceptance remain open. |
| Q07 | A-30 codebook prerequisite present; complete OHDSI namespace/semantic-join declaration and acceptance absent. |
| Q08 | Native era implementation retained; required recorded/native era interoperability declaration and acceptance remain open. |
| Q09 | Complete study-role/TAR declaration acceptance remains open. |
| Q10 | Required named window teaching/acceptance lane remains open. |
| Q11 | Executable imported policy retained and category-domain composition repaired; complete context/slice/codebook lifecycle remains open. |
| Q12 | Complete versioned CDM/episode/death/coverage declarations and acceptance remain open. |
| Q13 | Complete route fidelity contracts, codebook/README/book alignment and result acceptance remain open. |
| Q14 | Four formula builders and their compiled numerical/lineage oracles are still absent. Numeric primitives are now integrated. |
| Q15 | `number_daily` declaration/schema/IR/compiler/capability and acceptance are still absent. |
| Q16–Q22 | Required baseline, fraction, previous-day, plausibility, integer-scale, published growth and cross-source examples/acceptance remain open. |
| Q23 | Actual published downstream-consumer matrix remains open. |
| Q24–Q25 | Study codebook comparison and pooling teaching/acceptance remain open. |
| Q26 | `recipe_study_bundle` constructor/schema/serialization acceptance remains absent. |
| Q27 | Versioned actual-interview VA capstone remains open. |
| Q28 | All 24 library entries and their existing tests/fixtures are integrated. Full integrated per-entry A-31 acceptance is not certified. |
| Q29 | Complete question-to-route guide and executable call matrix remain open. |
| Q30 | Coverage/Circe/schema refusal and code-generation regressions added. Complete new-route diagnostics/explain/codegen remains open with the missing routes. |
| Q31 | Required native-v2 book render/chunk/exercise/two-site acceptance remains open. |
| Q32 | Equal versions and test-run sibling installation confirmed; deployed fresh/upgrade/profile/restart acceptance remains open. |
| Q33 | No new privacy configuration, state, quota, TLS requirement or answer-changing session policy introduced; deployed privacy acceptance remains open. |
| Q34 | Endorsed exact-integer numeric fixes/tests integrated; required cost teaching lane remains open. |
| Q35 | Existing pinned schema/phenotype and integrated synthetic library/numeric fixtures retained; actual Eunomia/versioned CDM acceptance remains open. |

## Validation

All R tests use the pod through `/workspace/dsomop/tools/rtest.sh`, with matching
`RTEST_WITH`. Numeric/Arrow dependencies use
`RTEST_ENV='R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps'`.
No local R test or full suite runs. Public API signatures and roxygen comments
were not changed; imported generated Rd/NAMESPACE files come from the pinned
prerequisite revisions.

Initial server development run at server `afd2d81` / client `b5ce286` selected
`test-audit-results-identity.R`, `test-audit-circe-oracle.R`,
`test-ohdsi-results.R`, `test-ohdsi-small-support.R`,
`test-analysis-pooling-contract.R`: 1,941 pass, 3 failed assertions, 5 errors,
1 skip. The overlapping-period values were correct; two assertions compared
Date to character and code generation evaluated assignments in a locked
namespace. Those test errors are repaired. The remaining failures identified
known private legacy measures requiring explicit contracts, and an old test
that expected arbitrary physical extras to be ignored; that expectation now
correctly requires refusal. The unchanged upstream SQL oracle skipped because
SqlRender/Java dependencies from fix 1 were removed by its documented cleanup.
The new self-contained R5 regression is now in `test-audit-circe-coverage.R`;
no new upstream parity execution is claimed.

Python contract check: **passed** — 3 schemas, 2 examples, 224 probes,
15 acceptance plans, 12 digest vectors, 18 normalized fixtures, 34 numeric
vectors and 11,043 corpus doubles. Evidence: `audit-fix3-contracts.log`.

A first integrated client run (server `3562441`, client `e354af7`) selected
`test-audit-circe-support.R`, `test-audit-vocabulary-policy.R`,
`test-category-review-schema.R`, `test-recipe-library.R`, and
`test-number-system-wire.R`. It was deliberately terminated while the lengthy
category schema matrix was still running: newer audit/category fixes had made
its snapshot obsolete. The partial full log is preserved as
`audit-fix3-client-integrated.full.log`; it has no final pass verdict. The final
run uses the dedicated `test-audit-category-defaults.R` for the newly changed
endpoint typing and its nested/joined dependents, plus codebook/layout tests.
Only the audit-owned timeout process PID 1879057 was stopped; the concurrent
category review process was left untouched.

The final combined client selection at client `5fb90c2` / server `25f8629`
(runtime identical to the subsequent NEWS-only commits `e2415d3` / `2aa0dcb`)
ran these exact files:

- `test-audit-circe-support.R`
- `test-audit-vocabulary-policy.R`
- `test-audit-category-defaults.R`
- `test-category-codebook.R`
- `test-category-layout.R`
- `test-recipe-library.R`
- `test-number-system-wire.R`

Filter: `^(audit-circe-support|audit-vocabulary-policy|audit-category-defaults|category-codebook|category-layout|recipe-library|number-system-wire)$`, timeout 1800 seconds.
It finished with 1,460 passed assertions, 48 failed assertions, one error and no
skips. All selected vocabulary, generated-date-default, codebook, layout and
number-wire files passed. The export test selected the default base population
instead of the imported population; it now explicitly selects `p`. All 48 library
failures were the two per-entry frozen comparisons' advisory `meta.client_version`
(2.10.4 fixture versus 2.10.5 compiler), not scientific payload differences. The
test now verifies the actual installed compiler version and compares the rest of
the frozen payload exactly. Immutable fixture bytes/checksums remain unchanged.
These test-only changes are client `0b5a49d`.

The affected client files alone were rerun at client `0b5a49d` with sibling server
`2aa0dcb`, filter `^(audit-circe-support|recipe-library)$`, timeout 1200 seconds:
**866 passed, zero failures/errors/skips**. The server's intervening `f54c4b5`
changes only a test assertion; runtime source is identical. Logs:
`audit-fix3-client-final.log`, `audit-fix3-client-final.full.log`,
`audit-fix3-client-verified.log`, `audit-fix3-client-verified.full.log`.

The final combined server selection at server `2aa0dcb` used the matching client
runtime from `e2415d3` (unchanged in test-only `0b5a49d`; that test commit occurred
around sibling synchronization). Its exact files were:

- `test-audit-circe-coverage.R`
- `test-audit-results-identity.R`
- `test-audit-vocabulary-policy.R`
- `test-ohdsi-results.R`
- `test-ohdsi-small-support.R`
- `test-analysis-pooling-contract.R`
- `test-compile-coverage.R`
- `test-compile-death-empty.R`
- `test-number-system-exact-expressions.R`
- `test-category-contract.R`
- `test-category-wire.R`

Filter: `^(audit-circe-coverage|audit-results-identity|audit-vocabulary-policy|ohdsi-results|ohdsi-small-support|analysis-pooling-contract|compile-coverage|compile-death-empty|number-system-exact-expressions|category-contract|category-wire)$`, timeout 1800 seconds.
Environment additionally set `DSOMOP_TEST_POSTGRES_HOST=omopdb`,
`DSOMOP_TEST_MARIADB_HOST=127.0.0.1`, `DSOMOP_TEST_MYSQL_HOST=127.0.0.1`,
`DSOMOP_TEST_MYSQL_PORT=3307`. It finished with **2,608 passed assertions, zero
failed assertions, one error, zero skips**. The error was an obsolete test
expectation: selecting `secret_metric` from an unknown layout is now refused at
schema validation, before column-contract validation. `f54c4b5` corrects the
expected bounded reason; no runtime change was required. All other selected
files passed. Coverage execution reported SQLite 3.53.3, DuckDB 2.0.0,
PostgreSQL 14.24, MariaDB 10.6.23 and MySQL 8.4.11. The new imported-coverage
oracle itself executes on synthetic SQLite; this does not assert that the
imported phenotype was freshly compared with upstream Circe on every backend.
Logs: `audit-fix3-server-final.log`, `audit-fix3-server-final.full.log`.

The affected server `test-ohdsi-results.R` alone was rerun at server `f54c4b5` /
client `0b5a49d`, filter `^ohdsi-results$`, timeout 600 seconds: **341 passed,
zero failures/errors/skips**. Evidence: `audit-fix3-server-verified.log` and
`audit-fix3-server-verified.full.log`. No other selected test file changed after its passing
run, and no runtime source changed after the combined selections.

The final Python contract check also passed with the same counts listed above
(`audit-fix3-contracts-final.log`). Eight paired source/schema files match
byte-for-byte; their SHA-256 values and full source commit identities are in
`audit-fix3-source.json`. Base-R parsing of the directly changed source files
passed; this was syntax verification only, not local test execution.

## Deviations and handoff

The major deviation is that the requested complete repair is **not finished**.
Prerequisite absence is no longer a reason for leaving dependent implementation
out: those sources are now integrated. R2 still needs the explicitly listed
residual implementation, and R3 still needs the named real acceptance artifacts.
Neither passing focused tests nor this report closes either finding.

Category integration must retain the audit-local R8 generated-endpoint fix and
imported category-domain validity support. Numeric integration must retain the
six additional endorsed server corrections beyond the category branch's numeric
baseline. Preserve the coverage precedence and nested diagnostic tests during
future merges. Original library template meanings/checksums were not rewritten.

Local disk was 9.7 GB free at entry and approximately 9.6 GB after integration.
CLEANUP.md was read; inspection found no eligible cache or immediate scratch
package copies to remove. No unrelated worktree, shared dependency library,
repository or evidence was deleted. No large local scratch package copies or
full-suite runs were made. Own pod mirrors/private libraries are removed only
after preserving final evidence; the final cleanup ledger is appended below.

## Commits

The source/test heads before the evidence archive are server `f54c4b5c81e7934e7ec211278086fee4c2eef25c` and client `0b5a49d9a8679d07f895b8849fc2061262aa8ce0`.

### dsOMOP

- `afd2d818e8e93dda3fb8d6a522163f8bbd8791a2` — Reject uncontracted result dimensions and test imported coverage execution
- `5d6c343eda9b5dfbc58f3573f5d7b61636d1cd6c` — Integrate pinned numeric and category prerequisites while retaining audit validation
- `92ee3311c2b111723b0b2d1dc1764597e8d8cc61` — Preserve complete files when resolving prerequisite integration conflicts
- `25f74a475de457141eed7928f6d04d6b07032544` — Integrate endorsed signed64 expression and selector corrections
- `dee1e35d79bb1c24a901291f46e038548c515047` — Integrate pinned endorsed recipe library and empty-cohort repair
- `356244160f0612972a3a2cefecf0937b9691d05e` — Contract legacy private measures and correct coverage oracle comparisons
- `a17d27de43b9b5f85445c1b8a244f940bab7c179` — Validate imported vocabulary policy in category domains
- `49641683143b6a49f2972a070eeeb54478f9f16a` — Separate imported coverage execution regression from upstream SQL oracle dependencies
- `25f8629e4beb4b4072894f549238a4652139a784` — Document closed layout matching and prerequisite integration limits
- `2aa0dcbb4c5b4f83397b099c514d087314087d23` — Record audit prerequisite composition corrections
- `f54c4b5c81e7934e7ec211278086fee4c2eef25c` — Assert layout refusal before requested-column validation

### dsOMOPClient

- `b5ce2860dcc261ab6288914063aa86405c0f5d33` — Honor explicit imported population coverage and inspect nested Circe clauses
- `55dbcf0698ce2e7492348c3816695123a805af13` — Integrate pinned numeric and category prerequisites while retaining audit validation
- `aa3dfea2c7e05ecc2996f8c75d1304bdf8d9b10c` — Preserve complete files when resolving prerequisite integration conflicts
- `3fcc902f147d3ea934bf869386b79798de91901f` — Retain the endorsed numeric exact-integer corrections in audit prerequisites
- `e354af7c8af73b8c2834ccacdccebd17a8718a95` — Integrate the endorsed 24-entry recipe library at pinned revision
- `5c106dc3a0ef02f2c9636a24e0c4d9b77032a1e6` — Apply category defaults to generated date endpoint codebooks
- `34c6cd2aa93cd5cda44370cd17269bfd04a13127` — Refuse loss of edited coverage in Circe export without blocking native compilation
- `b7e6a56f19d56e09ce1132b94c1ac044a6034d57` — Preserve imported vocabulary policy in category-domain composition
- `f03152a65d77a3387a7314fd3ec7639e522aa368` — Document explicit coverage precedence and remaining amendment obligations
- `7369f35fb80ba1d55c2c28e3ee73cb90ddb0dd95` — Keep independent Circe diagnostics when a nested criterion is malformed
- `5fb90c25a21c9ae8f6785da30d01bc7d65bae28f` — Isolate generated endpoint composition regressions for targeted audit verification
- `e2415d30a879f5137a3e17fd8cd7c6105e5b5f23` — Document vocabulary and generated-date composition corrections
- `0b5a49d9a8679d07f895b8849fc2061262aa8ce0` — Select the imported population in export tests and preserve historical library fixtures

## Cleanup and evidence archive

After all logs were copied back, only these disposable pod paths were deleted:

| Path | Bytes |
|---|---:|
| `/root/rt/wt/audit/dsOMOP` | 25,180,862 |
| `/root/rt/wt/audit/dsOMOPClient` | 20,502,546 |
| `/root/rt/lib/wt_audit_dsOMOP` | 6,165,341 |
| `/root/rt/lib/wt_audit_dsOMOPClient` | 11,823,887 |

`audit-fix3-cleanup.log` records the deletion. No audit test process was running;
the other category job was left untouched. Local disk remained at 9.6 GB free.
No local scratch package copies were created or deleted.

This report and all job logs/source inventory are committed under server
`tools/audit-fix3/`; full logs are gzip-compressed without changing their
uncompressed bytes. `evidence.json` records both archive and original SHA-256
values. The archive commit follows the source commits listed above and makes no
runtime changes. The requested external report has the same contents as the
committed report. Both worktrees are clean after the evidence commit.

**Disposition: R5 repaired with compile, serialization, codegen and execution
regressions; concrete R2 column-admission and nested-diagnostic defects repaired;
R1/R4 regressions retained. R2 and R3 remain open.** No complete amendment,
independent review or release approval is claimed.
