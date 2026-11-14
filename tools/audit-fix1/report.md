# Audit fix 1 — partial repair; audit remains blocked

14 November 2026. **I did not complete every blocking finding. R1 is repaired;
R2 and R3 remain open for the obligations below. Do not endorse this branch as
complete CR-32/A-31 implementation.** All implemented source changes are committed.
This report distinguishes tested repairs from missing work; no dependency issue
is used to close an independent acceptance obligation.

## Scope and revisions

Only the named audit package worktrees and requested design evidence/report
paths were written. Both branches are `feat/2.10.5-audit`; both DESCRIPTION
versions remain 2.10.5. Starting heads were server `8964fed849090f95ad79503fef6fbd93ef993c4d`
and client `64947ecd806efaf91e2ad6e047a10bc000a86d3f`. Both were clean, with no
unreviewed job-local WIP commits. David's COMMON.md and its later targeted-test
rule were followed. No full suites, push, PR, tag, stash, attribution trailer,
sub-agent, or Claude CLI invocation occurred. Roxygen used 7.3.3 on the pod;
only changed Rd files (and the earlier new exports) were pulled back.

Final source/test heads before this evidence archive:

- Server: `1490d727e5cd92ebfe6378bc96ce44850b8f84f1`.
- Client: `0de81b226e91f2853dec3246f8a5816bd0232b9b`.

Server commits:

- `6f4de2e` Validate exact physical result identities before driver coercion and admission
- `a3e3952` Restore declared identity types for empty SQLite results
- `8719d51` Exercise published audit estimands and bounded native feature controls
- `57e80c6` Use the supported dp mode in published incidence acceptance
- `508f095` Preserve typed optional results and verify pinned upstream cohort and published estimands
- `716f9fc` Require SQLite integer storage and clarify upstream fixture metadata
- `24c4616` Enforce the imported vocabulary validity policy before patient access
- `1490d72` Document imported policy and keep metadata probe outside the closed envelope

Client commits:

- `0dab697` Add Circe support inspection, structural diagnostics and source provenance
- `89961b4` Pin supported and refused PhenotypeLibrary acceptance and document bridge limits
- `7c3418d` Distinguish upstream status from unavailable deprecation metadata
- `ece8ba8` Carry imported vocabulary validity through plans and recipe lifecycle
- `b19c2f0` Document executable imported policy and pinned phenotype acceptance limits
- `0de81b2` Assert the existing merge disagreement diagnostic for policy conflicts

A following server-only commit archives this report and evidence. Its hash is
reported in the final response; it does not change executable source.

## Findings mapped to repairs and tests

### R1 — repaired

Integer result keys are checked against exact permitted representations before
projection or admission. Signed BIGINT decimal text is checked digit-by-digit
without conversion through double; integer64 uses exact character conversion.
Fractional, nonfinite, noncanonical, missing, out-of-range and classed values
refuse with the bounded generic integrity message, without their contents.

SQLite requires special handling before DBI converts mixed storage classes:
integer keys are selected through TYPEOF-gated exact text conversion. REAL,
TEXT and BLOB values in an INTEGER-declared column therefore cannot masquerade
as integers through driver coercion. Empty results restore the already-validated
identity types; ordinary small keys return integer and larger keys retain exact
canonical decimal text. Identity validation still runs when public projection
removes the identities.

`test-audit-results-identity.R` covers the review's 1.5/arbitrary-text cases,
NULL/BLOB/mixed storage, projection, valid exact identities above 2^31/2^53 and
signed64 limits, canonical spelling, generic messages, and empty tables.
`test-ohdsi-results.R`, `test-ohdsi-small-support.R` and
`test-analysis-pooling-contract.R` cover directly affected admission/pooling.
The final pre-policy regression run passed 2,462 assertions with no failures,
errors or skips; final policy-integrated results are recorded below.

### R2 — partial; remains blocking

Q03 now has exported `recipe_circe_support()` and `recipe_circe_check()`, a
versioned directional matrix, shared import/export refusal, bounded multi-issue
structural diagnostics, source bytes/hash, adapter/subset versions, sanitized
optional public origin, and derivative tracking through serialization. Generated
code now retains the origin too. Unknown upstream deprecation/review metadata
is not invented. Native populations without an export adapter are explicitly
unsupported. Tests cover parser/structural/provenance behavior and pinned
supported/refused definitions. This does not certify exhaustive multi-clause
coverage or every edited-population/merge/slice lifecycle obligation in A-31.

The paired fixtures pin OHDSI PhenotypeLibrary 3.37.0 commit
`40106e4e2892d883c208b9df10ad56711da3e21d`: accepted phenotype 304 is supported;
phenotype 10 deliberately refuses. Original JSON/SQL bytes, SHA-256 values,
source locations, upstream status, unknown metadata and Apache-2.0 license are
shipped. The narrowly admitted false ConditionTypeExclude flag on 304 does not
permit true/null/array alternatives. The server executes unmodified upstream
304 SQL with SqlRender 1.19.7 on typed PostgreSQL tables and compares actual
private membership/start/end dates against the native imported plan on the same
synthetic fixture. This is one upstream phenotype oracle, not general parity.

Q06 optional completion now uses a closed optional-column/type declaration:
statistics are double, the named descriptive fields character, and required
keys/support are never completed with logical NA. The full version/fingerprint/
relationship/status manifest and actual older/current upstream exports are
still missing; the existing CM 6.0.3 fixture remains explicitly schema-only.

Q11 now has the executable closed `ohdsi-import-1` policy in both wire schemas
and server IR. Both public import paths attach it to executable expressions;
registry included/excluded expressions keep separate policies. Server preflight
checks capability and valid explicit public concept records before patient
access. Expansion filters invalid descendants and mapped source records and
uses valid Maps-to source-to-selected-target direction. Classification C is
allowed; concept 0 needs its actual valid entry. Native declarations omit the
field and retain their existing behavior. Semantic digests differ by policy.

Legacy lowering, JSON/YAML save/load, real parameter binding, merge, generated
code and executable plan selection preserve the policy. Explicit v1 execution
checks the distinct capability too; automatic selection uses v2. An incapable
server refuses without fallback. Tests use a stale public vocabulary, native
controls, included/excluded references, zero, mapping direction, no patient
queries on refusal, unsupported/unknown policy, policy/digest preservation and
capability paths. Q11's integration into the A-30 codebook and complete slice/
full-lifecycle acceptance remains unclosed.

### R3 — focused Q04/Q05 controls repaired; remains blocking overall

The incidence acceptance now invokes published `omopAnalysisRunDS` and retains
the real release/protection implementation. It compares published components
and rate with the independently specified 40-person / 400-person-day / 20-event-
person / 40-event-record fixture passed through the same actual protection
contract. It also compares omitted versus explicit zero settings and release
attributes. Later episodes do not enter that oracle. No protection is disabled;
scoped `auto` is the supported mode, whereas dp-only rejects this scoped route.

The FE acceptance retains December-only events outside January/February cohort
periods and contrasts it with an executed native bounded recipe through the
real compiler: bounded count zero versus all-history presence across 80 episodes.
Tests are in `test-audit-settings.R`. The new upstream phenotype SQL comparison
is in `test-audit-circe-oracle.R`.

Actual old/current physical-result export acceptance, downstream consumers and
paired deployment/privacy acceptance are still missing. R3 is therefore not
closed by these focused controls.

## Retained Q01–Q35 acceptance ledger

| Obligation | Disposition after this repair |
|---|---|
| Q01–Q02 | Existing strict original-type/parser corrections retained; targeted import regressions pass. |
| Q03 | Support/check APIs, provenance, pinned supported/refused fixtures and one actual upstream membership/date oracle added. Exhaustive diagnostics and full derivative/merge/slice lifecycle not certified. |
| Q04 | Published protected-route component/rate oracle and omitted/zero comparison added; broader method/declaration/deployed acceptance remains separate. |
| Q05 | Actual bounded native control added against retained all-history FE behavior. |
| Q06 | R1 exact identity repair and typed optional completion added. Version/fingerprint/status manifest, relationships and real export acceptance absent. |
| Q07 | Covariate namespace/semantic-join/codebook lifecycle absent. Exact result identities alone do not close it. |
| Q08 | Recorded/native era codebook and boundary acceptance absent. |
| Q09 | Study roles and full time-at-risk codebook absent. |
| Q10 | Named native-window presets/examples and full coverage/repeated-episode acceptance absent. |
| Q11 | Executable versioned validity policy, paired contracts, capabilities and tested lifecycle added. Codebook integration and complete slice/lifecycle acceptance remain open. |
| Q12 | Full CDM 5.3/5.4 role/death/episode/coverage declaration and acceptance lane absent. |
| Q13 | Existing incidence/FE descriptions retained; complete per-route fidelity/result/codebook contracts absent. |
| Q14 | Four formula builders and numerical/lineage oracles absent. |
| Q15 | Typed number_daily aggregation absent. |
| Q16 | Runnable baseline/six-month comparison absent. |
| Q17 | Observation-fraction/count example absent. |
| Q18 | Episode-scoped previous/current-day rule absent. |
| Q19 | Pre-reduction plausibility variants absent. |
| Q20 | Integer/category example absent. |
| Q21 | Published ds.getWGSR/reference workflow absent. |
| Q22 | Cross-source scalar fallback example absent. |
| Q23 | Pinned published downstream-consumer matrix absent. |
| Q24 | Study codebook comparison absent. |
| Q25 | Pooling lesson and acceptance absent. |
| Q26 | Study bundle constructor/schema/serializer/privacy acceptance absent. |
| Q27 | Real-interview VA capstone absent. |
| Q28 | Endorsed 24-entry library pair not integrated; audit reconciliation/execution absent. |
| Q29 | Full question-to-route guide absent; AUDIT_CORRECTIONS.md documents only implemented corrections. |
| Q30 | New parser/settings/schema/policy diagnostics and policy/origin codegen added. Complete all-route explain/codegen/refusal acceptance absent. |
| Q31 | Native-v2 book/render/exercises/two-site acceptance absent. |
| Q32 | Versions match; fresh/upgrade/profile/restart smoke absent. |
| Q33 | No new privacy configuration/state/budget/TLS rule; deployed privacy acceptance not certified. |
| Q34 | Exact minor-unit cost example absent. |
| Q35 | Pinned phenotype SQL and CM schema artifacts added. Full dataset/CDM-version lane remains absent. |

## Validation and reproducibility

All R tests used `/workspace/dsomop/tools/rtest.sh` on the pod with matching
`RTEST_WITH`, timeout 1200, and anchored targeted filters. No full suites ran.
The later test-economy rule in COMMON.md overrides its earlier full-suite text.

- Server pre-policy combined run: server `716f9fc`, client `7c3418d`, 2,462
  passed, zero failures/errors/skips. Files: audit-settings, audit-circe-oracle,
  audit-results-identity, analysis-catalog, ohdsi-results, ohdsi-small-support,
  analysis-pooling-contract (each `test-<name>.R`). Logs:
  `audit-fix1-server-final.log` and `.full.log`.
- Client broad run: client `ece8ba8`, server `24c4616`, 2,415 passed, one new-test
  error caused only by expected merge-diagnostic wording. All other test files
  passed. Commit `0de81b2` corrects that assertion; runtime source is unchanged.
  Files: `test-audit-circe-support.R`, `test-audit-concept-import.R`, `test-audit-vocabulary-policy.R`, `test-recipe-circe.R`, `test-recipe-declarative.R`, `test-recipe-episode-policy.R`, `test-recipe-executable-contract.R`, `test-recipe-schema.R`, `test-recipe-setops.R`, `test-recipe.R`, `test-v2-execution.R`, `test-v2-merge.R`, `test-v2-params.R`, `test-v2-plan.R`, `test-v2-protocol.R`, `test-v2-review.R`. Logs: `audit-fix1-client-final.log` and `.full.log`.
- Final client policy run: client `0de81b226e91f2853dec3246f8a5816bd0232b9b`, server `1490d727e5cd92ebfe6378bc96ce44850b8f84f1`; **220 passed, zero
  failures/errors/skips** in test-audit-vocabulary-policy.R,
  test-audit-circe-support.R and test-audit-concept-import.R. Logs:
  `audit-fix1-client-policy-final.log` and `.full.log`.
- Final server policy-integrated run: server `1490d727e5cd92ebfe6378bc96ce44850b8f84f1`, client `0de81b226e91f2853dec3246f8a5816bd0232b9b`;
  **3,429 passed, zero failures/errors/skips**. Files: `test-analysis-catalog.R`, `test-analysis-pooling-contract.R`, `test-audit-circe-oracle.R`, `test-audit-results-identity.R`, `test-audit-settings.R`, `test-audit-vocabulary-policy.R`, `test-compile-concept.R`, `test-ir.R`, `test-ohdsi-results.R`, `test-ohdsi-small-support.R`, `test-vocabulary.R`.
  Logs: `audit-fix1-server-policy-final.log` and `.full.log`.
- Independent Python contract checks pass: 3 schemas, 2 examples, 224 probes,
  15 acceptance plans, 12 digest vectors, 18 normalized fixtures, 34 numbers,
  11,043 corpus doubles. Native digest vectors remain unchanged. Paired policy
  helper and plan-v2 schema bytes match; checksums updated. Log:
  `audit-fix1-contracts-final.log`.
- Roxygen 7.3.3 logs: `audit-fix1-roxygen.log` and
  `audit-fix1-policy-roxygen.log`. Earlier failed development runs are retained,
  not substituted for final evidence; `evidence-index.json` records their results.

The client uses existing shared pod dependencies
`/root/rt/numeric-deps:/root/rt/labels-deps`. The upstream SQL oracle additionally
uses this job's private `/root/rt/audit-fix1-deps/R` (rJava 1.0-18,
SqlRender 1.19.7) and privately unpacked OpenJDK 17.0.21 under
`/root/rt/audit-fix1-deps/java/usr/lib/jvm/java-17-openjdk-amd64`.
Set R_LIBS_USER to those three R-library directories, JAVA_HOME to that Java
path, and LD_LIBRARY_PATH to its `lib/server`. Installation logs preserve the
initial configuration/symlink failures and successful retry. No global Java or
R installation/configuration was changed. Oracle requirements are explicit;
a skip is not claimed as successful upstream acceptance.

## Deviations, integration notes and cleanup

The main deviation is substantive: this is a partial repair, not the requested
closure of every blocker. Missing independent Q06/Q13 and other obligations
remain work to implement; passing adjacent tests does not satisfy them.
No new broad dependency integration was performed. Available independent
reviews endorse numbers pair `01a2faf` / `ed73633` and library pair `2beed9c` /
`29396d7`; categorical review 4 still blocks `600c280` / `2c087a0` on R7,
prospective panel/survival schemas with date presentation. These facts explain
integration uncertainty, not completion or exemption from audit work. A question
about prerequisite revisions received no answer during this run.

Another integration job must preserve the optional validity field in both
schemas, the capability requirement on both plan versions, and the exact
SQLite identity admission path. Do not replace the strict imported policy with
client provenance, flatten site vocabulary expansions, or advertise the physical
CM lookup layout as supported. The upstream SQL bytes intentionally retain
original whitespace for checksum identity; range whitespace checks therefore
report upstream fixture whitespace; archived console logs may also retain
trailing whitespace. Working-tree source checks are clean.

No owner privacy settings, TLS requirement, call/account budgets, per-user or
per-session answer state, or extra support gate was introduced. Persistent
server-state rules remain unchanged. Existing core final release gates remain
in use; synthetic private oracle inspection is confined to tests.

Local free disk remained about 9.7 GB. CLEANUP.md was read and permitted cache/
scratch candidates checked; no removable local task cache was found. No large
local package copy was made and no protected paths/other worktrees were removed.
Pod cleanup after evidence capture: deleted only `/root/rt/wt/audit/dsOMOP` (24,298,185 bytes), `/root/rt/wt/audit/dsOMOPClient` (15,167,720 bytes), `/root/rt/lib/wt_audit_dsOMOP` (5,194,879 bytes), `/root/rt/lib/wt_audit_dsOMOPClient` (11,186,564 bytes), `/root/rt/audit-fix1-deps` (394,790,331 bytes). Shared dependencies and the copied evidence were preserved; see `audit-fix1-cleanup.log`.

The exact report is also archived in the server's `tools/audit-fix1/report.md`
with the evidence logs. The external requested path is
`/workspace/dsomop/design/astra-dev/audit-fix1.report.md`.
