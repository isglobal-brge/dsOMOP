# Audit fix 2 — partial repairs; full audit remains blocked

14 November 2026. **The request to fix every blocking finding is not complete.**
R4's declaration-name bypass is repaired with paired acceptance tests. R2 has
additional independent Circe lifecycle/diagnostic and physical-result repairs,
but substantial CR-32/A-31 implementation remains absent. R3's wider acceptance
remains open. This branch must not be endorsed as complete 2.10.5 audit scope.

## Scope, rules and commits

Work was confined to the two named `feat/2.10.5-audit` worktrees and requested
design report/evidence paths. Starting HEADs were server
`a22e6c04e85ae4b71bb83a3646e74781fd1168f9` and client
`0de81b226e91f2853dec3246f8a5816bd0232b9b`; both were clean. There were no new
job-local WIP commits to adopt. No protected checkout, other job worktree or
`/workspace/refs` was modified. No Claude CLI, sub-agent, push, PR, tag, stash
or attribution trailer was used. Both versions remain **2.10.5**.

Server commits:

- `80a0fc412f6dffc6b16675dc22b1e38532ac4281` — structural vocabulary visitor and preflight, regression tests, migration NEWS.
- `be89ee9c197c46d573ff8aa8d224822505474cd4` — physical schema descriptors/status/fingerprints, optional type validation and duplicate-key checks.
- `5685e10be0836a6b746060efe1f08ba43764d08b` — schema-gated availability and whole-relation key integrity before caller slicing.
- `db8f0c2b337d308697b1ce44e87d1ad5486c8579` — documentation of corrections and their acceptance limits.
- `2939af0898a7779ccc8b969960c7047f02e2a1ad` — preserve schema descriptors after final projection.

Client commits:

- `7669a87e501150bfff1c01d5ac0443bd71854a86` — capability detection for every valid registry name, metadata controls and NEWS.
- `86e3814e96b3fe582fd582132e8d29ae745d3aaa` — independent Circe clause diagnostics, edited-population translation, merge provenance and tests; fixes one new test fixture's missing table argument.
- `1f429fdff9876594a9edaf71fa9e8c5057ca276b` — retain the existing specific Age remedy.
- `abcd331159e8af2384325b20f925e9342b5c9457` — merged edited-import compilation/codegen checks and repair documentation.
- `6dfbba218538e3003a0e9dd2e083a245462b0b20` — supplementary conflicting/successive edit acceptance (tests only).

A final server commit archives this report and evidence under `tools/audit-fix2`;
it changes no runtime source. The external design directory is not a Git
repository, so the exact requested report is mirrored into that committed archive.

## Finding-to-fix and test mapping

### R4 — repaired

Both packages use the byte-identical `.ohdsiWalkValidity()` visitor. It tracks
root, registry and population contexts; registry keys are never treated as
advisory field names. Only envelope `meta` and a recipe population's actual
`circe_provenance` are skipped. Every executable registry entry and nested
inline definition, including exclusions, is visited. Server preflight uses
this same inventory instead of maintaining a separate recursive walker.
Legacy recipe index declarations are detected as well as wire concept nodes.
No otherwise valid user name is reserved.

Both `test-audit-vocabulary-policy.R` files exercise public constructors with
`ordinary`, `meta`, `metadata` and `circe_provenance` in both event and concept-set
registries. They check required operations and real incapable-server negotiation;
the server checks invalid-reference refusal, unavailable-capability refusal and
absence of patient reads. Advisory meta/provenance decoys remain non-executable.
The original review probe's cases are encoded in these regressions; I did not
claim a new independent review or alter the reviewer's original evidence.

### R2 — additional repairs, still blocking

**Q03:** the diagnostic pass now checks measurement bounds, occurrence, window,
age and sex clauses independently. Shared occurrence validation is used by the
importer and checker. One malformed semantic clause no longer hides these other
unsupported clauses. Tests assert structural paths, complete reports for these
combinations and absence of arbitrary sentinel contents in diagnostics.

An imported population's cached native translation previously overrode later
legacy edits. Compilation now rebuilds it through the supported local bridge
when the legacy index/filters change; an explicit native index takes precedence.
Conflicting edits to both representations refuse with `ambiguous_circe_edit`
and an explicit remedy. Translation digests distinguish subsequent valid edits
from conflicting stale translations. Prefixed merges refresh derivative
provenance immediately while preserving original source bytes/hashes.

`test-audit-circe-support.R` checks changed executable concept selection and
semantic digest, JSON/YAML save/load, generated code, prefixed merge, compilation
and codegen of the merged recipe, imported validity preservation and native
index precedence. Existing Circe/import/plan/merge tests cover direct dependents.
This does **not** certify exhaustive nested-clause diagnostics, whole-recipe
context provenance, every slice operation or A-30 codebook integration.

**Q06:** `.ohdsiResultSchemaManifest()` adds explicit family, named legacy layout,
contract version, unknown upstream version, prefix rules, required identities
and types, count/person bases, optional types, inline key relationship,
suppression policy and public status. The CM lookup variant remains recognized
unsupported, with its schema-only evidence explicitly labelled. The reader
inspects physical metadata and validates optional types before reading results.

Discovery exposes `supported`, `recognized_unsupported`, `admin_only` and
`unknown` through per-tool `schema_contracts`. Ordinary availability/public
inventories include supported public layouts only. Discovery does not count
unsupported layouts. Fingerprints hash canonical table/column/type/key metadata,
not row contents; column order and row values do not change them. Recognizing a
CM layout never infers a site's upstream package version. Released result
frames retain `dsomop.results_schema` after final projection.

Private integrity checks now cover the complete key relation before caller
filters/projection/limits: missing/invalid keys (including SQLite storage
classes) and duplicate key tuples refuse generically. A bad or duplicate key
outside the requested slice cannot be hidden by `limit=1` or a filter.
No keys, counts or examples enter the refusal. Existing release gates remain.

`test-audit-results-identity.R` covers four statuses, metadata-only/order-invariant
fingerprints, unsupported optional types before row reads, descriptor retention,
unsupported-layout availability, duplicate/invalid keys outside caller slices,
and the original R1 exact identity cases. Direct dependent results/pooling/
small-support files are included in verification.

Q06 remains **incomplete**: explicit version-evidence reconciliation, complete
unknown/extra-column layout recognition, cross-table reference uniqueness and
association checks, and actual old/current upstream export acceptance are not
implemented/certified. The existing CM artifact remains schema-only. The new
manifest is not evidence that a verified upstream export was executed.

### R3 — still blocking

The earlier reviewed focused incidence/FE controls are retained. This run adds
no actual old/current upstream physical exports, published downstream consumers,
paired deployment/restart/privacy acceptance or rendered book evidence. No
adjacent unit test is presented as a substitute. The existing pinned upstream
phenotype oracle was not rerun and is inherited evidence only.

### R1 — original closure retained

The original malformed SQLite identity cases and exact integer contract remain
covered by `test-audit-results-identity.R`. Whole-relation checks extend that
protection before caller slicing. This does not close the broader Q06 contract.

## Q01–Q35 disposition

| Obligation | This repair's evidence / retained gap |
|---|---|
| Q01–Q02 | Strict parser retained; `audit-concept-import` and `recipe-circe` regressions run. |
| Q03 | Independent clause diagnostics and edited-population/save/load/merge/codegen regressions added; exhaustive/context/slice/codebook lifecycle still open. |
| Q04–Q05 | Previously reviewed published incidence and bounded FE controls retained; no new deployed acceptance claimed. |
| Q06 | Schema descriptors/status/fingerprints, optional physical typing and complete inline key relation checks added; version/reference/actual-export obligations remain open. |
| Q07 | Exact identities retained; namespace/semantic joins and A-30 codebook lifecycle still absent. |
| Q08 | Recorded/native era codebook and complete boundary acceptance remain open. |
| Q09 | Study role/TAR declarations and complete acceptance remain open. |
| Q10 | Named native window presets and complete boundary/coverage/repeated-episode teaching remain open. |
| Q11 | R4 capability/preflight bypass repaired; tested edit/merge/codegen retains policy. Complete slice/context/codebook acceptance remains open. |
| Q12 | Complete CDM 5.3/5.4 role/death/episode/coverage declaration and acceptance remain open. |
| Q13 | Complete method fidelity/result/codebook contract remains open. |
| Q14 | Four formula builders and numerical/lineage oracles remain absent. |
| Q15 | Typed `number_daily` implementation and two-stage execution acceptance remain absent. |
| Q16 | Runnable two-site baseline/six-month comparison remains absent. |
| Q17 | Observation-fraction/count teaching/oracle remains absent. |
| Q18 | Episode-scoped previous/current-calendar-day rule remains absent. |
| Q19 | Pre-reduction plausibility example variants remain absent. |
| Q20 | Integer/category ordered-scale example remains absent. |
| Q21 | Actual published `ds.getWGSR` workflow/reference acceptance remains absent. |
| Q22 | Cross-source aligned scalar fallback example remains absent. |
| Q23 | Pinned published downstream-consumer matrix remains absent. |
| Q24 | Study codebook comparison remains absent. |
| Q25 | Pooling lesson and required acceptance remain absent. |
| Q26 | Local study bundle constructor/schema/serializer/privacy acceptance remains absent. |
| Q27 | Real interview-instance VA capstone remains absent. |
| Q28 | Endorsed library pair not integrated/reconciled/executed here. |
| Q29 | `AUDIT_FIX2.md` documents only implemented repairs; full route/terminology guide remains absent. |
| Q30 | New bounded diagnostics covered; complete explain/codegen/refusal matrix remains open. |
| Q31 | Native-v2 book/render/exercise/two-site acceptance remains open. |
| Q32 | Versions equal; fresh/upgrade/profile/restart smoke remains open. |
| Q33 | No new forbidden privacy controls/state; deployed privacy acceptance remains open. |
| Q34 | Exact minor-unit cost example remains absent. |
| Q35 | Existing pinned artifacts retained; full dataset/CDM-version lane remains open. |

## Validation

All R runs used `/workspace/dsomop/tools/rtest.sh` on the pod, timeout 1200,
anchored targeted filters and matching audit `RTEST_WITH`. Client runs also set
`RTEST_ENV='R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps'`.
The later COMMON.md test-economy rule takes precedence over its earlier full-suite
sentence. **No full suite ran.** No roxygen declarations/Rd files changed.

Final server verification: server `2939af0`, client `abcd331`: **2,057 passed,
0 failed/errors/skips**. Exact files: `test-audit-vocabulary-policy.R`,
`test-audit-results-identity.R`, `test-ohdsi-results.R`,
`test-ohdsi-small-support.R`, `test-analysis-pooling-contract.R`.
Evidence: `audit-fix2-server-verified.log` and `.full.log`.
Final client verification: client `abcd331`, server `2939af0`: **1,054 passed,
0 failed/errors/skips**. Exact files:
`test-audit-circe-support.R`, `test-audit-concept-import.R`,
`test-audit-vocabulary-policy.R`, `test-recipe-circe.R`, `test-v2-merge.R`,
`test-v2-plan.R`, `test-v2-execution.R`, `test-v2-protocol.R`.
Evidence: `audit-fix2-client-verified.log` and `.full.log`.

Supplementary final client controls: client `6dfbba2`, server `2939af0`:
**102 passed, 0 failures/errors/skips**, exact file
`test-audit-circe-support.R`, filter `^audit-circe-support$`. This adds acceptance
for conflicting edits and successive reconciled edits; runtime source is
unchanged from the eight-file client verification. Logs:
`audit-fix2-client-edit-controls.log` and `.full.log`.

Other development runs (retained, not substituted for final evidence):

- Server `80a0fc4`, client `7669a87`: **1,009 passed**, zero failures/errors/skips;
  exact files `test-audit-vocabulary-policy.R`, `test-compile-concept.R`,
  `test-ir.R`. Summary: `audit-fix2-server-policy.log`; its full pod log was
  superseded before archiving. The final server run repeats the policy file.
- Client `7669a87`, server runtime sources subsequently committed as `be89ee9`:
  **569 passed, 1 error** in a new test missing `omop_index_event(table=...)`;
  fixed by `86e3814`. Exact files `test-audit-vocabulary-policy.R`,
  `test-audit-circe-support.R`, `test-v2-execution.R`, `test-v2-protocol.R`.
  Logs: `audit-fix2-client-policy.log` and `.full.log`.
- Server runtime/test sources committed as `be89ee9` (commit made immediately
  after launch; NEWS added after mirroring), client `7669a87`: **1,935 passed**,
  zero failures/errors/skips. Exact files `test-audit-results-identity.R`,
  `test-ohdsi-results.R`, `test-ohdsi-small-support.R`,
  `test-analysis-pooling-contract.R`. Logs: `audit-fix2-server-schema.log`
  and `.full.log`.
- Client `86e3814`, server `be89ee9`: **1,051 passed, 1 error**, an existing
  assertion requiring the specific Age migration wording; fixed by `1f429fd`.
  Same eight files as the final client run. Logs: `audit-fix2-client-final.log`
  and `.full.log`.
- Server `5685e10`, client `1f429fd`: **2,055 passed, 2 failures**, both new
  descriptor-retention assertions; fixed by `2939af0`. Exact files
  `test-audit-vocabulary-policy.R`, `test-audit-results-identity.R`,
  `test-ohdsi-results.R`, `test-ohdsi-small-support.R`,
  `test-analysis-pooling-contract.R`. Logs: `audit-fix2-server-final.log`
  and `.full.log`.

Paired vocabulary visitor bytes match. Versions match. Source whitespace
checks pass. No vendor, Opal, real upstream export, Parquet, book render or
new independent review is claimed by these targeted checks.

## Deviations, prerequisites and handoff

The material deviation is **incomplete requested scope**. R2 and R3 remain
blocking. Independent Q06/Q13 work is not discharged by a prerequisite issue;
remaining work is expressly listed above.

The audit branches lack A-29 numeric and A-30 categorical/codebook APIs. I asked
which approved prerequisite pairs to integrate; no answer arrived during this
run. Existing review documents endorse numbers `01a2faf` / `ed73633` and library
`2beed9c` / `29396d7`; the available categorical review 4 still blocks `600c280` /
`2c087a0`. No speculative prerequisite integration was performed. This explains
the branch composition, not an exemption from the accepted amendment.

Integration must preserve the structure-aware visitor in both packages and
its reuse by preflight; registry names must never become metadata exceptions.
Keep the exact SQLite identity path and the complete-relation checks before
caller slicing. Do not mark a recognized structural layout as proof of an
upstream package version. Preserve existing release gates and keep schema
metadata from implying upstream method equivalence. Retain Circe translation
reconciliation and provenance on merge/codegen when integrating A-29/A-30.

No TLS requirement, owner privacy setting, quota/budget/accountant, per-user
answer state or new persistent server state was introduced. The metadata-only
manifest is shipped code; key checks are request-local SQL queries within the
existing public read snapshot.

## Disk and cleanup

Local disk had approximately 9.7 GB free. CLEANUP.md was read; permitted local
package/download caches checked were absent. No large local package copy or
full-suite run was made. Protected paths and other jobs were not deleted.
Pod cleanup after evidence capture (only this run's mirrors/private libraries):

- deleted /root/rt/wt/audit/dsOMOP 24465087 bytes
- deleted /root/rt/wt/audit/dsOMOPClient 15181460 bytes
- deleted /root/rt/lib/wt_audit_dsOMOP 5203461 bytes
- deleted /root/rt/lib/wt_audit_dsOMOPClient 11199960 bytes

See `audit-fix2-cleanup.log`. Shared dependency libraries and all evidence logs
were preserved. No local source copy was created or removed.

The supplementary edit-control run recreated only these same job mirrors/private
libraries; they were removed again after preserving its log:

- deleted /root/rt/wt/audit/dsOMOP 24468884 bytes
- deleted /root/rt/wt/audit/dsOMOPClient 15182439 bytes
- deleted /root/rt/lib/wt_audit_dsOMOPClient 11200011 bytes
