# WS-2c final result acceptance — A-19 / R5

This report distinguishes exact result evidence from a plan's ability to normalize
or lower to IR. The historical `IR=435 / no-IR=30 / refused=12` inventory is not a
result-equivalence claim. The 477 unique plan representations include 389 not
executed through a captured baseline plan endpoint, 28 baseline refusals and 60
successful baseline executions. Population-resolution records are counted
separately. Client tests use mocked transport; passing them does not establish
server result equivalence.

**Known compatibility gap, not an accepted refusal:** CR-25 remains pending.
Unmodified 2.10.2 validates a handle-owned temporary table name in
`populations.p.cohort_definition_id` with `valid=TRUE`, no errors or warnings,
and executes the expected six-person baseline result; the current closed
population representation admits numeric
cohort IDs, validates it with `valid=FALSE`, and refuses execution with a typed
`validation_error`. The committed
`probe_owned_cohort_reference.R` reproduces both outcomes. This case is outside
the frozen result/compatibility fixtures and prevents a universal claim that
all historically executable v1 plans are preserved. It is not relabeled safe,
newly refused, or otherwise waived by this report.

## Snapshots and scope

The pristine 2.10.2 baseline is pinned at server
`30b7905617dbe3de5d06699c324095635c68011f` and client
`7eb2f6c8c2b49e5492e1f778916b84319626341d`. The final result snapshots use server
`9b3cb5df1e579038118b1ed4718e219c872d0f47` and client
`5a53401af29f5184ac001165be698383f1705293`; their R source trees are respectively
`739831363264a462cf6e5161399f106ceb25bd56` and
`2dabab3a55a447957ba33c6afb375e91f8a9ed9a`. Subsequent reporting and probe
commits do not change those sources; the parent job report records the final
full-suite commits and any subsequent production change.

The final source includes `be967404bf42ad0f039275d431129946a0cb1777`, which
restores legitimate v1 vocabulary/metadata reference publication using private
executor proofs, and `8871d8e`, which prevents those proofs when an effective
visit filter makes the output patient-dependent. The final `9b3cb5d` guard
uses the compiler's effective concept-scope column as well: hidden private-key
or blocked extension selectors cannot acquire a reference exemption. Standard
metadata columns, admissible extension columns and ordinary patient-source
legacy overrides remain supported. The final focused reference regression
suite passes 169 checks without failures, errors or skips. The staged reference
regression uses supported CSV fallback because
Arrow is absent; it is not Parquet evidence. Earlier captures at `dc07756`
and full-server snapshot
`8602ffc` were stopped and are not final acceptance evidence. The replacement
current/hybrid captures include all reference-proof fixes; the parent report
identifies the matching complete-suite run. Complete captures at `6571ecd`
and `8871d8e` were clean after per-call reconciliation, but precede the final
reference-dependency guards and are retained only as superseded diagnostics.

Two complete baseline server/client suite runs produced 330 top-level records
and 383 records including nested diagnostics, independently in each run. There
are 301 frozen-corpus calls, 23 A-19 evidence calls and six open-item-(i) probes.
There are no baseline capture errors or malformed JSON lines. The client runs
produce no top-level server result records.

The current and hybrid captures run the 25 server test files that produced those
baseline records, plus the result-pin replay. Hybrid means the pinned 2.10.2 test
bodies on current production code; original assertion failures for an endorsed
change are diagnosed separately from an unexpected execution/result change.
The exact filter is:

```
^(analysis-catalog|analysis-ports|analysis-ports-1bc|analysis-ports-achilles-live|analysis-ports-cm-propensity|b10-probe-setop|blueprint|cohort|extraction|feature-semantics|filters|ir-results|ir-results-a19|longitudinal-panel|longitudinal-sql|output-contracts|plan|plan-cohort-filters|plan-index-event|privacy-dp|recipe-reach|recipe-setops|shortcuts-server|survival-sql|survival-validation|temporal-sql)$
```

This scope is not a substitute for both full current suites. Their results are
reported in the parent job report.

Baseline A's raw capture directory also contained an earlier orphan writer under
the same label. The reviewed derivative keeps process `619694`, named in the
complete `dsOMOP.log` startup marker, and excludes `618173`, whose file has no
helper/start record or corresponding suite log. Raw A remains untouched; the
reviewed metadata records the excluded file's SHA-256 and reason. Baseline B's
complete process is `619967`. No orphan or duplicated record enters pin selection
or the reported comparison.

## Exactness and declared exclusions

Numbers use 17 significant digits, including distinct signed zero, NA and NaN.
Dates and POSIX timestamps additionally retain their underlying 17-digit numeric
payload, so fractional Date days and submicrosecond timestamps are not silently
rounded by the readable ISO/UTC representation. The capture and both replay
canonical blocks are identical. Canonical comparison falls back to complete-tree
equality for any field not covered by a human-readable diagnostic; no otherwise
unexamined field can make unequal trees count as identical. Raw pseudonym hashes
are compared in addition to the pseudonym linkage structure.

For public assignment endpoints the compared result is the inventory and
complete values of the committed bindings. Their return/status transport is
reported separately: the new bounded status object is an explicit §7.3
publication-contract change, not a byte-identical return value. Each matched
successful pair records hashes of the actual compared canonical payload plus
raw pseudonym hash; the masked DP pair is labeled explicitly.

The new baseline helper SHA-256 is
`81bb8c59f2a1eef419018f7340544efd2e1a0b8d2736c9ad30ca6691dd758d25`.
The final helper is
`820caef965c6d68abd26a71d61ab9b5ff9d35b90e6df8deac046215290d11c05`.
`git diff 041033d dc07756 -- tools/ws2/capture_results.R` shows only a header
comment correcting the Date/POSIX description; executable capture code is
identical. This explains the differing whole-file hashes without waiving a
comparison difference.

Temporary directories, staging tokens, expiry timestamps and generated internal
table/file suffixes are normalized by documented rules. Staged descriptors,
file contents and file byte hashes are still compared. An original DP test uses
a fresh random noise root: one of 330 baseline records differs only in
`dataset_id`, `lineage_id` and `mac`. Those three provenance fields are explicitly
masked for that record, and no other record is excluded as nondeterministic.
The fixed-root A-19 DP fixture supplies the stronger deterministic evidence for
the sealing contract. The guarded historical compatibility fixture and the
separate policy-gate oracle have their own narrower canonical contracts; this
report's numeric Date/POSIX guarantee does not retroactively describe them.

## Corpus decisions and per-call attribution

The strict join uses only the pinned baseline inventory and stable block keys.
It contains 1,376 reviewed rows: 514 safe, nine corrected, 42 newly permitted,
one newly refused structurally, one newly refused at publication, 18 unchanged
publication refusals, 190 unchanged invalid and 601 non-plan. Regeneration and
`--check` both pass. The A-20 measurement-interval row remains assigned to WS-5.

Those rows describe 1,295 distinct blocks. Only 211 have a captured top-level
baseline call; the remaining 1,084 are not credited as result comparisons:
420 server gate-helper blocks, 210 client gate-helper blocks, 351 mocked-client
plan blocks, 46 SQL/extraction-helper blocks, 30 other server entry-point blocks,
20 cohort-helper blocks, five validation/preview blocks and two inventory
executor candidates. Of the last two, streamed Parquet intervals are skipped
because Arrow is unavailable, while the factor-level test only mentions
`omopPlanExecuteDS` in a comment and manually attaches a tag. That comment is an
inventory false positive, not an unobserved execution.

`reconcile_ws2c_results.py` attributes matched calls to specific mixed-block
subrows using the original input and baseline refusal. It fails for an unknown
mixed block. Successful control/intermediate calls in a refusal-class test are reported
as setup/control calls. The raw report still preserves block-class unions for
inventory purposes, but those unions are not used as the per-call acceptance
rule. Changed fixtures, unmatched calls and unexecuted blocks remain separate.

The audit corrected five specific misunderstandings in existing classifications:

* Private index resolution is newly permitted; that original call never
  attempted publication. Current tests check final publication separately.
* The original narrow-age recipe (`min=121,max=122`) has no cohort or explicit
  reference year and remains invalid. The declared valid-reference variant
  (`30..31` at `2020-07-01`) is newly permitted, with final support checked.
* The forged standalone cohort `safe_scope` remains invalid. Optional contracts
  permit absence, not forgery. Two other mixed bin blocks now split omitted
  contracts from supplied forged, mismatched or expired contracts.
* The original empty/tiny intersection calls have no private execution
  context and retain the standalone cohort-combination support gate. Separate
  context-bearing and plan/public-entry probes demonstrate newly permitted
  private execution; both meanings now have explicit subrows.
* The public common age-grid execution is safe: its original
  `nfilter.tab=1` result already contains all six bands. Pin `r005` verifies the
  entire result, including those bands; it is not silently corrected.

## Captured outcome reconciliation

The final capture labels are `ws2c-final4-current` and `ws2c-final4-hybrid`; the
pristine comparison labels are `ws2c-b18-a-reviewed` and `ws2c-b18-b`.
At the scoped snapshot, current tests pass 4,085 assertions with zero failures or
errors and three dependency skips. The hybrid historical tests record 3,948
passes, 24 failed assertions and nine errors, with the same three skips. These
are not reported as a green historical suite. The superseded `6571ecd` and
`8871d8e` captures differ by one passing
assertion per run, entirely in unchanged `test-privacy-dp.R`. Its bounded-mean
and binary-rate checks assert bounds only when the random-noise result is
non-NULL. Aggregate logs do not retain which conditional branch was omitted.
The snapshot-check history retains those exact totals and separately verifies
matched-pair outcomes and compared-payload hashes, A-19 evidence rows, and
open-item-(i) probe traces across the captured source revisions. All 590 matched
pair outcomes and compared-result hashes, all 23 A-19 evidence rows per run, and
the probe traces agree between `8871d8e` and final `9b3cb5d`; their assertion
totals also agree. The 27 affected historical blocks expect removed width/equality gates, old private support gates, the old
FALSE-versus-UNKNOWN intermediate date mask, old observation-period overlap
refusal, old diagnostics/status shapes, unrequested staged output, or release
of the under-supported recurrent component. Original assertion changes are
separate from the complete top-level result accounting below.

| Captured records | Current tests | Historical tests on current code |
| --- | ---: | ---: |
| All top-level calls | 372 | 361 |
| Frozen-corpus calls matched to the 301 baseline calls | 290 | 300 |
| Baseline calls unmatched | 11 | 1 |
| Current-only calls, excluding A-19/probes | 51 | 30 |
| Matched A-19 calls, reported separately | 23 | 23 |
| Open-item-(i) probe calls, reported separately | 8 | 8 |
| Capture errors / malformed lines / detected token leaks | 0 / 0 / 0 | 0 / 0 / 0 |

The reconciled classes below apply to individual matched frozen-corpus calls.
“Changed fixture” keeps the database-hash mismatch visible and is not credited
as exact unchanged-result evidence.

| Per-call disposition | Current tests | Historical tests on current code |
| --- | --- | --- |
| Safe | 179 identical | 181 identical |
| Corrected | 7 identical; 3 different; 1 retained refusal; 5 different with changed fixture | 7 identical; 4 different; 1 retained refusal; 5 different with changed fixture |
| Newly permitted | 1 baseline-only refusal | 5 baseline-only refusals; 2 more with changed fixture |
| Newly refused at publication | 1 current-only refusal; 1 changed-fixture result | 1 current-only refusal |
| Newly refused structurally | 1 current-only refusal | 1 current-only refusal |
| Unchanged publication refusal | 5 retained refusals; 2 with changed fixture | 5 retained refusals; 2 with changed fixture |
| Unchanged invalid | 63 retained refusals; 1 with changed fixture | 65 retained refusals; 1 with changed fixture |
| Non-plan | 6 identical; 3 retained refusals | 6 identical; 3 retained refusals |
| Successful control/intermediate calls in refusal-class blocks | 11 identical | 11 identical |

The raw block-union report retains two current and three hybrid “safe issues.”
They are the recurrent under-k memory/changed staged fixture in current tests,
and the overlapping index candidate, unrequested staged output and under-k
recurrent memory call in hybrid tests. The explicit per-call subrows above
classify each as newly permitted, corrected or newly refused; none is an
unexplained safe-result difference. Its four current and one hybrid “safe
unmatched” flags are accounted for in the complete unmatched inventory below.

The changed results have specific contracts: two F-26 age-group corrections;
the DP producer seal; and, only in the hybrid original call, no requested roots
producing no staged output. Five recipe-setops comparisons change only the
internal unrestricted-base `person_ids` representation from `integer(0)` to
`NULL`; their fixture hashes also differ and are retained as such. The current
recurrent staged fixture adds a genuine event person (two event rows become
three), so its value/pseudonym/file differences are expected fixture changes,
not a successful comparison of the original under-supported fixture.

The two changed-fixture newly permitted hybrid calls are the small private
population fold and criteria population. The three changed-fixture retained
refusals are bare empty/tiny intersections and a malformed scope-table sequence.
The separate fixed A-19 fixtures demonstrate the relevant semantics without
reclassifying those database-hash differences as equality.

The 11 unmatched baseline calls under current test names are exactly: two
ordered/narrow extraction blocks; the common age-grid execution; two index-event
coverage calls; the private index support call; the no-requested-root staged
call; the absent-output-symbol refusal; the invalid narrow-age recipe; the small
private population fold; and the small criteria population. The hybrid preserves
the old inputs/names and matches all 11. Its sole unmatched baseline call is the
original recurrent staged publication: the earlier memory publication is now
refused, so the old test attempts to read absent bindings and stops before the
staged call. The A-19 admissible memory/staged pair is exact separately.

Of 51 current-only calls, 30 are the new 27 memory and three staged pin replays.
The other 21 are renamed/expanded extraction, grid, index, staged-root, symbol,
narrow-age and private-population tests (17), three new DP assertions and one
new admissible recurrent memory call. They are not counted as historical
matches. All 30 hybrid-only calls are the added pin replays. There are no
unexplained unmatched frozen calls.

All 23 A-19 calls match in both runs: 12 identical, five documented result
corrections, two retained refusals, two newly permitted executions and two new
structural refusals. The corrections are the DP producer/lineage/MAC seal,
no-root staging, unrestricted-base resolver representation, and unrestricted
base output at threshold zero in memory and staged form. At threshold three
the unrestricted-base execution changes from refusal to success. The two
identifier-filter non-strict paths now refuse; strict mode still refuses.

## A-19 result contracts

| Item | Acceptance evidence and disposition |
| --- | --- |
| 1 / F-26 | Two baseline-output calls are corrected only in `demo.age_group`. The independent replay oracle derives annual ages from fixture cohort/person rows and applies base `cut`; it does not call production `.bandAges`. Every other field remains exact. The public common-grid call remains safe as above. |
| 2 | The original recurrent fixture has two event persons at threshold three and is newly refused at publication. Separate declared fixtures have four event persons and 12 risk-set persons; memory and staged outputs are compared exactly on both versions. The edited current test's extra rows are not baseline evidence. |
| 3 | A fixed-root DP preparation fixture checks the DP executor's sealed assignment, producer-bound attributes and preserved values. The baseline-age-at-index refusal remains `not_person_local` and happens before commit; the ordinary plan-loading control stays DP-free. |
| 4 | An internal staged call with no requested roots is corrected to publish nothing. The separately declared requested-root fixture compares descriptor and staged-file contents exactly. |
| 5 / A-E14 | A declared empty unrestricted base beside another population now resolves to every person in scope. Direct base-versus-explicit-population comparisons run in memory and staged modes at thresholds zero and three; the roster relation is checked separately. |
| 6 | The old inclusion-criteria test's `tryCatch` hid an invalid `temporal` structure. It is unchanged-invalid, not a lost successful result. |
| 7 | V1 keeps the historical DSL-admitted literals and the actual site's SQL conversion/refusal behavior; native v2 keeps strict scalar typing. The closed legacy envelope, required source columns and private-key exclusions remain checked. The compatibility/oracle dispositions are reported separately from unchanged results. |
| 8 | An identifier custom filter was already invalid in strict mode. Non-strict mode previously returned an output-error object; it now receives the typed structural refusal naming `/outputs/tte/filters/custom`. |

Open item (i) is measured at the actual entry points. At pristine 2.10.2 both the
empty and one-person intersections refuse at `.planResolvePopulations`, internal
`.planExecute`, and public `omopPlanExecuteDS`, even when the requested output is
over the adequate population A. Every refusing trace reaches
`.cohortCombine > .assertMinPersons`; `.cohortCombine` has no context parameter
there. This is execution evidence for an intermediate gate, not an inference
from the helper name or an IR count. Final-snapshot probe outcomes appear in the
raw report alongside these six baseline traces. Current bare helper calls still
refuse with `ctx NULL`; private-context resolution and internal/public plan
execution over adequate population A succeed. `test-ir-review.R` asserts that
separation, and the exact bare calls are `publication_refused_unchanged`, not
misreported as newly permitted.

The durable baseline pins comprise 27 memory results and three admissible staged
results. Staged fixtures contain six persons in streamed conditions, six in the
temporal `personRef`, and 12 in multi-state `msdata`. Non-person reference tables
retain their own exact shapes. The original recurrent two-person events fixture
is explicitly rejected by staged-pin selection; its admissible replacement is
in the separate A-19 evidence, not a relabeled unsafe baseline.

## Separate compatibility evidence

The immutable 1,156-case compatibility fixture records 778 baseline successes
and 378 baseline errors. Its explicit sidecar has 134 dispositions: 39 newly
refused structural cases, 83 newly permitted cases, four unchanged-invalid
cases and eight corrected F-26 cases. Of the cases without a disposition, 731
have a baseline result hash and 291 retain their original refusal. These are
case counts, not additional frozen-corpus result totals.

The 70 policy-oracle cases within the newly permitted class retain their raw
baseline refusals. Their independently captured control changes only five
endorsed assignment-policy checks in the pristine 2.10.2 gate; SQL compilation,
coercions and result construction remain historical. Two captures agree, with
exact plan-hash joins. This is a declared policy-adjusted oracle, not unchanged
raw-baseline equivalence. The other 13 newly permitted cases use explicit
original baseline controls.

For the eight F-26 age-break coercion variants, the original suppressed result
is retained. Each compares its entire current result to the existing same-plan,
same-literal `.tab0` baseline control. The dedicated metadata test verifies
that only `nfilter.tab` differs, both independent baseline captures succeeded,
and plan hashes and output shapes match. The independent age/base-`cut` oracle
in `test-ir-results.R` supplies the semantic check without using production
banding code. A bounded run at reference-proof commit `be96740`, before the final
visit-filter guard, passed all 6,913 checks for these eight cases, 21 reference-output cases and their metadata
guards; the full compatibility suite remains part of the full server run.

## Validation and durable evidence

The two pristine baseline runs pass server 14,003/14,002 assertions respectively
with 14 skips each, and client 4,156 assertions with seven skips each; all have
zero failures and errors. Replay of the regenerated baseline pins on pristine
2.10.2 passes 760 assertions with one skip and no failure/error. Static checks
pass the strict 1,376-row join, all three schemas, two examples, 145 probes,
15 acceptance plans, 12 digest vectors, 17 normalized fixtures, 34 number vectors
and 11,043 corpus doubles. All four comparator unit tests and the identical
canonical-copy check pass.

The committed evidence contains the [raw comparison](ws2c-result-evidence/b10-report.json)
and its [readable summary](ws2c-result-evidence/b10-report.md), with metadata and
raw outcomes, plus the [call dispositions](ws2c-result-evidence/call-dispositions.json)
and [CSV](ws2c-result-evidence/call-dispositions.csv) with the separately reviewed
per-call attribution. The [snapshot check](ws2c-result-evidence/snapshot-check.json)
records the final recapture comparison, and the [historical assertion review](ws2c-result-evidence/hybrid-assertion-review.csv)
accounts for all 27 historical failing blocks. Individual rows retain stable block key, function,
plan hash, occurrence, record IDs and exact compared-result hashes. This does
not replace the original JSONL captures, which remain in the ignored workspace
with their suite logs and baseline-derivation metadata.

## Reproduction

Use `tools/ws2/compare_results.py capture`, `pin`, `pin-a19`, `validate` and
`compare`, then `tools/ws2/reconcile_ws2c_results.py` on the raw report. Both
baseline captures add `test-ir-results-a19.R` and `test-b10-probe-setop.R`; the
current/hybrid captures also add `test-ir-results.R`. Capture metadata records
all added-file hashes. All R runs use the pod through `rtest.sh` with the matching
sibling package. The evidence workspace is the ignored
`tools/ws2/artifacts/ws2c/`; the committed raw comparison and call-disposition
reports accompany this report. The manually reviewed
`hybrid-assertion-review.csv` identifies every one of the 27 failing historical
assertion blocks, its exact failure/error count and the governing contract; it
does not alter result comparison outcomes.
