# WS-7 reducer and output compiler handoff

The normative contract is the endorsed design sections 2.6, 2.7, 2.9,
6.4, 6.11–6.13 and 7, with amendments A-5 and A-20. This document records
implementation interfaces for integration and documentation work.

## Execution and ownership

`.planExecute()` validates the complete wire plan before execution. Native
language 2.0 plans compile their sources and publication roots through
`.planExecuteNative()`. Explicitly empty publication roots remain a validated
no-op (A-22). Version 1 uses the compatibility IR validation followed by the
existing active v1 executors, preserving their result contracts and composite
inventories. The retired `.extractSurvival()` and `.extractIntervalsLongLegacy()`
are not active adapter entry points.

A-25 owned cohort references are integrated through that v1 adapter path.
Their registry ownership is checked before queries, membership is local to
the declared population, and borrowed tables survive success and refusal.
They do not supply an index anchor or establish DP locality.

The compiler uses a private child of the execution context. Its temporary
tables are registered for cleanup. WS-7 reducer/output realizations are
bounded by `dsomop.max_memory_rows`. An extra row is a non-skippable `validation_error`
with reason `reducer_row_cap`, never a partial result. Execution cleanup
preserves caller-owned tables. Compiler failures release both owners' tables
while preserving the IR for typed refusal transport; the execution/publication
boundary closes the context. Successful compilation keeps the immutable
roster registry for publication.
Both compiler and operation-table cleanup are attempted when a DROP fails.
WS-6 matching has its separate `match_candidates_exceeded` refusal; private
database snapshots are not WS-7 R realizations.

## Population and variable interfaces

`.planCompileOutputs(cctx, out_symbols, populations = NULL)` compiles roots
and their private output dependencies in topological order. An optional
`populations` mapping supplies `list(roster, coverage)` records for tests.
Normal execution calls `.compilePopulationByName(cctx, population)`.
Dictionary `source_outputs` remain part of static validation and concept
closure, but are not execution dependencies. Dictionary-only roots compile
neither clinical sources nor populations, and metadata-only sparse outputs
do not enter the live covariate registry. Joined inputs remain execution
dependencies.

Rosters follow the WS-5/A-20 contract in
`ws5-interval-anchor-coverage.md` and WS-6
`ws6-populations-matching.md`: explicit population namespace,
`cohort_row_id`, person lineage, lossless origin and visit metadata and the
selected `op_component_id`, associated with the original coverage object.
Candidate and final row keys are never interchangeable.
Matching strata source/search anchors are declared for the cases population's
final roster and the controls template's candidate roster. The shared window
resolver evaluates each side's person at the case time; missing anchors use
the declared `missing_strata` policy.

`.compileVariable(ctx, node, roster, anchors)` receives the output's
`.reduceConsumer()` record in `anchors`. It returns the stream, pure reducer,
declared empty value, typed columns, availability and an evaluator returning
one row per roster key. Source reducers, person-derived values, raw allowed
columns and population columns share this interface. Reducer fragments have
the `rd` identity family; output streams have `ox`. Their allocated identities
must be declared before compilation. Association, source, window, coverage
relation and observation domain remain part of stream identity.

All source anchors and windows resolve through `.anchorExpr()` and
`.compileWindow()`. Undefined windows and unavailable sources give typed NA.
Unavailable table catalogues retain packaged CDM aliases and types. Mode
uses the declared type when ordering tied integer values transported as
decimal text; genuine text retains lexical ordering.
Available empty windows give the reducer's documented empty value; `all`
over an empty window is NA. Any/all preserve UNKNOWN predicate values.
Coverage uses the selected recorded/effective relation explicitly. Observation
eligibility envelopes never replace actual recorded observed segments.
Temporal output selection also applies to era course reducers. Raw
observation columns use structural population indexing: the selected
record under unique/earliest coverage policies for indexed rosters.
Unindexed rosters and union-policy raw projections retain first-record
ordering; raw projections do not invent a combined observation-period row.

## Publication and documentation

`.compileOutput(ctx, node, inputs)` returns a validated `CompiledOutput` with
one candidate per independently accessible component. `inputs` supplies the
population, private compiled dependencies, requested symbol and execution
mode. No support gate runs inside the output compiler.

Panels validate their complete prospective column inventory before fetching
values; generated-name collisions are structural errors, without renaming.
Sparse person outputs choose first/last episodes from the complete roster,
using exact chronological keys, so an empty selected episode stays absent.
Only `collapse = "any"` takes the maximum across episode cells. Bounded cost
sources use their parent's declared/resolved concept set, including an empty
set, while retaining the cost stream and its numeric role. Unrestricted
source identity universes still require a separately reviewed policy.

Episode components declare roster lineage on `(person_id, cohort_row_id)`;
person components declare direct lineage. Dictionary, time, covariate and
transition reference components have explicit reference lineage. References
attached to patient families publish only with that family. Joined inputs
stay private unless separately selected as roots. Publication gates each
patient component on its own rows in both memory and staged execution.

Generated reference names normally remain `covariateRef` and `timeRef`.
If an output itself has one of those valid names, only the conflicting
reference name receives a deterministic bounded hash suffix, using the same
allocator as survival components. The dynamic publication inventory reports
the resulting name. The patient primary keeps its output name and requested
symbol; this preserves default joined-component lookup and the primary
staged semantic contract's explicit `component = NULL`. No wire names become
reserved, and ordinary companion names remain unchanged.

WS-10 should document native survival's explicit censor list, last-observed
day endpoints, and the v1 adapter's retained cohort-end cap and opt-in death
censoring. It should also distinguish dense panel missingness from sparse
omission, recorded coverage from stockpiled effective supply, and structural
zero from unavailable/undefined NA. Staged native output presently uses the
same bounded private materialization as memory output before publication;
staging does not remove the reducer memory cap.

The executable tests cover reducers, each output format, private publication
families, lifecycle failures and WS-0 acceptance rows. Acceptance and ordinary
format/publication scenarios compile real WS-6 rosters from source fixtures;
only deliberate malformed-metadata and boundary probes inject roster data.
The public client
`test-ws7-client-acceptance.R` gate executes all fifteen frozen recipes through
the real WS-6 compiler and WS-8/WS-9 APIs, on SQLite and PostgreSQL, in memory
and staged modes, with publicly verified k=5. It also executes all twelve
recipe-13 bound variants against their base recipes' frozen expected results:
108 recipe/backend/mode executions in total. Set
`DSOMOP_TEST_WS7_ACCEPTANCE_REQUIRED=true` to fail missing prerequisites.
It checks exact row cardinality and independent episode identities, oracle
numeric values at tolerance 1e-9, and memory/staged parity with only fractional
CSV rounding allowed at relative 1e-14. Text, identifiers, integers and NA
positions remain exact. The separate safe-v1 baseline replay retains its
byte-identity checks. The final job report records tested commits and results.
