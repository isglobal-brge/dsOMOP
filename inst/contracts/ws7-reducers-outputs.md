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

The compiler uses a private child of the execution context. Its temporary
tables are registered for cleanup and every fetched stream is bounded by
`dsomop.max_memory_rows`. An extra row is a non-skippable `validation_error`
with reason `reducer_row_cap`, never a partial result. Execution cleanup
preserves caller-owned tables. Compiler failures close both context owners;
successful compilation keeps the immutable roster registry for publication.
Both compiler and operation-table cleanup are attempted when a DROP fails.

## Population and variable interfaces

`.planCompileOutputs(cctx, out_symbols, populations = NULL)` compiles roots
and their private output dependencies in topological order. An optional
`populations` mapping supplies `list(roster, coverage)` records for tests.
Normal execution calls `.compilePopulationByName(cctx, population)`.

Rosters follow the WS-5/A-20 contract in
`ws5-interval-anchor-coverage.md`: explicit population namespace,
`cohort_row_id`, person lineage, lossless origin and visit metadata and the
selected `op_component_id`, associated with the original coverage object.
Candidate and final row keys are never interchangeable.

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
Available empty windows give the reducer's documented empty value; `all`
over an empty window is NA. Any/all preserve UNKNOWN predicate values.
Coverage uses the selected recorded/effective relation explicitly. Observation
eligibility envelopes never replace actual recorded observed segments.

## Publication and documentation

`.compileOutput(ctx, node, inputs)` returns a validated `CompiledOutput` with
one candidate per independently accessible component. `inputs` supplies the
population, private compiled dependencies, requested symbol and execution
mode. No support gate runs inside the output compiler.

Episode components declare roster lineage on `(person_id, cohort_row_id)`;
person components declare direct lineage. Dictionary, time, covariate and
transition reference components have explicit reference lineage. References
attached to patient families publish only with that family. Joined inputs
stay private unless separately selected as roots. Publication gates each
patient component on its own rows in both memory and staged execution.

WS-10 should document native survival's explicit censor list, last-observed
day endpoints, and the v1 adapter's retained cohort-end cap and opt-in death
censoring. It should also distinguish dense panel missingness from sparse
omission, recorded coverage from stockpiled effective supply, and structural
zero from unavailable/undefined NA. Staged native output presently uses the
same bounded private materialization as memory output before publication;
staging does not remove the reducer memory cap.

The executable tests cover reducers, each output format, private publication
families, lifecycle failures and WS-0 acceptance rows. Public client recipe
acceptance must additionally run with the real WS-6 population compiler and
the WS-8/WS-9 client APIs; roster-double results alone do not establish that
integration gate. The final job report records the exact tested commits and
results, including any outstanding integration gate.
