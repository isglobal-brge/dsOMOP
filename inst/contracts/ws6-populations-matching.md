# Population and matching compiler contract

This is the WS-6 handoff to WS-7 for design sections 2.5, 2.8, 6.4, 6.9,
6.10 and 6.13, as amended by A-1, A-11, A-17 and A-20. It describes private
compiler interfaces. It does not enable native output execution or certify
the integrated release. The WS-5 interval/anchor/coverage contract remains
applicable.

## Entry points and returned fragments

After source compilation, call `.planCompilePopulations(cctx, outputs)` for
the requested output names. It follows `.irOrder(ir)$populations` over the
output closures and compiles dependencies before consumers. A controls
template is instantiated by its matching node, not evaluated first as an
ordinary person roster. `.compilePopulationByName(ctx, name)` is the
memoised entry point for an individual population; it enables operation-local
observation snapshots before compiling dependencies.

The frozen compiler signature remains `.compilePopulation(ctx, node, deps)`.
The result contains:

| Field | Contract |
|---|---|
| `candidates` | The population's candidate fragment; NULL for a set operation. A match returns its controls-template candidates, whose population is the template. |
| `roster` | Final immutable episode fragment, even for a population of persons with NULL episode dates. |
| `coverage` | Original candidate coverage object, or a derived object for set operations/matching; NULL for a final unindexed person roster. |
| `segments` | Final observed-segment fragment with person lineage and distinct `(row_key, start_date, end_date)` tuples; NULL without indexed coverage. |
| `kind`, `indexed` | Population kind and A-11 indexed-status rule. |
| `pairs` | Matching only: private eligible-pair fragment. Never publish it. |

Use returned fragment names and `.fragmentName(..., ctx = ctx)` for declared
identities. Do not construct SQL identifiers from population names or recover
population names from allocated identifiers.

## Row identity and original association

Candidate fragments name their population with `attr(f, "population")` and
carry `cand_key`. Final rosters carry `cohort_row_id` and
`roster_namespace = <population id>`. Person lineage names `subject_id`.
Join downstream episode fragments on person plus row key within one roster
namespace; a row number from another population is not interchangeable.

Every final roster carries:

```
cohort_row_id, subject_id, cohort_start_date, cohort_end_date,
index_source, index_key, index_visit_id, origin_source, origin_key,
set_id, is_case, sampling_weight, op_start, op_end, op_component_id,
source_rank, key_1, key_2, key_3
```

The immutable identity is `(index_source, subject_id, cohort_start_date,
cohort_end_date, index_key)`. `index_key` is lossless text: a decimal event
key, the complete adapter key, the returned interval `era_key`, the
results-cohort tuple key, or a serialised case identity plus matched role.
Preserve these strings and BIGINT person/row keys without conversion through
floating point. An interval's key includes its visit association where the
course is visit-qualified; `era_no` alone is insufficient.

`source_rank` and `key_1..key_3` are private typed identity components used
for deterministic order and set algebra. `.rankedRosterSql()` assigns row
numbers once in chronological order: person, start, end, then typed source
and key components, with NULLs last. These columns are internal metadata,
not output variables or a replacement for the declared immutable identity.

`origin_source` and `origin_key` retain the original consumer association
separately from that identity. Ordinary indices carry their source/key;
adapter indices carry their parent source/key. Derivations preserve them.
Unindexed person rows, including final cohort-seeded person rows, carry the
private `origin_source = "(persons)"` marker. Their public `index_source`
and `index_key` remain NULL. A missing required episode origin still fails
closed; it does not imply a person association.
Matched cases retain them and their original `index_visit_id`, although their
new `index_source` is `match:<population>`. Controls have no index-event
visit or parent association. Preserve both origin columns together; do not
overwrite them with the new matched identity.

`set_id` and `cohort_row_id` are reviewed analytic keys under A-E10 and survive
identifier handling. `set_id` identifies a matched set, never a genuine
person. Every matched member carries its own `subject_id`; publication
support is distinct persons of the component's own rows.

## Immutable private snapshots and lifecycle

Raw candidates and final rosters are materialised once in context-owned
session tables through `.populationFreezeFragment()`. Matching also freezes
its case identities and strata before fetching cases or pairs. The native
population path freezes the valid observation-period source once for the
operation, so transferred component numbers and recorded segments remain
consistent when source observation rows change between statements. An
already compiled unfrozen observation source is promoted before native
population coverage is materialised.

These are operation-local materialisations of the specified relations. When
an enclosing request snapshot is active, its source certification and read
snapshot also cover these relations. The native population materialisation
stays in the database. Inside an active MySQL or MariaDB request snapshot,
ordinary consistent SELECT results instead pass privately through R into a
typed session table: ALTER TABLE can end the transaction, and INSERT ... SELECT
can read newer committed rows. This fallback holds the complete relation in R
and therefore requires memory proportional to its size. Neither population
path counts rows for admission, truncates results or imposes the R memory-row
cap. Explicit bounded matching realisations retain their separate bounds.

Snapshot creation preserves the complete fragment contract and attributes,
then replaces its SELECT with a read of the private table. On MySQL the
private `mysql_stockpile_source` attribute is also used for population and
observation snapshots. Preserve this attribute when passing fragments to
`.buildStatement()`; rebuilding a fragment from only its frozen record fields
loses the transport needed to read a physical temporary table once when
several consumers reuse it. MariaDB uses its ordinary temporary-table path.

Names are owned before CREATE. Creation/insertion failure attempts immediate
cleanup; failed drops remain registered for retry. WS-7 must retain native
context cleanup on success, refusal and error, including supplied contexts.
Use `.planContextClose()` for the execution lifecycle and
`.compileReleaseTempTables()` for standalone compiler contexts. None of these
snapshots are persistent server state.

## Candidate semantics, parents and set operations

An index population applies offsets before coverage. It then derives ends,
resolves candidate anchors and evaluates the complete criterion tree. Only
TRUE survives; missing anchors remain UNKNOWN even for absence predicates.
`first`, `last` and `nth` choose before criteria; `first_eligible` chooses
the earliest candidate passing the complete criteria and end check. `all`
keeps every eligible candidate. Finite ends are capped at `op_end` when
covered; a kept uncovered index has NULL observation bounds and can retain
its finite/event end. End-before-start candidates are ineligible.

A child without its own index copies the parent's final row key to
`cand_key`, inherits its episodes and applies its own criteria. A child with
its own index restricts its candidates to the parent's persons. Set-operation
parents restrict persons after the operation. `intersect` and `setdiff`
preserve the left member's episodes; `union` deduplicates full identical
origins only. Contradictory coverage, association or matching metadata for
one identity refuses with `setop_identity_conflict` rather than choosing a
member. Effective coverage compatibility is checked statically through
nested set operations and matches.

The start-date column's `nullable` contract is independent of A-11's
`indexed` flag. A mixed union is indexed and can retain undated person
rows: union combines member nullability, intersection/difference inherit
the left contract, and child candidate/selected/final fragments preserve it.

A-21's closed `legacy_empty = TRUE` criteria variant with
`legacy_episodes = "observation_periods"` compiles to typed empty candidate,
selected and final-roster relations, with NULL coverage and segments. Its
parent, criteria and cohort seed must be absent or NULL, and a named-cohort
compatibility reference is forbidden. The lowerer keeps an ignored episode
policy in its advisory annex. Other unsupported compatibility variants
remain refused by the native compiler; this property is still IR-only.

## Coverage and the candidate-to-final handoff

Compile coverage once per `(population, suffix)` on its raw candidates.
Criterion-specific `observed(gap = ...)` requests have separate suffixes.
The selected `op_component_id`, `op_start` and `op_end` travel with person
and origin metadata through selection, parenting, set operations and
matching. Final row numbers can differ from candidate keys.

WS-7 consumes `result$coverage` with `result$roster` and phase `"final"`, or
the returned `result$segments`. Do not compile the same coverage identity
again for the final roster or join its candidate `row_key` directly to
`cohort_row_id`. `.compileObservedSegments()` joins the final roster's person
and transferred component to the original coverage object's recorded
relation. Component numbers belong to their originating policy/request;
they are not interchangeable across different coverage policies.

An eligibility envelope may bridge tolerated gaps. Actual observed days,
denominators and outcome ascertainment use the union of recorded segments
of the selected component, never the envelope. The index itself must lie in
an actual recorded observation period. A kept uncovered row has no component:
observation-dependent reducers return NA, while defined finite
non-observation windows can still be evaluated.

## Criteria, anchors and streams use one association

Use `.consumerAssociation()`, `.consumerRelation()` and `.consumerJoinSql()`
for event and interval consumers. Population `has` criteria now use these
same helpers. They bind an index-visit source to `index_visit_id`, and an
episode-member consumer to the retained parent origin and person before
interval construction. Namespace comparison is byte-exact on MySQL/MariaDB.
Missing required metadata fails closed; a NULL index visit matches nothing.

Person-associated adapter consumers deduplicate physical events across
parent links before counting or ranking. Episode-associated consumers retain
their own parent restrictions. Interval consumers select the association's
course or recorded/effective relation; never substitute an unqualified
person-level interval relation for a parent/visit consumer.

Resolve anchors through `.anchorExpr()` and windows through
`.compileWindow()`, carrying their returned joins/dependencies. Source
anchors used by criteria and final outputs have different candidate/final
identities and compile in each phase. Matching strata windows also support
source anchors and nested search windows: the index context is the case time,
with each anchor resolved for that side's person. Their definitions belong to
the cases population's final inventory and the controls template's candidate
inventory (A-20(b)); missing anchors yield UNKNOWN strata and follow
`missing_strata`. WS-7 still owns declaring its bounds
and stream identities and passing allocated `names =` to reducer helpers.

## Exact matching and output responsibilities

Only rows with a defined start date enter the frozen case relation, before
case ranks or pairs are assigned. Mixed unions retain their person rows in
the source roster. The controls template is instantiated once for the complete
frozen dated-case roster, with `cand_key` per `(case row, person)` and index equal to case time.
The template's own population metadata, coverage and candidate-anchor
identities apply. Its parent or results-cohort seed narrows candidate persons.
Instantiated raw candidates carry private `origin_source = "(persons)"`,
preserved through coverage and criteria stages. The association resolver
recognises controls templates before following their IR parent: criteria,
source anchors, adapter intervals and strata all evaluate at person scope,
deduplicating physical members shared by episodes. Ordinary episode rosters
retain parent scoping and fail closed on missing required provenance.
One template cannot also be an ordinary roster or serve a second matching
node in the same plan.

Matching strata use the complete declared window, including source anchors
and nested search windows (A-20(b)). An omitted anchor remains case-time
relative. Source anchors have separate identities on the cases' final
roster and the controls template's candidate roster; both sides resolve
against their own person and row key. Template-only criterion anchors do
not become case-side anchors. Undefined anchors retain UNKNOWN strata and
the requested `missing_strata` policy. Built-in and unbounded windows keep
their ordinary semantics.

Eligibility requires actual recorded coverage at case time, survival to that
time, the declared outcome-history and strata rules, and TRUE template
criteria. An outcome exactly at case time excludes a control even under
`prior_outcome = "allow"`. The complete eligible-pair relation is fetched
once with `cap + 1`, where the cap is the lower of `max_candidates` and
`dsomop.max_memory_rows`. An extra row refuses with
`match_candidates_exceeded` before selection; there is no preliminary pool
COUNT and no truncated pool. The separate case realisation is bounded too.

Ordering uses the published `dsomop-match-v1` typed length-prefixed encoding
of seed, full immutable case identity and control person key, followed by
SHA-256 and a numeric person-key tie break. Encoding and digest vectors are
shipped in `tests/testthat/fixtures/match-vectors.json`; WS-7 must not hash
only dates, row numbers or `set_id`. Cases with no pair rows are still
visited. Under `replacement = "none"`, a dropped insufficient case commits
no control allocation; its tentative controls remain available later.

The matched roster has index/start/end equal to case time, one new identity
per case/control membership, `set_id`, `is_case` and `sampling_weight`.
`set_id` is the case's rank in the complete frozen case roster; dropping a
case leaves a gap rather than renumbering the retained sets.
Cases have weight 1. Controls under `between_sets` have `N_c / m_c`, using
the complete eligible pool and selected count; controls under `none` have
NA. A released weight encodes eligible-pool information and can reveal
`N_c` when `m_c` is known. It is assigned data subject to final-support and
downstream DataSHIELD controls, not a hidden or suppressed statistic.

Keep candidate relations, pairs, strata, unused cases and internal origin
metadata private. Build final patient publication candidates from the actual
output rows and their genuine-person lineage. Small private populations or
zero-control cases are not intermediate disclosure refusals; the final
publication contract decides whether an output can be released.
