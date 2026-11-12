# Interval, anchor and coverage compiler contract

This is the WS-5 handoff for the endorsed design sections 6.4 and 6.6–6.14,
as amended by A-17 and A-20. It describes private compiler fragments, not a
new publication API. Fragment names below illustrate structured identities;
consumers use the context allocator and the returned fragment names.

## Intervals and course ends

`.compileInterval(ctx, node, event_fragment)` returns `list(course, recorded,
effective)`. Recorded and effective relations describe closed day intervals;
the course is an event-shaped envelope. Effective coverage exists only for a
stockpiled interval and is selected explicitly with `effective_supply`.

The three end quantities must remain distinct:

| Quantity | Meaning |
|---|---|
| Original `end_recorded` | The source event's recorded end role, before the era's end/fallback override. |
| Normalised recorded end | The era's own `end`/`fallback` applied to the original roles; `end_date` of each recorded interval. |
| Unextended course end | The maximum effective end for a stockpiled course, or maximum normalised recorded end otherwise, before persistence. |

Course `end_date` is the maximum effective end plus persistence for a
stockpiled source, or maximum normalised recorded end plus persistence
otherwise. Course `end_recorded` is always the maximum normalised recorded
end. The unextended course end is `end_date - persistence`; it is not another
column. The original source role and the course column named `end_recorded`
therefore have different, explicit meanings.

`min_days` measures the inclusive normalised recorded span from the earliest
start to the latest normalised recorded end of the course. It excludes both
persistence and stockpile carry. Rejected courses leave all three relations.
For overlapping 30-day fills on days 1 and 15, the recorded span is 44 days
and effective coverage is 60 days: `min_days = 50` retains no course or fill,
even when persistence is positive.

Course grouping uses a running maximum of previous ends, including
persistence, so nesting and adjacency are handled correctly. Stockpile carry
uses the previous row's effective end, never a running maximum:

```
carry[i] = min(cap, max(0, effective_end[i - 1] - start[i] + 1))
effective_end[i] = start[i] + supply[i] - 1 + carry[i]
```

Whole associated histories are processed before applying consumer windows,
preserving pre-window carry-in. Supply precedence is positive integer supply,
then valid original recorded length, then one day. The carry cap discards
excess stock at each fill; it does not shorten earlier effective intervals.
The normalised recorded end travels separately through effective
materialisation and course marks. A retained-course flag in the marks avoids
reading the stockpile temporary table again through a course-existence join.
On MySQL, the effective-source CTE uses `DISTINCT` over its already-unique
rows to prevent merging into separate consumer branches. This allows one
statement to reuse the course, recorded and effective relations while reading
the physical stockpile table once.

## Source quality and consumer association

Missing starts, ends before their starts, and present invalid supplies used
as the only end source contribute no coverage, course or supply days. A NULL
supply is absent, so it can fall back to a one-day record. A usable recorded
end, explicit point end, or `fallback = "none"` is a different case. Direct
event coverage applies the same policy using that event's declared end policy
and original roles. Source events retain those roles so an era can apply its
own end/fallback override.

`supply_days` sums valid supply per fill; missing or invalid supply contributes
zero. `fills = "clipped"` includes the supplied portion of pre-window fills
and sums overlaps rather than unioning them. `covered_days` measures a union.
Missing concepts cannot form concept or ingredient groups; `by = "event"`
still admits otherwise valid rows. A present concept without an ingredient
mapping instead follows the declared `unmapped` policy.

The unreduced episode-member adapter keeps one row per episode–member link,
including `(parent_source_ns, parent_event_id)` and physical event identity.
Deduplication happens for the consumer's association, before era normalisation:

- A person-indexed consumer uses each physical event once per person, even
  when that event is linked to several episodes.
- An episode-indexed consumer restricts members to the roster row's parent
  namespace/key and the same person before course, stockpile and carry-in
  computation. Its courses are keyed per parent. Shared members contribute
  once to each associated episode, and other episodes' fills never carry in.

`.consumerAssociation()` resolves the association for anchors and streams,
including supply streams and interval consumers. Index-visit events join
`source.visit_id = roster.index_visit_id`; NULL visits match no source row.
Intervals over these sources partition normalisation, course formation and
stockpile carry by visit before that join, preserving visit identity in the
course key. A cost adapter inherits its parent event's visit restriction;
visit and parent restrictions apply together when both are required.
Episode-member consumers join both parent identity columns and person.
Missing required roster metadata fails closed. Stream sharing includes the
source, coverage relation, observation component/domain, window and consumer
association; different visits or parents cannot share a broadened stream.

## Roster, anchor and coverage handoff

Candidates carry `cand_key` and explicit `attr(f, "population")`; final
rosters carry `cohort_row_id` and `roster_namespace`. Both carry their person
lineage, applicable dates, `index_visit_id`, lossless `index_source` and
`index_key`, and the selected `op_component_id`. Population names are never
recovered by stripping prefixes from allocated fragment identifiers.

Coverage is compiled once per `(population, suffix)` on candidates. Population
derivation and matching transfer the selected component into final
`op_component_id` together with person and origin mapping. Final observed
segments use that original coverage object's recorded relation. Recompiling
coverage with the same identity against a different final roster is refused;
candidate row keys must not be joined directly to renumbered final row keys.
A component id belongs to its originating coverage policy/request.

Matching templates instantiate one candidates relation for the complete case
roster, with a private key per `(case row, person)` and index date equal to
the case time. They use the template population's coverage and anchor
identities. A child without its own index projects its parent's final row key
as its candidate key.

Eligibility envelopes and actual observed segments are separate. Union
coverage may merge periods separated by the allowed gap, but the index must
lie in an actual recorded period. Matching and outcome ascertainment read
the selected component's union of recorded periods; tolerated gaps supply no
observed or denominator days. Invalid observation periods cover nothing.
Observed-segment tuples `(row_key, start_date, end_date)` are distinct even
when source records have duplicate dates.

Consumers resolve anchors through `.anchorExpr()` and windows through
`.compileWindow()`, including the returned dependencies and joins.
`.compileAnchor()` accepts source anchors only. Candidate and final source
anchors have separate identities. An absent required anchor makes a window
undefined: criteria remain UNKNOWN, including absence/count-zero criteria,
and reducers return NA. The IR anchor inventory must include named output
anchors, nested search anchors and matching-template anchors; undeclared
identities remain refused.

## Reducer results and execution lifetime

WS-7 registers its stream/bound identity families with
`.fragmentIdentityGenerators()` and passes allocated `names =` to
`.intervalReducerValues()`. There is no context-free naming fallback.

| State | Result |
|---|---|
| Source or selected interval relation unavailable | Typed `NA_integer_` for every roster row. |
| Available source, defined window, no qualifying events | Zero. |
| Undefined required window | NA. |
| Kept row without an observation component | NA for `observed_days` and `covered_days(observed_only = TRUE)`; finite non-observation coverage and supply consumers still operate. |

Every private fetch inside these helpers is bounded by
`dsomop.max_memory_rows`: roster bounds, observed segments, covered keys,
source streams and stockpile rows fetch at most `cap + 1` and reject if the
extra row arrives. An optional stockpile pre-count does not replace the
bounded realised fetch. Rows are never silently truncated.

Cap refusals use `code = "validation_error"` and reason `stockpile_row_cap`
or `reducer_row_cap`. Alongside `adapter_person_mismatch` and
`cost_currency_mixed`, these form the closed server/client reason vocabulary.
Status transport contains only the constant code/message and validated reason;
raw rows, counts, field paths and attached condition state remain private.
Both client `on_small_site` policies treat these refusals as errors.

Compiler cleanup is registered before native compilation/materialisation and
runs through `.planContextClose()` on success, refusal and error, including a
child compile context. It preserves pre-existing session objects.
`.compileReleaseTempTables()` clears ownership only after successful release,
retains failed names for retry and surfaces aggregated cleanup failures.
Only publication candidates passing the final publication contract may be
released; these fragments and temporary tables remain private.

The live MySQL 8.4 regression must exercise positive `min_days`, each of the
course/recorded/effective relations, a downstream consumer reusing them and
cleanup. Every generated statement reads a physical stockpile temporary
table at most once. MariaDB execution and SQL snapshots do not replace that
MySQL evidence. On MySQL, `.buildStatement()` reads each bounded stockpile
table once into a statement-local JSON payload and exposes typed rows inside
a lateral query. This avoids MySQL's repeated table resolution through shared
CTEs. The payload stays in the database, preserves BIGINT identities and NULLs,
and introduces no persistent objects or data-owner settings. Final result
ordering is preserved outside the lateral query. MariaDB keeps its existing
CTE path. Consumers use the ordinary fragment builder; they do not construct
this transport themselves.

Native end-to-end publication and the integrated v1 result
reconciliation remain separate WS-6/WS-7 and WS-2c integration checks; WS-5's
stream-level acceptance harness alone does not establish them.
