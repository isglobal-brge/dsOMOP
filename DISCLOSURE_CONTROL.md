# Disclosure control in 2.10.4

This is the recipe-language-2 release contract (design §§7.1–7.8 and approved
amendments, including A-23). Native execution remains subject to the installed
capabilities during integration. Core plans, recipes and verbs use the
DataSHIELD publication rule below; explicitly prepared DP outputs additionally
use authenticated person-local provenance and sealing.

## The release boundary

Private populations, candidate rosters, anchors, event streams, matching pairs,
cohort handles and computed inputs may be small or empty. Only publication
roots leave the private execution context. `publish` names those roots; absent
it, all declared outputs are roots. A partial execution `out` mapping without
an explicit `publish` selects only the mapped roots. An explicitly empty
publication set is a real no-root execution, validated before it returns.

Every independently accessible patient component of a root must represent at
least `nfilter.subset` distinct genuine persons. Count the rows of the actual
materialisation once, not a separate count query or an unrelated roster.
Repeated episodes of one person count once. Empty patient components have
support zero. Every patient row must resolve to exactly one nonmissing person,
either directly or through a declared immutable, origin-qualified roster key.
Orphans, ambiguous mappings, conflicting direct identities and wrong namespaces
are structural `lineage_malformed` failures; `set_id` alone is never lineage.

Plan/vocabulary-only reference objects (dictionary, covariate/time/transition
references, explain) do not need support. Reference companions of a patient
family publish only with that passing family. Aggregate previews are not
reference objects and keep their own controls.

The rule covers assigned frames, every logical staged component (all its chunks
together), and any patient-attributable frame returned by a server call. It
also covers standalone filter, select, merge, bind, mutation, cohort-from-table
and analysis assignment routes. An internal helper is not a release bypass.
A root with any small patient component is refused with the constant message:

> Disclosive: operation blocked - insufficient individuals to meet disclosure threshold. No further details available.

Support counts, candidate counts and private strata are never returned. Audit
records retain pass/fail, not the count.

## Atomic publication and federation

The publisher materialises privately, validates lineage and support, removes
private columns, tokenises identifiers and performs any DP sealing before any
assignment. It reserves fresh destinations against every occupied name and
returns the actual dynamic inventory. Existing objects are not overwritten,
except an explicitly authorised in-place verb target with a rollback copy.

A staged family commits by atomic manifest replacement after all its files are
complete. `omopStagedDatasetPath()` checks descriptor, permissions, semantic
and bundle contracts, expiry and exact committed manifest membership. A private,
uncommitted or revoked component cannot be read. Revocation removes descriptors
and data; federation rollback removes both. Errors during tokenisation, sealing,
assignment, chunk writing, moving or manifest replacement revoke all owned
artefacts. Cleanup is verified and failure is a technical error.

`omopPlanExecuteDS` is an assign method. Its bounded status is stored in the
execution symbol, read through the narrow aggregate `omopPlanStatusDS`, then
that symbol is deleted. The status contains `ok`, `partial` or `refused`, the
published component inventory, typed rejected roots and unavailable output
names. No support or patient values occur in it. A failed status read triggers
cleanup. Approved adapter reasons (for example `adapter_person_mismatch`,
`cost_currency_mixed`, `stockpile_row_cap`, `reducer_row_cap`) are bounded
`validation_error` reasons, not support failures.

| Server mode | `on_small_site = "error"` (default) | `on_small_site = "skip"` |
|---|---|---|
| Strict (default) | A small root refuses that site. Client rolls back all execution-owned artefacts across the federation and raises an error. | Failing site commits nothing (`all_or_none`); client keeps passing sites and returns NULL for the skipped site. |
| Non-strict | Passing complete families commit; status is `partial`. Client returns them with a warning naming rejected families and codes. | Any small family prevents the entire site's commit, even for otherwise passing families; client returns NULL for that site. |

The client passes its choice as a commit policy before publication, not as a
request to undo an otherwise successful partial commit. Only
`insufficient_support` is skippable. Validation, resource, dialect, execution,
transport and cleanup failures are errors with federation rollback under both
settings. Factor harmonisation uses retained sites and published components.

## Column classes: assignment and aggregates are different policies

| Class | Examples | Assignment | Aggregate endpoints |
|---|---|---|---|
| 1: blocked | Direct identifiers, untyped extension identifiers, `*_source_value`, `value_as_string`, `value_source_value`, `sig`, `stop_reason`, note text/title/NLP, narrative fields, addresses/geo, provider/care-site identifying fields, lot/device identifiers; `*_source_concept_id` by default | Refused by the shared assignment predicate. The existing `block_sensitive = FALSE` path additionally needs the administrator's `dsomop.allow_sensitive_columns`. Renaming or declaring a categorical type cannot evade it. | Existing blocking/admission rules remain. |
| 2: quasi-identifiers | Birth year/month/day/datetime, `dob`/birth-date extensions, exact age, birth-year bands of width ≥1, exact dates under the absolute-date policy | Projected when requested; support is the only disclosure gate. Explain/lint reports INFO `quasi_identifier`. | Existing admission, grid, coarsening, suppression and banding remain. |
| 3: ordinary | Other reviewed columns | Ordinary publication policy. | Existing endpoint policy. |

Assignment classification is separate from the blueprint's aggregate
`is_sensitive`/`is_blocked` flags. The common corpus `inst/column_classes.csv`
pins both policies side by side. Profiling still refuses birth fields, exact
birth fields remain BLOCKED in query-library SQL, external aggregate contracts
and the OHDSI results registry retain their checks, and Achilles age grouping
keeps its deciles and suppression. `ds.summary` on an assigned object remains
subject to dsBase's controls.

Assigned age groups use a pure banding helper: an increasing integer grid with
at least two lower bounds starting at zero is valid. There is no width floor,
server-grid restriction or per-band suppression. A qualifying assigned object
may therefore retain a one-person `18-18` band for `c(0, 18, 19, 65)`. Age is
annual reference-year minus birth-year, not a fabricated birthday. Absolute
dates still require the existing absolute-date policy. Birth fields may appear
in person predicates; unrestricted text predicates remain forbidden.

## Standalone verbs and operational caps

The verb result is a new release, checked on its own distinct-person support.
Filtering keeps TRUE rows only; UNKNOWN is not TRUE. No intermediate population,
30-day window, five-year age-range, filter-level cardinality or density gate
can reject a qualifying final assignment. Class-1 blocking, protected keys,
date policy, lineage and resource constraints still apply.

Mutation accepts only the closed `mutate-v1.json` AST: references, typed
literals, arithmetic, predicates, `ifelse`, `case_when`, `coalesce`, public
`cut` breaks and supported date arithmetic. It never evaluates analyst R.
Assignments may depend on earlier assignments. Neither reads nor writes of
person/episode/set keys or `dsomop_protected` columns are allowed; class-1
references and new class-1 names are refused even with the sensitive-column
override. Integer/numeric branches unify without silently narrowing integer64
values. UNKNOWN tests yield NA (the first non-FALSE `case_when` test decides);
division by zero and non-finite arithmetic yield typed NA. DP provenance is
invalidated by mutation.

An in-place request authorises only `newobj = x`. The client retains a rollback
copy until federation commit is verified and restores every possibly written
site, including a missing acknowledgement. Restoration must be confirmed;
unresolved restoration is a technical error and the recoverable copy is kept.
A failure disposing of backups after confirmed commit is reported as cleanup
failure with the committed state stated accurately.

Operational caps bound work, not analyst histories or queries. The mutation
schema permits at most 64 assignments. Text literals/labels retain
`nfilter.string`; expression nesting retains `dsomop.max_filter_depth` (default
32). Existing filter-node/value caps (1,024/10,000), feature/pivot caps
(1,000/1,000), output-column cap (5,000), temporal-bin cap (10,000), plan-output
cap (100), live temporary-table cap (256), and memory, staged row/byte and
matching-candidate limits remain applicable. Read current advertised limits;
requests cannot relax them. There are no call quotas, privacy accountants or
per-user/per-session budgets that change answers.

## DP and unchanged aggregate controls

The DP adapter walks the normalized graph. Ordinary person-local sources,
criteria, eras and reducers may be prepared and sealed. Matching taints its
descendants; unsealed external workspace frames and persistent-cohort scopes
are refused with `not_person_local`. Sealing occurs privately before commit;
a sealing failure leaves no publication. A recipe is not differentially
private merely because its final support passes.

Core exploration and every aggregate route keep their own admission,
suppression and banding controls: previews, cutpoints, factor levels, cohort
discovery, profiling, query library, OHDSI results and age histograms. Community
tool policy and explicit typed DP releases keep their existing protection.
Noise is generated deterministically on demand from the secret noise root and
release semantics; no answer store, call counter or privacy budget is created.
The noise root and pseudonymisation key are the only persistent service state.
Private execution files and rollback copies are temporary owned artefacts.
Default identity initialization is automatic; no database TLS/SSL requirement
is introduced.

The analyst observes a pass/fail bit per root, bounded typed structural
refusals, timing, and permitted downstream aggregates. Matching weights can
encode pool-derived quantities. Differencing between two releasable objects
remains possible, as with DataSHIELD subsets; this contract does not claim to
prevent it. These limitations do not relax downstream packages' controls.
