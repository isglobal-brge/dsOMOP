# Disclosure control in 2.10.4

This is the recipe-language-2 release contract (design §§7.1–7.8 and approved
amendments, including A-23). Native execution is subject to the installed
capabilities at every site. Core plans, recipes and verbs use the
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
| 2: quasi-identifiers | Birth year/month/day/datetime, `dob`/birth-date extensions, exact age, birth-year bands of width ≥1, exact dates and native date-derived forms | Projected when requested; support is the only disclosure gate. Explain/lint reports INFO `quasi_identifier`. | Existing admission, grid, coarsening, suppression and banding remain. |
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
annual reference-year minus birth-year, not a fabricated birthday. Exact
date assignment needs no absolute-date permission; all date-derived forms
retain class-2 provenance and aggregate controls. Birth fields may appear
in person predicates; unrestricted text predicates remain forbidden.

## Standalone verbs and operational caps

The verb result is a new release, checked on its own distinct-person support.
Filtering keeps TRUE rows only; UNKNOWN is not TRUE. No intermediate population,
30-day window, five-year age-range, filter-level cardinality or density gate
can reject a qualifying final assignment. Class-1 blocking, protected keys,
date semantics, lineage and resource constraints still apply.

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

## DP preparation and aggregate controls

The DP adapter walks the normalized graph. Ordinary person-local sources,
criteria, eras and reducers may be prepared and sealed. Matching taints its
descendants; unsealed external workspace frames and persistent-cohort scopes
are refused with `not_person_local`. Sealing occurs privately before commit;
a sealing failure leaves no publication. A recipe is not differentially
private merely because its final support passes.

Core exploration and every aggregate route keep their own admission,
suppression and banding controls: previews, cutpoints, factor levels, cohort
discovery, profiling, query library, OHDSI results and age histograms. Community
tools and typed releases use the repaired 2.10.3 protected-component contract
described below; the recipe publication rule never relaxes those controls.
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


# Disclosure control of the OHDSI community tools

dsOMOP's differential-privacy machinery concerns only the integrated OHDSI
community tools: Achilles results, OHDSI results tables, the analysis catalog,
the query library and the typed release channel. dsOMOP's core (connecting,
loading tables through plans and recipes, filters, concept factors, merges and
row binding, cohorts and dsOMOP's own exploration statistics) works under the
standard DataSHIELD disclosure controls (`nfilter.*`) only and never depends on
the protected-release mechanism. Core plan outputs carry verified DP provenance
only after the approved preparation path (`omopDpPlanExecuteDS`, which seals
person-local plan outputs); ordinary loading (`omopPlanExecuteDS`, recipes)
does not.

The community tools are enabled by default (`dsomop.tools.enabled`, profile
fallback `default.dsomop.tools.enabled = TRUE`); custodians can disable them
with `dsomop.tools.enabled = FALSE`. The DP service is enabled by default since
dsOMOP 2.6.0; custodians can opt out with `options(dsomop.dp.enabled = FALSE)`
or `DSOMOP_DP_ENABLED=0`. `dsomop.tools.mode` selects how the tools protect
their outputs (`"auto"` by default, `"standard"` or `"dp"`); the custodial
option `dsomop.dp.exclusive = TRUE` with DP enabled selects `"dp"`. Analysts
cannot set these policies or bypass the noise through a client option.

## Protected releases: guarantees and limits (2.10.3)

dsOMOP's protected OHDSI-tool releases use deterministic disclosure controls
without a privacy accountant, total-epsilon budget, query cap, rate limit, or
admission state tied to the number of questions asked. Repeating an equivalent
supported question, or changing a parameter within its documented public snap
interval, reuses the same protected components. Canonical component ownership
covers automatic and typed aliases. Output metadata identifies the effective
population, grid, clipping bounds, contribution policy and approximation.

There are two protected routes. A fixed-synopsis route derives all supported
views from one calibrated public atom basis for an unchanged source state.
The automatic membership generation below is not a detector of every edit. Unlimited post-processing of that basis does not add a new raw-data
release. A flexible-scope route uses secret, deterministic fingerprints of
realised bounded contributions to prevent different formulations of the same
component from drawing fresh noise. That second route is a heuristic disclosure
control: its data-dependent correlations are not a general
differential-privacy guarantee.

These controls do not guarantee protection against all adaptive queries.
Distinct overlapping populations, supported partition recombinations outside
one common synopsis, auxiliary knowledge, model outputs and other permitted
exact methods can still enable inference or averaging. Contributor-based
sharing can reveal equality relationships between selected sets; a two-query
transcript need not be epsilon-DP even if each isolated noise marginal is
epsilon-calibrated. Exact support/model-admission rules also reveal threshold
information. The reported noise calibration and bounded-vector mechanism
epsilon are not a database-wide, cumulative or lifetime guarantee. No finite
transcript epsilon is reported for the service.

The default reference calibration is 0.1, with fixed component allocations; it
is not 0.1 independently per cell. A custodian can choose another deployment
calibration, accepting its noise/utility tradeoff. Lower noise does not cure
unlimited adaptive inference. Public snapping, clipping and contribution limits
change precision or the released estimand; small supported populations may
have little useful moment/rate precision. No precision or availability changes
according to the number of past questions.

Changes to source data, secrets, grids, scales or mechanism versions can create
additional disclosures. They do not erase past answers or reset privacy
history. Final aggregate requests pin one physical connection for generation,
support and component reads. PostgreSQL uses REPEATABLE READ (temporary-table
DDL remains permitted), MySQL and MariaDB use consistent-snapshot transactions
for InnoDB base tables, and DuckDB and SQLite use explicit transactions.
Nontransactional MySQL/MariaDB source base tables fail closed, since that
transaction cannot make their concurrent reads coherent. Requests roll back on exit;
losing the snapshot fails the whole request, rather than reconnecting one
SELECT. SQL Server and Oracle have SERIALIZABLE adapters; Redshift uses its
native transaction. These three adapters have SQL-shape tests but no live vendor
execution here. Engines without a native adapter report `not_isolated`: their
connection is pinned, but concurrent writes can produce a mixed-state response.
This is request-local state only and needs no database option.
Private contributor hashes, person keys, the membership generation and hidden
support are never output. Audit is observational (ordinary server log messages)
and is never consulted for admission.

### The automatic data generation of the fixed synopses

Fixed-synopsis requests derive a secret person-membership generation inside
each request's database snapshot. The digest covers the canonical ordered
person identifiers, with unambiguous framing, and is keyed in R under a subkey
of the noise root. Database digest computation never receives the secret key;
engines without a suitable database implementation stream bounded chunks.
Fixed-capable pinned loaders also compute membership inside their preparation
snapshot to authenticate the sealed table's origin. A later fixed typed release
recomputes live membership and requires an origin match; stale prepared tables
use the realised route (or are refused in DP-exclusive mode). There is no
cross-request, per-handle or negative-result generation cache.
Clinical-table COUNT/MAX scans are removed. A transient read failure fails the
current request; it does not switch to a new noisy stream or poison later calls.

Adding, removing or replacing a person identifier changes this generation,
including balanced replacement below the maximum identifier. A person-set
change globally redraws fixed components, including components unrelated to
that person. This is a deliberate, narrower exception to irrelevant-insert
replay. An unrelated clinical insert with unchanged membership does not redraw
an unchanged component. This mechanism requires no epoch bump, triggers, ETL
coordination or owner configuration.

The generation detects membership, **not an immutable CDM snapshot**. Clinical
edits and clinical-only withdrawals leaving the person row can retain the same
generation. Component contribution fingerprints remain in fixed keys: changed
bounded inputs no longer deliberately reuse their old noise. This mitigation
does not conceal component-change patterns or prove a joint DP transcript over
edits; independent discrete draws can coincide, and auxiliary information can
make a change pattern informative. A fixed-vector claim concerns an unchanged
source state, with the stated exact-support qualification. No finite cumulative
or evolving-data guarantee is claimed. Optional `dsomop.dp.privacy_epoch`
rotation is administrative hardening that creates additional disclosures; it
is never a prerequisite for ordinary private operation and erases no prior
answer. Workers need the same corpus identity and secrets.

`snapshot_id` is deprecated display metadata. Changing or clearing its option
has no effect on noise, routes, policy admission or restored sealed-table
validation. Historical signed capsule fields are still authenticated; the
historical display label is not compared with the current label. The actual
domain, epoch, person map and MAC checks remain enforced.

The intended deployment remains DataSHIELD's layered controls and governance:
approved access and uses, custodian-selected methods, output checks and audit.
This revision removes the tested dsOMOP exact-output bypasses and cheap
cosmetic-variant averaging attacks; it does not replace the rest of that
system. Its boundary includes the combined Opal method profile, overlapping
dsBase/core functions and available auxiliary information. Shared
Rock credentials are not distinct analyst identities for privacy accounting or
reliable individual attribution.

### Modes and metadata

- `fixed_synopsis`: `mechanism_epsilon` identifies the declared bounded vector
  (the union of the owned components) and `delta = 0`;
  `guarantee_scope = "fixed_vector_before_exact_support_gate"`;
  `api_transcript_epsilon = NA` because the full service includes exact gates
  and other outputs. `"dp"`/exclusive mode chooses this mechanism family; it is
  not a claim about all enabled Opal methods. A request is a fixed synopsis
  when it is unscoped, its contract declares a public basis, and the automatic
  data generation of the connected database is available (above); the noise
  of each atom is keyed on (corpus, data generation, privacy epoch, owner,
  population, atom) and the atom's contribution fingerprint. The label carries
  `data_generation` (the person-membership mechanism) and
  `data_generation_residual` (what the generation cannot see), never the
  generation value.
- `realised_sticky`: `noise_reference_epsilon` plus actual per-component
  allocations, scales and sensitivities; `mechanism_epsilon = NA`,
  `api_transcript_epsilon = NA`,
  `guarantee_scope = "heuristic_realised_contribution_reuse"`. Every cohort- or
  table-scoped request, and unscoped contracts without a complete public basis,
  take this route in auto mode; DP-only mode refuses it
  with a stable unsupported-mechanism message. Each component's noise is keyed
  on its owner, coordinate and a secret HMAC fingerprint of the sorted
  (person token, bounded contribution) map; request text, sessions, handles,
  scope names, lineage spelling and the data generation never enter it. A
  realised release states no `delta`. Person-time components belong to
  scientific owners (first-outcome status of one outcome definition; observed
  person-time of one time contract), never to subgroup labels, predicates,
  scope names or window spelling: an arm or unstratified population is the
  root of its own realised map, so equal memberships share their components.
- `standard` and `mixed`: explicit per-block protection; no blanket epsilon.
  On the standard route (standard mode, or auto mode without the DP service:
  `standard_fallback`) the demonstrated-disclosive fields are unavailable:
  exact scoped moments and quantiles of per-person distributions, exact
  flexible-window person-time, rates, MDRRs and effect estimates/CI/SE
  (`masked_fields`, `masked_reason = "protected_output_required"` or
  `"protected_mechanism_unavailable"`). Imported precomputed Achilles
  distributions have no person-level basis: in auto mode their summaries are
  unavailable (`"protected_basis_unavailable"`). A mixed frame states no
  frame-level epsilon or delta; its `blocks` name each block's protection.
  Public parameters are safe metadata; hidden support and map fingerprints are
  not.
- Threshold views of one protected histogram (the QueryLibrary long
  observation-period count) are released only from that histogram, in every
  mode; without the DP service they return a stable unavailable row after the
  parent-population admission only, so the presence of a row never re-tests
  the raw tail at the threshold.
- Source: every protected release and label carries
  `source$logical_source_id`, a public opaque identifier derived from the
  public noise-key fingerprint and corpus domain. Two connections to one
  logical node carry the same identifier; dsOMOPClient refuses to pool them.
- Status: `privacy_call_quota = "none"`, `history_based_admission = FALSE`; no
  balance, spent-epsilon, remaining-release or reset field.

Component allocations of the reference calibration e: an eligible-person total
0.2e wherever it occurs (one owner shared by typed and automatic routes). The
public marginal hierarchy: every owner declares its eligible population; the
population's one parent total is shared by all its stratifications, the strata
totals of a public partition (another 0.2e) are reconciled to that signed
parent for pooling and their displayed values are projected onto the
parent's displayed total, so a child request never moves the parent and
recombined children equal it; cells are reconciled to their stratum. Labels
report the union of the owned components. Then: a histogram/partition 0.8e; histogram plus bounded moments 0.4e (cells), 0.2e
(normalized sum) and 0.2e (square sum); bounded moments alone 0.4e/0.4e;
person-time 0.3e (first-outcome status) and 0.5e (observed time, each person
clipped to 3650 days); multi-label or record-capped cells and distinct-label
cardinalities e with the fixed cap K (default 10) as sensitivity. At e = 0.1
the noise standard deviation is about 70.7 people for a person total, 17.7 for
a partition cell and 35.4 for a histogram-and-moments cell; moments of
populations below a few thousand people are therefore imprecise, and their
projection onto the feasible set can be degenerate.

### Protected estimators and summaries

Outputs that 2.10.2 released exactly and that the review showed to be
disclosive are released from protected components instead of being withheld:

- Continuous values (`fe.continuous`, the continuous rows of
  `char.target_covariates`, temporal value distributions) are reduced per
  person within concept × unit: one value per person and stratum (their mean
  in that unit), never averaged across units. NULL and 0 units form one
  explicit unknown stratum; values change unit only through conversions
  declared in the registry. Registered concept × unit scales (built-ins for
  HbA1c %, body weight kg, height cm, BMI, systolic and diastolic blood
  pressure mmHg; custodians add or replace entries through
  `dsomop.release.measurement_scales`) give bounded moments; any other stratum
  gets a coarse public 1-2-5 histogram with mean and SD unavailable. The cap
  of 10 strata per person applies once across all of a person's strata. The
  outputs carry `unit_concept_id` and `unit_name`; labels name the unit and
  scale and say `weighting = "person"`.
- The eight QueryLibrary record summaries retain the first K=10 eligible
  records per person, by event date then stable record identifier, across all
  strata before view selection. N counts contributors and R counts records;
  record means and SDs use R. Registered scales enable bounded S/Q moments.
  Without a scale, the same capped records supply a signed 1-2-5 value histogram
  with explicit tails, reconciled to protected R, with interval quantiles.
  Moments stay unavailable (`public_scale_unavailable`). Displayed R lies in
  [N, K N] for displayed N. Mean+SD allocations are N .2e, R .3e and S/Q
  .3e/.2e; mean-only uses R/S .4e/.4e. Unregistered H uses the moment share.
  Since each record enters either moments or H, their union uses max(S+Q,H).
  Strata are data-enumerated and support-gated: a public description of possible
  concepts/units/years is not a materialized complete basis. These summaries
  therefore use `realised_sticky`, including unscoped requests; DP-exclusive
  mode refuses them. No owner scale configuration is required for the histogram.
- CohortMethod Cox and admitted interaction Cox use a common full-horizon
  exit-bin/status basis with stable identities, shared C6 person/event parents,
  and a 3650-day contribution horizon. Scientifically equal bounded maps replay
  across window spelling and equivalent subgroup labels. Fits are grouped-time
  Breslow estimates on protected cells, not raw continuous-time/Efron fits.
  Complete subgroup families retain final complement admission; Unknown/Other
  remain explicit. Unidentifiable or noise-dominated fits carry an unavailable
  reason. Noise uncertainty is deterministic post-processing, not an R RNG or
  call-history input, and provides approximate, not guaranteed, coverage.
- Cox component pooling preserves the protected event/non-event parents.
  Interaction exports include the complete signed canonical family and its
  public view mapping; the client sums site components, projects the full
  family under its arm/status parents, and then selects the requested view.
  Coarser age views sum the same projected atoms. Public family and view
  identities distinguish supplemental rows; incompatible or incomplete
  metadata refuses pooling. Effects remain inverse-variance meta-analysis
  of per-site protected fits, not a pooled C8 refit.
- Crossed C6 age-by-year cells use separate nonnegative transports for E, NE,
  T and R, preserving both age and year margins and N=E+NE. The bounds
  T<=3650 N and E<=R<=10 E are enforced at the tree levels, not at crossed cells.
  The versioned Dykstra display retains converged results; at its finite
  iteration limit, nonnegative row normalization and column mass transfer
  restore both margins. This fallback is feasible, not an exact L2 minimizer.
  C8 interaction display likewise repairs both margins after finite IPF;
  its repaired output is not claimed to be an exact KL or L2 minimizer.
- Recurrent incidence counts and `char.time_to_event` retain K=10 events or
  occurrences per person. Event-time recurrence caps across all episodes and
  bins before view selection; 1-, 30- and 365-day views share a public signed-day
  hierarchy and explicit tails. `first_occurrence_only=1` remains a distinct
  first-event estimand. Recurrent `persons` counts distinct contributors among
  the same K-capped occurrences, using a separate complete presence vector over
  998 canonical intervals. Each person occupies at most K bins per scale, so
  this vector has joint sensitivity 3K. Static reference-epsilon fractions are
  N .2, R .2, each event level A/B/C .1, and presence P .3 (sum 1); first-event
  allocations remain N .2 and each event level .8/3. Displayed recurrent R lies
  in [N,K N]. The versioned radial presence projection enforces event/person
  bounds, containment and registered partition inequalities. It moves from a
  feasible concave event-mass anchor toward the bounded noisy vector; retreat
  toward that anchor can add bias, and it is not an L2 projection. Every view
  selects canonical presence coordinates: coarse persons are never sums of
  fine-bin persons. This preserves distinct contributors to the capped
  substrate, not uncapped legacy presence in bins excluded by the global cap.
- SCCS uses (person, merged observation-period) strata, a single K=10 cap across
  informative strata, and 36 logit-spaced q points in [0.0001, .99]. Strata with
  q=0/1 are noninformative and excluded before the cap. The signed-cell fit solves
  an estimating equation; it is not an ordinary nonnegative-count likelihood.
  Negative weights can make the score nonmonotone, and the root need not be
  unbiased. No-root, multiple admissible root and nonpositive-information cases are refused;
  unresolved numerical root certification is also unavailable.
  Gridding changes the estimand: the homogeneous pseudo-true log-IRR shift is
  logit(q)-logit(q_grid). The earlier <=.04 statement described selected scenarios,
  not a bound: q=.1796156 to .152 gives about +.200, and q=.999 to .99 gives
  about +2.312. Endpoint clipping can therefore dominate scientific error.
  Merged-period strata differ from the previous longest-period person fit.

- In standard mode with the DP service, the masked person-time, rate, MDRR and
  effect fields of incidence and CohortMethod frames are filled from the same
  protected run as auto mode (a `mixed` label with per-block protection);
  without the DP service they stay masked.

Utility at the default reference e=.1 can be poor, especially with sparse
outcomes, high censoring, narrow windows and extreme exposure fractions. Finite
fits do not imply informative intervals. Runnable scripts, seeds and replicate
summaries accompany this revision in `tools/p0r3-record-utility.R` and
`inst/utility/p0r3-survival-utility.R`, with adjacent evidence directories;
release reports distinguish finite/informative rates, unconditional/conditional
coverage and log-effect bias/RMSE. The earlier unretained simulation numbers are
not evidence for the revised mechanism. For record summaries, calibration alone
gives N noise SD about 707.1, R SD 471.4 for mean+SD and 353.6 for mean-only;
wide public bounds amplify moment error. These are scalar reference amplitudes,
not a DP guarantee for data-enumerated strata.

The client pools compatible signed record components and derives display values
once. Protected effects use inverse-variance meta-analysis of site estimates,
not a pooled Cox/SCCS sufficient-statistic refit. Quantiles report their public
bin intervals and open/censored flags. Installing a new client does not patch
an old server: the client warns and refuses mixed protected pooling. Old typed
clients receive a version-specific refusal from the new server.

Public grids: birth years 1900 to the custodial reference year
(`dsomop.dp.reference_year`, default the calendar year) plus unknown; ages in
5-year bands 0 to 110+; counts per person 0-20, 25, 30, 40, 50, 75, 100+. The
clinical day anchors C = {0, 1, 7, 14, 21, 28, 30, 60, 90, 120, 180, 270, 365,
545, 730, 1095, 1460, 1825, 3650} are fixed points of every day grid: the
signed offset grid G is every multiple of 30 days with |z| <= 121 plus C and
-C; the look-back / follow-up grid L is G within [0, 3650]; the duration
histogram and threshold grid D is C plus 7300 and 10957 days. A closed window
[a, b] snaps to [floor_G(a), ceil_G(b)] and is converted once to the half-open
[a, b + 1) in SQL, so [1, 365] stays 365 inclusive days, [0, 0] one day and
[-365, -1], look-back 365 and end 545 are unchanged; durations snap to the
nearest point of L (strictly positive ones to L without 0) and `min_days` to D
(ties to the smaller point, finite values saturating). Ages snap to 5-year
boundaries, age bands to {5, 10, 20}, alpha and power to 0.01 steps, model
feature counts to {10, 20, 50, 100} and calibration bins to {5, 10, 20}. A
requested reference year is a view of the released birth-year basis on the
protected route (labels report `basis_reference_year` and
`view_reference_year`); the standard route freezes it at the custodial year.
Requested and effective values are reported in `effective_parameters`. On
scales without a public lower bound the first histogram bin is the explicit
underflow bin (-Inf, b2); quantiles invert the displayed histogram's CDF and,
in an open tail bin, report its finite edge.

Typed releases are views of one registered complete public basis (B05).
Categorical variables use the complete domain their producer declares plus
explicit Other and Unknown cells; requested levels are a validated view of
that basis, never a mechanism input, and levels outside it are refused
whatever the data hold. Numeric variables use a fixed atom basis (a
registered scale's grid, otherwise the universal signed nice-number grid;
dates on fixed year/month atoms): requested breaks snap to atom boundaries,
the complete signed basis is projected before each displayed bin sums its
projected atoms. Signed atoms remain the pooling inputs; the payload reports
`basis_id`, `view_of_basis` and the effective breaks. Irrelevant contribution
caps are normalised (a count's cap is 1). Typed bounded means need one
registered public scale (current-client status `public_scales`; custodian
extension `dsomop.release.public_scales`). The release-person map is sealed
with the table (B01), so no earlier key resolution or handle state can change
a contributor's identity, and counts, capped record counts and partitions of
one table share one eligible-population person total (B02).

Observed time at risk: CohortMethod, incidence, Kaplan-Meier and interaction
ports intersect the requested window with the coalesced observation period that
contains the canonical first index, as a half-open interval (persons whose index
lies outside observation contribute no risk time; outcomes outside the observed
window never count). Protected releases further cap that interval at the public
contribution horizon T = 3650 days, before both event selection and
person-time: `[lo, min(hi, lo + 3650))`. A requested window longer than T
(e.g. `[0, 3650]`, 3651 inclusive days) is therefore clipped for release, and
the label says so; in-session exact fits keep the whole observed window.

### DataSHIELD comparison

Permitted exact counts and means on two target-identifiable cohorts A and
A\{p} permit `n_A*mean_A - n_B*mean_B = x_p`. A retained-cohort size floor
alone does not invalidate this algebra. But DataSHIELD does not simply ignore
the issue: `dataFrameSubsetDS1` warns about and logs small complement
differences, and method privacy profiles can restrict subsetting-capable
functions. Current `dataFrameSubsetDS1/2` are profile-gated; the 2025 platform
paper describes more restrictive profiles and the Five Safes approach. A dsOMOP
scope interface may be more expressive than those strict profiles. Thus
"better than standard DataSHIELD" is not a universal ordering, and "dsBase
already permits differencing" is context, not an excuse for new exact leak
paths. Verify the installed versions and profile in Opal. Sources:
[dsBase meanDS](https://raw.githubusercontent.com/datashield/dsBase/master/R/meanDS.R),
[dataFrameSubsetDS1](https://raw.githubusercontent.com/datashield/dsBase/master/R/dataFrameSubsetDS1.R),
[dataFrameSubsetDS2](https://raw.githubusercontent.com/datashield/dsBase/master/R/dataFrameSubsetDS2.R),
[Avraam et al., DataSHIELD disclosure-risk model](https://academic.oup.com/bioinformaticsadvances/article/5/1/vbaf046/8068803).

## Community-tool gate

One shared policy in `R/profiling.R` (`.dsomopRequireCommunityTool`, which
checks that `dsomop.tools.enabled` is not `FALSE` and then
`.dsomopRequireStandardStatistics`)
gates the following endpoints in `R/interface.R` before handle lookup,
argument decoding, cohort resolution, cached-result access or queries.

| Gated endpoint | Covered output |
| --- | --- |
| `omopAchillesStatusDS` | Number of distinct populated analyses |
| `omopAchillesResultsDS` | Precomputed counts |
| `omopAchillesDistributionDS` | Support counts, means and distributions |
| `omopAchillesCatalogDS` | Populated analysis IDs, whose length is a distinct count |
| `omopOhdsiStatusDS` | Result-table row totals |
| `omopOhdsiTablesDS` | Result-table row counts |
| `omopOhdsiResultsDS` | Contracted counts, rates, means and other results |
| `omopOhdsiSummaryDS` | Result-table row counts |
| `omopQueryExecDS` | Aggregate query results |
| `omopAnalysisRunDS` | Aggregate analysis results |
| `omopAnalysisRunAssignDS` | Server-side analysis tables |
| `omopDpStatusDS`, `omopDpPlanExecuteDS` | DP status and sealed preparation |
| `omopDpReleaseDS` (since 2.10.3) | Typed protected releases; every attempt is written to the redacted audit log |

`omopAnalysisRunDS`, `omopQueryExecDS` and the DP endpoints use the route-aware
gate (`dp_routes = TRUE`): DP-only mode then refuses standard statistics
question by question and admits fixed-synopsis releases. The typed release
checks the gate before decoding its arguments or reading any provenance,
handle or data.

dsOMOP's own endpoints are never gated by DP: table and column statistics,
domain coverage, missingness, value counts, concept prevalence, crosstabs,
numeric range, histograms and quantiles, date counts, concept drilldown and
location, safe cutpoints, plan preview, cohort list and definitions, observed
concept-factor levels (`omopFactorLevelsDS`) and capabilities (including the
banded population size). They apply the standard suppression and banding.

Only sealed person-local tables (community-tool loaders and the DP preparation
path) can be released through `omopDpReleaseDS` (`ds.omop.dp.release` on the
client). The seven typed statistics release protected components (see above):
counts, capped record counts and registered-scale bounded means can use a
fixed synopsis only for an unscoped pinned loader table with authenticated origin
membership matching the connected source inside the current release snapshot.
A restored table without a live matching source, or with changed source
membership, retains the realised-sticky route and is refused in DP-exclusive
mode. It is not silently relabelled as a fresh copy of the current corpus.
Every other typed release remains realised-contribution protected. Statistic
aliases share their declared eligible-population person total.

Typed categories require registered complete domains with explicit Other and
Unknown cells. Numeric/date views use registered fixed atoms. Requested levels,
positive sets and break boundaries are validated post-processing views, never
new noise owners. Binary numerator and denominator masks identify canonical
atom indices and preserve all/nonmissing eligibility through pooling. Semantic
domain labels, not only bit-pattern widths, enter basis identity. Weighted
histograms own one protected weighted total; server and client both reconcile
and project the complete atom vector before summing any requested bins. They
never reconcile a record-weighted histogram to person N. Multi-label and
record-capped vectors retain their declared whole-person cap. These repairs
close the enumerated cosmetic/view inconsistencies; distinct realised questions
still carry the documented equality/overlap residual.

The gate covers the registered community-tool surface. It does not alter
internal profiling helpers, controller-only `omopAchillesHeelDS`, or
methods from other DataSHIELD packages, whose contracts remain outside this
community-tool gate. It also does not make metadata admission or
assignment side channels differentially private, retract earlier releases, or
provide a finite cumulative privacy budget.

## DP identity and deployment

When `dsomop.dp.domain` / `DSOMOP_DP_DOMAIN` is unset or has an empty
package default, initialization derives it from the connected resource; an
explicitly configured value takes precedence. The public `snapshot_id` status
label is a digest of that domain unless the deprecated `dsomop.dp.snapshot_id` /
`DSOMOP_DP_SNAPSHOT_ID` supplies a valid display label. The domain is a SHA-256
hash of versioned, canonical database coordinates: normalized DBMS, lowercase
host, effective port, database (canonical absolute file path for SQLite/DuckDB),
and effective CDM and vocabulary schemas. Credentials, transport settings,
resource display names, results schemas and temporary schemas are excluded.
SQLite's implicit and explicit `main` schema have the same identity.

The optional snapshot label is display-only; absent or malformed labels
fall back to a digest of the canonical corpus domain. Current policy derivation
never queries CDM/vocabulary metadata to obtain this label, so a display change
cannot alter database reads, snapshot availability or provenance admission.

Without a configured domain, in-memory databases and unavailable/ambiguous
resource coordinates fail closed because no unambiguous identity is available.
An explicit domain is an optional override for those special resources.
Conflicting option and environment values still fail closed. No generic shared
fallback domain is used.
`omopInitDS()` establishes the resource from which DP status derives defaults.

Before 2.10.3 the noise of a typed release depended on the canonical request
semantic, the keyed fingerprint of the exact bounded statistic and a private
automatic database version (the largest primary key of every clinical table),
so every distinct request spelling drew fresh noise and every reload re-drew
every answer. 2.10.3 removes that path: protected releases are keyed as
described in "Modes and metadata". The fixed synopses' automatic data
generation (the request-local person-membership digest, above) replaces the old
database version for that route only.

- Realised-sticky releases follow their data. A reload or in-place edit that
  changes a person's bounded contribution changes that component's
  fingerprint and re-draws it; components whose contribution maps are
  unchanged keep their draws (an unrelated insertion no longer re-draws them).
  Repeating a question across a refresh therefore shows which components
  changed; auxiliary knowledge can disclose more. The conditional equality of
  [isglobal-brge/dsOMOP#20](https://github.com/isglobal-brge/dsOMOP/issues/20)
  remains, by design, for unchanged components.
- Fixed synopses follow the request-local person-membership generation:
  adding, removing or replacing a person globally redraws fixed atoms.
  Clinical edits with unchanged membership retain the stated component-change
  residual; contribution fingerprints prevent intentional old-noise reuse for
  changed bounded maps, not every possible inference from their differences.
- `dsomop.dp.privacy_epoch` / `DSOMOP_DP_PRIVACY_EPOCH` is an optional
  administrative noise-profile version. Rotation redraws all components and
  adds a disclosure; it is not required after data loads and resets no history.

Security-policy changes during a session fail closed; snapshot display labels
are excluded. Different derived resources have separate sessions; DSLite
sessions retain separate policies within their shared R process. Optional
explicit identities represent replicas or aliases with different coordinates.

The only DP state is the noise root, automatically created at
`~/.dsomop/secrets/dp_noise_root` under the R service user's home, or under an
optional `DSOMOP_STATE_DIR` / `dsomop.state_dir`. First use creates the root
from the operating-system CSPRNG, sets private directory/file permissions and
logs creation once. Concurrent workers and ordinary service restarts reuse it
automatically; no provisioning or post-load rotation is required. Owner, mode,
symlink and hard-link checks apply; a damaged, unsafe or
substituted root is never replaced and fails closed with one stable analyst
error and the reason in the server log. A custodian may instead inject the root
through `dsomop.dp.noise_root` / `DSOMOP_DP_NOISE_ROOT`; then nothing is written
for DP. Persistence follows the service filesystem; container replacement with an
empty home does not preserve prior roots.
`DSOMOP_TEST_ALLOW_EPHEMERAL_STATE=1` permits temporary paths in tests only; it
does not supply a resource identity or bypass policy checks.

The mechanism remains person-bounded sticky discrete Laplace with the exact
sampler below, default reference epsilon 0.1, admitted range [1e-6, 8] and
delta 0; components receive the fixed allocations above. Resource domains
separate the HMAC subkeys (person tokens, contribution fingerprints, noise and
provenance), so equal components on distinct resources use different PRF keys.
Discrete noise draws can coincide by chance; independence does not imply
unequal numeric outputs on every call.

Each answer is a deterministic function of the root, the domain, the
component's owner, family, coordinate, fixed noise profile (mechanism,
sampler, reference epsilon, allocation, sensitivity, lattice and epoch) and
contribution fingerprint, plus the automatic data generation and population
for a fixed synopsis. Nothing else is stored: a repeated
valid question recomputes the same canonical bytes after restarts and on every
R server with the same root, configuration and data, with no store, capacity
limit or lock. Provenance and typed admission run on every request.

dsOMOP 2.7.1 to 2.9.x instead bound each public request identity, snapshot and
epoch permanently to its first complete response in an authenticated store
under `release-bindings/`. 2.10.0 does not read that store, so upgrading
re-draws previously asked questions once; `omopInitializeReleaseStore()` is a
deprecated no-op.

There is no finite cumulative budget or full temporal-transcript DP claim.
Distinct requests, data changes and public rotations compose; admission
failures, administration, timing, availability and other DataSHIELD outputs
remain separate. See README.md for deployment and upgrade notes. The
QueryLibrary sticky catalogue is public metadata and remains available before
DP initialization; executing a release still validates policy.

## Exact discrete-Laplace sampler (2.6.1)

The previous `hmac-inverse-cdf-52bit-v1` sampler transformed finite 52-bit
uniforms. At epsilon 8 and sensitivity 1 its noise support was only [-4, 4],
so neighbouring counts could have different output supports and violate pure
DP. The endpoint reproduction and complete affected-statistic inventory are in
[SAMPLER_DIAGNOSIS.md](SAMPLER_DIAGNOSIS.md).

The replacement `dsomop-dp-exact-discrete-laplace-v1` follows Canonne, Kamath
and Steinke (2020), [The Discrete Gaussian for Differential Privacy, section
5](https://arxiv.org/html/2004.00010v4#S5). All sampler probability decisions use
exact integer/rational arithmetic. For rational x in [0, 1], successive exact
Bernoulli(x/k) trials stop at their first failure; the parity of the stopping
index gives Bernoulli(exp(-x)) by the alternating exponential series. Larger
x is split into its integer and fractional parts. No exponential is evaluated
numerically.

For the exact rate epsilon/sensitivity = s/t, draw U uniformly from
{0, ..., t-1}, accepting it with probability exp(-U/t). Independently let V
count consecutive successful Bernoulli(exp(-1)) trials. Then
G = floor((U + tV)/s) has geometric survival probability
Pr[G >= k] = exp(-k s/t). Choose a fair sign and restart for negative zero.
The resulting integer Z has probability mass

`Pr[Z = z] = (1 - alpha) / (1 + alpha) * alpha^abs(z)`,
where `alpha = exp(-epsilon / sensitivity)`.

There is no fixed noise bound, bit budget or rejection cutoff. For neighbouring
integer statistics differing by at most sensitivity, the probability ratio at
every integer output is at most exp(epsilon). Since 2.10.3 each protected
component is drawn with its fixed allocation of the reference calibration and
its declared sensitivity (see "Modes and metadata"). Noise is added using
arbitrary-precision integers; the signed component is kept for pooling and only
projected (top-down simplex, feasible moment set) for display. Projection and
clipping are post-processing.

The canonical inputs are the exact IEEE-754 binary rational values of the
validated R epsilon and sensitivity, converted exactly: each positive double
is an odd integer times a power of two, and the ratio is reduced by the gcd of
the odd parts. For example, R's `0.1` is `3602879701896397/36028797018963968`, not decimal 1/10.
The ratio is formed using rational arithmetic; no printed decimal
approximation is reparsed. Division of epsilon by two in means and rates is
exact throughout the admitted range. Integer sensitivities and grid sizes are
represented exactly as well.

The deterministic bit stream uses HMAC-SHA256 under the existing secret noise
key, with domain `dsomop-dp-hmac-bit-stream-v1`. Its context and coordinate
select a stream; an arbitrary-precision per-coordinate draw counter, serialized
as canonical decimal text, selects each 256-bit block. All bytes are consumed
in order, least-significant bit first within each byte. Counters neither wrap
nor round. The same key and complete context replay the same bits and noise,
while component/coordinate and counter separation select distinct PRF inputs.
No random-generator state is persisted, and since 2.10.0 no response either: a
repeated question re-enters this sampler with the same key and complete context
and draws the same noise.

With independent random bits the implemented mechanism has the exact ideal
distribution and pure differential privacy (delta 0). The deployed keyed
stream makes the corresponding cryptographic pseudorandomness assumption
about HMAC; a finite secret key is not an information-theoretic source of
infinitely many independent bits, and the ideal DP theorem does not make a
fixed secret, publicly unknown noise realization private across adaptively
chosen questions. The limits of "Protected releases: guarantees and limits"
apply.

The mechanism identifier is `dsomop-sticky-discrete-laplace-prf-v2`, and the
hashed policy schema changes from 2 to 3. Both the public sampler and mechanism
metadata distinguish old and repaired releases. The first-answer release-envelope protocol in 2.7.1 is
`dsomop-dp-release-v3`, with privacy contract
`fixed_per_release_snapshot_first_answer_v1`. Status truthfully declares
`history_dependent = TRUE`, `persistent_state = "noise_root_and_release_bindings"`,
`release_binding = "snapshot_first_answer_v1"`, `privacy_call_quota = "none"`
and `service_capacity = "public_identity_reservations_v1"`. dsOMOPClient 2.7.4
recognizes the older stateless v2 contract explicitly and refuses mixed-contract
pooling by default. With `dsomop.dp.allow_legacy_servers = TRUE`, it permits
legacy and mixed-contract pooling, warns for each legacy server about the
unrotated-refresh equality residual, and records every server's contract in the
result, including pooled-only results. Sampler, mechanism and canonical JSON are
unchanged by this first-answer update; the versioned policy schema is now 4.
Since 2.10.0 the same protocol carries the seed-only privacy contract
`fixed_per_release_seed_data_versioned_v1`: status declares
`history_dependent = FALSE`, `persistent_state = "noise_root_only"`,
`release_binding = "seed_data_versioned_v1"`, `privacy_call_quota = "none"`
and `service_capacity = "none"`. dsOMOPClient 2.10.0 accepts it alone or pooled
with first-answer servers and records each server's contract. Sampler,
mechanism, canonical JSON and policy schema 4 are unchanged.

Changing the sampler changes the policy hash and sticky release identity, so an
upgraded query can receive a new draw. Replay across replicas depends on the
same version, root, domain and mechanism. This repair does not retroactively
protect old releases
or erase their contribution to privacy loss.

Since 2.10.3 the same protocol advertises the protected-release privacy
contract `protected_release_fixed_basis_or_realised_v1` with guarantee
`fixed_synopsis_or_heuristic_realised_contribution_reuse_v1` and binding
`fixed_basis_or_realised_contributions_v1` (status also reports
`release_protocol = "dsomop-protected-release-v1"`, the guarantee scope of each
route, the fixed-synopsis statistics, the registered public scales and
`history_based_admission = FALSE`). dsOMOPClient 2.10.3 validates and pools
these releases from their signed components; older clients refuse them, and no
client pools them with servers of the per-release contract. The sampler,
mechanism identifier and canonical JSON are unchanged.

## Upgrading to 2.10.3

The paired server/client release uses automatic private roots, resource identity
and request-local membership generations. Existing valid secrets are retained;
no custodian configuration, manual epoch rotation, database trigger, ETL hook or
TLS requirement is introduced. Persistent state remains the DP noise root and
pseudonymisation key; no answer cache, privacy accountant, call quota, budget or
per-user/session admission history is added. Intermediate preparation remains
private and permissive; final publication applies its `nfilter` and protected
contract checks.

The repaired component ownership, public grids and projections can change
answers relative to 2.10.2. New protected payloads require dsOMOPClient 2.10.3
and compatible server contracts for pooling. Mixed-version sites remain visible
through split results. The installed package and published Opal profile are
reported separately; ordinary package-settings publication exposes the new
methods after an upgrade. No extra privacy setup follows publication.

These changes close the enumerated exact-output and cosmetic-variant defects;
they do not retract earlier releases or remove the residual risks documented
above. Fixed-synopsis epsilon describes a bounded vector before exact support
gates, and realised-contribution reuse remains heuristic. Clinical edits with
unchanged person membership, overlapping scopes, other packages' methods and
auxiliary information remain outside a finite evolving-transcript DP guarantee.
