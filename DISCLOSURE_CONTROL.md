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
views solely from one calibrated public atom basis for an immutable dataset
generation. Unlimited post-processing of that basis does not add a new raw-data
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
history. One request's components, contribution maps and support checks are
read by separate SELECT statements without a common database read snapshot
(dsOMOP 2.10.3): a concurrent data load during a request could mix two data
states. Load data in batches outside analysis hours; fixed synopses require a
declared generation in any case. Stable data and identity replay across sessions and workers. Private
contributor hashes, person keys and exact hidden support are never output.
Audit is observational and is not an accountant.

The intended deployment remains DataSHIELD's layered controls and governance:
approved access and uses, custodian-selected methods, output checks and audit.
This revision removes the tested dsOMOP exact-output bypasses and cheap
cosmetic-variant averaging attacks; it does not replace the rest of that
system. Custodians must assess the combined Opal method profile, including
overlapping dsBase/core functions and available auxiliary information. Shared
Rock credentials are not distinct analyst identities for privacy accounting or
reliable individual attribution.

### Modes and metadata

- `fixed_synopsis`: `mechanism_epsilon` identifies the declared bounded vector
  (the union of the owned components) and `delta = 0`;
  `guarantee_scope = "fixed_vector_before_exact_support_gate"`;
  `api_transcript_epsilon = NA` because the full service includes exact gates
  and other outputs. `"dp"`/exclusive mode chooses this mechanism family; it is
  not a claim about all enabled Opal methods. A request is a fixed synopsis
  only when it is unscoped, its contract declares a public basis, and the
  custodian declares an immutable data generation (`dsomop.dp.snapshot_id`);
  the noise of each atom is keyed on (corpus, generation, owner, population,
  atom).
- `realised_sticky`: `noise_reference_epsilon` plus actual per-component
  allocations, scales and sensitivities; `mechanism_epsilon = NA`,
  `api_transcript_epsilon = NA`,
  `guarantee_scope = "heuristic_realised_contribution_reuse"`. Every cohort- or
  table-scoped request, and every unscoped request without a custodial
  generation, takes this route in auto mode; DP-only mode refuses it with a
  stable unsupported-mechanism message. Each component's noise is keyed on its
  owner, coordinate and a secret HMAC fingerprint of the sorted
  (person token, bounded contribution) map; request text, sessions, handles,
  scope names, lineage spelling and the database version never enter it. A
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
in an open tail bin, report its finite edge. Typed histogram breaks snap to the 2-significant-digit
decimal lattice (first days of months for dates, UTC midnights for datetimes);
typed bounded means need one registered public scale (`omopDpStatusDS()`
`public_scales`; custodian extension `dsomop.release.public_scales`).

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
counts, capped record counts and registered-scale bounded means of unscoped
pinned loader tables of a custodial generation are fixed synopses; every other
typed release is realised-contribution protected and is refused in DP-only
mode. Statistic aliases of one person total (count, `bounded_record_count`
with k = 1, the denominators of rates and means) share the person-total
owner. A typed variable has no registered label universe, so each declared
label or bin is its own canonical component, keyed by the label and its
realised contributions and never by the other declared levels: unused levels,
dummy positives, reordered levels and clamp-only outer breaks add no fresh
draw of an existing cell. The declared levels are the view: exported signed
cells are reconciled to the one noisy person total over the declared cells,
and displayed counts are their Euclidean projection onto that total.
Multi-label and record-capped cells and distinct-label cardinalities use the
fixed cap K as sensitivity for every requested k. Residual: because the
analyst declares the domain, known-empty (dummy) levels or bins move the
reconciliation shift; their contrasts with a real cell let an analyst estimate
that cell's single fixed draw, so each distinct typed partition can add one
noisy estimate of its person total (the split-partition residual above),
never a fresh draw of the same cell.

The gate covers the registered community-tool surface. It does not alter
internal profiling helpers, controller-only `omopAchillesHeelDS`, or
methods from other DataSHIELD packages. Custodians must review those separately
in the deployed method allowlist. It also does not make metadata admission or
assignment side channels differentially private, retract earlier releases, or
provide a finite cumulative privacy budget.

## DP identity and deployment

When `dsomop.dp.domain` / `DSOMOP_DP_DOMAIN` and
`dsomop.dp.snapshot_id` / `DSOMOP_DP_SNAPSHOT_ID` are unset or have empty
package defaults, initialization derives them from the connected resource.
Each explicitly configured value takes precedence. The domain is a SHA-256
hash of versioned, canonical database coordinates: normalized DBMS, lowercase
host, effective port, database (canonical absolute file path for SQLite/DuckDB),
and effective CDM and vocabulary schemas. Credentials, transport settings,
resource display names, results schemas and temporary schemas are excluded.
SQLite's implicit and explicit `main` schema have the same identity.

Snapshot precedence is **explicit configuration → complete `cdm_source` →
fallback**. The snapshot hashes the resource identity and the source fields
`cdm_source_name`, `cdm_release_date`, `cdm_version` and `vocabulary_version`,
converted to text. One complete row retains the `dsomop-dp-cdm-snapshot-v1`
contract; multiple complete rows are all canonically sorted and hashed under
`dsomop-dp-cdm-snapshot-multi-v1`.

Missing/unreadable tables, zero rows, or missing/blank fields fall back to all
canonically sorted `vocabulary_id`/`vocabulary_version` pairs from `vocabulary`
(including the `None` row), plus resource identity, under
`dsomop-dp-cdm-snapshot-fallback-v1`. If vocabulary metadata is also unavailable
or incomplete, resource identity alone is hashed under
`dsomop-dp-cdm-snapshot-resource-only-v1`. Each explicit identifier wins
independently, and an explicit snapshot skips metadata derivation.

In-memory databases and unavailable/ambiguous resource coordinates still fail
closed with instructions to configure both identifiers. Conflicting option and
environment values still fail closed. No generic shared fallback domain is used.
Initialize the resource with `omopInitDS()` before calling DP status when using
derived defaults.

Before 2.10.3 the noise of a typed release depended on the canonical request
semantic, the keyed fingerprint of the exact bounded statistic and a private
automatic database version (the largest primary key of every clinical table),
so every distinct request spelling drew fresh noise and every reload re-drew
every answer. 2.10.3 removes that path: protected releases are keyed as
described in "Modes and metadata", and no database version is read.

- Realised-sticky releases follow their data. A reload or in-place edit that
  changes a person's bounded contribution changes that component's
  fingerprint and re-draws it; components whose contribution maps are
  unchanged keep their draws (an unrelated insertion no longer re-draws them).
  Repeating a question across a refresh therefore shows which components
  changed, never by how much: the conditional equality of
  [isglobal-brge/dsOMOP#20](https://github.com/isglobal-brge/dsOMOP/issues/20)
  remains, by design, for unchanged components.
- Fixed synopses follow the custodial generation: the noise of an atom depends
  on (corpus, generation, owner, population, atom), not on the data.
  **Custodians must change `dsomop.dp.snapshot_id` with every reload or
  edit**: an unchanged generation adds the same noise to the new exact values,
  so the difference of two releases would reveal the exact change.
- `dsomop.dp.privacy_epoch` / `DSOMOP_DP_PRIVACY_EPOCH` is part of every noise
  profile: bumping it re-draws every fixed and realised component at once.

Policy changes during a session fail closed. Use a separate session for a
different derived resource; DSLite server sessions keep separate policies
within their shared R process. For replicas and database aliases of one
logical dataset, configure matching explicit identifiers if canonical
connection coordinates differ.

The only DP state is the noise root, `DSOMOP_STATE_DIR/secrets/dp_noise_root`
(or under `dsomop.state_dir`); mount the state directory across restarts. First
use creates the root from the operating-system CSPRNG and logs a custodian
message. Owner, mode, symlink and hard-link checks apply; a damaged, unsafe or
substituted root is never replaced and fails closed with one stable analyst
error and the reason in the server log. A custodian may instead inject the root
through `dsomop.dp.noise_root` / `DSOMOP_DP_NOISE_ROOT`; then nothing is written
for DP. The default `~/.dsomop` is suitable only when the home is persistent.
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
component's owner, family, coordinate and fixed noise profile (mechanism,
sampler, reference epsilon, allocation, sensitivity, lattice and epoch), and
either the custodial generation and population (fixed synopsis) or the
contribution fingerprint (realised sticky). Nothing else is stored: a repeated
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
upgraded query can receive a new draw. All replicas of one logical node must
use the same version. This repair does not retroactively protect old releases
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
