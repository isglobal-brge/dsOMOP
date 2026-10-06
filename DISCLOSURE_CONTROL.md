# Differential-privacy release channel

The DP release channel is enabled by default since dsOMOP 2.6.0. Custodians
can opt out with `options(dsomop.dp.enabled = FALSE)` or `DSOMOP_DP_ENABLED=0`.
Explicit option and environment settings must agree. The custodial option
`dsomop.dp.exclusive` defaults to `TRUE` since dsOMOP 2.7.0: while DP is
enabled, covered statistics can be released only through the typed DP channel.
The DataSHIELD profile fallback is `default.dsomop.dp.exclusive = TRUE`.
Custodians can restore standard population statistics with
`options(dsomop.dp.exclusive = FALSE)` (or set the profile fallback to `FALSE`).
Analysts cannot set this policy or bypass the noise through a client option.

With exclusivity off, the standard suppression-and-banding surface remains
available. With both `dsomop.dp.enabled` and `dsomop.dp.exclusive` true, the
standard statistical endpoints refuse with a message directing analysts to
`omopDpReleaseDS` (`ds.omop.dp.release` on the client). Disabling DP also
disables the exclusivity gate. `omopDpStatusDS()` reports the configured
`exclusive` flag even when DP is disabled; the gate is active only when both
`enabled` and `exclusive` are true. The flag is read on each request and does
not change the DP mechanism, policy hash, sticky noise, or release identity.

## Exclusive standard-surface policy

One shared policy in `R/profiling.R` (`.dsomopStandardStatisticsAllowed` and
`.dsomopRequireStandardStatistics`) gates the following aggregate wrappers in
`R/interface.R` before handle lookup, argument decoding, cohort resolution,
cached-result access or queries. Entire statistical endpoints refuse, including
requests that would otherwise produce an empty or fully suppressed result.

| Refusing endpoint | Covered output |
| --- | --- |
| `omopTableStatsDS` | Row and distinct-person counts |
| `omopColumnStatsDS` | Total, missing, distinct and person counts; means |
| `omopDomainCoverageDS` | Per-domain person counts |
| `omopMissingnessDS` | Missingness counts and rates |
| `omopValueCountsDS` | Value frequencies |
| `omopConceptPrevalenceDS` | Person and record counts |
| `omopCrossTabDS` | Contingency counts and optional margins |
| `omopNumericRangeDS` | Population count and distribution summary |
| `omopNumericHistogramDS` | Histogram counts |
| `omopNumericQuantilesDS` | Population-derived distribution profiling |
| `omopDateCountsDS` | Temporal counts |
| `omopConceptDrilldownDS` | Counts, means, frequencies and missingness |
| `omopLocateConceptDS` | Per-table record and person counts |
| `omopSafeCutpointsDS` | Banded bin counts |
| `omopPlanPreviewDS` | Per-output population counts |
| `omopCohortListDS` | Cohort sizes |
| `omopCohortGetDefinitionDS` | Population-admitted definition records, including local count columns |
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
| `omopFactorLevelsDS` | Observed levels, whose length is a distinct count |

`omopGetCapabilitiesDS` uses the same policy to omit `total_persons` in
exclusive mode. Its remaining structural metadata keeps existing client
connections usable. Schema, column types, join relationships, vocabulary
reference data, static analysis definitions, result contracts, disclosure
settings and DP status/catalogue metadata remain available. Vocabulary
concept totals count public reference records, not the clinical population.
Reference tables are expected to contain reference metadata, including any
custodial extension columns.
`omopCohortGetDefinitionDS` also refuses: its population-admitted response
forwards unrestricted definition columns that can include local cohort counts.

Server-side assignment and authenticated DP input preparation remain available.
For memory-mode plans, dsOMOPClient 2.7.3 automatically skips observed
factor-level discovery with an explanatory message when any selected server
is exclusive. With older clients, first set
`plan <- ds.omop.plan.options(plan, factor_concepts = FALSE)`, then call
`ds.omop.plan.execute(plan)`.
Supply public DP categories, bounds and breaks directly
in the typed DP specification; the standard profiling endpoints cannot be used
to learn them in exclusive mode. The seven supported typed DP statistics
remain available with identical sticky releases in either mode. dsOMOPClient
2.7.3 also displays exclusivity in DP status and directs refused helpers to
`ds.omop.dp.release`; it remains compatible with 2.6.0 servers that do not
report an `exclusive` field.

The gate covers dsOMOP's registered standard aggregate surface. It does not
alter internal profiling helpers, controller-only `omopAchillesHeelDS`, or
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
`dsomop-dp-cdm-snapshot-resource-only-v1`. Startup proceeds with one warning per
R process, shared across DSLite sessions: custodians must bump
`dsomop.dp.privacy_epoch` after **each data reload**, or configure
`dsomop.dp.domain` and `dsomop.dp.snapshot_id` explicitly. A snapshot unchanged
across data reloads freezes previously released answers; fallback metadata
cannot automatically track those changes. Each explicit identifier
wins independently, and an explicit snapshot skips metadata derivation.

In-memory databases and unavailable/ambiguous resource coordinates still fail
closed with instructions to configure both identifiers. Conflicting option and
environment values still fail closed. No generic shared fallback domain is used.
Initialize the resource with `omopInitDS()` before calling DP status when using
derived defaults.

The derived snapshot follows metadata, not every data edit. Custodians **must
bump `dsomop.dp.privacy_epoch` / `DSOMOP_DP_PRIVACY_EPOCH` when data change without
metadata changes**, and restart affected sessions. Policy changes during a
session fail closed. Use a separate session for a different derived resource;
DSLite server sessions keep separate policies within their shared R process.
For replicas and database aliases of one logical dataset, configure matching
explicit identifiers if canonical connection coordinates differ.

A persistent private state directory is required for the noise root and release
bindings, including with an injected root: mount `DSOMOP_STATE_DIR` (or
`dsomop.state_dir`) across restarts and replicas. Initialize with the administrative
`omopInitializeReleaseStore()` and pin its UUID externally through
`DSOMOP_DP_RELEASE_STORE_ID` / `dsomop.dp.release_store_id`. The default `~/.dsomop` is suitable only when the home is persistent.
`DSOMOP_TEST_ALLOW_EPHEMERAL_STATE=1` permits temporary paths in tests only; it
does not supply a resource identity or bypass policy checks.

The mechanism remains person-bounded sticky discrete Laplace, with default
release epsilon 0.1, admitted range [1e-6, 8] and delta 0. Contribution and level
caps are unchanged. Resource domains separate HMAC subkeys; both identifiers enter
the policy hash and release context. Equal canonical queries on distinct
resources therefore use different PRF keys. Discrete noise draws can coincide
by chance; independence does not imply unequal numeric outputs on every call.

In 2.7.1, each public request identity, snapshot and epoch binds permanently to
its first complete canonical response. Every later valid request replays those
bytes, whether zero, one or many persons changed. Provenance and typed admission
still run; hits neither compare private fingerprints nor call the payload
function. The fingerprint remains private defence in depth on the fresh path.
There is no refresh boundary and no analyst force-refresh control. Answers can
remain stale indefinitely until the custodian rotates the snapshot or epoch and
restarts sessions. New requests against mutable data need not agree with the
source version reflected by earlier answers.

Responses and the table head are authenticated before decoding, and publication
commits before return. Missing/corrupt state or root fails closed; established
roots are never automatically replaced. Keep a consistent root/store/lock backup
and the expected UUID outside the payload directory; stop workers before restore.
Whole-state rollback to an earlier valid backup is an explicit operator/storage
trust assumption, not cryptographically detected by the record MACs.

Capacity is public storage admission, not an epsilon budget: the default 1 GiB
`dsomop.dp.release_store_bytes` / `DSOMOP_DP_RELEASE_STORE_BYTES` reserves once
per new public identity, retains answers permanently without eviction, and allows
existing identities to replay at capacity. Validation/statistic preparation may
already have read private data before a new identity is refused.

This closes the conditional repeated-identity temporal equality selection in
[isglobal-brge/dsOMOP#20](https://github.com/isglobal-brge/dsOMOP/issues/20),
conditional on retained state and the same public admission schedule. It adds
no finite cumulative budget or full temporal-transcript DP claim. Distinct
requests and public rotations compose; admission failures, administration,
timing, availability and other DataSHIELD outputs remain separate.
See README.md for initialization, capacity reservations, recovery and migration. The QueryLibrary sticky catalogue is public metadata and remains
available before DP initialization; executing a release still validates policy.

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
every integer output is at most exp(epsilon). Histogram coordinates use the
person-level L1 bound; mean and rate components retain their sequential
half-epsilon allocations. Noise is added using arbitrary-precision integers,
then clipped to the existing public bounds (normally [0, 2^53-1]) before
conversion back to an R number. Clipping is post-processing and preserves the
privacy inequality.

The canonical inputs are the exact IEEE-754 binary rational values of the
validated R epsilon and sensitivity, converted with `gmp::as.bigq`. For
example, R's `0.1` is `3602879701896397/36028797018963968`, not decimal 1/10.
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
No random-generator state is persisted. Since 2.7.1 the complete first response
is persisted separately; successful repeats do not re-enter this sampler.

With independent random bits the implemented mechanism has the exact ideal
distribution and pure differential privacy (delta 0). The deployed keyed
stream makes the corresponding cryptographic pseudorandomness assumption
about HMAC; a finite secret key is not an information-theoretic source of
infinitely many independent bits. The per-release scope and composition
limitations above are unchanged.

The mechanism identifier is `dsomop-sticky-discrete-laplace-prf-v2`, and the
hashed policy schema changes from 2 to 3. Both the public sampler and mechanism
metadata distinguish old and repaired releases. The first-answer release-envelope protocol in 2.7.1 is
`dsomop-dp-release-v3`, with privacy contract
`fixed_per_release_snapshot_first_answer_v1`. Status truthfully declares
`history_dependent = TRUE`, `persistent_state = "noise_root_and_release_bindings"`,
`release_binding = "snapshot_first_answer_v1"`, `privacy_call_quota = "none"`
and `service_capacity = "public_identity_reservations_v1"`. dsOMOPClient 2.7.4
recognizes the older stateless v2 contract explicitly and refuses mixed-contract
pooling. Sampler, mechanism and canonical JSON are unchanged by this first-answer
update; the versioned policy schema is now 4.

Changing the sampler changes the policy hash and sticky release identity, so an
upgraded query can receive a new draw. All replicas of one logical node must
use the same version. This repair does not retroactively protect old releases
or erase their contribution to privacy loss.
