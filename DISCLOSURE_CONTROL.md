# Differential-privacy release channel

Since dsOMOP 2.8.0, differential privacy (DP) concerns only the integrated
OHDSI community tools: Achilles results, OHDSI results tables, the analysis
catalog and the query library. dsOMOP's core (connecting, loading tables
through plans and recipes, filters, concept factors, merges and row binding,
cohorts and dsOMOP's own exploration statistics) works exactly as it did
before DP was added, under the standard DataSHIELD disclosure controls
(`nfilter.*`) only. It never initializes, consults or depends on DP, and its
tables never carry DP provenance.

The community tools are enabled by default (`dsomop.tools.enabled`, profile
fallback `default.dsomop.tools.enabled = TRUE`); custodians can disable them
with `dsomop.tools.enabled = FALSE`. The DP release
channel is enabled by default since dsOMOP 2.6.0; custodians can opt out with
`options(dsomop.dp.enabled = FALSE)` or `DSOMOP_DP_ENABLED=0`. Explicit option
and environment settings must agree. The custodial option
`dsomop.dp.exclusive` defaults to `FALSE` since 2.8.0 (it defaulted to `TRUE`
in 2.7.x and then covered dsOMOP's own statistics too). When a custodian sets
it and DP is enabled, the community-tool endpoints release results only
through the typed DP channel. Analysts cannot set this policy or bypass the
noise through a client option. `omopDpStatusDS()` reports the configured
`exclusive` flag; the flag is read on each request and does not change the DP
mechanism, policy hash, sticky noise, or release identity.

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

dsOMOP's own endpoints are never gated by DP: table and column statistics,
domain coverage, missingness, value counts, concept prevalence, crosstabs,
numeric range, histograms and quantiles, date counts, concept drilldown and
location, safe cutpoints, plan preview, cohort list and definitions, observed
concept-factor levels (`omopFactorLevelsDS`) and capabilities (including the
banded population size). They apply the standard suppression and banding.

Only outputs of the community tools can carry DP provenance and be released
through `omopDpReleaseDS` (`ds.omop.dp.release` on the client). Supply public
DP categories, bounds and breaks directly in the typed DP specification. The
seven supported typed DP statistics remain available with identical sticky
releases whether or not exclusivity is set.

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

Since 2.10.0 the noise also depends on a private automatic database version:
without an explicit snapshot, each R session reads the largest primary-key
identifier of every present clinical table once (an index lookup), keys the
result with the protected subkey and puts it in every release identity. It is
never released. A reload that adds rows therefore re-draws every answer at the
next session, with no epoch bump. An in-place edit that adds no row with a
larger identifier keeps the version: custodians **must bump
`dsomop.dp.privacy_epoch` / `DSOMOP_DP_PRIVACY_EPOCH` after such edits** and
restart affected sessions. An explicit `dsomop.dp.snapshot_id` replaces the
automatic version (the database is then not read) and must change with every
reload. Policy changes during a session fail closed. Use a separate session for
a different derived resource; DSLite server sessions keep separate policies
within their shared R process. For replicas and database aliases of one logical
dataset, configure matching explicit identifiers if canonical connection
coordinates differ.

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

The mechanism remains person-bounded sticky discrete Laplace, with default
release epsilon 0.1, admitted range [1e-6, 8] and delta 0. Contribution and level
caps are unchanged. Resource domains separate HMAC subkeys; both identifiers enter
the policy hash and release context. Equal canonical queries on distinct
resources therefore use different PRF keys. Discrete noise draws can coincide
by chance; independence does not imply unequal numeric outputs on every call.

Each answer is a deterministic function of the root, the public question
(lineage, typed statistic, bounds, reducer, contribution contract, policy
hash, domain, snapshot, epoch, mechanism and sensitivity), the keyed
fingerprint of the bounded statistic and the database version. Nothing else is
stored: a repeated valid question recomputes the same canonical bytes after
restarts and on every R server with the same root, configuration and data, with no
store, capacity limit or lock. Provenance and typed admission run on every
request. Changing the root, snapshot, epoch or database version re-draws every
answer; changing only the bounded statistic re-draws that answer.

dsOMOP 2.7.1 to 2.9.x instead bound each public request identity, snapshot and
epoch permanently to its first complete response in an authenticated store
under `release-bindings/`. 2.10.0 does not read that store, so upgrading
re-draws previously asked questions once; `omopInitializeReleaseStore()` is a
deprecated no-op.

For [isglobal-brge/dsOMOP#20](https://github.com/isglobal-brge/dsOMOP/issues/20)
(the conditional equality across an unrotated refresh), a refresh that adds
rows changes the database version, so every answer re-draws and equality
reveals nothing. An in-place edit that adds no row with a larger identifier,
without an epoch bump, re-draws only the statistics it changed: repeating a
question across it shows whether that statistic changed, never by how much.
There is no finite cumulative budget or full temporal-transcript DP claim.
Distinct requests, database versions and public rotations compose; admission
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
every integer output is at most exp(epsilon). Histogram coordinates use the
person-level L1 bound; mean and rate components retain their sequential
half-epsilon allocations. Noise is added using arbitrary-precision integers,
then clipped to the existing public bounds (normally [0, 2^53-1]) before
conversion back to an R number. Clipping is post-processing and preserves the
privacy inequality.

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
