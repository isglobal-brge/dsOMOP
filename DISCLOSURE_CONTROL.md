# Differential-privacy release channel

The DP release channel is enabled by default since dsOMOP 2.6.0. Custodians
can opt out with `options(dsomop.dp.enabled = FALSE)` or `DSOMOP_DP_ENABLED=0`.
Explicit option and environment settings must agree. Ordinary aggregates keep
their existing disclosure contracts; analysts request DP releases explicitly.

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
across data reloads lets an analyst difference releases across reloads; fallback
metadata cannot automatically track those changes. Each explicit identifier
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

A persistent private state directory is required for the default file-backed
noise root: mount `DSOMOP_STATE_DIR` (or `dsomop.state_dir`) across restarts and
replicas. The default `~/.dsomop` is suitable only when the home is persistent.
`DSOMOP_TEST_ALLOW_EPHEMERAL_STATE=1` permits temporary paths in tests only; it
does not supply a resource identity or bypass policy checks.

The mechanism remains person-bounded sticky discrete Laplace, with default
release epsilon 0.1, admitted range [1e-6, 8] and delta 0. Contribution and level
caps are unchanged. Resource domains separate HMAC subkeys; both identifiers enter
the policy hash and release context. Equal canonical queries on distinct
resources therefore use different PRF keys. Discrete noise draws can coincide
by chance; independence does not imply unequal numeric outputs on every call.

The protected-statistic fingerprint remains in sticky release keys, preventing
noise cancellation when bounded statistics change under stale metadata.
This is a per-release guarantee, with no finite cumulative privacy budget.
See README.md for the complete deployment, root lifecycle and composition
contract. The QueryLibrary sticky catalogue is public metadata and remains
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
No release history or random-generator state is persisted.

With independent random bits the implemented mechanism has the exact ideal
distribution and pure differential privacy (delta 0). The deployed keyed
stream makes the corresponding cryptographic pseudorandomness assumption
about HMAC; a finite secret key is not an information-theoretic source of
infinitely many independent bits. The per-release scope and composition
limitations above are unchanged.

The mechanism identifier is `dsomop-sticky-discrete-laplace-prf-v2`, and the
hashed policy schema changes from 2 to 3. Both the public sampler and mechanism
metadata distinguish old and repaired releases. The release-envelope protocol
`dsomop-dp-release-v2`, canonical-JSON protocol, semantic-release protocol,
privacy-contract label and privacy-guarantee label keep their existing versions
because their field structure and semantics are unchanged. dsOMOPClient 2.7.2
already accepts these new sampler/mechanism strings and delta 0, verifies
payloads against preflight metadata, and rejects mixed versions across selected
servers. No client update is required.

Changing the sampler changes the policy hash and sticky release identity, so an
upgraded query can receive a new draw. All replicas of one logical node must
use the same version. This repair does not retroactively protect old releases
or erase their contribution to privacy loss.
