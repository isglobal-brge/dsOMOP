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

The snapshot is a separate SHA-256 fingerprint of that resource identity and
exactly one `cdm_source` row's `cdm_source_name`, `cdm_release_date`, `cdm_version`
and `vocabulary_version`, converted to text. All four fields must be present
and non-empty. Missing, unreadable or ambiguous metadata, an in-memory database,
or unavailable database coordinates fail closed with instructions to configure
both identifiers. No generic shared fallback domain is used. Initialize the
resource with `omopInitDS()` before calling DP status when using derived defaults.

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
does not supply a resource identity, metadata, or bypass policy checks.

The mechanism remains person-bounded sticky discrete Laplace, with default
release epsilon 0.1, maximum epsilon 8 and delta 0. Contribution and level caps
are unchanged. Resource domains separate HMAC subkeys; both identifiers enter
the policy hash and release context. Equal canonical queries on distinct
resources therefore use different PRF keys. Discrete noise draws can coincide
by chance; independence does not imply unequal numeric outputs on every call.

The protected-statistic fingerprint remains in sticky release keys, preventing
noise cancellation when bounded statistics change under stale metadata.
This is a per-release guarantee, with no finite cumulative privacy budget.
See README.md for the complete deployment, root lifecycle and composition
contract. The QueryLibrary sticky catalogue is public metadata and remains
available before DP initialization; executing a release still validates policy.
