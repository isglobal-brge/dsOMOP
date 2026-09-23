# Differential-privacy release channel

The DP release channel is enabled by default since dsOMOP 2.6.0. Custodians
can opt out with `options(dsomop.dp.enabled = FALSE)` or `DSOMOP_DP_ENABLED=0`.
Explicit option and environment settings must agree. The custodial option
`dsomop.dp.exclusive` defaults to `FALSE` for compatibility. **Set
`options(dsomop.dp.exclusive = TRUE)` in production** to make the typed DP
channel exclusive for standard statistical releases while DP is enabled.
The DataSHIELD profile fallback is `default.dsomop.dp.exclusive = FALSE`;
custodians can set it to `TRUE` instead. Analysts cannot set this policy
through a dsOMOP endpoint.

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
For memory-mode plans, first set
`plan <- ds.omop.plan.options(plan, factor_concepts = FALSE)`, then call
`ds.omop.plan.execute(plan)` to skip automatic observed factor-level discovery.
Supply public DP categories, bounds and breaks directly
in the typed DP specification; the standard profiling endpoints cannot be used
to learn them in exclusive mode. The seven supported typed DP statistics
remain available with identical sticky releases in either mode. Older
dsOMOPClient 2.7.2 already preserves the new status field and displays it in its
ordinary list output; no client upgrade is required.

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
