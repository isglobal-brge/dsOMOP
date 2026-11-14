# Numeric CDM fixture

Source: [OHDSI EunomiaDatasets / Synthea27Nj, CDM 5.4](https://github.com/OHDSI/EunomiaDatasets/tree/3d349dcdd79cc346b8e36eb837e49feffe7f7497/datasets/Synthea27Nj),
[Apache-2.0](https://github.com/OHDSI/EunomiaDatasets/blob/3d349dcdd79cc346b8e36eb837e49feffe7f7497/LICENSE).
These are synthetic clinical records with actual numeric lab values, not real patients.
Despite the dataset name the archive contains **28 persons**. The pinned archive is
3,351,707 bytes, SHA-256 `cdd3971be325e17b0e872bf4560711ab4c346917ecb079e2faf32badb55cb693`.
Its 10,040 measurements include 9,107 non-null numbers (90.707%) and 8,403 with
both a number and a nonzero mapped unit (83.695%). There are no reference ranges.

It is small enough to load quickly and has repeated hemoglobin measurements
(108 rows, all 28 people, concept 3000963, g/dL unit 8713), alongside glucose,
HbA1c, cholesterol and vital signs. This makes it preferable to larger 10k-person
archives for deterministic numerical checks and book examples. GiBleed remains
useful for other examples but has no numeric measurement values.

Load on the R test pod (DBI, RPostgres, duckdb, openssl, jsonlite):

```sh
Rscript tests/fixtures/numeric/load.R /tmp/numeric-data
```

The default PostgreSQL target is host `omopdb`, database `omop`, user/password
`omop`/`omop`, schema `numeric_cdm`, with TLS disabled. Override connection
settings with `PGHOST`, `PGPORT`, `PGDATABASE`, `PGUSER`, `PGPASSWORD`.
The DuckDB file is `/tmp/numeric-data/numeric_cdm.duckdb`.
Set `NUMERIC_POSTGRES=0` or `NUMERIC_DUCKDB=0` to omit one backend.
Repeated loads replace only the imported tables in that schema/file. The manifest
records the pin, license, source row counts and measured coverage. CSV extraction
is temporary; neither CSVs nor binary databases belong in git.

Enable tests with `DSOMOP_TEST_NUMERIC_CDM=/tmp/numeric-data`. Once enabled,
missing files, backends, Arrow, or dsBase dependencies **fail**, rather than skip.
The client requires the matching server source as a sibling worktree. Server
checks use both databases; client checks use three disjoint PostgreSQL views,
partitioned by person ID into 9/10/9 people, and real DSLite DataSHIELD sessions.

`matrix.R` contains native plans and an independent base-R oracle over imported
source rows, ordered by measurement date then measurement ID. Test-only range
views supply 12 and 15 g/dL limits to exercise both abnormal tails; imported rows
remain unchanged. These limits are fixture values, not clinical guidance.
The conversion tests declare g/dL -> mg/dL (factor 1000) and verify that value
predicates and reference ranges use the converted units.

Native `sparse` supports binary/count analyses. Its numeric counts are checked;
numeric lab means/last values use the supported `temporal_covariates` format.
The tests assert the native sparse refusal as well as that supported route.
`baseline` and `features` are tested through their public client aliases to wide;
survival covariates are a native joined output with private wide inputs.

The matching Opal harness has `scripts/numeric/load.R` (same loader) and scenario
N, registering `omop_data.numeric_cdm` as the second resource. The coordinator
runs real Opal workflows after integration; local DSLite evidence is not an Opal pass.
