# dsOMOP

## Introduction

<img src="man/figures/dsomop_logo.png" align="left" width="110" style="margin-right: 10px;" />


The `dsOMOP` package is designed to facilitate interaction with remote databases formatted in the [Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM)](https://www.ohdsi.org/data-standardization/) from within a [DataSHIELD](https://www.datashield.org/about/) environment. It provides reviewed, typed operations for fetching and transforming data into objects usable by server-side DataSHIELD analyses. Privacy depends on the deployed method allowlist, server options, database privileges and the contract of each operation; installing the package alone is not a blanket disclosure-safety guarantee.

Key features of the `dsOMOP` package include:

- **Typed extraction and transformation:** Reviewed plan, recipe and catalog
  operations for selected OMOP tables, filters and output grains. The package
  deliberately does not expose arbitrary client-authored SQL or joins.
- **DataSHIELD disclosure controls:** Applies distinct-person admission, small-cell suppression, count banding and server-owned policy checks on the reviewed routes that declare those contracts.
- **Support for database interaction:** Includes methods for inspecting table,
  column and concept catalogs and for filtering through typed, reviewed
  criteria. The available operations are intentionally narrower than the full
  relational capabilities of the underlying database.

## Structure

The `dsOMOP` ecosystem comprises two essential components designed to work in tandem: the server-side package (`dsOMOP`) and the client-side package (`dsOMOPClient`). Each component plays a pivotal role in the integration of OMOP CDM databases within the DataSHIELD environment. For comprehensive details on installation, setup, and usage, please refer to the respective repositories:

- **Server-Side package `dsOMOP`**: This component is installed on the DataSHIELD server and is responsible for direct interactions with the OMOP CDM databases. It retrieves and transforms data into protected server-side objects for explicitly reviewed DataSHIELD methods. For code, installation instructions, and more, visit [https://github.com/isglobal-brge/dsOMOP](https://github.com/isglobal-brge/dsOMOP).

- **Client-Side package `dsOMOPClient`**: Utilized by researchers and data analysts, this package facilitates the communication with the `dsOMOP` package on the server. It sends data requests and receives processed data for analysis, ensuring a user-friendly experience for specifying data needs and analysis parameters. For code, installation instructions, and more, visit [https://github.com/isglobal-brge/dsOMOPClient](https://github.com/isglobal-brge/dsOMOPClient).

## Installation

To install the server-side package `dsOMOP`, follow the steps below. This guide assumes that you have administrative access to the DataSHIELD server and the necessary permissions to install R packages.

### Prerequisites

Use the most restrictive DataSHIELD privacy-control level that supports the
reviewed methods deployed at your site. `dsOMOP` does **not** require the global
privacy level to be changed to `permissive` (or `banana`) for wide outputs.
Weakening that global setting would also affect unrelated DataSHIELD packages
and can invalidate the assumptions made by `dsOMOP`'s own controls.

The server administrator should instead:

- allowlist only the `dsOMOP` methods that have been reviewed for the local
  deployment;
- configure the DataSHIELD and `dsomop.*` thresholds through the installed
  package's `DESCRIPTION` fields or the server administration panel
  (`DATASHIELD` is the source-tree mirror and is not shipped in tarballs);
- give the ordinary analysis database identity read access to the CDM and only
  the minimum temporary-table permissions required by the selected backend;
- keep persistent cohort writes disabled unless a separately reviewed results
  schema and database role are configured.

### Persistent server identity

Set `DSOMOP_STATE_DIR` to an absolute, owner-only directory on a durable volume
shared by every worker that represents the same logical node. `configure` only
prepares directories; it never generates secrets while installing a package or
building a reusable image. Package `.onLoad()` also never reads or generates
key material: it validates only the public lifecycle settings. On first use by
an actual OMOP handle, compatibility mode validates an injected secret or, when
none is supplied, creates and atomically persists a 256-bit pseudonym root under
`$DSOMOP_STATE_DIR/secrets` (mode `0600`, parent mode `0700`). Install and
development/image loads therefore cannot bake a node identity into an image.

Choose the lifecycle provider explicitly in deployed services with
`DSOMOP_PSEUDONYM_PROVIDER=file`, `injected`, or `scoped`. The default `auto`
uses a root or resource-scoped key when configured and otherwise creates the
durable file-backed root. `file` rejects simultaneously configured injected
keys; `injected` requires
`DSOMOP_PSEUDONYM_ROOT`; and `scoped` requires exact
`DSOMOP_PSEUDONYM_KEY_<resource-hash>` values. Environment variables and the
equivalent `dsomop.pseudonym_*` R options may not disagree. For production file
custody, set `DSOMOP_PSEUDONYM_REQUIRE_EXISTING=true` after provisioning the
root. If the volume is absent or empty, first use then fails before creating a
directory, lock, or replacement key. The file provider also requires atomic
same-directory hard links for its no-replace commit; use `injected` or `scoped`
custody when the shared filesystem cannot provide that primitive.
The bootstrap also requires a successful durability sync of the candidate key
before commit and of the containing directory after the no-clobber commit. A
missing or failing sync primitive is an error: dsOMOP does not return key
material whose persistence across a crash has not been established.

Alternatively inject the root through the service secret manager as
`DSOMOP_PSEUDONYM_ROOT`; use either 64 hexadecimal characters (32 bytes) or a
non-hex text secret of at least 32 bytes. It is validated on first handle use
and is not copied to disk. Inject it into every service worker and restart, not
only into the `R CMD INSTALL`/`configure` environment. A different key is
derived for every OMOP resource to prevent cross-resource linkage. A scoped
`DSOMOP_PSEUDONYM_KEY_<resource-hash>` remains available when a custodian needs
an exact per-resource key. Here `<resource-hash>` is the first 32 lowercase hex
characters of SHA-256 over the UTF-8 stable resource identity (normally the
resource URL, then its name/server fallback). Injected exact keys are likewise
never copied into the dsOMOP state directory. Prefer the root setting unless
there is a deliberate need to manage each resource key independently.

For token compatibility, the historical `DSOMOP_PSEUDONYM_KEY` setting retains
its old meaning: one exact global token key. It is not resource-separated and
therefore permits cross-resource linkage. It is rejected by default, including
when a stale legacy setting coexists with a root. A controller may temporarily
enable it only through the explicit administrative opt-in
`DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS=true` (or
`options(dsomop.allow_legacy_global_pseudonyms = TRUE)`) while completing a
reviewed migration. The capability report exposes this opt-in as
`pseudonymization$legacy_global_opt_in`; migrate deliberately to
`DSOMOP_PSEUDONYM_ROOT` when invalidating/reissuing existing tokens is
acceptable. Existing valid per-resource files under the selected state root are
retained. If the state directory itself is moved, the custodian must move its
`keys/` and `secrets/` contents with it rather than expecting an automatic search
of old paths.

The same rule applies to historical serialized handles that contain raw
`handle$person_key` bytes. They are rejected by default, and malformed/weak
legacy material is always rejected: the compatibility path requires exactly 32
raw bytes plus the explicit legacy-global administrative opt-in above. dsOMOP
cannot safely infer the original resource scope from those bytes, so it never
silently migrates such a handle; recreate it from its resource when that scope
cannot be proven.

Back up the state volume together with the service configuration. Deleting or
rotating the pseudonym root invalidates tokens stored in live/saved DataSHIELD
objects; replicas with different roots will also produce incompatible join
keys. The per-resource derivation includes the stable resource URL/name, so a
rename or repointing also needs an explicit token-migration decision. The
default `~/.dsomop` is persistent only if that home directory itself survives
container or pod replacement.

Raw key bytes are not cached in the DataSHIELD session handle, so saving a
workspace does not copy the pseudonymization secret into that workspace. The
injected secret or durable state volume must still be available when the
workspace is restored. The capability report exposes only a non-secret,
resolved-key `key_id`, provider, and positive integer epoch so workers and
artifacts can reject incompatible tokens without disclosing the root. Configure
`DSOMOP_PSEUDONYM_EPOCH` (default `1`) identically on all replicas, and advance
it only as part of a coordinated key rotation. The epoch is a lifecycle marker,
not a key derivation or rotation mechanism by itself. Live/restored handles pin
provider, `key_id`, epoch, and require-existing policy and fail closed if that
contract changes.

Every person-bearing in-memory `omop.table` also carries a public, non-secret
`dsomop_pseudonymization` contract (token protocol, key-contract version,
`key_id`, epoch and resource-scope flag). Unary manipulation preserves and
validates it. `omopMergeDS()` and `omopBindRowsDS()` require exact equality, so
tables from different keys, epochs, protocols or scopes cannot be combined.
Older workspace objects lacking the contract fail closed and must be recreated;
matching token-looking strings are not treated as proof of compatibility.

This is pseudonym identity state, not a differential-privacy noise or sticky
noise root. No privacy budget or DP ledger is implemented by this lifecycle.
`nfilter.noise` is DataSHIELD's minimum Gaussian plot-noise variance fraction;
it is neither replay-stable nor an epsilon/delta guarantee, and dsOMOP does not
silently apply it to unrelated query results. The current capability report therefore sets
`formal_dp_enabled`, `sticky_noise_enabled` and `privacy_ledger_enabled` to
`FALSE`; QueryLibrary candidates that need formal DP remain blocked pending a
separate bounded-sensitivity mechanism and durable composition ledger.

The DataSHIELD method allowlist is part of the security boundary. An
`omop.table` class or a `dsomop_protected` attribute protects calls handled by
`dsOMOP`; it cannot make arbitrary methods from other server packages safe.
Review the complete server method allowlist together with the effective privacy
options before exposing a resource to analysts.

Never register `c=base::c` or `list=base::list` as **aggregate** methods. In
particular, an aggregate `list` alias lets a caller evaluate `list(x)` and
return an arbitrary server object without passing through a dsOMOP disclosure
gate. dsOMOP transports multiple cohort references through bounded scalar
`scope_cohort_1`, `scope_cohort_2`, ... arguments and multiple analysis tables
through strictly typed `scope_table_1`, `scope_table_2`, ... arguments instead.
An assign-only constructor can be reviewed separately because it does not
itself return its result, but dsOMOP does not require generic constructors for
this workflow.

This check must cover the effective allowlist for the whole DataSHIELD server,
not only dsOMOP's manifest. Installing dsFlower, dsVert, or any other package
that exposes either generic constructor in aggregate mode reopens the same
server-wide exfiltration path even though dsOMOP omits it. Remove those aliases
from the package/server configuration and verify the effective registry before
granting analyst access.

Third-party analysis packs are a separate administrator trust boundary and are
disabled by default. To enable one, set an exact package/version pin, for
example `options(dsomop.analysis_pack_allowlist = c(myPack = "1.2.3"))`.
Discovery ignores every unpinned or version-mismatched package. Enabled entries
must declare a closed versioned output contract; both raw and final aggregate
or assign results pass the external-pack firewall. This controls catalog
outputs, but does not sandbox deliberately malicious R code installed with the
server process's own privileges.

### Package installation

If you prefer using a graphical user interface (GUI) provided by your server for package installation, you can easily install the `dsOMOP` package directly from GitHub. Navigate to the package installation section in your server's GUI, and specify the following details:

- **User/organization:** `isglobal-brge`
- **Package name:** `dsOMOP`
- **Git reference:** `2.0.0`

#### Installing from the R console

If you are using an Opal server and have access to an administrator account, you can install the package from the R console using the `opalr` package. If you do not have the `opalr` package installed, you can install it using the following command:
```R
install.packages("opalr")
```

To create a login object for the server, change the following code to match your specific server details and administrator credentials:
```R
library(opalr)

# Change the URL and credentials to match your Opal server and administrator account!
o <- opal.login(username = "administrator", password = "password", url = "https://opal-demo.obiba.org/")
```

You can then install the `dsOMOP` package using the following command:
```R
dsadmin.install_github_package(o, 'dsOMOP', username='isglobal-brge', ref='2.0.0')
```

## Creating OMOP CDM resources

The resources used by dsOMOP are of the type `omop.dbi.db`, which hold the connection details to OMOP CDM databases. Each declared database engine has its own resource factory: `postgresql`, `mysql`, `mariadb`, `sqlserver`, `synapse`, `pdw`, `oracle`, `redshift`, `bigquery`, `snowflake`, `spark`, `databricks`, `sqlite` or `duckdb`.

These resources contain the following parameters:
- `host`: The hostname or server/account identifier. For BigQuery this field is
  the GCP project ID.
- `port`: The port number on which the database server is listening.
- `database`: The database name (the BigQuery dataset). For file-based engines
  (SQLite, DuckDB) this is the path to the database file, and `host`/`port` are
  omitted.
- `cdm_schema` (optional): The schema holding the OMOP CDM tables. Defaults to the engine default (for PostgreSQL, `public`).
- `vocabulary_schema` (optional): The schema holding the vocabulary tables. Defaults to the CDM schema.
- `results_schema` (optional): The schema holding cohorts, Achilles and other
  OHDSI result tables. Auto-detection is limited and should be pinned when the
  deployment uses a separate results daimon.
- `warehouse` / `driver` (backend-specific, optional): Snowflake warehouse or
  an ODBC driver-name override.

Where the backend uses username/password authentication, credentials are not URL
parameters: they are supplied separately as the resource identity and secret.
BigQuery and file-backed engines use the service account's configured cloud or
filesystem identity instead.

To configure a resource for dsOMOP, ensure that you have the above details accurately filled out to establish a successful connection to your OMOP CDM database.

### Database support contract

The factories above are connector and SQL-adapter contracts, not a claim of
feature parity on every vendor/version. The current implementation uses DBI
directly; it does **not** call OHDSI `DatabaseConnector` or `SqlRender` at
runtime. Its local SQL translator covers the SQL patterns currently emitted by
dsOMOP (principally `TOP` and `DATEADD`), not the complete SqlRender grammar.

| Backend | R driver | Metadata path | Cross-statement temporary object | Verification in this repository |
|---|---|---|---|---|
| PostgreSQL | RPostgres; RPostgreSQL fallback | `information_schema` | session temp table | SQL contract tests only |
| SQLite | RSQLite | SQLite catalogs, including attached schemas | session temp table | executable integration tests |
| DuckDB | duckdb | DBI/`information_schema` | session temp table | optional executable tests when installed |
| MySQL / MariaDB | RMariaDB | `information_schema` | session temporary table | SQL contract tests only |
| SQL Server / Synapse / PDW | odbc | `information_schema` | unavailable for dsOMOP cross-statement recipes | SQL contract tests only |
| Oracle | ROracle or odbc | Oracle catalogs | unavailable for dsOMOP cross-statement recipes | SQL contract tests only |
| Redshift | RPostgres | `information_schema` | session temp table | SQL contract tests only |
| BigQuery | bigrquery | dataset-qualified `INFORMATION_SCHEMA` | unavailable for dsOMOP cross-statement recipes | SQL contract tests only |
| Snowflake | odbc | `information_schema` with Snowflake casing | session temp table | SQL contract tests only |
| Spark / Databricks | odbc | `SHOW TABLES` / `DESCRIBE TABLE` | session temporary view | SQL contract tests only |

No network/vendor backend currently runs in live CI. A site must therefore test
its exact driver, authentication, catalog/schema layout, permissions and server
version before production use. Operations that need a temporary cohort or
working relation fail explicitly on backends whose safe cross-statement
materialisation is not implemented; simple metadata or single-statement
queries may still work. The `database_support` element returned in the server
capabilities reports this adapter profile.

### OHDSI alignment and interoperability

The implementation follows OMOP table/field semantics and uses the following
OHDSI assets, but "aligned" does not mean that every HADES package is embedded:

- OMOP CDM metadata are vendored from official `CommonDataModel` releases:
  [v5.3.2 commit `dd85c0d30bb3dd4bd16c5dbef7dbf9dd93075fa2`](https://github.com/OHDSI/CommonDataModel/commit/dd85c0d30bb3dd4bd16c5dbef7dbf9dd93075fa2)
  and [v5.4.2 commit `aa047a3c620b5c842b4370a0c965e2aa72203b1d`](https://github.com/OHDSI/CommonDataModel/commit/aa047a3c620b5c842b4370a0c965e2aa72203b1d).
  `inst/ohdsi/UPSTREAM_METADATA.json` pins each CSV SHA-256. If
  `CommonDataModel` is installed, its published CSV metadata can supply another
  explicitly supported version. An unknown version fails closed. Non-standard
  tables/columns are invisible unless the controller lists them in
  `dsomop.allowed_cdm_extensions`.
- The package can consume standard cohort/result tables and selected Achilles
  results. Some catalog analyses are local ports with OHDSI-inspired IDs or
  output shapes; they are not calls into Achilles, CohortMethod,
  FeatureExtraction or other HADES packages.
- Circe/ATLAS JSON interoperability is a documented subset implemented by
  dsOMOPClient. `CirceR` and `CohortGenerator` are not runtime dependencies, and
  the package does not promise arbitrary Circe expression execution. Age
  criteria deliberately use the same annual expression as OHDSI Circe
  (`YEAR(event_date) - person.year_of_birth`), rather than silently inventing a
  birthday from nullable OMOP month/day fields; see the audited Circe-be source
  at [commit `498893689a9cf4f09c2a43cc893bb01116db7184`](https://github.com/OHDSI/Circe-be/blob/498893689a9cf4f09c2a43cc893bb01116db7184/src/main/java/org/ohdsi/circe/cohortdefinition/builders/ConditionOccurrenceSqlBuilder.java#L173-L176).
- Sparse and temporal-covariate outputs use a FeatureExtraction-style shape,
  including reference tables, but are not FeatureExtraction `CovariateData`
  objects and do not implement its complete settings/covariate universe.
- FeatureExtraction analysis IDs and concept/code references used by local score
  adapters are audited against
  [v3.14.0 commit `53266f0233c2ee7cae127e8669ad35b0d60406ae`](https://github.com/OHDSI/FeatureExtraction/commit/53266f0233c2ee7cae127e8669ad35b0d60406ae).
  The adapters are explicitly not upstream-equivalent: they use
  `condition_occurrence`, local vocabulary/descendant rules and dsOMOP date
  semantics instead of FeatureExtraction's `condition_era`, cohort-relative
  windows and index-specific inclusion/exclusion SQL. The analysis-catalog
  comorbidity entry is a one-point-per-component burden approximation, not a
  published Charlson/DCSI/CHADS2/CHA2DS2-VASc/HFRS implementation.
- Standard concept IDs follow OMOP vocabulary semantics, but dsOMOP does not
  make different site vocabulary snapshots equivalent. Descendant/mapped
  expansion and local mappings must be versioned and compared for a federated
  phenotype.
- The Query Library is a local curated catalog, not a vendored or automatically
  synchronized copy of OHDSI QueryLibrary. A machine-readable per-query audit
  pins [upstream commit `df8a21074b08519e581ca1afb7510468538117a4`](https://github.com/OHDSI/QueryLibrary/commit/df8a21074b08519e581ca1afb7510468538117a4),
  file hashes, dependencies, output risks and redesign triage so drift is
  detectable; it does not enable any upstream query or imply local template
  lineage. See `inst/queries/QUERY_REFERENCE.md` for provenance and the backlog.

### Staged outputs

`output_mode = "staged"` stores outputs in a private server-local directory and
assigns descriptors instead of final in-session data frames. Arrow produces
Parquet; without Arrow the fallback is CSV. Only long, untranslated event
outputs stream through bounded chunks today. Wide, feature, baseline, survival
and composite outputs are materialised in R before being written, so staged
mode does not make every format constant-memory.

The path is readable only by the server OS identity (directory `0700`, files
`0600`). A descriptor is neither a client download URL nor a general export
permission. Another package running as the same OS account may implement the
reviewed descriptor contract; it should resolve files through
`omopStagedDatasetPath()` rather than trust the embedded path. That resolver
checks token confinement, type, expiry, owner-only permissions and the optional
non-secret pseudonym-key ID/epoch (required for person-bearing files). Every v2
plan descriptor must also carry a canonical component semantic contract (output
type/format/grain plus age/date semantics) and an output-level bundle contract.
The bundle binds sibling files to the same logical output and high-entropy
staging token without incorrectly requiring `personRef`, reference and sparse
components to have the same shape. The resolver accepts
`expected_semantic_contract` for exact same-shape comparisons and
`expected_bundle_contract` for compatible sibling comparisons; either mismatch
fails before a consumer opens the file. Comparing those contracts and key
identity prevents invalid combinations after policy, key or replica changes. A
different service account needs a separately reviewed broker.
Person-bearing staged output requires a resource-scoped provider; the deprecated
global legacy key is retained for historical in-memory tokens but is rejected
at this broader interoperability boundary.
The owner-only v2 `manifest.json` stores complete descriptors (including token,
origin, pseudonymization, component semantics and bundle semantics) with
explicit JSON nulls. Internal `.readStagingManifest()` reconstructs and resolves
each descriptor through the same validation path, rather than treating a
partial manifest projection as an authority.
Row/byte/output/directory limits and TTL cleanup reduce resource exhaustion, but
production deployments still need filesystem quotas and a controller-owned
periodic cleanup job.

Extraction shape is also bounded before expensive R-side expansion. The
server defaults are `dsomop.max_feature_specs = 1000`,
`dsomop.max_pivot_concepts = 1000`, `dsomop.max_output_columns = 5000` and
`dsomop.max_temporal_bins = 10000`, plus filter depth/nodes/values of
32/1,024/10,000, 100 outputs per plan and 256 live temporary tables per handle.
They bound explicit features, wide/sparse/temporal expansion, recursive filter
work, multi-output amplification and session-owned database state. These are
controller-configurable operational resource caps, not statistical disclosure
parameters. A federated request uses the lowest compatible advertised plan cap;
each server independently enforces its handle-state cap.

### Creating resources from GUI

dsOMOP modifies the interface provided by the Opal server to incorporate an option in its resource creation panel dedicated to OMOP CDM databases. This allows users to easily configure and manage resources specifically designed for OMOP CDM databases directly from the GUI.

To use it, simply access the management dashboard of a project and go to the `Resources` subsection. In the `Add Resource` option, you will find the `OMOP CDM` category:

<p align="center">
  <img src="man/figures/add_resource.png" alt="Add Resource">
</p>

### Creating resources from the R console

If you are using an Opal server and have access to an administrator account, you can create the OMOP CDM resources from the R console using the `opalr` package. If you do not have the `opalr` package installed, you can install it using the following command:
```R
install.packages("opalr")
```

To create a login object for the server, change the following code to match your specific server details and administrator credentials:
```R
library(opalr)

# Change the URL and credentials to match your Opal server and administrator account!
o <- opal.login(username = "administrator", password = "password", url = "https://opal-demo.obiba.org/")
```

You can then use the following function to create an OMOP CDM resource. The `factory` argument selects the database engine (here `postgresql`):
```R
opal.resource_extension_create(o,
  project = "my_project",
  name = "my_resource",
  provider = "dsOMOP",
  factory = "postgresql",
  parameters = list(
    host = "localhost",
    port = 5432,
    database = "my_database",
    cdm_schema = "cdm",
    vocabulary_schema = "vocab"
  ),
  credentials = list(
    username = "my_username",
    password = "my_password"
  )
)
```

## Extending backend support

Adding a DBMS is not only a resource-factory change. A production adapter needs
a reviewed connection path, identifier/schema qualification, metadata discovery,
SQL translation, date arithmetic, temporary-object semantics, reconnect tests
and executable integration tests. Until that full contract exists, an adapter
should be described as experimental rather than as supported feature parity.

## Acknowledgements

- The development of dsOMOP has been supported by the **[RadGen4COPD](https://github.com/isglobal-brge/RadGen4COPD)**, **[P4COPD](https://www.clinicbarcelona.org/en/projects-and-clinical-assays/detail/p4copd-prediction-prevention-personalized-and-precision-management-of-copd-in-young-adults)**, **[CADSET](https://www.ersnet.org/science-and-research/clinical-research-collaboration-application-programme/cadset-chronic-airway-diseases-early-stratification/)**, and **[DATOS-CAT](https://datos-cat.github.io/LandingPage)** projects. These collaborations have not only provided essential financial backing but have also affirmed the project's relevance and application in significant research endeavors.
- This project has received funding from the **[Spanish Ministry of Education, Innovation and Universities](https://www.ciencia.gob.es/en/)**, the **[National Agency for Research](https://www.aei.gob.es/en)**, and the **[Fund for Regional Development](https://ec.europa.eu/regional_policy/funding/erdf_en)** **(PID2021-122855OB-I00)**. We also acknowledge support from the grant **CEX2023-0001290-S** funded by **MCIN/AEI/10.13039/501100011033**, and support from the **[Generalitat de Catalunya](https://web.gencat.cat/en/inici/index.html)** through the **[CERCA Program](https://cerca.cat/en/)** and the **Consolidated Group on HEALTH ANALYTICS (2021 SGR 01563)**.
- Additionally, this project has received funding from the **[Instituto de Salud Carlos III (ISCIII)](https://www.isciii.es/)** through the project **"PMP21/00090,"** co-funded by the **[European Union's](https://european-union.europa.eu/index_en)** **Resilience and Recovery Facility**. It has also been partially funded by the **"Complementary Plan for Biotechnology Applied to Health,"** coordinated by the **[Institut de Bioenginyeria de Catalunya (IBEC)](https://ibecbarcelona.eu/)** within the framework of the **Recovery, Transformation, and Resilience Plan (C17.I1)** – Funded by the **[European Union](https://european-union.europa.eu/index_en)** – **[NextGenerationEU](https://next-generation-eu.europa.eu/index_en)**.

- This project **(PID2021-122855OB-I00)** has received funding from **MCIN /AEI /10.13039/501100011033 / FEDER, UE**.

<p align="center">
  <img src="man/figures/ciencia.png" alt="MCIN/AEI/FEDER" height="80">
</p>

## Contact

For further information or inquiries, please contact:

- **Juan R González**: juanr.gonzalez@isglobal.org
- **David Sarrat González**: david.sarrat@isglobal.org

For more details about **DataSHIELD**, visit [https://www.datashield.org](https://www.datashield.org).

For more information about the **Barcelona Institute for Global Health (ISGlobal)**, visit [https://www.isglobal.org](https://www.isglobal.org).
