# dsOMOP 2.10.2

- Reads every OMOP CDM version label. `cdm_source.cdm_version` values with a
  patch number ("v5.3.1", as in Eunomia GiBleed and OHDSI Broadsea's demo CDM,
  or "5.4.2") use their minor version's specification, as the OHDSI tools read
  them; 2.10.1 and earlier refused such databases with "No supported OHDSI CDM
  specification is available for version 'v5.3.1'". A label with no available
  specification (an older 5.x, a custom label, 6.0 without CommonDataModel) is
  read with the specification its table structure matches (5.3 or 5.4), and an
  empty label is inferred from the structure.

# dsOMOP 2.10.1

Plug and play on Opal, Armadillo and DSLite: install, create a resource,
connect.

- No database connection requires TLS any more. A resource that leaves its
  TLS settings empty connects the way the driver does by default, as dsOMOP
  1.x did: PostgreSQL receives no `sslmode` (libpq's "prefer": TLS only when
  the database offers it), MySQL/MariaDB do not require TLS, and Microsoft's
  SQL Server ODBC drivers get `Encrypt=no` and `TrustServerCertificate=yes`.
  2.10.0 required verified TLS (`verify-full`) for every database that was not
  on localhost, so a database on the same machine or Docker network without
  TLS (OHDSI Broadsea's atlasdb, a plain postgres container) failed with
  "server does not support SSL, but SSL was required". A resource that names
  a TLS mode still gets exactly that mode.
- Resources created with the dsOMOP 1.x form (format `omop.cdm.db`,
  `postgresql://host:port/db//dsomop::/schema:cdm/vocabulary_schema:vocab`,
  also mysql and mariadb) open again: the resolver rewrites their URL into the
  2.x form and they are validated and connected exactly like 2.x resources.
  The resource and its pseudonymization identity are unchanged. The resource
  form still only creates 2.x resources (`omop.dbi.db`).
- A resource that names no CDM schema uses the engine's default schema when
  that holds the `person` table, and otherwise the single schema that does
  (Broadsea keeps its CDM in `demo_cdm`); the vocabulary follows, or the single
  schema holding `concept`. Several candidates are never guessed between: the
  error lists them so the data owner can name one.
- Upgrading on Opal: Opal publishes a package's DataSHIELD methods when it is
  first installed, not when an installed package is upgraded. After upgrading
  dsOMOP, publish its settings again (Administration > DataSHIELD > Packages >
  dsOMOP > Publish) or analysts get "No such DataSHIELD 'ASSIGN' method with
  name: omopInitDS". dsOMOPClient 2.10.1 reports this case by name
  (`ds.omop.server.version()`).

# dsOMOP 2.10.0

- Seed-only DP: the first-answer store of 2.7.1 to 2.9.x is removed. Every
  answer is a deterministic function of the secret noise root, the canonical
  question, the protected statistic and a private automatic database version
  (the largest identifier of each present clinical table, keyed with the root
  and never released). Nothing but the root is stored: no store, capacity
  limit, lock or scaling cost. Answers are identical after restarts and on
  every R server that shares the root (`DSOMOP_DP_NOISE_ROOT` /
  `dsomop.dp.noise_root`), and a reload that adds rows re-draws every answer
  at once, without an epoch bump. In-place edits that add no row with a larger
  identifier still need an epoch bump (README, isglobal-brge/dsOMOP#20).
- Status reports `release_binding = "seed_data_versioned_v1"`,
  `persistent_state = "noise_root_only"`, `service_capacity = "none"`,
  `history_dependent = FALSE` and privacy contract
  `fixed_per_release_seed_data_versioned_v1` (protocol `dsomop-dp-release-v3`,
  sampler and mechanism unchanged). Explicit DP releases need dsOMOPClient
  2.10.0; the analysis-catalog DP questions work with any client.
- Upgrading re-draws previously asked questions once. Existing
  `release-bindings/` files are no longer read and can be archived.
  `omopInitializeReleaseStore()` is deprecated and does nothing; the options
  `dsomop.dp.release_store_id` and `dsomop.dp.release_store_bytes` and their
  DataSHIELD profile defaults are removed.
- The noise root is created on first use with a custodian message, and a
  damaged, unsafe or substituted root fails closed (one stable analyst error,
  the reason in the server log) instead of being replaced.
- Fallback snapshot identities no longer warn: the database version tracks
  reloads.
- `omopDpReleaseDS()` binds the session's OMOP resource when no community
  tool has, so the database version is always available.

# dsOMOP 2.9.2

- Fix federated pooling of the reviewed QueryLibrary questions (2.9.0 and
  2.9.1): their pooling contracts declared every non-count column as a label,
  so a federation whose servers returned more than one row (every DP
  histogram and partition, and multi-row summaries) failed with "group keys
  are not unique". Concept, unit, currency, gender and visit ids, bins and
  partition cells are now grouping keys; names and bin bounds remain labels.
  Pooled DP counts are the sums of the per-server noisy counts.

# dsOMOP 2.9.1

- New community-tool assign method `omopDpPlanExecuteDS()`: prepares
  person-local tables for sticky DP releases from the same typed plans and
  recipes as `omopPlanExecuteDS()`. It runs the unchanged core plan engine and
  then seals each person-local output with the authenticated DP provenance
  `omopDpReleaseDS()` requires (canonical plan lineage and resource dataset
  identity). Ordinary loading with `omopPlanExecuteDS()` still never involves
  DP. It needs the community tools and an available DP service, creates
  in-memory tables only, and refuses outputs that are not person-local, with
  the reason (for example a baseline `age_at_index`, whose protection depends
  on other persons' ages: use an age at a public reference year instead).
  Filtering or otherwise manipulating a prepared table drops its provenance,
  so the release population must be defined in the plan.

# dsOMOP 2.9.0

- Automatic protection for the OHDSI community tools, with nothing to
  configure: the new custodial option `dsomop.tools.mode` (default `"auto"`).
  The core is unchanged and never uses DP.
- The 18 query-library questions that were blocked because they release exact
  person-level distributions are answered again, by reviewed server-owned
  replacements (their raw SQL still never runs; `person.load` stays blocked):
  - per-person durations and counts (`condition.duration_stats`,
    `drug.duration_stats`, `drug.days_supply_stats`,
    `drug_era.exposure_count_stats`, `condition_era.eras_per_person_stats`,
    `visit.duration_stats`, `observation_period.length_by_gender`) are released
    in auto mode as histograms of one value per person (the mean of the
    person's records, or their count) over fixed public bins, with sticky
    discrete-Laplace noise of sensitivity 1 person;
  - demographic distributions (`person.year_of_birth_distribution`,
    `person.birth_month_distribution`, `person.gender_by_age_decade`) as
    complete public partitions with the same noise;
  - values, quantities and costs (`measurement.value_stats`, now per unit;
    `drug.quantity_stats`; `drug_cost.*`, now per currency) and the
    all-concept or by-year length summaries use the standard person-supported
    summary (both the record and the person count must reach
    `dsomop.nfilter.dist`).
  When the DP service is disabled or unavailable, auto mode releases the same
  tables under the standard controls (`standard_fallback`). `"standard"` mode
  returns the original summaries under the standard controls only, and `"dp"`
  mode (also selected by `dsomop.dp.exclusive = TRUE` with DP enabled) refuses
  every standard statistic of the tools.
- Every analysis-catalog result carries the attribute `dsomop_protection`
  (`"standard"`, `"dp"` or `"standard_fallback"`, the estimand and, for DP,
  epsilon, sensitivity, adjacency and the noise standard deviation).
- Disclosure repairs in the community tools:
  - released rows are ordered from released (banded) values and public keys,
    so the order of QueryLibrary and live Achilles results no longer reveals
    the order of counts inside one band;
  - Patient-Level Prediction false-positive and false-negative cells are
    suppressed below `nfilter.tab` like the other confusion cells;
  - CohortMethod outcome counts and the MDRR, the CohortMethod interaction
    estimates, and the SCCS incidence-rate ratio require `nfilter.tab`
    distinct persons with an outcome (per arm or window), not only enough
    persons in the arm; the first SCCS attrition step requires
    `nfilter.tab` persons with an observation period for its observation
    metrics;
  - the Patient-Level Prediction and propensity models apply the DataSHIELD
    `glm` admission rules before fitting: each binary covariate needs
    `nfilter.tab` persons with and without it, and the number of parameters
    stays within `nfilter.glm` (default 0.33) times the number of persons;
  - Achilles status and catalog report the reviewed analyses the present
    result tables can serve instead of which analyses a site populated, and
    the OHDSI status and summary no longer release result-table row counts.
- New DataSHIELD options: `default.dsomop.tools.mode="auto"` and
  `nfilter.glm=0.33`. `omopDpStatusDS()` reports `tools_mode`.

# dsOMOP 2.8.0

- The core no longer depends on differential privacy (DP). Connecting, loading
  tables through plans and recipes, filters, concept factors, merges and row
  binding, cohorts and dsOMOP's own exploration statistics work as they did
  before DP was added, under the standard DataSHIELD disclosure controls only.
  On the same database, plans, filters, selections, merges and row binds load
  tables identical to dsOMOP 2.2.34 (values, column types and person tokens).
  Removed from the core: the DP-exclusive gate on dsOMOP's own statistics,
  plan preview, cohort list and definitions, factor-level discovery and the
  capabilities population count; DP provenance sealing of plan outputs and of
  filter, select, merge, bind and factor results; the DP runtime bootstrap in
  `omopInitDS()`; the merge DP preflight; the DP-era restrictions on merge keys
  (the episode aliases `row_id`, `rowid` and `rowId`, and compatible key
  storage types, work again) and on row binding (compatible column types and
  differently ordered factor levels bind again); the DP lineage canonicalizer
  in derived-column execution (extra fields such as `label`, and policies in
  any case, work again); and DP helpers in staged-output descriptors, which now
  use their own canonical fingerprint. Disclosure settings report the pre-DP
  constant flags again.
- Integer `*_concept_id` columns can be filtered by equality and membership,
  like factor columns and under the same categorical controls.
- The OHDSI community tools (Achilles results, OHDSI results tables, the
  analysis catalog and the query library) stay enabled by default under the
  standard controls; custodians can disable them with
  `dsomop.tools.enabled = FALSE`. `dsomop.dp.exclusive` now defaults to
  `FALSE` and applies only to these tools. DP releases accept only
  outputs of the community tools. Their DP lineage keeps the 2.7.x canonical
  identities, so existing first answers are unchanged.
- DP-era fixes to the core are kept: concept-factor safety checks, merge
  suffix protection, the filter's missing-value exclusion, identifier
  sanitization, staged streaming and the Opal cohort transport fixes.

# dsOMOP 2.7.2

- Install on any R server without extra system libraries. The exact
  discrete-Laplace sampler no longer uses `gmp`, whose source build needs the
  GNU MP headers (`libgmp-dev`). On R servers that install CRAN source
  packages (for example Rock images whose repository is
  `https://cloud.r-project.org`), 2.7.0 and 2.7.1 could not be installed
  (`configure: error: Header file gmp.h not found`, then `dependency 'gmp' is
  not available`), and the Opal administration UI showed no error. The exact
  arithmetic is now plain R (`R/privacy_bigint.R`) and draws exactly the same
  noise as before: the sampler protocol, the self-test answers and sticky
  release identities are unchanged. `gmp` is only a suggested package, used by
  tests that compare both implementations.
- Package loading no longer runs the DP self-test or validates pseudonym
  settings. R test-loads every package while installing it, so any failing
  check there made the package impossible to install. The DP bootstrap still
  runs the canonical-encoder and sampler self-test before the first release and
  refuses releases if it fails; pseudonym settings are validated when first
  used.

# dsOMOP 2.7.1

- Resolve the conditional temporal equality oracle in
  [isglobal-brge/dsOMOP#20](https://github.com/isglobal-brge/dsOMOP/issues/20)
  with one permanent first answer per public request, snapshot and privacy
  epoch. Valid repeats return the entire stored canonical response, even after
  resealing one or many changed persons or restarting R; hits do not fingerprint
  private contents or evaluate the payload again. Provenance/MAC and typed
  admission remain mandatory.
- Validate public DP specifications before private input processing or stored
  answer lookup. First/last reducers require `order_by` even on empty or
  all-missing data; public column types, levels, bounds and reducer requirements
  are checked during parsing for every primitive. Empty categorical inputs now
  prepare zero counts instead of failing before lookup.
- Add an authenticated, atomically committed persistent binding store under
  `DSOMOP_STATE_DIR`. Fresh state and 2.7.0-style state automatically initialize
  on first use when no store, configured UUID or local pin exists, with a
  custodian message and a synced owner-only `release-store-id` pin outside the
  store directory. Keep `omopInitializeReleaseStore()` for explicit setup.
  Configured UUIDs take precedence and must match the pin and authenticated
  header. Missing/corrupt established state or root fails closed; roots are no
  longer silently regenerated. Injected roots require bindings too. Replicas
  and backups must retain the consistent root/pin/store/lock set.
- Recommend external UUID pinning: without it, deleting the whole state root
  permits a fresh installation (new root/domain with the file-backed provider),
  and deleting both store and pin while retaining the noise root is not detected.
  Whole-state rollback remains an operator/storage trust assumption.
- Reserve public capacity once per new request identity, default 1 GiB through
  `dsomop.dp.release_store_bytes` / `DSOMOP_DP_RELEASE_STORE_BYTES`. Keep existing
  answers readable at capacity with no eviction or privacy call quota.
- Known scaling limitation: every request authenticates the full first-answer
  inventory in O(entries). A review benchmark measured about 25 seconds for an
  existing-answer replay at 40,000 entries; concurrent requests may time out.
  Size reservation capacity accordingly, or lower `dsomop.dp.release_store_bytes`
  before the inventory grows. Incremental authentication is future work.
- Answers stay stale until explicit custodian snapshot/epoch rotation and session
  restart. Different requests against a mutable source need not reflect one
  coherent database snapshot. This repeated-payload guarantee is not a finite
  cumulative budget or a full temporal-transcript DP guarantee.
- Publish release protocol v3 and the snapshot-first-answer status contract;
  pair with dsOMOPClient 2.7.4, whose legacy-server transition option is opt-in
  with warnings and per-server contract metadata. Old stateless answers cannot
  be reconstructed; plan the publication migration even though store setup is
  automatic. Keep all seven
  primitives, QueryLibrary paths, exact sampler, sensitivities, contribution
  bounds, epsilon/delta defaults, component allocation and clipping unchanged.

# dsOMOP 2.7.0

- Make the typed DP channel exclusive by default while DP is enabled. The
  custodial `dsomop.dp.exclusive` option and its DataSHIELD fallback
  `default.dsomop.dp.exclusive` default to `TRUE`; custodians can set `FALSE`
  to restore standard population statistics. Analysts cannot bypass this
  policy through a client option.
- A shared interface policy refuses 28 standard statistical routes, including
  profiling, query/analysis results, result inventories and observed factor
  levels, and directs analysts to `ds.omop.dp.release`. Structural capabilities
  remain available without `total_persons`; schema, vocabulary metadata and
  server-side DP input preparation remain available.
- Report the configured `exclusive` flag in DP status, including when DP is
  disabled. Switching exclusivity does not change the mechanism, policy hash
  or sticky releases. dsOMOPClient 2.7.3 automatically skips observed factor
  discovery during exclusive-mode plan preparation and displays exclusivity.
- Replace the finite-precision inverse-CDF noise sampler with the exact
  integer/rational discrete-Laplace algorithm of Canonne, Kamath and Steinke
  (2020), driven by the deterministic keyed HMAC bit stream. The sampler
  protocol is `dsomop-dp-exact-discrete-laplace-v1`, the mechanism identifier is
  `dsomop-sticky-discrete-laplace-prf-v2`, and the hashed policy schema is 3.
- DP noise calibration, contribution bounds and output clipping are unchanged.
  The implemented sampler now has the exact ideal
  distribution: pure DP with delta 0 holds with independent random bits;
  deployment through keyed HMAC relies on its cryptographic pseudorandomness
  assumption. This is still a per-release guarantee, without a cumulative
  privacy budget.
- Add `gmp` for exact arithmetic. Epsilon and sensitivity retain the exact
  binary rational values of the supplied R numbers, with no decimal rounding.
  New sampler metadata changes sticky release identities; upgrading can draw
  new noise and does not repair previous disclosures. dsOMOPClient 2.7.2 accepts
  the new metadata and delta 0 unchanged, and rejects mixed sampler versions
  within one federated release. See `DISCLOSURE_CONTROL.md` and
  `SAMPLER_DIAGNOSIS.md` for the algorithm and reproduced defect.

# dsOMOP 2.6.0

- The DP release channel is enabled by default since 2.6.0. Custodians may opt
  out with `dsomop.dp.enabled = FALSE` or `DSOMOP_DP_ENABLED=0`.
- Unconfigured domain and snapshot identifiers are derived from hashed canonical
  database coordinates and CDM source metadata; explicit configuration wins.
  Precedence: explicit configuration → complete `cdm_source` → fallback.
  Multiple complete source rows are canonically sorted and hashed; the original
  single-row hash is unchanged. Missing/empty/incomplete source metadata falls
  back to sorted vocabulary ID/version pairs plus resource identity, or resource
  identity alone when vocabulary metadata is unavailable. One warning per R
  process reminds custodians to bump `dsomop.dp.privacy_epoch` after each reload:
  unchanged snapshots allow differencing releases across reloads. Bump the epoch
  whenever data change without metadata changes, then restart sessions.
  Unresolvable resource identity and conflicting configuration still fail closed.
- The default noise root requires a persistent private state directory
  (`DSOMOP_STATE_DIR` / `dsomop.state_dir`), shared across replicas. The ephemeral
  state override remains test-only. DSLite sessions isolate their DP policies.
- Epsilon (default 0.1, maximum 8), delta (0), discrete-Laplace mechanism and
  contribution caps are unchanged. Ordinary aggregate contracts are unchanged.
