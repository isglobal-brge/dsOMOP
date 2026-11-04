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
