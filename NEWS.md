# dsOMOP 2.7.1

- Resolve the conditional temporal equality oracle in
  [isglobal-brge/dsOMOP#20](https://github.com/isglobal-brge/dsOMOP/issues/20)
  with one permanent first answer per public request, snapshot and privacy
  epoch. Valid repeats return the entire stored canonical response, even after
  resealing one or many changed persons or restarting R; hits do not fingerprint
  private contents or evaluate the payload again. Provenance/MAC and typed
  admission remain mandatory.
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
