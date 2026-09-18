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
