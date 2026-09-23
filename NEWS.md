# dsOMOP 2.6.1

- Replace the finite-precision inverse-CDF noise sampler with the exact
  integer/rational discrete-Laplace algorithm of Canonne, Kamath and Steinke
  (2020), driven by the deterministic keyed HMAC bit stream. The sampler
  protocol is `dsomop-dp-exact-discrete-laplace-v1`, the mechanism identifier is
  `dsomop-sticky-discrete-laplace-prf-v2`, and the hashed policy schema is 3.
- Noise calibration, contribution bounds, output clipping and the public policy
  surface are unchanged. The implemented sampler now has the exact ideal
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
