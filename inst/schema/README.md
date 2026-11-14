# dsOMOP 2.10.3 plan schemas (WS-0 frozen artefacts)

The package ships these versioned schema contracts:

- `plan-v2.json` — the closed wire schema (design section 4.1 plus amendments A-7
  `when_index_source` and A-14 `source_analyses`);
- `ir-v2.json` — the internal compatibility IR: wire schema plus the closed
  legacy variants that only `.irNormalize()` can produce (section 4.5,
  amendments A-2, A-10, A-15, A-16, A-21, A-25 and A-26); never accepted on the wire;
- `mutate-v1.json` — the mutation AST with the predicate grammar copied in
  (section 4.6);
- `examples/` — canonical examples A (normalised form) and B (as emitted);
- `probes/` — the probe corpus with the documented verdict per instance;
- `digest-vectors.json` — raw canonicalisation vectors (section 4.4): serializer
  tests of the unnormalised instances;
- `normalized/`, `normalized-vectors.json` — the normal form (amendment A-11) of
  examples A/B and the acceptance plans with their normalised-document hashes
  and semantic digests: the digests the federation signature and the DP capsule
  bind; produced by `tests/fixtures/contracts/normalize_reference.py`, reproduced by the
  vendored `R/plan_normalize_v2.R`;
- `SHA256SUMS` — checksums; `dsOMOPClient/inst/schema/` vendors byte-identical
  copies (without `ir-v2.json`).

Validate schema changes with the R tests and the independent Python contract checker.

A-28/CR-29 adds closed date_spec/date_source and four native derived variants.
Omitted legacy date policies remain absent for native presentation; unchanged plans
retain their previous normal forms and digests.
