# dsOMOP 2.10.3 plan schemas (WS-0 frozen artefacts)

Generated from the endorsed design (`design-2.10.3.endorsed.md`, SHA-256
`0988b2da...7940`) by `tools/ws0/gen_schema_artefacts.py`:

- `plan-v2.json` — the closed wire schema (design section 4.1, verbatim);
- `ir-v2.json` — the internal compatibility IR: wire schema plus the closed
  legacy variants that only `.irNormalize()` can produce (section 4.5); never
  accepted on the wire;
- `mutate-v1.json` — the mutation AST with the predicate grammar copied in
  (section 4.6);
- `examples/` — canonical examples A (normalised form) and B (as emitted);
- `probes/` — the probe corpus with the documented verdict per instance;
- `digest-vectors.json` — the three semantic-digest vectors (section 4.4);
- `SHA256SUMS` — checksums; `dsOMOPClient/inst/schema/` vendors byte-identical
  copies (without `ir-v2.json`).

Changes need both reviewers (design section 12.1).
