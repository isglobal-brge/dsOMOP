# WS-0 generators (dsOMOP 2.10.3 contracts)

Development scripts that produce the frozen WS-0 artefacts from the endorsed
design (`design-2.10.3.endorsed.md`, SHA-256 `0988b2da...7940`). They are
not run at install or test time; their outputs are committed.

| script | produces | run |
|---|---|---|
| `gen_schema_artefacts.py` | `inst/schema/` (plan-v2.json, ir-v2.json, mutate-v1.json, examples, probes, digest vectors, SHA256SUMS) | `PYTHONPATH=<jsonschema> python3 tools/ws0/gen_schema_artefacts.py <design.md> inst/schema` |
| `gen_acceptance_corpus.py` | `inst/plans/acceptance/NN-name/{recipe.R, plan.json, README.md}` (+ `circe.json` for 12) | `PYTHONPATH=<jsonschema> python3 tools/ws0/gen_acceptance_corpus.py inst/schema inst/plans/acceptance` |
| `acceptance_expected.R` | `inst/plans/acceptance/NN-name/expected.json` (hand-computed results on the extended reach fixture, SQL + base R, no engine) | `Rscript tools/ws0/acceptance_expected.R <package dir>` (needs RSQLite, DBI, jsonlite, openssl) |
| `gen_corpus_classes.py` | `tests/testthat/fixtures/v1-corpus-classes.csv` (one class per plan-building test of both 2.10.2 suites) | `python3 tools/ws0/gen_corpus_classes.py <dsOMOP> <dsOMOPClient> tests/testthat/fixtures/v1-corpus-classes.csv` |

After regenerating `inst/schema/`, copy the vendored files to
`dsOMOPClient/inst/schema/` (everything except `ir-v2.json`) and keep
`R/json_schema.R` and `R/semantic_digest.R` byte-identical in both packages;
`test-ws0-schema.R` pins both.
