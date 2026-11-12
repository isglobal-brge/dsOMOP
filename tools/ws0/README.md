# WS-0 generators (dsOMOP 2.10.3 contracts)

Development scripts that produce the frozen WS-0 artefacts from the endorsed
design (`design-2.10.3.endorsed.md`, SHA-256 `0988b2da...7940`). They are
not run at install or test time; their outputs are committed.

| script | produces | run |
|---|---|---|
| `gen_schema_artefacts.py` | `inst/schema/` (plan-v2.json, ir-v2.json, mutate-v1.json, examples, probes, digest vectors, SHA256SUMS) | `PYTHONPATH=<jsonschema> python3 tools/ws0/gen_schema_artefacts.py <design.md> inst/schema` |
| `gen_acceptance_corpus.py` | `inst/plans/acceptance/NN-name/{recipe.R, plan.json, README.md}` (+ `circe.json` for 12) | `PYTHONPATH=<jsonschema> python3 tools/ws0/gen_acceptance_corpus.py inst/schema inst/plans/acceptance` |
| `acceptance_expected.R` | `inst/plans/acceptance/NN-name/expected.json` (hand-computed results on the extended reach fixture, SQL + base R, no engine) | `Rscript tools/ws0/acceptance_expected.R <package dir>` (needs RSQLite, DBI, jsonlite, openssl) |
| `corpus_inventory.py` | inventory of every plan-building test block of both suites (signals only, no classes) | `python3 tools/ws0/corpus_inventory.py <dsOMOP> <dsOMOPClient> inventory.json` |
| `review_corpus_classes.py` | `tests/testthat/fixtures/v1-corpus-classes.csv`: the hand-reviewed class, prior/intended behaviour and rationale of every plan instance (the decisions live in the script) | `python3 tools/ws0/review_corpus_classes.py inventory.json <dsOMOP> <dsOMOPClient> tests/testthat/fixtures/v1-corpus-classes.csv` |
| `check_contracts.py` | nothing: the mandatory independent oracle (Python jsonschema 2020-12 + `jcs_reference.py`) over schemas, examples, probes, acceptance plans and variants, digest vectors, numbers and checksums; run by `.github/workflows/ws0-contracts.yaml` and before every resubmission | `PYTHONPATH=<jsonschema> python3 tools/ws0/check_contracts.py <dsOMOP> [<dsOMOPClient>]` |
| `jcs_reference.py` | the reference RFC 8785 canonicaliser / ECMAScript number formatter shared by the generator and the checker | imported |

After regenerating `inst/schema/`, copy the vendored files to
`dsOMOPClient/inst/schema/` (everything except `ir-v2.json`) and keep
`R/json_schema.R`, `R/semantic_digest.R` and `inst/column_classes.csv`
byte-identical in both packages; `test-ws0-schema.R` and `check_contracts.py`
pin all of them. The PostgreSQL loading path of the acceptance fixture is
`tests/fixtures/load_reach_pg.R` (`test-ws0-postgres.R` proves the
SQLite/PostgreSQL equivalence on the pod).
