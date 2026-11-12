# WS-0 generators (dsOMOP 2.10.3 contracts)

Development scripts that produce the frozen WS-0 artefacts from the endorsed
design (`design-2.10.3.endorsed.md`, SHA-256 `0988b2da...7940`). They are
not run at install or test time; their outputs are committed.

| script | produces | run |
|---|---|---|
| `gen_schema_artefacts.py` | `inst/schema/` (plan-v2.json, ir-v2.json, mutate-v1.json, examples, probes, digest vectors, SHA256SUMS) | `PYTHONPATH=<jsonschema> python3 tools/ws0/gen_schema_artefacts.py <design.md> inst/schema` |
| `gen_acceptance_corpus.py` | `inst/plans/acceptance/NN-name/{recipe.R, plan.json, README.md}` (+ `circe.json` for 12) | `PYTHONPATH=<jsonschema> python3 tools/ws0/gen_acceptance_corpus.py inst/schema inst/plans/acceptance` |
| `acceptance_expected.R` | `inst/plans/acceptance/NN-name/expected.json` (hand-computed results on the extended reach fixture, SQL + base R, no engine) | `Rscript tools/ws0/acceptance_expected.R <package dir>` (needs RSQLite, DBI, jsonlite, openssl) |
| `corpus_inventory.py` | inventory of every test block of both suites that builds a plan directly or THROUGH a helper defined in the test/helper files (transitive closure), plus the gate-helper blocks the design names; signals only, no classes | `python3 tools/ws0/corpus_inventory.py <dsOMOP> <dsOMOPClient> inventory.json` |
| `corpus_decisions.csv` | the hand-written disposition of every inventoried block (one row, or `#k` sub-rows when a block mixes dispositions): class, prior behaviour, intended behaviour, rationale. Nothing is inferred from it; edit it when a test block is added or changes | edited by hand |
| `review_corpus_classes.py` | `tests/testthat/fixtures/v1-corpus-classes.csv`: the join of `corpus_decisions.csv` with the live inventory. It FAILS on a block without a decision, a decision without a block, a plan-building block marked `non_plan` (unless the rationale starts with `no plan:`) or a non-plan block with another class (unless it starts with `gate:`) | `python3 tools/ws0/review_corpus_classes.py <dsOMOP> <dsOMOPClient> tests/testthat/fixtures/v1-corpus-classes.csv [--check]` |
| `check_contracts.py` | nothing: the mandatory independent oracle (Python jsonschema 2020-12 + `jcs_reference.py`) over schemas, examples, probes, acceptance plans and variants, digest vectors, numbers and checksums; run by `.github/workflows/ws0-contracts.yaml` and before every resubmission | `PYTHONPATH=<jsonschema> python3 tools/ws0/check_contracts.py <dsOMOP> [<dsOMOPClient>]` |
| `jcs_reference.py` | the reference RFC 8785 canonicaliser / ECMAScript number formatter shared by the generator and the checker; `--corpus` regenerates `tests/testthat/fixtures/es6-number-corpus.csv` (random doubles, RFC 8785 Appendix B, every power of two and its adjacent doubles, near-power-of-two doubles) | `python3 tools/ws0/jcs_reference.py --corpus tests/testthat/fixtures/es6-number-corpus.csv` |
| `acceptance_expected.R` (score reference) | also writes `inst/plans/acceptance/score-reference.json`: the paired v1 (all-time history) / v2 (history up to the reference date, NULL = index) Charlson, CHADS2 and CHA2DS2-VASc expectations with fixed and index-resolved reference dates (`tests/fixtures/score_reference.R`, CR-2 / A-2) | same command |

After regenerating `inst/schema/`, copy the vendored files to
`dsOMOPClient/inst/schema/` (everything except `ir-v2.json`) and keep
`R/json_schema.R`, `R/semantic_digest.R` and `inst/column_classes.csv`
byte-identical in both packages; `test-ws0-schema.R` and `check_contracts.py`
pin all of them. The PostgreSQL loading path of the acceptance fixture is
`tests/fixtures/load_reach_pg.R` (`test-ws0-postgres.R` proves the
SQLite/PostgreSQL equivalence on the pod).

`check_contracts.py` additionally regenerates the ECMAScript number corpus and,
when both package paths are given, the reviewed corpus classes, and fails if
either frozen fixture differs from the regenerated rows. The synthetic
GiBleed-shaped generator (`tests/fixtures/generate_synthetic_cdm.R`) loads into
PostgreSQL through its `conn`/`schema` arguments with typed DATE columns;
`test-ws0-postgres.R` proves the load and the cardinality equivalence against
the in-memory frames and a SQLite load on the pod.
