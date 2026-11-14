# Contract test fixtures and independent oracles

`check_contracts.py` validates the packaged schemas, probes, acceptance plans,
normal forms, semantic digests and checksums. It is used by the contract CI
workflow and `test-ws0-schema.R`:

```sh
python3 tests/fixtures/contracts/check_contracts.py . ../dsOMOPClient
```

It requires Python with `jsonschema`. The optional client argument also checks
vendoring and the frozen compatibility classifications; that comparison needs
the pinned baseline commits in each repository's Git history.

- `jcs_reference.py`, `normalize_reference.py`, `date_normalize.py` and
  `number_normalize.py` implement the independent canonicalisation and
  normalisation oracles used by the checker.
- `corpus_inventory.py`, `review_corpus_classes.py`, `corpus_baseline.json` and
  `corpus_decisions.csv` reproduce `tests/testthat/fixtures/v1-corpus-classes.csv`
  from the pinned released test suites. The JSON holds baseline commit IDs,
  not a job log or evidence manifest.
- `gen_acceptance_corpus.py` creates the packaged acceptance recipes and plans.
  `acceptance_expected.R` calculates their expected values using SQL and base R
  over the reach fixture, independently of the package execution engine.
