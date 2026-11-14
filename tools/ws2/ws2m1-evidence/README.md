# WS2m1 integration and pending B1 evidence

B1 is **not fixed**. CR-27 remains proposed and awaits both designated
reviewers' endorsements. The implementation and desired-contract tests
remain unapplied at `../ws2f-evidence/pending-b1-implementation.patch` and
`../ws2f-evidence/pending-b1-tests.R`. `integration-verification.json`
records that the patch applies cleanly to the merged head; this is not a
claim that the patch was executed or its desired-contract tests passed.

- `server-integration-*`: six targeted server files on `7f536fe`, matching
  client `acdfc75`. 1,326 passes, one error in the pre-integration strata
  fixture, one optional Python-oracle skip. Retained as diagnostic evidence.
- `server-anchor-*`: rerun only `test-ir-review.R` after correcting that
  fixture in `0ae44f8`; 361 passes, no failures/errors/skips. This supersedes the failing file's first result.
- `client-integration-*`: `test-ws0-schema.R` on `acdfc75`, matching server
  `7f536fe`; 145 passes, no failures/errors/skips.
- `contracts-final.log`: independent Python oracle, all checks passed;
  three schemas, two examples, 195 probes, 15 acceptance plans, 12 digest
  vectors, 17 normalised fixtures, 34 numbers, 11,043 corpus doubles.
- `test-selections.json`: exact test files, filters and tested commits.
- `final-source-hashes.json`: SHA-256 of relevant merged sources/tests.

Every R run used `/workspace/dsomop/tools/rtest.sh` and the matching sibling
through `RTEST_WITH`, with the PostgreSQL/MariaDB/MySQL environment settings
specified in COMMON.md. No full suite was run. These selected tests do not
establish a full vendor acceptance pass or resolve the membership digest
collision. The unchanged membership baseline fixture tests cover complete
historical results, not the missing semantic option.

The first local generator attempt lacked Python `jsonschema`; it was rerun
successfully through `uv run --with jsonschema`. An initial oracle invocation
found the server-only `ir-v2.json` entry mistakenly copied into the client's
checksum inventory during conflict resolution. `acdfc75` removed that entry;
the client never received the schema. The final oracle and client schema
checks pass. No independent-oracle exception was suppressed.
