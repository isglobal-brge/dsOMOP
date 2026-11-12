# A-25 full-result acceptance

Nine complete validation/execution records match exactly between the immutable
2.10.2 baseline `30b7905617dbe3de5d06699c324095635c68011f` and current runtime
`14e634ee819ec7aba678c9f9ddc7f082a51bd0a6`. Baseline replay: 58 assertions;
current replay: 68 assertions. Both had zero failures, errors and skips.
These are focused probes, not full package suites.

The records cover population-local named criteria with an independent sibling,
whitespace canonicalization, numeric `1`, `"1"` and `"01"`, criteria anchored to
an independent recurrent scope under `first_episode`, `last_episode` and
`any_episode`, and recurrent named index populations whose filter uses each
event's own anchor. Every record calls the real `.planValidate()` and
`.planExecute()`. The criteria fixtures' named cohort dates lie outside the
observation periods and event dates, so adopting them as a filter anchor would
change the results. The recurrent scope preserves the historical two output
rows per person; neither replay silently deduplicates those rows.

`baseline-results.rds` and `current-results.rds` contain the complete R values:
plans, validation findings, every output field, row order, type and attribute,
and supplemental episode-level results. Their SHA-256 digests agree. The
current probe also applies `expect_identical()` to each complete record.
Supplemental criteria episodes prove observation-period materialization;
supplemental index episodes prove original event dates, event identity and
recurrence. Those resolver checks accompany full execution, rather than serving
as its replacement.

The source snapshots came directly from `git archive`. Every baseline runtime
file and all four fixture/helper files in the supplied 52-file manifest were
verified. No validator or executor was replaced or patched. `verification.json`
records source hashes, fixture/probe hashes, log hashes and result hashes. Raw
focused logs retain complete `dput()` records. Disposable local source snapshots
were removed after capture to preserve workspace disk space; the Git objects,
source manifests and pod mirrors preserve their provenance.

The current release validates and lowers v1 to the closed IR, then executes v1
through its historical executor. These captures establish that integrated
validation/execution path. They do not claim an unavailable native population
compiler executed the new variant. The dedicated
`tests/testthat/test-ir-owned-cohort.R` additionally checks both IR variants,
strict native-wire refusal, canonical digest/explain, exact registry ownership
without result/count queries, private small intermediates, final support gates,
and DP refusal including descendants, joins and the final preparation endpoint.

Reproduce on the test pod from a checkout of this commit:

```sh
python3 tools/ws2/a25-replay.py 14e634ee819ec7aba678c9f9ddc7f082a51bd0a6
```

The runner checks `df -h /workspace` and requires at least 15 GiB free before
creating narrow snapshots inside the assigned worktree. It runs them through
`rtest.sh`, refuses to overwrite an existing snapshot, and removes each new
local snapshot after recording its source manifest and evidence log; the
verification records the removed path and its byte size. After one
successful baseline capture, `--current-only <commit>` reuses its locally saved
RDS and captures another immutable current source. It copies that baseline RDS
into the new snapshot and writes result captures outside the parent pod mirror
so a concurrent parent `rsync --delete` cannot remove them.

Two harness diagnostics are retained under ignored `tools/ws2/artifacts/`: the
initial fixture-helper lookup omitted an explicit evaluation environment, and
an early final replay could not reopen a baseline RDS removed by the parent
mirror sync. Both are corrected. An earlier successful current replay at
`3906474` also matched all nine records; the committed evidence is the final
runtime `14e634e` replay.
