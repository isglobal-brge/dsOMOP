# WS-2f verification evidence

The applied fixes cover B2, B3 and N1–N3 of `astra-ws2c-review.md`.
**B1 is unresolved:** CR-27 requires both designated reviewer endorsements
before its frozen IR addition. `pending-b1-implementation.patch` and
`pending-b1-tests.R` are unapplied drafts, not tested implementation.

## Authoritative final checks

Tested server: `35318226db115acc548a7cb92dd334ce381f5dac`.
Tested client: `b69ed1c9845c691c0d49b5ac89c2f865959e3cd7`.

| Check | Passed | Failed | Errors | Skipped | Evidence |
|---|---:|---:|---:|---:|---|
| Full dsOMOP | 48,528 | 0 | 0 | 11 | `server-full-{summary.log,raw.log,results.json}` |
| Full dsOMOPClient | 4,922 | 0 | 0 | 7 | `client-complete-{summary.log,raw.log,results.json}` |
| Final N3 focused regressions | 1,499 | 0 | 0 | 1 | `n3-final-*` |
| Pristine membership baseline | 64 | 0 | 0 | 0 | `baseline-*` |

Both full runs used an empty filter, matching `RTEST_WITH`, and all four
vendor environment settings, including MySQL port 3307. The server log
confirms SQLite 3.53.3, DuckDB 2.0.0, PostgreSQL 14.24, MariaDB 10.6.23 and
MySQL 8.4.11. Optional skips and existing warnings are retained in the logs.

`contracts-final.log` records the separate passing oracle: three schemas,
193 probes, 15 acceptance plans, 12 digest vectors, 17 normalized fixtures,
34 number vectors and 11,043 corpus doubles.

`full-source-manifest.json`, `remote-source-manifest.json` and
`final-source-verification.json` verify all 2,536 tracked files locally and
on the pod. Both integration heads remained unchanged and were already
ancestors. Both versions remain 2.10.2; shared normalizers are identical;
the native wire schema hash is unchanged.

## Earlier attempts and audits

- `server-full-initial-*`: one obsolete exact diagnostic expectation failed;
  commit `c0d9700` corrected it. This run is not claimed green.
- `server-full-superseded-*`: intentionally stopped when final N3 source
  review found canonical-preview and internal-frame scope regressions;
  `3531822` fixed them. This is not a completed full run.
- `client-full-*` and `client-final-*`: earlier green client runs on the
  preceding server heads, superseded by `client-complete-*`.
- `client-complete-attempt1-*`: an overlapping parent rsync removed the
  isolated remote log. No RESULT was produced; the successful retry started
  after parent synchronization completed.
- `focused-*` and `corpus-validation-*`: diagnostic runs before corrections
  and the controlled golden regeneration. Final full suites supersede them.
- `golden-diff.json`: exactly seven intentional golden changes and 470
  unchanged records; historical result payloads were not regenerated.
- `*-cleanup.json`: exact own-scratch deletions after evidence preservation.

The pristine membership pin is `30b7905617dbe3de5d06699c324095635c68011f`.
The committed fixture retains 12 complete baseline records; current
membership-result tests compare all of them exactly.
