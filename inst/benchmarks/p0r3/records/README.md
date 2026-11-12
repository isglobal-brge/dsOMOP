# Record-summary cross-engine snapshot-label regression

The integrated full suite exposed sixteen stale equality assertions in
`test-tools-release-records.R`: four protected questions on each of DuckDB,
PostgreSQL, MySQL and MariaDB compared the entire attributed result with
SQLite. The expected transaction mechanism now differs between engines.

`before.log` reproduces the failures from a tree identical to integrated
server `8e9cbf3` (snapshot-fork baseline `e1433d4`): 348 passed, sixteen failed
assertions, zero errors or skips. The standard reporter prints the first ten
failure details and the complete per-test totals. Each printed difference is
the `dsomop_protection$read_snapshot` field; the successful rerun confirms that
this was the only differing field in all sixteen comparisons.

Test-only commit `a46c366` checks the expected transaction label explicitly:
SQLite `serializable_transaction`, DuckDB `snapshot_transaction`, PostgreSQL
`repeatable_read_transaction`, and MySQL/MariaDB
`consistent_snapshot_transaction`. It excludes only that field from the
cross-engine equality check; the numeric values and every other custom
attribute remain compared. `after.log` records 381 passed, zero failures,
errors or skips. Runtime R source remains identical to `8e9cbf3`.

Both runs used the exact matching client source `95282f6`, archived into the
snapshot fork's sibling directory. Only the snapshot fork's pod mirror and
private test library were used:

```sh
RTEST_WITH=/workspace/dsomop/wt/p0c-snapshot/dsOMOPClient \
RTEST_ENV='DSOMOP_TEST_POSTGRES_HOST=omopdb DSOMOP_TEST_MARIADB_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_PORT=3307' \
/workspace/dsomop/tools/rtest.sh /workspace/dsomop/wt/p0c-snapshot/dsOMOP '^tools-release-records$' 1200
```

This focused run executed all five database fixtures. It verifies protected
record-summary parity and transaction labels; it does not add concurrent-write
or physical-disconnection coverage. Full-suite acceptance is recorded
separately by the coordinating session.
