# Protected-route cross-engine snapshot-label regression

The integrated full suite exposed ninety-two stale equality assertions in
`test-vendor-release-routes.R`: twenty-three protected questions on each of
DuckDB, PostgreSQL, MySQL and MariaDB compared their entire attributed results
with SQLite. Sorting the result rows preserves custom attributes, including
the engine-specific `dsomop_protection$read_snapshot` field.

`before.log` reproduces the failures from a tree identical to integrated
server `f6dff81` (snapshot-fork baseline `ad97041`): 148 passed, ninety-two
failed assertions, zero errors or skips. Each non-SQLite engine has exactly
twenty-three failures. The default reporter prints the first ten failure
details and the complete per-test totals. Every printed difference is the
snapshot label.

Test-only commit `4e973ac` checks that label explicitly: SQLite
`serializable_transaction`, DuckDB `snapshot_transaction`, PostgreSQL
`repeatable_read_transaction`, and MySQL/MariaDB
`consistent_snapshot_transaction`. It excludes only that field from the
cross-engine equality check. All twenty-three questions, protected-route
expectations, SQL-helper assertions, numeric values, projection payloads and
other custom attributes remain compared. `after.log` records 333 passed,
zero failures, errors or skips. The successful rerun confirms that no other
field differed in any of the ninety-two comparisons. Runtime R source is
unchanged from `f6dff81` (and `8e9cbf3`).

Both runs used the exact matching client source `95282f6`, archived into the
snapshot fork's sibling directory. Only the snapshot fork's pod mirror and
private test library were used. The reproduction command was:

```sh
RTEST_WITH=/workspace/dsomop/wt/p0c-snapshot/dsOMOPClient \
RTEST_ENV='DSOMOP_TEST_POSTGRES_HOST=omopdb DSOMOP_TEST_MARIADB_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_PORT=3307' \
/workspace/dsomop/tools/rtest.sh /workspace/dsomop/wt/p0c-snapshot/dsOMOP '^vendor-release-routes$' 1800
```

The successful rerun appended
`R_PROFILE_USER=/root/rt/wt/p0c-snapshot/dsOMOP/inst/benchmarks/p0r3/vendor-routes/report-all.R`
to `RTEST_ENV`. That profile changes only the reporter's detail limit, so any
remaining failure would be printed in full. It does not alter test behavior.

All five database fixtures executed. This focused run verifies protected
answer parity, SQL-helper semantics and transaction labels; it does not add
concurrent-write or physical-disconnection coverage. Full-suite acceptance is
recorded separately by the coordinating session.
