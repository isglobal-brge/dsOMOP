# Profiler column-gate snapshot regression

The full server run exposed a stale test fixture in `test-profiling.R`:
it disconnected SQLite after caching the blueprint, then expected the public
column-policy refusal. The request wrapper now opens a transaction before
entering that gate, so the closed connection correctly fails snapshot setup.

`before.log` reproduces the failure on a tree identical to server `b765e03`
(snapshot-fork baseline `0e534ce`): 164 passed, zero failed assertions, one error.
`after.log` records 180 passed, zero failures, errors or skips after test-only
commit `5ad0b2c`. Runtime R source is unchanged from `b765e03`.

The corrected test keeps SQLite connected, intercepts the profiler's
`.executeQuery` path, retains all thirteen policy-refusal assertions, and
asserts zero data queries, a cleared request-snapshot context, and a valid
connection afterward. Transaction setup and rollback remain real database
operations; no runtime exception for a closed connection was introduced.

Both focused runs used the matching client source `ce974d9`, copied to the
snapshot fork's sibling directory so the main test mirror was never touched:

```sh
RTEST_WITH=/workspace/dsomop/wt/p0c-snapshot/dsOMOPClient \
RTEST_ENV='DSOMOP_TEST_POSTGRES_HOST=omopdb DSOMOP_TEST_MARIADB_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_PORT=3307' \
/workspace/dsomop/tools/rtest.sh /workspace/dsomop/wt/p0c-snapshot/dsOMOP '^profiling$' 1200
```

The profiling fixture itself uses SQLite. Supplying the prescribed vendor
environment does not make this a vendor-disconnect test. Full-suite acceptance
is recorded separately by the coordinating session.
