# Final p0r3 full-suite validation

Both full suites completed with zero failures and errors.

| Suite | Passed assertions | Skips | Unexpected warnings |
|---|---:|---:|---:|
| dsOMOP | 20,525 | 10 | 0 |
| dsOMOPClient | 7,149 | 7 | 0 |

The client log contains 196 explicit legacy-server compatibility warnings
exercised by the tests. Its seven skips are optional ggplot2 rendering checks
(ggplot2 absent). Server skips are two CommonDataModel, four SqlRender, two
Arrow, one Node.js and one macOS-only test on Linux.

The final server suite tested d399c759d9d3d52284140b4d01ddac7ffb94cc91 with
matching client 95282f6c439c8a3667e3770ebe810f3de79615d9, through the clean
p0c-hier worktrees. It began at 13:52 UTC on 12 November 2026 and completed
on the pod at 14:29:25 UTC. The local disk filled at 14:25; the test process
continued independently. Recovery verified all 248 frozen source/test hashes
against the main worktrees, testing worktrees and pod mirrors, plus the runner
and matching client installation. This is a completed full run, not an
interrupted run or a reconstructed pass. The source manifest and recovery
record retain exact trees, hashes and log provenance.

The full client suite tested ce974d9 with server f9cc54c. Final server R source
is byte-identical to f9cc54c. Final client 95282f6 changes only roxygen comments
in three R files and generated Rd: non-comment lines and parsed expressions
are identical. The client documentation verification record is explicitly
retrospective, not raw captured stdout. No executable source changed after
these full suites; this evidence commit changes only this directory.

Reproduction of the final server full suite uses the matching sibling and all
prescribed vendor environment variables:

```sh
RTEST_WITH=/workspace/dsomop/wt/p0c-hier/dsOMOPClient \
RTEST_ENV='DSOMOP_TEST_POSTGRES_HOST=omopdb DSOMOP_TEST_MARIADB_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_HOST=127.0.0.1 DSOMOP_TEST_MYSQL_PORT=3307' \
/workspace/dsomop/tools/rtest.sh /workspace/dsomop/wt/p0c-hier/dsOMOP '' 14400
```

`disk-cleanup.json` lists every deleted permitted cache path and its allocated
size. No repository, worktree, evidence, protected session rollout or active
build mirror was deleted. Shared free space remained below the 15 GB target;
no full rerun or large copy was required for recovery. Additional uncertain
cleanup candidates were left untouched.

Developer findings, all 38 deviations and utility limitations are documented
in the requested design/2.10.5/p0-report.md, p0-deviations.md and
design/astra-dev/p0r3.report.md deliverables. Real Opal/Rock acceptance and the
coordinator's fresh independent re-review remain separate release gates.
