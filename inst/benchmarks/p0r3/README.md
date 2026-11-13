# Person-membership generation and read-snapshot evidence

This directory contains the R01/R02/R03 evidence for p0r3. All identifiers in
these artifacts are synthetic. The benchmark uses exactly the integers
1 through 10,000,000, so it needs no random seed.

From the package directory on the configured test pod:

```sh
DSOMOP_BENCHMARK_SOURCE=$(git rev-parse HEAD) Rscript inst/benchmarks/p0r3/membership.R inst/benchmarks/p0r3/reproduction/membership-raw.csv
```

The optional second argument selects comma-separated engines. The script
creates isolated fixtures with the existing vendor-test helper, connects with
its restricted runtime credentials, and removes the fixtures on exit. DuckDB
uses an owned temporary database file. It neither restarts services nor changes
database options, and it does not require TLS. PostgreSQL, MariaDB and MySQL
connection defaults are the same as the pod vendor tests.

Each measured operation opens the native request transaction, computes all
membership hash leaves in the database, downloads the leaves in bounded
batches, combines them in R, validates the request, and rolls back. The timer
includes the final source-engine checks. It excludes fixture creation, the
one-time EXPLAIN query, and the final small root-key HMAC. Production applies
that HMAC in R; the benchmark never accesses the production noise root. Every
measured iteration performs a complete scan without a retained generation.

`cold_connection_first_digest` means the first digest on a fresh connection
after creating the fixture. It does **not** mean cold operating-system or
database caches: fixture construction itself touches the data, and this is a
shared test pod whose caches were not forcibly dropped. The next two requests
are labelled `warm`. Concurrent suites caused substantial load and timing
variation; these observations are not a production latency estimate or a
controlled comparison of engine performance. They show the actual cost of a
ten-million-person scan on this pod and do not establish cheap metadata access.
R CPU columns cover the R process, not separate database-server CPU; DuckDB
executes inside that process, so those CPU columns are not directly comparable
across the engines.

The pod reported an AMD Ryzen 9 7950X (16 cores, 32 logical CPUs), about
187.8 GiB of RAM, Ubuntu 22.04.5 LTS and R 4.6.2. A captured load average was
17.13/17.49/16.64; load changed during concurrent tests. Full CPU, memory and
package-version observations are retained with the run.

The final run is stored under `final/`, with the exact source revision and
SHA-256 checksums, complete raw timings, emitted SQL, EXPLAIN output, and log.
The script checks ten million members and identical digests across all twelve
requests and all four engines. All twelve final-source runs completed and
returned the same synthetic membership digest. Final elapsed times:

| Engine | First request (s) | Warm 1 (s) | Warm 2 (s) |
| --- | ---: | ---: | ---: |
| postgresql | 17.992 | 13.817 | 14.991 |
| mariadb | 117.614 | 111.280 | 104.856 |
| mysql | 50.776 | 54.180 | 52.225 |
| duckdb | 6.072 | 6.320 | 6.167 |


`pre-final/` preserves a completed earlier run and the first failed harness
attempt. That attempt exposed MySQL's `ONLY_FULL_GROUP_BY` rejection; the native
SQL now groups by the exact selected bucket expression. The earlier completed
run used `COUNT(*)` for each already unique primary key and predated the final
nontransactional-table checks. It therefore has the same canonical payload but
is not the final-source performance result.

`snapshot-focused.log` records 56 passing assertions and zero failures, errors,
or skips on final runtime revision `2ac52c8`. These include PostgreSQL, MariaDB,
MySQL, DuckDB and SQLite execution; independent-connection writes and
temporary materialization on PostgreSQL/MariaDB/MySQL; exact signed 64-bit
identifiers, duplicate membership and native/streaming equivalence; SQLite
rollback and injected connection-loss/commit guards; and a concurrent
InnoDB-to-MyISAM change. Connection loss was injected through the connection
validity check, not a network or server disconnect. The SQL
Server, Oracle and Redshift adapters have structural SQL tests only; those
services were unavailable for execution.

The native adapter choices are documented by the vendors: SQL Server's
[SERIALIZABLE isolation](https://learn.microsoft.com/en-us/sql/t-sql/statements/set-transaction-isolation-level-transact-sql?view=sql-server-ver17),
Oracle's [SET TRANSACTION](https://docs.oracle.com/en/database/oracle/oracle-database/18/sqlrf/SET-TRANSACTION.html),
and Redshift's [transaction snapshot semantics](https://docs.aws.amazon.com/redshift/latest/dg/c_serial_isolation.html).
These references support the SQL choices; they do not replace execution tests
against the unavailable services.

`generation-typed-focused.log` records 115 passing assertions and zero failures,
errors, or skips at revision `3f46e4d`, before the final duplicate-set and
nontransactional-source refinements. It covers current-data replay across
handles/history, membership replacement, transient failure recovery, label
independence, and fixed-loader live-source generation matching. Final combined
server/client suite evidence is maintained by the coordinating session.

The round-3 MySQL/MariaDB implementation checked only visible base tables in
its declared namespaces; the independent review's external-view witness
invalidated that capability boundary. R09 supersedes it: each actual source is
now pinned with transaction metadata locks, and current `SHOW CREATE` metadata
certifies its transitive base-table dependencies, including nested/definer views
across schemas. Every dependency must be InnoDB and verifiable; hidden metadata,
stored routines and nontransactional dependencies refuse the whole request.
Current definitions and engines are revalidated before publication, and the
public label carries the capability actually established in the request.
Unrelated unread objects do not block a safe source. Certification is local to
the request and adds no owner configuration or persistent cache. The old logs
above remain evidence for their recorded revisions; new regression and full
suite results are recorded in the round-4 developer report. Engines without a
native adapter retain the declared `not_isolated` residual.

The generation detects changes to the person-ID set, not every clinical edit.
Contribution fingerprints still change keys for affected bounded components;
comparisons across edits can reveal component change/equality patterns and have
no joint guarantee over an evolving source. No cross-request generation cache,
negative cache, epoch change, or snapshot label is needed. Fixed-capable loaders
prepare an origin generation inside their load snapshot; later fixed releases
recompute the current generation in a new request snapshot and require it to
match the authenticated origin. A restored table without that matching live
source uses its realised route.
