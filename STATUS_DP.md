# DP default-on release status

Work is confined to the two requested repositories, on `feat/dp-default-on`.
No pushes or tags. Server: **2.6.0**. Client: **2.7.2** (documentation only).

## Exact commits

Server baseline: `436669c5b582929dae4d3fd9527e7974a4eb92ee`.

- `922abddeb0b8c18e15b9d0deb4b94dc84bb020b3` — default-on runtime, resource-derived
  identities, isolated DSLite policies, generated option help and tests.
- `812b6ad563962dcf7f319e441cf03e52952ec944` — README, new DISCLOSURE_CONTROL.md
  and NEWS.md deployment/release documentation.
- `fa9b32db46bb31a7f6d0e5d176fed4e81553df10` — exclude local validation artifacts
  and this report from source packages.

Client baseline: `d21ed5cfa95cf417be1972d4257982b66a94a1c0`.

- `a5e95af0a90d669610f47016ed8d16dcfac0ab01` — version 2.7.2, README, security
  vignette, affected checked-in website passages and NEWS.md. No client API or
  argument defaults needed changing. No matching default-off Studio text found.

The subsequent report-only commit records this file; its identifier is available
in the branch log and is not embedded recursively in its own contents.

## Validation

| Run | Test blocks | Passed expectations | Failures/errors | Warnings/skips |
| --- | ---: | ---: | ---: | ---: |
| Baseline `test-privacy-dp.R` | 42 | 341 | 0 / 0 | 0 / 0 |
| Final targeted `test-privacy-dp.R` | 49 | 372 | 0 / 0 | 0 / 0 |
| DP file within full suite | 49 | 373 | 0 / 0 | 0 / 0 |
| Final targeted `test-resource.R` | — | 260 | 0 / 0 | 0 / 0 |
| Targeted manifest/leak gates | — | 50 | 0 / 0 | 0 / 0 |
| Full server source suite | 1,247 | 7,457 | 0 / 0 | 0 / 3 |
| R CMD check installed suite | — | 7,392 | 0 / 0 | 0 / 26 |

The full server suite passed. Its three skips require external PostgreSQL,
MySQL and MariaDB hosts (`DSOMOP_TEST_POSTGRES_HOST`,
`DSOMOP_TEST_MYSQL_HOST`, `DSOMOP_TEST_MARIADB_HOST`). Installed-package tests also passed. Their 26 skips comprise the same three
vendor-host skips, one missing sibling-client checkout test, one source-only
configure test, five source-only DATASHIELD manifest tests, and sixteen tests
requiring a loadable working-tree client. Those source-tree checks were exercised
in the full source run above.

Commands and evidence:

- Baseline/targeted tests: `devtools::test(filter = "privacy-dp")`, with additional
  focused resource/manifest tests during development (`.dp-before.rds`,
  `.dp-targeted.rds`, `.dp-targeted.log`).
- Full source suite: `devtools::test(reporter = "summary")` (`.dp-full.log`,
  `.dp-full.rds`).
- `R CMD build --no-build-vignettes .` succeeded.
- `_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual` completed with
  zero errors/warnings and one NOTE for bundled local validation logs; its
  installed-package suite passed (`.dp-check.log`, `.dp-check/dsOMOP.Rcheck`).
- The first archive exposed accidentally bundled local validation logs. After
  the build exclusion fix, the rebuilt archive contains none of these files.
  `_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual --no-tests` on that
  rebuilt archive is **Status: OK**, with zero errors/warnings/notes
  (`.dp-check-clean.log`). This second check intentionally avoids a third full
  suite run; package code and tests are identical between the two archives.
- Environment: R 4.5.2, aarch64 macOS. Optional suggested packages unavailable:
  ROracle, odbc, bigrquery, gnm. They were not installed for this task.
- Client changes are documentation only; text audit and `git diff --check`
  passed. No client suite rerun was needed for unchanged R code.

The pre-change baseline was the DP file, not the full suite. The full source
suite is run once after implementation, plus the installed-package suite required
by R CMD check. Local `.dp-*` logs/results preserve the evidence and are excluded
from both source builds and git status. Legacy endpoint fixtures explicitly use
the custodial opt-out; DP tests install their own settings, and the new default-on
SQLite initialization test clears that opt-out. Test temporary storage remains
behind `DSOMOP_TEST_ALLOW_EPHEMERAL_STATE=1`.

## Derived identity and privacy argument

The enabled default is TRUE in the option reader, DESCRIPTION Options and
DATASHIELD manifest. Explicit FALSE / `DSOMOP_DP_ENABLED=0`, conflicting-option
checks and the session enablement-change guard remain effective.

Initialization connects the resource before resolving DP policy, with the existing
handle cleanup path covering derivation failures. The domain is `resource_` plus
SHA-256 of canonical JSON tagged `dsomop-dp-resource-v1`, containing:

- normalized DBMS, lowercase host and effective port;
- database name, or normalized absolute existing SQLite/DuckDB file path;
- effective CDM and vocabulary schemas (SQLite implicit/explicit main canonicalized).

Credentials, raw URLs, display names, transport/security options, results schemas
and temporary schemas do not enter this identity. Known default ports are
normalized. Host aliases are not resolved by DNS. In-memory databases and
unavailable/ambiguous coordinates require explicit custodial identifiers.

The snapshot is `cdm_` plus SHA-256 of canonical JSON tagged
`dsomop-dp-cdm-snapshot-v1`, containing that resource hash and exactly one
`cdm_source` row's **cdm_source_name, cdm_release_date, cdm_version,
vocabulary_version**. Values are converted to text; each field must exist and be
nonmissing/nonblank. Zero/multiple rows, missing tables/fields, unreadable metadata
and failed identity resolution fail closed with an error naming both
`dsomop.dp.domain` and `dsomop.dp.snapshot_id`. Each explicit identifier takes
precedence independently; configuring both avoids metadata derivation entirely.

Different resource domains select different HMAC subkeys under the same persistent
root, separating semantic IDs, provenance, protected fingerprints and noise.
Domain and snapshot also enter the existing policy hash, public snapshot hash and
sticky release identity. Thus equal canonical queries on distinct resources do
not reuse a noise stream. Discrete numeric draws can coincide by chance; the test
asserts separation of keys and IDs, rather than requiring unequal draws.

The existing keyed bounded-statistic fingerprint remains in release keys.
The derived snapshot follows metadata, not every data edit: custodians must bump
`privacy_epoch` when data change without metadata changes. Metadata is reread at
policy validation; changes trigger the existing policy-change error. The
mechanism, epsilon default 0.1, epsilon maximum 8, delta 0 and contribution/level
caps are unchanged. This remains a per-release claim without a finite cumulative
privacy budget.

DSLite server environments use separate package-local runtime state, while
ordinary process-isolated servers retain their existing state location. Roots are
never embedded in OMOP handles. A session cannot silently switch to a different
derived resource policy; it must restart/use a separate session. The public sticky
catalogue remains policy-independent. Its release algorithms still use the normal
policy/provenance guard. The public noise-domain fingerprint remains root-scoped,
so the client's existing duplicate-node pooling protection is unchanged.

## Author release decisions and deployment review

- Confirm that the four requested metadata fields are the intended versioned
  fingerprint contract. This implementation requires all four, and rejects
  multiple cdm_source rows. Sites with incomplete metadata must configure both
  identifiers explicitly; this is intentionally fail closed.
- Mount persistent private state for the default file provider, including across
  container replacement and replicas. The existing injected-root alternative is
  unchanged. Test ephemeral-state permission does not weaken identity checks.
- Review replica/alias coordinates. The same logical dataset reached by different
  host aliases, mount paths or metadata formatting should use matching explicit
  domain/snapshot settings and the same root to avoid independent samples.
- Install the updated DataSHIELD profile defaults. Existing explicit custodial
  opt-outs, including retained default-option overrides, remain honored.
- Default availability does not automatically route ordinary aggregates through
  DP. Analysts still call the dedicated release API; ordinary contracts remain.
- Release/tag/push remains the author's action. The affected generated client
  website passages were aligned without regenerating the entire existing site.
