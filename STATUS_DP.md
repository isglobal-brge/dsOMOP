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

Previous report-only commit: `0734aea8c57eb8cc837f44dc4d25cd883e56d60c`.

Reviewer follow-up:

- `1104dc995cfddbe74c56b446f8f2c21d2b78b38d` — source multi-row and metadata
  fallback derivation, process-wide warning, explicit-snapshot bypass and tests.
- `bce6eba3ef2de8d91ced13268e9f4f5304eac31d` — fallback precedence and privacy
  rationale in README, disclosure controls and NEWS.

The final report-only commit is recorded in branch history, avoiding a recursive
self-reference. Client remains unchanged at the accepted commit above.

## Reviewer follow-up validation

| Run | Test blocks | Passed expectations | Failures/errors | Warnings/skips |
| --- | ---: | ---: | ---: | ---: |
| Follow-up baseline DP file | 49 | 373 | 0 / 0 | 0 / 0 |
| Follow-up final DP file | 52 | 393 | 0 / 0 | 0 / 0 |
| Follow-up full server source suite | 1,250 | 7,477 | 0 / 0 | 0 / 3 |
| Follow-up clean-archive installed suite | — | 7,412 | 0 / 0 | 0 / 26 |

`R CMD build --no-build-vignettes .` succeeded, followed by
`_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual --output=.dp-followup-check dsOMOP_2.6.0.tar.gz`:
**Status: OK**, zero errors, warnings or notes, including installed-package tests.
The three external-vendor source skips and 26 installed-package skips have the
same reasons as the original delivery below. Source expectation count increased
by 20 (7,457 → 7,477); installed expectation count by 20 (7,392 → 7,412).
Archive SHA-256:
`2ee2edd10c7be3d43b85006e7655322dbdbcb3999dbf8832792622ba3172a7e9`.
The source and installed full suites each ran once for this follow-up.

The targeted run verifies exact tags for single/multi/fallback/resource-only
hashes, source/vocabulary row-order invariance, explicit configuration (including
snapshot-only overrides), missing/empty/incomplete/blank source data, unavailable
vocabulary, warning-once behavior across runtime restarts/resources, separation
of resource snapshots and policy-change rejection when vocabulary changes.
Existing default-on, opt-out, conflict, persistent-root and DSLite tests pass.

Evidence: `.dp-followup-before.{log,rds}`, `.dp-followup-targeted.{log,rds}`,
`.dp-followup-full.{log,rds}`, `.dp-followup-build.log`,
`.dp-followup-check.log`, `.dp-followup-check/dsOMOP.Rcheck`.
The clean archive excludes all `.dp-*` artifacts and this report. Roxygen help
was regenerated. No mechanism, epsilon/delta, cap, sticky catalogue or client API
changes were needed. The accepted client documentation remains accurate at its
existing level of detail; no further client change/version bump was made.

## Original delivery validation (historical)

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

Snapshot precedence is explicit → complete `cdm_source` → vocabulary fallback
→ resource-only fallback. The snapshot is `cdm_` plus SHA-256 of canonical JSON
containing a protocol tag, resource hash and metadata. A single complete source
row preserves **exactly** `dsomop-dp-cdm-snapshot-v1` and the four text fields
**cdm_source_name, cdm_release_date, cdm_version, vocabulary_version**. Multiple
complete rows use `dsomop-dp-cdm-snapshot-multi-v1`; all rows (including duplicates)
are ordered by their canonical JSON bytes using radix sorting.

Missing/unreadable/empty source tables or any absent, NA or blank required field
fall back to all `vocabulary_id`/`vocabulary_version` pairs, including `None`,
with the same canonical sorting, tagged `dsomop-dp-cdm-snapshot-fallback-v1`.
Vocabulary is read from the handle's vocabulary schema. If it too is unavailable,
empty or incomplete, metadata is null and the tag is
`dsomop-dp-cdm-snapshot-resource-only-v1`. Resource identity remains mandatory;
there is no shared generic domain. Unresolvable/in-memory coordinates and
conflicting option/environment values still fail closed.

Each explicit identifier wins independently. An explicit snapshot skips metadata
reads and fallback warnings even when the domain must be derived. Configuring
both identifiers skips derivation entirely. Fallback emits one warning per R
process, held in package state independently of DSLite session/runtime state,
naming both explicit options and requiring an epoch bump after every reload.
The warning uses R's standard warning/log channel. A fallback snapshot cannot
track data reloads: unchanged snapshot labels permit comparisons/differencing
across reloads. Epoch rotation remains mandatory; it does not create a cumulative
privacy budget or eliminate composition of distinct releases.

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

- The author-decided fallback is implemented; no fingerprint decision is pending.
  This implementation uses all vocabulary pairs (including `None`), not only
  `None`. Single-row fingerprints remain unchanged. Reviewers can verify exact
  protocol tags through the hash-contract tests.
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
