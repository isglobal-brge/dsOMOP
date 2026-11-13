# WS-10 validation evidence

Documentation-only package commits tested: dsOMOP `986f46e`, dsOMOPClient
`2bbab80`. Harness tested: `0f3100b` in the separate dsomop-e2e clone.
The R test pod loaded the matching sibling sources; both DESCRIPTION versions
remain integration-owned 2.10.2 until the coordinated 2.10.4 bump.

- `server-targeted.log`: `assignment-policy|ws0-acceptance-corpus|publication-integration`,
  672 passed, zero failures/errors/skips; all three vendor environment flags set.
- `client-targeted.log`: `versions|recipe-executable-contract`, 131 passed,
  zero failures/errors/skips. The supplied filter also included `plan-publication`,
  which matches no file; the actual publication file was run separately.
- `client-publication.log`: `publication-status`, 114 passed, zero failures/errors/skips.
- `harness-validation.log`: harness `scripts/o/validate.R` on the pod, validating
  R/YAML/vignette syntax, v1 performance plans, both independent matching seeds,
  and selected installed assignment/query-library/OHDSI/external-contract tests.
- `server-full.log`, `client-full.log`: the common instructions changed during
  the run; rtest correctly refused full development suites. The later Test
  economy section reserves those suites for integration/release.
- `cleanup.json`: exact paths and bytes removed under CLEANUP.md; evidence
  formats were preserved. Own validation scratch copies were removed afterward.

No real Opal acceptance run is claimed. The new harness refuses the currently
vendored 2.10.3 candidate and requires both packages at integrated 2.10.4.
See `/workspace/dsomop/design/astra-dev/ws10.report.md` for integration gates.
