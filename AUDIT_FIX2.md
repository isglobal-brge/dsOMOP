# Audit repair 2: implemented corrections and limits

These changes are development work for 2.10.5. The full CR-32/A-31 acceptance
inventory remains open; this document is not a release certification.

Imported vocabulary declarations require `vocabulary.ohdsi_import_validity`
regardless of their event or concept-set names. `meta`, `metadata` and
`circe_provenance` are valid registry names. Only the plan/recipe envelope's
`meta` and a recipe population's `circe_provenance` are advisory locations.
Detection and server preflight use the same structural visitor. Invalid explicit
imported references refuse before patient access; native definitions keep their
existing policy. Genuine metadata cannot select execution policy.

Physical result readers inspect required identities, count bases and optional
column types before admission. `.ohdsiStatus()` (available through the existing
public status endpoint) includes per-tool `schema_contracts`: table name,
status, named layout, upstream version and a structural fingerprint. The four
statuses are `supported`, `recognized_unsupported`, `admin_only` and `unknown`.
The ordinary public table inventory and availability list only supported public
layouts. Unsupported layouts are not counted or read by discovery. A supported
released frame carries the descriptor in `dsomop.results_schema`.

Fingerprints hash sorted column names, physical types, canonical table identity
and the declared result key. They never hash result contents. Historical inline
layouts explicitly report upstream version `unknown`. A recognizable
`target_comparator_id` layout does not prove a database was produced by
CohortMethod 6.0.3; the shipped 6.0.3 artifact remains schema evidence only.
EvidenceSynthesis registration retains admin-only admission.

Before caller projection, filtering or row limits, the reader privately checks
that the complete inline key relation has valid nonmissing keys and no duplicate
key tuples. SQLite also checks storage classes before driver coercion. Failure
returns the same generic integrity error, without a key, count or example.
Re-export one valid exact identity tuple per result row. Existing person,
statistic and count release protections still govern supported rows.

This is not the complete Q06 contract: version-evidence reconciliation, verified
actual old/current upstream exports, cross-table reference relationships and
complete unknown-layout/extra-column recognition remain outstanding. No lookup
join adapter, upstream-equivalence claim or live-analysis fallback is added.
