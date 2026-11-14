# Pinned OHDSI PhenotypeLibrary acceptance inputs

Unmodified public definitions and generated SQL from OHDSI/PhenotypeLibrary
commit `40106e4e2892d883c208b9df10ad56711da3e21d`, package 3.37.0, Apache License
(as declared by upstream DESCRIPTION). Source URLs, original SHA-256 hashes,
definition IDs and available review status are in `manifest.json`. No runtime
network fetch is used. Metadata not supplied by upstream remains unknown.

304 (Neurofibromatosis type 1) exercises the conditional first-event bridge.
The server's `test-audit-circe-oracle.R` runs the pinned generated SQL through
SqlRender 1.19.7 on a private PostgreSQL synthetic fixture, and compares exact
membership/start/end with the imported native plan. The JSON and SQL bytes are
also bundled with the server test. This one oracle does not establish universal
upstream equivalence or close the separate imported-vocabulary policy obligation.

10 (Nausea or Vomiting) deliberately exercises unsupported multiple entry,
All selection, downstream limits and domain-field clauses. The checker reports
these without executing or returning a weakened definition.

Raw upstream SQL whitespace is retained so the original checksum remains valid.
