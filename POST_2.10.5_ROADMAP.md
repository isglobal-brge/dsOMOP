# Post-2.10.5 audit roadmap

The coordinator scope decision appended to `design/2.10.3/astra-audit-review-4.md`
on 14 November 2026 limits this audit release to released-defect corrections,
Q14's four versioned formula constructors, and Q15's calendar-day collapse.
This supersedes the earlier all-Q01–Q35 2.10.5 ledger in historical reports.

For 2.10.6, retain these A-31 obligations:

- Q07–Q13: explicit namespace, role, window, CDM and method declarations,
  including the versioned `ohdsi-declaration-1` interoperability record.
- Q16–Q22, Q24–Q25 and Q27: tested study teaching and workflow deliverables,
  including growth references and verbal-autopsy interview association.
- Q26: the public client `recipe_study_bundle` API and portable study record.
- Q29–Q31: complete route guidance, diagnostics/code generation, native book
  and exercises. The existing numeric/category contracts remain in force.
- Q06's older CohortMethod physical-export acceptance and expanded upstream
  reference/version reconciliation; Q23's actual downstream-consumer matrix.
- The remaining broad-amendment integrated library, exact-cost teaching and
  actual-Eunomia/versioned-CDM evidence (Q28/Q34/Q35). Existing prerequisite
  implementations and their earlier acceptance are retained, without claiming
  that this narrower fix round completes these additional obligations.

Fresh/upgrade/profile/restart and deployed privacy-boundary acceptance run in
2.10.5 integration e2e on real Opal, as directed by the coordinator. A local
DSLite run is not substituted for that deployment evidence.

Exploratory work performed before the narrowed scope was read is retained in
Git history: server through `6e0811b`, client through `1fe72d4`. It is not
shipped in this tree. Selectively recover and review it for 2.10.6; do not
cherry-pick it wholesale. In particular, the exploratory WHO consumer test
exposed a reference-range discrepancy, and the pending Eunomia/VA/declaration
changes require fresh validation. The audit-fix4 report distinguishes those
exploratory logs from the final 2.10.5 acceptance runs.
