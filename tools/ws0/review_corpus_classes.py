#!/usr/bin/env python3
"""WS-0 (review B7): the REVIEWED v1 corpus classes.

Every row of tests/testthat/fixtures/v1-corpus-classes.csv is a hand-reviewed
disposition of one plan instance (a test block, or a sub-row `#k` when a block
mixes dispositions) across every test file of both 2.10.2 suites that builds,
compiles or executes a plan (the inventory of corpus_inventory.py) plus the
disclosure helper tests the design names. Classes:
  safe                       executes today and keeps byte-identical results
  corrected                  executes today with a result the ledger declares wrong
  newly_permitted            refused today by an intermediate/granularity gate; runs in 2.10.3
  newly_refused_structural   accepted today, invalid in 2.10.3 (schema/shape)
  newly_refused_publication  accepted today, refused at publication in 2.10.3
  publication_refused_unchanged  a final-publication (or retained creation-gate) refusal that stays
  unchanged_invalid          argument/structure/class-1 validation that stays an error
  non_plan                   a block in an inventoried file that builds no plan
The decisions below were written by reading each block's assertions (Fable,
12 Nov 2026); the generator only applies them. Usage:
  python3 tools/ws0/review_corpus_classes.py <inventory.json> <dsOMOP> <dsOMOPClient> <out.csv>
"""
import csv, json, os, re, sys
INV, S, C, OUT = sys.argv[1:5]
inv = json.load(open(INV))

NP = "non_plan"; SAFE = "safe"; UI = "unchanged_invalid"; NPERM = "newly_permitted"; PRU = "publication_refused_unchanged"
NRS = "newly_refused_structural"; NRP = "newly_refused_publication"; CORR = "corrected"

# ---- files whose blocks never build recipe/plan executions (matched by the broad inventory regex) ----
NON_PLAN_FILES = {("dsOMOP", f) for f in ("test-analysis-catalog.R", "test-analysis-ports-achilles-live.R", "test-analysis-ports-cm-propensity.R",
                                          "test-analysis-ports.R", "test-blueprint.R", "test-dp-exclusive.R", "test-tools-auto.R",
                                          "test-disclosure.R", "test-cohort-disclosure.R", "test-profiling.R", "test-crosstab.R",
                                          "test-ohdsi-results.R", "test-achilles.R", "test-queries.R")} | \
                 {("dsOMOPClient", f) for f in ("test-dp-exclusive-client.R", "test-privacy-client.R", "test-querylibrary-sticky.R",
                                                "test-session-helpers.R", "test-versions.R", "test-cohort-security.R")}

# ---- explicit reviewed decisions: (package, file, line) -> list of (suffix, class, prior, intended, rationale) ----
D = {}
def dec(pkg, f, line, cls, prior, intended, rationale, suffix=""):
    D.setdefault((pkg, f, line), []).append((suffix, cls, prior, intended, rationale))
d = lambda *a, **k: dec("dsOMOP", *a, **k)
c = lambda *a, **k: dec("dsOMOPClient", *a, **k)
# --- design section 10.9 and ledger rows ---
d("test-filters.R", 18, NPERM, "client-authored ordered threshold refused", "numeric threshold operators compile", "F-23 / design 7.4: operator refusals deleted")
d("test-filters.R", 31, NPERM, "ordered threshold cannot reach extraction", "compiles and executes", "design 7.4")
d("test-filters.R", 450, NPERM, "unissued narrow threshold refused before querying", "runs; publication is the only gate", "design 7.4: bin contract optional")
d("test-filters.R", 503, NPERM, "exact-match filter blocked as fingerprinting", "compiles", "design 7.4: operator refusals deleted")
d("test-filters.R", 530, NPERM, "narrow dates and unsafe operators refused", "compile", "design 7.4")
d("test-filters.R", 657, NPERM, "numeric bins must be authenticated to the session", "bin contract optional", "design 7.4")
d("test-recipe-reach.R", 283, NPERM, "tiny intermediate intersection refused when the gate is on", "private intermediate runs; the final object is gated", "design 7.1/7.2")
d("test-recipe-reach.R", 322, NPERM, "narrow age_range blocked while the gate is on", "any width on assignment paths", "F-26 / design 7.4")
d("test-recipe-setops.R", 247, NPERM, "empty intersection with outputs=list() refused", "private intermediates resolve; nothing is published", "design 7.1: private computed inputs are never gated")
d("test-recipe-setops.R", 268, NPERM, "tiny non-zero intersection with outputs=list() refused", "private intermediates resolve", "design 7.1")
d("test-recipe-setops.R", 443, NPERM, "too-small scope intersected into a population refused", "runs privately", "design 7.1")
d("test-recipe-setops.R", 672, NPERM, "too-small criteria population refused before any output", "private intermediate resolves; publication gates final objects", "design 7.1/7.2")
d("test-plan-cohort-filters.R", 373, NPERM, "numeric has_measurement needs an issued bin", "no issued bin required", "design 7.4")
d("test-plan-index-event.R", 146, NPERM, "ambiguous coverage aborts the plan", "coverage policy (unique: drop)", "A-E6 / design 1.3")
d("test-plan-index-event.R", 243, PRU, "index-event population below k refused", "refused at publication with the constant message", "design 7.2: the final object stays gated")
d("test-output-contracts.R", 72, NPERM, "requested day_of_birth refused as blocked", "birth components are class 2 on assignment paths", "F-25 / design 7.6", suffix="#1")
d("test-output-contracts.R", 72, UI, "unknown person column 'not_a_person_column' not found", "still not found", "structural validation unchanged", suffix="#2")
d("test-output-contracts.R", 99, SAFE, "public common age-grid coarsening accepted", "accepted", "unchanged", suffix="#1")
d("test-output-contracts.R", 99, NPERM, "off-grid age grid refused", "any strictly increasing integer grid starting at 0 on assignment paths", "F-26 / A-1 grid-shape validation", suffix="#2")
d("test-output-contracts.R", 10, NPERM, "exact year_of_birth never crosses the output boundary", "returned when requested; age_group kept by default", "F-25", suffix="#1")
d("test-output-contracts.R", 10, SAFE, "baseline row per cohort member with age_group", "same", "unchanged", suffix="#2")
d("test-disclosure.R", 329, NPERM, "age_group filter enforces the 5-year minimum band width", "no width floor on assignment paths", "F-26 / design 7.4 (aggregate .computeAgeGroups unchanged)")
d("test-disclosure.R", 282, NPERM, "classifyFilter blocks narrow age ranges", ".classifyFilter returns constrained for every known type", "design 7.4")
d("test-disclosure.R", 349, SAFE, "age grid public and configurable (aggregate policy)", "unchanged for aggregate callers", "design 7.6: aggregate grouping untouched")
d("test-feature-semantics.R", 294, SAFE, "future birth years give a missing age", "unchanged", "design 2.7")
c("test-recipe-executable-contract.R", 117, SAFE, "client compiles person.year_of_birth (line 135)", "compiles and now executes on the server", "F-3 resolved in the server's direction", suffix="#2")
# --- server blocks that assert an error (reviewed one by one) ---
d("test-cohort.R", 67, UI, "invalid inclusion criterion fails and temp tables are released", "same", "structural validation unchanged")
d("test-cohort.R", 177, UI, "value_threshold is not executable", "still not executable (legacy_spec refuses it)", "IR legacy_spec excludes value_threshold", suffix="#1")
d("test-cohort.R", 177, NPERM, "unissued numeric bin refused", "bin contract optional", "design 7.4", suffix="#2")
d("test-cohort.R", 362, UI, "persistent cohort id and overwrite validation", "same", "argument validation unchanged")
d("test-cohort.R", 512, PRU, "small cohort set operation refused (creation gate)", "standalone creation gate retained until the section 10.4 audit", "design 1.4")
d("test-cohort.R", 629, UI, "cohort name/identifier validation", "same", "unchanged")
d("test-extraction.R", 67, UI, "block_sensitive=FALSE needs the server permission", "same", "class-1 admin path unchanged (design 7.6)")
d("test-extraction.R", 614, UI, "date_handling validation", "same", "closed date_handling object (A-1)")
d("test-feature-semantics.R", 73, UI, "same-date values need a primary key order", "same", "unchanged")
d("test-feature-semantics.R", 468, UI, "time_since reference validation", "same", "unchanged")
d("test-filters.R", 583, UI, "compileSelect rejects scopes it cannot apply", "same", "filterable-column rule stays")
d("test-longitudinal-panel.R", 198, UI, "person_period caps", "same", "operational caps stay (design 7.7)")
for ln in (40, 87, 119, 150, 158):
    d("test-manipulate.R", ln, UI, "verb argument / protected-column validation", "same", "protected keys and argument checks unchanged")
d("test-manipulate.R", 198, PRU, "filter isolating too few persons refused", "refused at publication", "design 7.2")
d("test-manipulate.R", 208, NPERM, "numeric thresholds refused in omopFilterDS", "numeric operators permitted", "design 7.4 (manipulate.R:309-322)")
d("test-manipulate.R", 216, PRU, "merge yielding too few persons refused", "refused at publication", "design 7.2: final-support rule")
d("test-manipulate.R", 229, PRU, "self-bind doubling two persons refused at k=3", "refused: support counts distinct persons", "design 7.1/7.2")
d("test-output-contracts.R", 1550, UI, "missing results cohort refused", "same", "unchanged")
d("test-phase-b.R", 105, PRU, "cohort-from-table creation gate", "retained until the section 10.4 audit", "design 1.4")
for ln in (176, 520, 2221, 2388, 2420, 2456, 2576, 2600, 2643):
    d("test-privacy-dp.R", ln, UI, "DP option / provenance validation", "same", "DP path unchanged (design 7.5 keeps its checks)")
d("test-recipe-reach.R", 257, UI, "blocked value_as_string value_source refused at compile", "still refused: class 1", "design 7.6 class 1")
d("test-survival-sql.R", 205, PRU, "survival final population below k refused", "refused at publication", "design 7.2")
d("test-survival-sql.R", 240, PRU, "unobservable TAR entry / washout drops leave fewer than k", "episodes drop privately; the final object is gated", "design 6.12/7.2 (WS-7 re-verifies the count on the fixture)")
d("test-survival-validation.R", 1, UI, "survival option validation", "same", "format/order matrix unchanged")
d("test-temporal-sql.R", 326, NPERM, "missing or duplicate index observation periods abort", "coverage policy drops privately", "A-E6")
d("test-temporal-sql.R", 521, UI, "temporal compiler argument validation", "same", "unchanged")
for ln in (43, 77, 119, 144):
    d("test-phase1-leak-gates.R", ln, UI, "class-1 / untyped identifier concept_col overrides refused", "still refused (class 1)", "design 7.6 class 1 unchanged")
for ln in (168, 201, 229, 254, 301, 308, 326, 350, 367):
    d("test-phase1-leak-gates.R", ln, NP, "aggregate endpoint / manifest check", "n/a", "aggregate controls and manifests are outside the plan corpus")
# --- client blocks that assert an error ---
c("test-cohort-atomic-client.R", 347, UI, "source symbol must exist on every server", "same", "unchanged")
c("test-cohort-atomic-client.R", 407, UI, "occupied workspace symbol never replaced", "same", "design 7.3: fresh names, never overwrite")
c("test-longitudinal-panel.R", 31, UI, "v1 person_period rejects a grain option", "same for v1 emission", "v1 plans unchanged")
for ln in (88, 93, 149, 176, 220):
    c("test-manipulate.R", ln, UI, "verb argument validation", "same", "unchanged")
c("test-manipulate.R", 128, UI, "ds.omop.filter op must be ==, !=, in, not_in", "same for the legacy op argument; omop_where(expr=) adds comparisons", "design 2.11")
c("test-phase-b.R", 17, UI, "symbol validation", "same", "unchanged")
for ln in (160, 178, 700, 714):
    c("test-phase-e-ergonomics.R", ln, UI, "out symbol / save-load validation", "same", "unchanged")
for ln in (25, 47, 120, 284, 325, 353, 422, 468, 516, 549):
    c("test-plan-dsl.R", ln, UI, "plan.cohort / harmonisation validation", "same", "harmonisation signature extended, refusals unchanged")
c("test-plan-dsl.R", 884, UI, "missing required bin arguments", "same", "argument validation unchanged (review B7)")
for ln in (131, 220, 254, 278, 288, 298, 309, 431, 526):
    c("test-recipe-circe.R", ln, UI, "Circe export subset limits", "same", "Circe export unchanged in 2.10.3")
c("test-recipe-episode-policy.R", 1, UI, "episode_policy enum", "same", "unchanged")
c("test-recipe-executable-contract.R", 16, UI, "mixed-level OR group refused", "same", "unchanged", suffix="#1")
c("test-recipe-executable-contract.R", 16, NPERM, "NOT groups refused", "omop_not supported in both grammars", "design 6.6", suffix="#2")
c("test-recipe-executable-contract.R", 36, UI, "tampered filters revalidated", "same", "unchanged")
c("test-recipe-executable-contract.R", 73, NPERM, "covariates_sparse has no executable mapping", "format sparse reachable", "B-22", suffix="#1")
c("test-recipe-executable-contract.R", 73, UI, "other non-executable labels/options refused", "same", "unchanged", suffix="#2")
c("test-recipe-executable-contract.R", 117, UI, "wide event rows need an explicit reduction", "same", "unchanged")
c("test-recipe-executable-contract.R", 166, UI, "compiled output name collision", "same", "structural (dynamic inventory is server-side)")
c("test-recipe-executable-contract.R", 216, UI, "feature windows need grain='episode'", "same", "unchanged", suffix="#1")
c("test-recipe-executable-contract.R", 216, NPERM, "mixing windowed and unwindowed variables refused", "one roster, many sources and windows", "B-19", suffix="#2")
c("test-recipe-executable-contract.R", 412, NPERM, "survival/intervals reject visit and concept-column overrides", "sources carry visit/concept_col", "design 2.2/6.12")
c("test-recipe-executable-contract.R", 431, NPERM, "temporal covariates reject an undeclared Cartesian product", "registry covariate identities", "design 6.12")
c("test-recipe-filters-phasec.R", 91, NPERM, "covariates_sparse fails instead of degrading", "format sparse", "B-22")
for ln in (51, 85, 215):
    c("test-recipe-schema.R", ln, UI, "preview schema validation", "same", "unchanged")
c("test-recipe-setops.R", 156, UI, "set-op validation", "same", "unchanged")
for ln in (288, 684, 1092, 1106, 1958):
    c("test-recipe.R", ln, UI, "constructor / editing validation", "same", "unchanged")
c("test-recipe.R", 938, NPERM, "omop_output rejects covariates_sparse", "format sparse", "B-22")
c("test-staged-harmonization-regression.R", 1, UI, "staged execution invalidates harmonisation", "same", "unchanged")
for ln in (68, 160, 240, 298):
    c("test-survival-longitudinal.R", ln, UI, "survival option combinations", "same", "format/order/tie matrix unchanged (tie_policy=error stays refused)")

GATE_RX = re.compile(r"Disclosive|disclosure|nfilter|insufficient|blocked|narrow|width|safe_scope|issued|fingerprint|too small", re.I)
rows = []
def add(pkg, f, b, suffix, cls, prior, intended, rationale):
    rows.append({"plan_id": f"{pkg}:{f}:{b['line']}{suffix}", "package": pkg, "file": f, "line": b["line"], "test": b["test"],
                 "class": cls, "review": "reviewed", "prior_behaviour": prior, "intended_behaviour": intended, "rationale": rationale})
seen = set()
for b in inv:
    pkg, f = b["package"], b["file"]
    key = (pkg, f, b["line"]); seen.add(key)
    if key in D:
        for suffix, cls, prior, intended, rationale in D[key]:
            add(pkg, f, b, suffix, cls, prior, intended, rationale)
        continue
    if (pkg, f) in NON_PLAN_FILES or not b["plan_calls"]:
        add(pkg, f, b, "", NP, "no plan built", "n/a", "block does not build or execute a plan")
        continue
    if b["expect_errors"]:
        gate = any(GATE_RX.search(m) for m in b["expect_errors"])
        if gate:
            raise SystemExit(f"unreviewed gate-asserting block {key}: {b['expect_errors']}")
        add(pkg, f, b, "", UI, "argument/structure validation error", "same error", "validation unrelated to a removed gate; reviewed by its assertion text")
        continue
    add(pkg, f, b, "", SAFE, "executes / compiles and asserts a result the design preserves", "byte-identical result (golden corpus)",
        "no removed gate and no corrected defect is asserted (ledger A-E1/A-E2/A-E4/B-8 checked)")
# every explicit decision must have matched a live block
missing = [k for k in D if k not in seen]
if missing:
    raise SystemExit(f"decisions without a live test block: {missing}")
rows.sort(key=lambda r: (r["package"], r["file"], r["line"], r["plan_id"]))
with open(OUT, "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=["plan_id", "package", "file", "line", "test", "class", "review", "prior_behaviour", "intended_behaviour", "rationale"])
    w.writeheader(); w.writerows(rows)
from collections import Counter
print(len(rows), "rows;", dict(Counter(r["class"] for r in rows)))
