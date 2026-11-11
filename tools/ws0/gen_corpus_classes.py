#!/usr/bin/env python3
"""WS-0: enumerate every test that builds or executes a plan in the 2.10.1/2.10.2
suites of both packages and assign its corpus class (design-2.10.3 section 5):
safe / corrected / newly_permitted / newly_refused_structural /
newly_refused_publication. Rows whose class comes from the design's explicit
lists (section 10.9 and the defect ledger) carry source=design; the other rows
are classified by inspection heuristics (source=heuristic) and must be confirmed
by WS-1/WS-2 when the golden corpus is executed. Writes
tests/testthat/fixtures/v1-corpus-classes.csv in the server package.
"""
import csv, os, re, sys
S, C, OUT = sys.argv[1], sys.argv[2], sys.argv[3]
FILES = {
 "dsOMOP": ["test-plan.R", "test-plan-cohort-filters.R", "test-plan-index-event.R", "test-recipe-reach.R",
            "test-recipe-setops.R", "test-filters.R", "test-output-contracts.R", "test-survival-sql.R",
            "test-survival-validation.R", "test-temporal-sql.R", "test-longitudinal-sql.R",
            "test-longitudinal-panel.R", "test-extraction.R", "test-feature-semantics.R", "test-manipulate.R",
            "test-cohort.R", "test-shortcuts-server.R", "test-phase1-window.R", "test-phase1-leak-gates.R",
            "test-staging-contract.R"],
 "dsOMOPClient": ["test-recipe.R", "test-recipe-setops.R", "test-recipe-circe.R", "test-recipe-declarative.R",
                  "test-recipe-executable-contract.R", "test-recipe-filters-phasec.R", "test-recipe-lint.R",
                  "test-phase1-filters.R", "test-plan-dsl.R", "test-shortcuts.R", "test-survival-longitudinal.R",
                  "test-longitudinal-panel.R", "test-recipe-episode-policy.R", "test-feature.R"],
}
# design section 10.9 and ledger: (package, file, line range) -> class, reason
DESIGN = [
 ("dsOMOP", "test-filters.R", 18, 44, "newly_permitted", "numeric threshold operators compile (design 7.4; F-23)"),
 ("dsOMOP", "test-filters.R", 450, 467, "newly_permitted", "narrow threshold no longer gated before querying (design 7.4)"),
 ("dsOMOP", "test-filters.R", 503, 582, "newly_permitted", "exact-match and narrow-date filters permitted (design 7.4)"),
 ("dsOMOP", "test-filters.R", 657, 700, "newly_permitted", "bin contract optional (design 7.4)"),
 ("dsOMOP", "test-recipe-reach.R", 283, 342, "newly_permitted", "tiny intermediate intersection and narrow age_range run; publication is the only gate (design 7.2)"),
 ("dsOMOP", "test-recipe-setops.R", 247, 287, "newly_refused_publication", "set-op result below k is refused at publication, not at the intermediate (design 5, 7.2)"),
 ("dsOMOP", "test-recipe-setops.R", 443, 475, "newly_permitted", "small scope intersected into a population runs privately (design 7.1)"),
 ("dsOMOP", "test-recipe-setops.R", 672, 689, "newly_refused_publication", "small criteria population: refused only when published (design 7.2)"),
 ("dsOMOP", "test-plan-cohort-filters.R", 373, 409, "newly_permitted", "has_measurement numeric filters need no issued bin (design 7.4)"),
 ("dsOMOP", "test-plan-index-event.R", 146, 170, "newly_permitted", "ambiguous coverage follows the coverage policy instead of aborting (A-E6)"),
 ("dsOMOP", "test-plan-index-event.R", 243, 250, "newly_refused_publication", "index-event population gated at publication (design 7.2)"),
 ("dsOMOP", "test-disclosure.R", 339, 339, "newly_permitted", "single-year age band permitted on assignment paths (F-26)"),
 ("dsOMOP", "test-output-contracts.R", 49, 50, "newly_permitted", "year_of_birth returned when requested (F-25)"),
 ("dsOMOP", "test-output-contracts.R", 88, 88, "newly_permitted", "day_of_birth returned when requested (F-25)"),
 ("dsOMOPClient", "test-recipe-executable-contract.R", 135, 141, "safe", "client compiles person.year_of_birth; now also executable on the server (F-3)"),
]
PERMIT_RX = re.compile(r"expect_error\([^\n]*(Disclosive|disclos|nfilter|blocked|narrow|width|safe_scope|issued bin|bin contract|fingerprint|too small|insufficient)", re.I)
REFUSE_RX = re.compile(r"(setop|set-op|set_op)[^\n]*(filter_tree|cohort_definition_id|unknown kind)|tie_policy\s*=\s*['\"]error", re.I)
CORRECT_RX = re.compile(r"(exclude[^\n]*descend|parent_id|same[- ]day[^\n]*union|union[^\n]*same[- ]day|empty concept set|resolves to (no|nothing))", re.I)
rows = []
for pkg, base in (("dsOMOP", S), ("dsOMOPClient", C)):
    for fn in FILES[pkg]:
        path = os.path.join(base, "tests", "testthat", fn)
        if not os.path.exists(path):
            continue
        lines = open(path, encoding="utf-8").read().split("\n")
        starts = [(i + 1, m.group(1)) for i, l in enumerate(lines)
                  for m in [re.match(r'\s*test_that\((["\'])(.*?)\1', l)] if m]
        starts = [(i + 1, re.match(r'\s*test_that\((["\'])(.*?)\1', lines[i]).group(2)) for i in range(len(lines))
                  if re.match(r'\s*test_that\((["\'])(.*?)\1', lines[i])]
        for k, (ln, name) in enumerate(starts):
            end = starts[k + 1][0] - 1 if k + 1 < len(starts) else len(lines)
            body = "\n".join(lines[ln - 1:end])
            cls, src, reason = "safe", "heuristic", "no removed gate or corrected defect referenced"
            for dpkg, dfn, a, b, dcls, dreason in DESIGN:
                if dpkg == pkg and dfn == fn and a <= ln <= b:
                    cls, src, reason = dcls, "design", dreason
                    break
            if src == "heuristic":
                if REFUSE_RX.search(body):
                    cls, reason = "newly_refused_structural", "asserts a shape now rejected by the schema (A-E2 / tie_policy=error)"
                elif CORRECT_RX.search(body):
                    cls, reason = "corrected", "touches a corrected defect (A-E1 / B-8 / A-E4)"
                elif PERMIT_RX.search(body):
                    cls, reason = "newly_permitted", "asserts a disclosure refusal on a private intermediate or a granularity gate (design 7.4)"
            rows.append({"package": pkg, "file": fn, "line": ln, "test": name, "class": cls,
                         "source": src, "reason": reason})
os.makedirs(os.path.dirname(OUT), exist_ok=True)
with open(OUT, "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=["package", "file", "line", "test", "class", "source", "reason"])
    w.writeheader(); w.writerows(rows)
from collections import Counter
print(len(rows), "tests;", Counter(r["class"] for r in rows), Counter(r["source"] for r in rows))
