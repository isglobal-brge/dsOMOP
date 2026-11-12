#!/usr/bin/env python3
"""WS-0 (review B7): inventory of every test block that builds, compiles or
executes a plan in the pre-change suites of both packages, with the signals a
reviewer needs (gate assertions, functions called, options set). It SUGGESTS
nothing; the reviewed classes live in tests/testthat/fixtures/v1-corpus-classes.csv
and are written by hand from this inventory (tools/ws0/review_corpus_classes.py).

Usage: python3 tools/ws0/corpus_inventory.py <dsOMOP> <dsOMOPClient> <out.json>
"""
import json, os, re, sys
S, C, OUT = sys.argv[1], sys.argv[2], sys.argv[3]
PLAN_RX = re.compile(r"\.planExecute\(|omopPlanExecuteDS|omopDpPlanExecuteDS|\.planValidate\(|omopPlanPreviewDS|omopPlanValidateDS|recipe_to_plan\(|ds\.omop\.plan|omop\.plan\(|ds\.omop\.recipe|recipe_execute\(|omop_recipe\(|omop\.cohort|omopCohortCreateDS|omopCohortCombineDS|omopCohortFromTableDS|\.cohortCreate\(|\.cohortCombine\(|omopMergeDS|omopFilterDS|omopSelectDS|omopBindRowsDS|ds\.omop\.(merge|filter|select|bind|cohort)|\.compileLongitudinalSurvivalSql|\.compileIntervalsLongSql|\.compileTemporalSqlComponents|\.extractPersonPeriod|\.compileSelect\(|\.toFeatures\(|\.compileCohortFilterLeaf|\.windowPredicateSql|\.extractBaseline|ds\.omop\.dp\.prepare|dp_prepare|omop\.dp|\.planExecuteImpl|\.plan_execute_impl|omop_study\(|omop_output\(|omop_population\(|omopStagedDatasetPath|staged_manifest|\.stagedPlan|omopCohortListDS|\.assertMinPersons|cohort_disclosure")
GATE_RX = re.compile(r"Disclosive|disclos|nfilter|blocked|narrow|width|safe_scope|issued|bin|fingerprint|too small|insufficient|fail-closed|fail closed|minimum", re.I)
def blocks(path):
    lines = open(path, encoding="utf-8").read().split("\n")
    starts = [(i + 1, m.group(2)) for i, l in enumerate(lines) for m in [re.match(r'\s*test_that\((["\'])(.*?)\1', l)] if m]
    out = []
    for k, (ln, name) in enumerate(starts):
        end = starts[k + 1][0] - 1 if k + 1 < len(starts) else len(lines)
        body = "\n".join(lines[ln - 1:end])
        errs = re.findall(r'expect_error\((?:[^()]|\([^()]*\))*?,\s*(["\'])(.*?)\1', body, re.S)
        errs = [e[1] for e in errs]
        errs += re.findall(r'expect_(?:warning|message)\((?:[^()]|\([^()]*\))*?,\s*(["\'])(.*?)\1', body, re.S) and []
        calls = sorted(set(m.group(0) for m in PLAN_RX.finditer(body)))
        opts = re.findall(r"nfilter\.subset\s*=\s*([0-9]+)", body)
        skips = re.findall(r"skip_if[^\n]*", body)[:3]
        out.append({"line": ln, "test": name, "n_lines": end - ln + 1, "plan_calls": calls, "expect_errors": errs,
                    "gate_words": sorted(set(w.lower() for w in GATE_RX.findall(body))), "nfilter_subset": opts, "skips": skips})
    return out
inv = []
for pkg, base in (("dsOMOP", S), ("dsOMOPClient", C)):
    tdir = os.path.join(base, "tests", "testthat")
    for fn in sorted(os.listdir(tdir)):
        if not fn.startswith("test-") or not fn.endswith(".R"):
            continue
        bl = blocks(os.path.join(tdir, fn))
        if not any(b["plan_calls"] for b in bl):
            continue
        for b in bl:
            b.update({"package": pkg, "file": fn})
            inv.append(b)
json.dump(inv, open(OUT, "w"), indent=1)
from collections import Counter
print(len(inv), "blocks in", len(set((b["package"], b["file"]) for b in inv)), "files;", sum(1 for b in inv if b["plan_calls"]), "with plan calls;", sum(1 for b in inv if b["expect_errors"]), "with expect_error")
