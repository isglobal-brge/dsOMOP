#!/usr/bin/env python3
"""WS-0 (review B7, CR-7): inventory of every test block of the BASELINE
(2.10.2) suites of both packages that builds, compiles or executes a plan, with
the signals a reviewer needs (gate assertions, functions called, options set).
It SUGGESTS nothing; the reviewed classes live in tools/ws0/corpus_decisions.csv
(one explicit disposition per block or sub-row, written by hand from this
inventory) and are joined by tools/ws0/review_corpus_classes.py.

The corpus describes the 2.10.2 suites as pinned in tools/ws0/corpus_baseline.json
(one commit per package, read with `git show`), never the live working tree:
later edits to a v1 test cannot move, add or remove corpus rows (CR-7). Blocks
are identified by the STABLE key `package:file:<test_that description>`, with
`@k` appended to the k-th repetition of a description inside one file and `#sub`
for the sub-rows of a mixed block; baseline line numbers are informational.

Plan-building calls are detected directly (PLAN_RX) and TRANSITIVELY through
helper functions defined in the test files and helper-*.R files: a helper
whose body calls a plan function (or another plan-building helper) counts as a
plan call wherever it is used, so `.ie_resolve(...)`, `.so_plan(...)` and the
like are inventoried as plan-building (re-review B7). Gate helpers the design
names (GATE_HELPER_RX) are inventoried too so their blocks get a disposition.

Usage: python3 tools/ws0/corpus_inventory.py <dsOMOP> <dsOMOPClient> <out.json> [--live]
  --live inventories the working trees instead of the pinned baseline (exploration only).
"""
import json, os, re, subprocess, sys

PLAN_RX = re.compile(r"\.planExecute\(|omopPlanExecuteDS|omopDpPlanExecuteDS|\.planValidate\(|omopPlanPreviewDS|omopPlanValidateDS|recipe_to_plan\(|ds\.omop\.plan|omop\.plan\(|ds\.omop\.recipe|recipe_execute\(|omop_recipe\(|ds\.omop\.cohort\.(create|from_table|combine)|omopCohortCreateDS|omopCohortCombineDS|omopCohortFromTableDS|\.cohortCreate\(|\.cohortCombine\(|\.cohortFromTokenFrame\(|omopMergeDS|omopFilterDS|omopSelectDS|omopBindRowsDS|ds\.omop\.(merge|filter|select|bind)|\.compileLongitudinalSurvivalSql|\.compileIntervalsLongSql|\.compileTemporalSqlComponents|\.extractPersonPeriod|\.extractTemporalCovariates\(|\.compileSelect\(|\.compileTemporalWhere\(|\.toFeatures\(|\.compileCohortFilterLeaf|\.windowPredicateSql|\.extractBaseline|ds\.omop\.dp\.prepare\(|dp_prepare|\.planExecuteImpl|\.plan_execute_impl|omop_study\(|omop_output\(|omop_population\(|omopStagedDatasetPath|staged_manifest|\.stagedPlan|omopCohortListDS|\.planResolvePopulations\(|\.planResolveIndexEvent\(|\.resolvePopulation\(|\.extractTable\(")
# gate helpers: not plan builders, but the design names their tests in the corpus review (section 10.9)
GATE_HELPER_RX = re.compile(r"\.assertMinPersons|cohort_disclosure|\.classifyFilter\(|\.computeAgeGroups\(|\.assertCustomFilterSafe\(|\.checkDisclosure|\.resolveConceptScopeColumn\(|\.validateDateBounds\(|omop_filter\(|omop_filter_value\(|omop_filter_has_measurement\(")
GATE_RX = re.compile(r"Disclosive|disclos|nfilter|blocked|narrow|width|safe_scope|issued|bin|fingerprint|too small|insufficient|fail-closed|fail closed|minimum", re.I)
DEF_RX = re.compile(r"^(\s*)(`?[A-Za-z_.][\w.]*`?)\s*(<-|=)\s*function\s*\(", re.M)
BASELINE_FILE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "corpus_baseline.json")


def _skip_string(text, i):
    ch = text[i]; j = i + 1
    while j < len(text) and text[j] != ch:
        j += 2 if text[j] == "\\" else 1
    return j


def _function_extent(text, start):
    """End offset of the function definition whose `function(` parenthesis is at `start`."""
    i = start; depth = 0
    while i < len(text):
        ch = text[i]
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                break
        elif ch == "#":
            i = text.find("\n", i)
            if i < 0: return len(text)
        elif ch in "\"'":
            i = _skip_string(text, i)
        i += 1
    j = i + 1
    while j < len(text) and text[j] in " \t\r\n":
        j += 1
    if j < len(text) and text[j] == "{":
        depth = 0; k = j
        while k < len(text):
            ch = text[k]
            if ch == "{":
                depth += 1
            elif ch == "}":
                depth -= 1
                if depth == 0:
                    return k + 1
            elif ch == "#":
                k = text.find("\n", k)
                if k < 0: return len(text)
            elif ch in "\"'":
                k = _skip_string(text, k)
            k += 1
        return len(text)
    depth = 0; k = j   # single-expression body: to the end of the line, continuing while brackets stay open
    while k < len(text):
        ch = text[k]
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif ch == "#":
            k = text.find("\n", k)
            if k < 0: return len(text)
            continue
        elif ch in "\"'":
            k = _skip_string(text, k)
        elif ch == "\n" and depth <= 0:
            return k
        k += 1
    return len(text)


def helper_definitions(text):
    """name -> [body text] of every `name <- function(...)` definition in `text`."""
    out = {}
    for m in DEF_RX.finditer(text):
        name = m.group(2).strip("`")
        end = _function_extent(text, m.end() - 1)
        out.setdefault(name, []).append(text[m.end():end])
    return out


def _calls(name, body):
    return re.search(r"(?<![\w.])" + re.escape(name) + r"\s*\(", body) is not None


def plan_helpers(defs):
    """Names of helpers that build plans directly or through other helpers (fixpoint)."""
    building = set(); changed = True
    while changed:
        changed = False
        for name, bodies in defs.items():
            if name in building:
                continue
            for body in bodies:
                if PLAN_RX.search(body) or any(_calls(h, body) for h in building):
                    building.add(name); changed = True; break
    return building


def helper_chain(name, defs, building, seen=None):
    seen = set(seen or ()); seen.add(name)
    for body in defs.get(name, []):
        direct = sorted(set(m.group(0) for m in PLAN_RX.finditer(body)))
        if direct:
            return [name, direct[0]]
        for h in sorted(building):
            if h not in seen and _calls(h, body):
                return [name] + helper_chain(h, defs, building, seen)
    return [name]


def load_baseline(path=BASELINE_FILE):
    """The pinned baseline commits {package: sha}, or None when no pin file exists."""
    if not os.path.exists(path):
        return None
    pins = json.load(open(path, encoding="utf-8"))
    return {k: v for k, v in pins.items() if k in ("dsOMOP", "dsOMOPClient")}


def _git(repo, *args):
    return subprocess.run(["git", "-C", repo] + list(args), check=True, capture_output=True, text=True).stdout


def _test_files(base, sha):
    """[(name, text)] of tests/testthat/*.R at the baseline commit, or in the working tree when sha is None."""
    if sha is None:
        tdir = os.path.join(base, "tests", "testthat")
        names = sorted(fn for fn in os.listdir(tdir) if fn.endswith(".R"))
        return [(fn, open(os.path.join(tdir, fn), encoding="utf-8").read()) for fn in names]
    listing = _git(base, "ls-tree", "--name-only", sha, "tests/testthat/")
    names = sorted(os.path.basename(p) for p in listing.split("\n") if p.endswith(".R"))
    return [(fn, _git(base, "show", f"{sha}:tests/testthat/{fn}")) for fn in names]


def blocks_from_text(text, global_defs):
    lines = text.split("\n")
    local_defs = {k: list(v) for k, v in global_defs.items()}
    for k, v in helper_definitions(text).items():
        local_defs.setdefault(k, []).extend(v)
    building = plan_helpers(local_defs)
    starts = [(i + 1, m.group(2)) for i, l in enumerate(lines) for m in [re.match(r'\s*test_that\((["\'])(.*?)\1', l)] if m]
    out = []
    for k, (ln, name) in enumerate(starts):
        end = starts[k + 1][0] - 1 if k + 1 < len(starts) else len(lines)
        body = "\n".join(lines[ln - 1:end])
        errs = [e[1] for e in re.findall(r'expect_error\((?:[^()]|\([^()]*\))*?,\s*(["\'])(.*?)\1', body, re.S)]
        calls = sorted(set(m.group(0) for m in PLAN_RX.finditer(body)))
        gate_calls = sorted(set(m.group(0) for m in GATE_HELPER_RX.finditer(body)))
        via = {h: " -> ".join(helper_chain(h, local_defs, building)) for h in sorted(building) if _calls(h, body)}
        opts = re.findall(r"nfilter\.subset\s*=\s*([0-9]+)", body)
        skips = re.findall(r"skip_if[^\n]*", body)[:3]
        out.append({"line": ln, "test": name, "n_lines": end - ln + 1, "plan_calls": calls, "plan_helpers": via, "gate_calls": gate_calls,
                    "expect_errors": errs, "n_expect": len(re.findall(r"expect_[a-z_0-9]+\(", body)),
                    "n_expect_error": len(re.findall(r"expect_error\(", body)),
                    "gate_words": sorted(set(w.lower() for w in GATE_RX.findall(body))), "nfilter_subset": opts, "skips": skips, "body": body})
    return out


def inventory(S, C, baseline="pinned"):
    """Inventory both suites. baseline: "pinned" (tools/ws0/corpus_baseline.json), None (working trees) or {package: sha}."""
    if baseline == "pinned":
        baseline = load_baseline()
    inv = []
    for pkg, base in (("dsOMOP", S), ("dsOMOPClient", C)):
        sha = (baseline or {}).get(pkg)
        files = _test_files(base, sha)
        global_defs = {}
        for fn, text in files:
            if fn.startswith("helper"):
                for k, v in helper_definitions(text).items():
                    global_defs.setdefault(k, []).extend(v)
        for fn, text in files:
            if not fn.startswith("test-"):
                continue
            bl = blocks_from_text(text, global_defs)
            if not any(b["plan_calls"] or b["plan_helpers"] or b["gate_calls"] for b in bl):
                continue
            seen = {}
            for b in bl:
                n = seen.get(b["test"], 0) + 1
                seen[b["test"]] = n
                b.update({"package": pkg, "file": fn, "baseline": sha,
                          "key": f"{pkg}:{fn}:{b['test']}" + (f"@{n}" if n > 1 else "")})
                inv.append(b)
    return inv


def builds_plan(b):
    return bool(b["plan_calls"] or b["plan_helpers"])


if __name__ == "__main__":
    S, C, OUT = sys.argv[1], sys.argv[2], sys.argv[3]
    inv = inventory(S, C, baseline=None if "--live" in sys.argv[4:] else "pinned")
    json.dump([{k: v for k, v in b.items() if k != "body"} for b in inv], open(OUT, "w"), indent=1)
    print(len(inv), "blocks in", len(set((b["package"], b["file"]) for b in inv)), "files;",
          sum(1 for b in inv if builds_plan(b)), "build plans (",
          sum(1 for b in inv if not b["plan_calls"] and b["plan_helpers"]), "only via helpers );",
          sum(1 for b in inv if b["expect_errors"]), "with expect_error;",
          "baseline:", sorted(set(str(b["baseline"]) for b in inv)))
