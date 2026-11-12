#!/usr/bin/env python3
"""WS-0 (review B7, re-review B7): the REVIEWED v1 corpus classes.

Every row of tests/testthat/fixtures/v1-corpus-classes.csv is an explicit,
hand-written disposition of one test block (or of one sub-row `#k` when a
block mixes dispositions) from tools/ws0/corpus_decisions.csv, joined with the
live inventory of tools/ws0/corpus_inventory.py. Nothing is inferred: a block
without a decision, a decision without a live block, a plan-building block
marked `non_plan`, or a block that builds no plan but carries another class
(without the explicit `gate:` / `no plan:` justification prefix) makes this
script fail. Classes:
  safe                       executes today and keeps byte-identical results
  corrected                  executes today with a result the ledger declares wrong
  newly_permitted            refused today by an intermediate/granularity gate; runs in 2.10.3
  newly_refused_structural   accepted today, invalid in 2.10.3 (schema/shape)
  newly_refused_publication  accepted today, refused at publication in 2.10.3
  publication_refused_unchanged  a final-publication (or retained creation-gate) refusal that stays
  unchanged_invalid          argument/structure/class-1 validation that stays an error
  non_plan                   a block in an inventoried file that builds no plan
Usage:
  python3 tools/ws0/review_corpus_classes.py <dsOMOP> <dsOMOPClient> <out.csv> [--check]
With --check the fixture at <out.csv> is compared with the regenerated rows
instead of being written (the CI oracle uses this).
"""
import csv, io, os, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from corpus_inventory import inventory, builds_plan

CLASSES = ("safe", "corrected", "newly_permitted", "newly_refused_structural", "newly_refused_publication",
           "publication_refused_unchanged", "unchanged_invalid", "non_plan")
FIELDS = ["plan_id", "package", "file", "line", "test", "class", "review", "prior_behaviour", "intended_behaviour", "rationale"]


def load_decisions(path):
    rows = list(csv.DictReader(open(path, newline="", encoding="utf-8")))
    by_block = {}
    for r in rows:
        pid = r["plan_id"]
        base = pid.split("#")[0]
        pkg, f, line = base.split(":")
        by_block.setdefault((pkg, f, int(line)), []).append(r)
    return rows, by_block


def build_rows(S, C, decisions_path):
    inv = inventory(S, C)
    decisions, by_block = load_decisions(decisions_path)
    problems = []
    seen = set()
    out = []
    for b in inv:
        key = (b["package"], b["file"], b["line"])
        seen.add(key)
        decs = by_block.get(key)
        if not decs:
            problems.append(f"no decision for {key[0]}:{key[1]}:{key[2]} ({b['test']!r})")
            continue
        plan = builds_plan(b)
        for r in decs:
            cls = r["class"]
            if cls not in CLASSES:
                problems.append(f"{r['plan_id']}: unknown class {cls!r}")
            rat = r["rationale"].strip()
            if plan and cls == "non_plan" and not rat.startswith("no plan:"):
                problems.append(f"{r['plan_id']}: builds a plan ({b['plan_calls'] or list(b['plan_helpers'])}) but is non_plan")
            if not plan and cls != "non_plan" and not rat.startswith("gate:"):
                problems.append(f"{r['plan_id']}: builds no plan but is {cls} (prefix the rationale with 'gate:' if a gate helper is under review)")
            if not r["prior_behaviour"].strip() or not r["intended_behaviour"].strip() or not rat:
                problems.append(f"{r['plan_id']}: empty prior/intended/rationale")
            out.append({"plan_id": r["plan_id"], "package": b["package"], "file": b["file"], "line": b["line"], "test": b["test"],
                        "class": cls, "review": "reviewed", "prior_behaviour": r["prior_behaviour"],
                        "intended_behaviour": r["intended_behaviour"], "rationale": r["rationale"]})
        suffixes = [r["plan_id"].split("#")[1] if "#" in r["plan_id"] else "" for r in decs]
        if len(suffixes) > 1 and "" in suffixes:
            problems.append(f"{key}: sub-rows and an unsuffixed row cannot be mixed")
        if len(set(suffixes)) != len(suffixes):
            problems.append(f"{key}: duplicate decision rows")
    for key in by_block:
        if key not in seen:
            problems.append(f"decision without a live test block: {key[0]}:{key[1]}:{key[2]}")
    out.sort(key=lambda r: (r["package"], r["file"], r["line"], r["plan_id"]))
    return out, problems


def to_csv_text(rows):
    buf = io.StringIO()
    w = csv.DictWriter(buf, fieldnames=FIELDS, lineterminator="\n")
    w.writeheader(); w.writerows(rows)
    return buf.getvalue()


def main():
    S, C, OUT = sys.argv[1:4]
    check = "--check" in sys.argv[4:]
    decisions_path = os.path.join(S, "tools", "ws0", "corpus_decisions.csv")
    rows, problems = build_rows(S, C, decisions_path)
    if problems:
        print("corpus review problems:")
        for p in problems:
            print(" -", p)
        sys.exit(1)
    text = to_csv_text(rows)
    from collections import Counter
    summary = f"{len(rows)} rows; {dict(sorted(Counter(r['class'] for r in rows).items()))}"
    if check:
        current = open(OUT, encoding="utf-8").read() if os.path.exists(OUT) else ""
        if current != text:
            print("v1-corpus-classes.csv is out of date with corpus_decisions.csv / the live inventory; regenerate it")
            sys.exit(1)
        print("v1-corpus-classes.csv is in step;", summary)
    else:
        with open(OUT, "w", encoding="utf-8", newline="") as fh:
            fh.write(text)
        print(summary)


if __name__ == "__main__":
    main()
