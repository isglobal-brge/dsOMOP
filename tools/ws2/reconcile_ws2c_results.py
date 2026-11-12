#!/usr/bin/env python3
"""Attribute the frozen mixed blocks' captured calls to reviewed corpus subrows.

This is a reporting layer over compare_results.py, not an equality checker. It
never changes a comparison outcome or turns an unmatched call into a match.
The rules below refer to the pinned 2.10.2 test bodies. An unreviewed mixed block
fails, so a block-level union of acceptable outcomes cannot silently waive a
call. A setup/control call can have no corpus subrow; it must be reported as such.
"""
import argparse
import collections
import csv
import json
import os

import compare_results as capture


def attribute(record, by_block):
    key = record["_key"]
    rows = by_block.get(key, [])
    classes = {row["class"] for row in rows}
    status, error, _ = capture.effective(record)
    error = error or ""

    def select(cls=None, suffix=None, reason=""):
        selected = [row for row in rows if
                    (cls is None or row["class"] == cls) and
                    (suffix is None or row["plan_id"] == key + suffix)]
        if not selected:
            raise ValueError("Missing reviewed subrow: " + key + " " + str((cls, suffix)))
        selected_classes = {row["class"] for row in selected}
        if len(selected_classes) != 1:
            raise ValueError("Ambiguous subrow class: " + key)
        return dict(disposition=next(iter(selected_classes)),
                    plan_ids=[row["plan_id"] for row in selected], attribution=reason)

    if not rows:
        return dict(disposition="unclassified", plan_ids=[], attribution="No frozen corpus row")
    if len(classes) == 1:
        if status == "ok" and classes <= {"unchanged_invalid", "publication_refused_unchanged"}:
            return dict(disposition="setup_control", plan_ids=[],
                        attribution="Successful fixture preparation in a refusal-class block; the subsequently refused analysis/helper is not this captured call")
        return select(reason="Single class in the frozen block")

    test = (record.get("context") or {}).get("test")
    # The block key is stable even where the test context omits its title.
    test = key.split(":", 2)[2]
    if test == "baseline output produces one row per cohort member":
        return select(suffix="#2", reason="The only executor call requests derived age_group, not the year_of_birth column of subrow #1")
    if test == "baseline accepts only a public common age-grid coarsening":
        return select(suffix="#1", reason="The captured executor call uses the accepted common grid; off-grid subrow #2 is validation only")
    if test == "streamed event_select keeps an omitted ordering date private":
        symbols = (record.get("call") or {}).get("out_symbols") or {}
        suffix = "#no_requested_roots" if not symbols.get("items") else "#requested_roots"
        return select(suffix=suffix, reason="The recorded requested-root list determines the publication contract")
    if test == "recurrent plan assignment splits and pseudonymizes both components":
        return select(suffix="#below_k", reason="Original pinned fixture has two event persons at threshold three; admissible variants are the separate A-19 memory/staged evidence")
    if test == "reach SAFETY: narrow age_range blocked while gate is ON":
        return select(suffix="#missing_reference", reason="Original pinned min=121/max=122 plan omits an explicit reference; the valid-reference variant is separately tested")
    if test == "legacy or forged cohort numeric thresholds fail closed":
        suffix = "#1" if "value_threshold" in error else "#2"
        return select(suffix=suffix, reason="Five legacy value_threshold inputs and the sixth unissued value_bin input are distinct calls")
    if test == "cohort criteria fail closed instead of becoming unfiltered":
        if status == "ok":
            return dict(disposition="setup_control", plan_ids=[],
                        attribution="This .cohortCreate call creates the base consumed by later .applyInclusionCriteria assertions; it does not execute the short calendar criterion")
        return select(suffix="#1", reason="The captured refusing calls are empty concept_set and unknown concept_sset; the width-floor subrow calls an unwrapped criterion helper")
    if test == "baseline rejects blocked and unknown person columns":
        return select(suffix="#1" if "birth components" in error else "#2",
                      reason="The literal requested column distinguishes exact birth from a nonexistent person column")
    if test == "survival custom filter on an identifier column is rejected":
        return select(cls="unchanged_invalid" if status == "error" else "newly_refused_structural",
                      reason="Strict mode already refused the private identifier; non-strict mode formerly returned a per-output error and now refuses structurally")
    if test == "DP preparation seals plan outputs while plan loading stays DP-free":
        if record["fn"] == "omopPlanExecuteDS":
            return dict(disposition="setup_control", plan_ids=[],
                        attribution="DP-free plan-loading control; it is not the DP executor's corrected seal contract")
        if status == "ok" or "age at index" in error:
            return select(suffix="#1", reason="DP executor seal contract or preserved not-person-local baseline-age refusal, A-19 item 3")
        return select(suffix="#2", reason="Tools-disabled or staged DP preparation remains invalid")
    if classes == {"safe", "newly_permitted"} and test == "index events exclude unobserved candidates and reject ambiguous coverage":
        return select(cls="newly_permitted" if status == "error" else "safe",
                      reason="Second recorded fixture adds overlapping observation periods; first has unambiguous coverage")
    if classes in ({"safe", "unchanged_invalid"}, {"safe", "publication_refused_unchanged"}):
        cls = "safe" if status == "ok" else next(c for c in classes if c != "safe")
        return select(cls=cls, reason="The pinned test's accepted and refusing calls map to its separate accepted/refusal subrows")
    raise ValueError("Mixed block needs explicit call attribution: " + key)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work", required=True)
    parser.add_argument("--baseline", required=True)
    parser.add_argument("--report", required=True)
    parser.add_argument("--classes", default=os.path.join(capture.SERVER_ROOT, "tests/testthat/fixtures/v1-corpus-classes.csv"))
    parser.add_argument("--out", required=True)
    args = parser.parse_args()
    _, blocks = capture.load_classes(args.classes)
    baseline = capture.Run(args.work, args.baseline)
    by_id = {record["id"]: record for record in baseline.top.values()}
    report = json.load(open(args.report, encoding="utf-8"))
    results = []
    for comparison in report["comparisons"]:
        label = comparison["current"]["label"]
        for pair in comparison["pairs"]:
            record = by_id[pair["baseline_id"]]
            attribution = attribute(record, blocks)
            results.append(dict(run=label, key=pair["key"], function=pair["fn"],
                                baseline_id=pair["baseline_id"], current_id=pair["current_id"],
                                plan_sha256=pair["plan_sha256"], occurrence=pair["occurrence"],
                                baseline_status=capture.effective(record)[0],
                                outcome=pair["outcome"], block_class=pair["cls"],
                                **attribution))
    os.makedirs(args.out, exist_ok=True)
    with open(os.path.join(args.out, "call-dispositions.csv"), "w", encoding="utf-8", newline="") as target:
        writer = csv.DictWriter(target, fieldnames=list(results[0]))
        writer.writeheader()
        for result in results:
            writer.writerow(dict(result, plan_ids=" | ".join(result["plan_ids"])))
    counts = collections.defaultdict(collections.Counter)
    for result in results:
        counts[(result["run"], result["disposition"])][result["outcome"]] += 1
    summary = [dict(run=run, disposition=cls, outcomes=dict(outcomes))
               for (run, cls), outcomes in sorted(counts.items())]
    with open(os.path.join(args.out, "call-dispositions.json"), "w", encoding="utf-8") as target:
        json.dump(dict(scope="Matched frozen-corpus top-level calls only; A-19 evidence, unmatched calls and unexecuted blocks remain in b10-report.json", summary=summary, calls=results), target, indent=1)
    for row in summary:
        print(row["run"], row["disposition"], row["outcomes"])


if __name__ == "__main__":
    main()
