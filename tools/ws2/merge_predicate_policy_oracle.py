#!/usr/bin/env python3
"""Merge two independent policy-adjusted baseline runs, preserving provenance.

Usage: merge_predicate_policy_oracle.py REPO SNAPSHOT_E SNAPSHOT_F OUTPUT
Each snapshot contains dsOMOP and an oracle-<label>.json copied from the pod.
Only the exact reviewed 70 cases can enter this fixture. Original refusals
remain immutable; this fixture supplies results under the approved policy.
"""
import hashlib
import json
from pathlib import Path
import subprocess
import sys

BASE = "30b7905617dbe3de5d06699c324095635c68011f"
CLIENT = "7eb2f6c8c2b49e5492e1f778916b84319626341d"


def sha(data):
    return hashlib.sha256(data).hexdigest()


def read(path):
    return json.loads(path.read_text())


def no_temporal(node):
    if isinstance(node, dict):
        classes = node.get("class", [])
        if isinstance(classes, str):
            classes = [classes]
        assert not set(classes).intersection({"Date", "POSIXct", "POSIXlt", "POSIXt"}), (
            "Temporal objects require numeric-payload capture before exact hashing"
        )
        for value in node.values():
            no_temporal(value)
    elif isinstance(node, list):
        for value in node:
            no_temporal(value)


def source_proof(repo, snapshot):
    paths = subprocess.check_output(
        ["git", "ls-tree", "-rz", "--name-only", BASE, "--", "R"], cwd=repo
    ).decode().strip("\0").split("\0")
    hashes = {}
    for rel in paths:
        original = subprocess.check_output(["git", "show", f"{BASE}:{rel}"], cwd=repo)
        actual = (snapshot / "dsOMOP" / rel).read_bytes()
        assert actual == original, f"Baseline R source changed: {rel}"
        hashes[rel] = sha(actual)
    return sha(json.dumps(hashes, sort_keys=True, separators=(",", ":")).encode())


def main(repo, left, right, output):
    raw = read(repo / "tests/testthat/fixtures/ir-v1-compat.json")
    originals = {case["id"]: case for case in raw["cases"]}
    ids = (repo / "tools/ws2/predicate-policy-case-ids.txt").read_text().splitlines()
    assert len(ids) == len(set(ids)) == 70
    runs = []
    captures = []
    for snapshot in (left, right):
        paths = list(snapshot.glob("oracle-*.json"))
        assert len(paths) == 1, f"Expected one complete capture in {snapshot}"
        run = read(paths[0])
        assert run["baseline"] == BASE
        assert run["kind"] == "policy-adjusted baseline predicate oracle; not original baseline equivalence"
        tool = snapshot / "dsOMOP/tools/ws2/capture_predicate_policy_oracle.R"
        assert sha("\n".join(tool.read_text().splitlines()).encode()) == run["tool_sha256"]
        records = {case["id"]: case for case in run["results"]}
        assert len(run["results"]) == 70 and set(records) == set(ids)
        for case_id, record in records.items():
            original = originals[case_id]
            assert original["baseline"]["status"] == "error"
            assert record["plan_sha256"] == original["plan_sha256"], case_id
            if record["status"] == "ok":
                assert sha(record["result"].encode()) == record["result_sha256"]
                no_temporal(json.loads(record["result"]))
            else:
                assert record["status"] == "error" and record["error"]["message"]
        captures.append({
            "label": run["label"],
            "capture_sha256": sha(paths[0].read_bytes()),
            "tool_sha256": run["tool_sha256"],
            "baseline_R_manifest_sha256": source_proof(repo, snapshot),
            "option_state": run["option_state"],
        })
        runs.append((run, records))
    assert captures[0]["label"] != captures[1]["label"]
    for field in ("gate_changes", "original_gate_sha256", "adjusted_gate_sha256", "tool_sha256", "option_state"):
        assert runs[0][0][field] == runs[1][0][field], f"Capture disagreement: {field}"
    cases = {}
    for case_id in ids:
        a, b = (run[1][case_id] for run in runs)
        assert a == b, f"Nondeterministic policy oracle: {case_id}"
        case = {"status": a["status"], "plan_sha256": a["plan_sha256"],
                "original_baseline_error": originals[case_id]["baseline"]["error"]}
        if a["status"] == "ok":
            case["result_sha256"] = a["result_sha256"]
            case["outputs"] = a["summary"]
        else:
            case.update(message=a["error"]["message"], code=a["error"].get("code"))
        cases[case_id] = case
    fixture = {
        "version": 1,
        "description": "NEWLY PERMITTED predicate policy: independent 2.10.2 executor with only five reviewed section 7.4 gate branches adjusted; not unchanged baseline equivalence.",
        "baseline": {"dsOMOP": BASE, "dsOMOPClient": CLIENT},
        "reference": runs[0][0]["reference"],
        "captures": captures,
        "gate_changes": runs[0][0]["gate_changes"],
        "original_gate_sha256": runs[0][0]["original_gate_sha256"],
        "adjusted_gate_sha256": runs[0][0]["adjusted_gate_sha256"],
        "encoding": "Complete compatibility canonical result at 17 significant digits; no Date/POSIX objects occur. Original raw baseline refusals and plan hashes are retained.",
        "cases": cases,
    }
    output.write_text(json.dumps(fixture, indent=2, ensure_ascii=False) + "\n")
    print(f"Verified {len(cases)} deterministic policy-adjusted baseline cases")


if __name__ == "__main__":
    assert len(sys.argv) == 5, __doc__
    main(*(Path(arg).resolve() for arg in sys.argv[1:]))
