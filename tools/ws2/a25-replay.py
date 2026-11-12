#!/usr/bin/env python3
"""Replay A-25 full results on immutable source snapshots via the test pod.

Run from any directory: python3 tools/ws2/a25-replay.py [current-commit]
All snapshots stay under the assigned worktree's ignored tools/ws2/artifacts.
Only new probe fixtures/tests are overlaid: every R source and original helper
is read directly from the selected git object. The baseline's 52-file supplied
manifest is also verified when present. No R package function is patched.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tarfile

ROOT = Path(__file__).resolve().parents[2]
PIN = "30b7905617dbe3de5d06699c324095635c68011f"
ARTIFACTS = ROOT / "tools/ws2/artifacts/a25-replay"
EVIDENCE = ROOT / "tools/ws2/a25-evidence"
RTEST = Path("/workspace/dsomop/tools/rtest.sh")
SELECTED = ["R", "DESCRIPTION", "NAMESPACE", "LICENSE", "inst",
            "tests/testthat/helper-db.R", "tests/testthat/helper-safe-bins.R",
            "tests/testthat/helper-vendor-db.R", "tests/fixtures/create_test_db.R"]


def run(args, **kwargs):
    return subprocess.run(args, check=True, text=True, **kwargs)


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def snapshot(side, revision):
    path = ARTIFACTS / side
    if path.exists():
        raise RuntimeError(f"Refusing to replace existing snapshot {path}")
    path.mkdir(parents=True)
    proc = subprocess.Popen(["git", "archive", revision, *SELECTED], cwd=ROOT,
                            stdout=subprocess.PIPE)
    with tarfile.open(fileobj=proc.stdout, mode="r|") as archive:
        archive.extractall(path=path)  # trusted local git archive
    if proc.wait() != 0:
        raise RuntimeError("git archive failed")
    manifest = {str(p.relative_to(path)): sha(p) for p in sorted(path.rglob("*")) if p.is_file()}
    if side == "baseline":
        supplied = ROOT / "tools/ws2/artifacts/v1-owned-validated-manifest.json"
        if supplied.exists():
            pinned = json.loads(supplied.read_text())
            assert pinned["baseline"] == PIN
            assert len(pinned["sha256"]) == 52
            assert all(manifest[name] == value for name, value in pinned["sha256"].items())
    tools = path / "tools/ws2"
    tools.mkdir(parents=True)
    shutil.copyfile(ROOT / "tools/ws2/a25-full-results.R", tools / "a25-full-results.R")
    shutil.copyfile(ROOT / "tests/testthat/test-ir-owned-cohort.R", tools / "a25-fixture.R")
    (path / "tests/testthat/test-a25-replay.R").write_text(
        'Sys.setenv(A25_FIXTURE_FILE = testthat::test_path("..", "..", "tools", "ws2", "a25-fixture.R"))\n'
        'source(testthat::test_path("..", "..", "tools", "ws2", "a25-full-results.R"), local = TRUE)\n')
    return path, manifest


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("revision", nargs="?", default="HEAD")
    parser.add_argument("--current-only", action="store_true",
                        help="Reuse the successful baseline evidence and capture a new current snapshot")
    args = parser.parse_args()
    revision = run(["git", "rev-parse", args.revision],
                   cwd=ROOT, capture_output=True).stdout.strip()
    host, port = run(["python3", "/workspace/astra/runpod/deckpod.py", "ssh-target"],
                     capture_output=True).stdout.split()
    ssh = ["ssh", "-i", str(Path.home() / ".ssh/runpod_cover"), "-o", "StrictHostKeyChecking=no",
           "-o", "UserKnownHostsFile=/dev/null", "-o", "LogLevel=ERROR", "-p", port, f"root@{host}"]
    EVIDENCE.mkdir(parents=True, exist_ok=True)
    evidence = {"baseline": PIN, "current": revision, "scope": "focused A-25 replay; not full suites",
                "probe_sha256": sha(ROOT / "tools/ws2/a25-full-results.R"),
                "fixture_sha256": sha(ROOT / "tests/testthat/test-ir-owned-cohort.R"), "runs": {}}
    baseline_rds = None
    jobs = [("baseline", PIN), ("current", revision)]
    if args.current_only:
        previous = json.loads((EVIDENCE / "verification.json").read_text())
        assert previous["baseline"] == PIN
        baseline = previous["runs"]["baseline"]
        assert not (baseline["failed"] or baseline["errors"] or baseline["skipped"])
        assert baseline["results_rds_sha256"] == sha(EVIDENCE / "baseline-results.rds")
        evidence["runs"]["baseline"] = baseline
        baseline_rds = EVIDENCE / "baseline-results.rds"
        jobs = [("current", revision)]
    for side, commit in jobs:
        snapshot_name = side if not args.current_only else side + "-" + commit[:12]
        path, manifest = snapshot(snapshot_name, commit)
        remote = Path("/root/rt") / path.relative_to("/workspace/dsomop")
        output_rds = f"/root/a25-{side}-{commit}.results.rds"
        env = os.environ.copy()
        env["RTEST_ENV"] = f"A25_OUTPUT_RDS={output_rds}"
        if baseline_rds:
            shutil.copyfile(baseline_rds, path / "tools/ws2/a25-baseline-results.rds")
            env["RTEST_ENV"] += f" A25_BASELINE_RDS={remote}/tools/ws2/a25-baseline-results.rds"
        print(f"Running {side} {commit}", flush=True)
        result = run([str(RTEST), str(path), "a25-replay", "900"], env=env, capture_output=True)
        (EVIDENCE / f"{side}-summary.log").write_text(result.stdout + result.stderr)
        raw = run([*ssh, "cat " + shlex.quote(str(remote) + ".last.log")], capture_output=True).stdout
        (EVIDENCE / f"{side}-full.log").write_text(raw)
        totals = re.findall(r"RESULT failed: (\d+) errors: (\d+) skipped: (\d+) passed: (\d+)", raw)
        if not totals:
            raise RuntimeError(f"Missing result totals for {side}; see {EVIDENCE}")
        failed, errors, skipped, passed = map(int, totals[-1])
        evidence["runs"][side] = {"failed": failed, "errors": errors, "skipped": skipped,
            "passed": passed, "snapshot_path": str(path.relative_to(ROOT)), "source_sha256": manifest, "raw_log_sha256": sha(EVIDENCE / f"{side}-full.log")}
        (EVIDENCE / "verification.json").write_text(json.dumps(evidence, indent=2) + "\n")
        print(result.stdout, flush=True)
        if failed or errors or skipped:
            raise RuntimeError(f"Replay did not pass cleanly for {side}")
        # RDS is complete, lossless evidence, small enough to preserve locally.
        payload = subprocess.check_output([*ssh, "cat " + shlex.quote(output_rds)])
        (EVIDENCE / f"{side}-results.rds").write_bytes(payload)
        evidence["runs"][side]["results_rds_sha256"] = hashlib.sha256(payload).hexdigest()
        if side == "baseline":
            baseline_rds = EVIDENCE / "baseline-results.rds"
    supplied = ROOT / "tools/ws2/artifacts/v1-owned-validated-manifest.json"
    if supplied.exists():
        pinned = json.loads(supplied.read_text())
        assert pinned["baseline"] == PIN and len(pinned["sha256"]) == 52
        observed = evidence["runs"]["baseline"]["source_sha256"]
        assert all(observed[name] == value for name, value in pinned["sha256"].items())
        evidence["supplied_baseline_manifest_verified_count"] = 52
    evidence["all_nine_full_records_identical"] = (
        evidence["runs"]["baseline"]["results_rds_sha256"] == evidence["runs"]["current"]["results_rds_sha256"])
    assert evidence["all_nine_full_records_identical"]
    (EVIDENCE / "verification.json").write_text(json.dumps(evidence, indent=2) + "\n")
    print(f"All nine full records match exactly. Evidence: {EVIDENCE}", flush=True)


if __name__ == "__main__":
    main()
