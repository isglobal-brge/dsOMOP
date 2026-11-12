#!/usr/bin/env python3
r"""WS-2 tooling (dsOMOP 2.10.3), review B10: result fidelity of the v1 corpus.

Six subcommands (paths absolute or relative to the working directory). The
baseline captures, the pins and the full current-side re-run on the final
commits are (A19 = the A-19 evidence test and the open-item (i) probe, copied
into every snapshot so that one pair of baseline captures pins everything):

  W=<scratch>/b10
  A19="--add-tests tests/testthat/test-ir-results-a19.R --add-tests tools/ws2/test-b10-probe-setop.R"
  python3 tools/ws2/compare_results.py capture --side baseline --label b17-a --work $W $A19
  python3 tools/ws2/compare_results.py capture --side baseline --label b17-b --work $W $A19
  python3 tools/ws2/compare_results.py pin     --work $W --baseline b17-a --baseline-repeat b17-b
  python3 tools/ws2/compare_results.py pin-a19 --work $W --baseline b17-a --baseline-repeat b17-b
  python3 tools/ws2/compare_results.py capture --side current --label cur --work $W $A19
  python3 tools/ws2/compare_results.py capture --side hybrid  --label hyb --work $W $A19
  python3 tools/ws2/compare_results.py compare --work $W --baseline b17-a \
      --baseline-repeat b17-b --current cur --current hyb --out $W/report
  python3 tools/ws2/compare_results.py validate --side baseline --label val-base --work $W
  python3 tools/ws2/compare_results.py validate --side current  --label val-cur  --work $W

(--server-commit / --client-commit select other commits than HEAD.)

  capture  snapshot both packages (git archive of the commits), copy
           tools/ws2/capture_results.R into each snapshot's tests/testthat/ as
           helper-zzz-b10-capture.R (and every --add-tests FILE of this worktree,
           under its own name; a test-ir-results*.R file brings this worktree's
           fixtures/ir-v1-results*.json along), run the suites on the test pod through
           tools/rtest.sh (server suite with RTEST_WITH=<client snapshot>, client
           suite with RTEST_WITH=<server snapshot>; no vendor-database variables)
           and fetch the JSON-lines captures into <work>/caps/<label>/ (meta.json
           records commits, helper hash, added tests and suite results). --side
           baseline takes the commits pinned in tools/ws0/corpus_baseline.json;
           --side current defaults to HEAD of --server-repo / --client-repo
           (defaults: the worktree holding this script and its sibling
           dsOMOPClient); --side hybrid runs the PINNED 2.10.2 tests/ against the
           current package code (the frozen classes describe exactly those blocks,
           CR-7). --suites server,client (default both), --filter REGEX, --timeout
           SECONDS. Pod output: /root/rt/ws2b-b10/caps/<label>/; rtest.sh mirrors a
           snapshot at /root/rt/<local path>; the snapshot is deleted after the
           capture unless --keep-snapshot.

  compare  pair the TOP-LEVEL records of a baseline capture with those of each
           current / hybrid capture (a .planExecute() or .planResolvePopulations()
           call made by a test, or an omopPlanExecuteDS() / omopDpPlanExecuteDS()
           call; calls nested in another captured call are diagnostics only).
           Doubles are compared losslessly (17 significant digits). Pairs share the test block
           key, the function and the plan hash (the ir-v1-corpus.json form), first
           with the same complete input (output mode, requested symbols, server
           options, DSOMOP_* environment, fixture data), in call order; then, for
           the rest, with the same input except the fixture data, flagged
           `_fixture_differs`. Each pair is bound to tests/testthat/fixtures/
           v1-corpus-classes.csv at block level (a block with `#sub` rows is bound to
           all of them) and gets an outcome: identical / different (with every
           output and aspect that differs) / refused at baseline only / refused
           currently only / both refused (same message or not). A second baseline
           capture (--baseline-repeat) detects nondeterministic records, which are
           excluded; a record that agrees once the DP provenance fields keyed by a
           per-run random noise root (dataset_id, lineage_id, mac) are masked is
           compared with them masked, and listed. The JSON + Markdown report adds per-class counts (calls and
           blocks, incl. blocks not executed), the documented class outcomes, the
           ir-v1-corpus.json entries executed at baseline and what the unexecuted
           blocks call (WS-0 inventory of the pinned baseline). Records of the A-19
           evidence test (test-ir-results-a19.R) are reported per A-19 item, with
           the expectation the test asserts, and the open-item (i) probe records
           (test-b10-probe-setop.R) with the refusing call chain they print.

  pin      write tests/testthat/fixtures/ir-v1-results.json and
           ir-v1-results-staged.json from the BASELINE captures. Memory pins:
           safe-class .planExecute() calls made directly by a test on an unmodified
           default create_test_handle() fixture, memory mode. Staged pins:
           safe-class .planExecute() / omopPlanExecuteDS() calls made directly by a
           test in staged mode with at least one requested root, on an ADMISSIBLE
           fixture (every staged component with a person column holds >=
           nfilter.subset distinct persons) that the replay can rebuild exactly
           (the recorded person-key handle fields and changed-table rows; no TEMP
           tables, no other handle change). Both: succeeded without warnings, no
           data frame inside the plan, identical in both baseline captures
           (selection rules recorded in the fixtures); tests/testthat/
           test-ir-results.R replays them.

  pin-a19  write tests/testthat/fixtures/ir-v1-results-a19.json: the BASELINE
           records of tests/testthat/test-ir-results-a19.R (every top-level captured
           call of each test block, in call order), identical in both baseline
           captures; that test replays them with the expectation of each A-19 item.

  validate run tests/testthat/test-ir-results*.R and their fixtures (copied from
           this worktree) on a fresh snapshot: --side baseline must pass (the pins
           reproduce 2.10.2 itself); --side current is the B10 check of the code.

  check    the shared `b10-canonical` block of capture_results.R and its verbatim
           copies in tests/testthat/test-ir-results*.R agree.

The canonical result form and the volatile-field rules are documented in
tools/ws2/capture_results.R.
"""
import argparse, collections, csv, datetime, glob, hashlib, json, os, re, shutil, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
SERVER_ROOT = os.path.dirname(os.path.dirname(HERE))
RTEST = "/workspace/dsomop/tools/rtest.sh"
POD_BASE = "/root/rt/ws2b-b10"
SSH_KEY = os.path.expanduser("~/.ssh/runpod_cover")
SSH_OPTS = ["-i", SSH_KEY, "-o", "StrictHostKeyChecking=no", "-o", "UserKnownHostsFile=/dev/null",
            "-o", "LogLevel=ERROR", "-o", "ServerAliveInterval=30"]


# --------------------------------------------------------------------------- pod helpers
def pod_target():
    out = subprocess.run(["python3", "/workspace/astra/runpod/deckpod.py", "ssh-target"],
                         check=True, capture_output=True, text=True).stdout.split()
    return out[0], out[1]


def ssh(cmd):
    h, p = pod_target()
    return subprocess.run(["ssh"] + SSH_OPTS + ["-p", p, "root@" + h, cmd], capture_output=True, text=True)


def rsync_from_pod(remote, local):
    h, p = pod_target()
    os.makedirs(local, exist_ok=True)
    e = "ssh " + " ".join(SSH_OPTS) + " -p " + p
    return subprocess.run(["rsync", "-az", "-e", e, "root@%s:%s/" % (h, remote), local + "/"],
                          capture_output=True, text=True)


def git(repo, *args):
    return subprocess.run(["git", "-C", repo] + list(args), check=True, capture_output=True, text=True).stdout.strip()


def snapshot(repo, commit, dest):
    if os.path.exists(dest):
        shutil.rmtree(dest)
    os.makedirs(dest)
    archive = subprocess.Popen(["git", "-C", repo, "archive", commit], stdout=subprocess.PIPE)
    subprocess.run(["tar", "-x", "-C", dest], stdin=archive.stdout, check=True)
    if archive.wait() != 0:
        raise SystemExit("git archive failed for %s %s" % (repo, commit))


def rtest_name(pkg_dir):
    """rtest.sh's NAME of a package directory (the path without /workspace/dsomop/; a path
    outside it keeps its leading slash)."""
    return pkg_dir[len("/workspace/dsomop/"):] if pkg_dir.startswith("/workspace/dsomop/") else pkg_dir


def pod_name(pkg_dir):
    """The path rtest.sh mirrors a package directory at, below /root/rt/."""
    return rtest_name(pkg_dir).lstrip("/")


def drop_snapshot(snap, pkg_dirs):
    """Remove a snapshot locally and its rtest.sh mirrors / private libraries on the pod."""
    for d in pkg_dirs:
        lib = "/root/rt/lib/" + rtest_name(d).replace("/", "_")
        ssh("rm -rf '/root/rt/%s' '%s' '%s.install.log'" % (pod_name(d), lib, lib))
    ssh("rmdir '/root/rt/%s' 2>/dev/null || true" % pod_name(snap))
    shutil.rmtree(snap, ignore_errors=True)


def add_tests(paths, s_dir):
    """Copy test files of this worktree into a server snapshot's tests/testthat/ (a
    test-ir-results*.R file brings this worktree's fixtures/ir-v1-results*.json along, so
    that it asserts against its pins instead of skipping)."""
    added = []
    for p in paths or []:
        src = p if os.path.isabs(p) else os.path.join(SERVER_ROOT, p)
        if not os.path.isfile(src):
            raise SystemExit("--add-tests: no such file " + src)
        files = [src]
        if os.path.basename(src).startswith("test-ir-results"):
            files += sorted(glob.glob(os.path.join(SERVER_ROOT, "tests", "testthat", "fixtures", "ir-v1-results*.json")))
        for f in files:
            sub = "fixtures" if f != src else ""
            dest = os.path.join(s_dir, "tests", "testthat", sub)
            os.makedirs(dest, exist_ok=True)
            shutil.copy(f, os.path.join(dest, os.path.basename(f)))
            added.append(dict(path=os.path.relpath(f, SERVER_ROOT),
                              sha256=hashlib.sha256(open(f, "rb").read()).hexdigest()))
    return added


def cmd_capture(a):
    work = os.path.abspath(a.work)
    srv_repo = os.path.abspath(a.server_repo)
    cli_repo = os.path.abspath(a.client_repo)
    pins = json.load(open(os.path.join(srv_repo, "tools", "ws0", "corpus_baseline.json")))
    if a.side == "baseline":
        s_commit, c_commit = pins["dsOMOP"], pins["dsOMOPClient"]
    else:
        s_commit = git(srv_repo, "rev-parse", a.server_commit)
        c_commit = git(cli_repo, "rev-parse", a.client_commit)
    label = a.label
    snap = os.path.join(work, "snap", label)
    s_dir, c_dir = os.path.join(snap, "dsOMOP"), os.path.join(snap, "dsOMOPClient")
    snapshot(srv_repo, s_commit, s_dir)
    snapshot(cli_repo, c_commit, c_dir)
    tests_from = None
    if a.side == "hybrid":
        # current package code, the PINNED 2.10.2 tests/ (the suites the frozen classes describe)
        tests_from = {"dsOMOP": pins["dsOMOP"], "dsOMOPClient": pins["dsOMOPClient"]}
        for repo, d, pkg in ((srv_repo, s_dir, "dsOMOP"), (cli_repo, c_dir, "dsOMOPClient")):
            shutil.rmtree(os.path.join(d, "tests"))
            arch = subprocess.Popen(["git", "-C", repo, "archive", pins[pkg], "tests"], stdout=subprocess.PIPE)
            subprocess.run(["tar", "-x", "-C", d], stdin=arch.stdout, check=True)
            if arch.wait() != 0:
                raise SystemExit("git archive of the baseline tests failed for " + pkg)
    helper = os.path.join(HERE, "capture_results.R")
    for d in (s_dir, c_dir):
        shutil.copy(helper, os.path.join(d, "tests", "testthat", "helper-zzz-b10-capture.R"))
    added = add_tests(a.add_tests, s_dir)
    caps = os.path.join(work, "caps", label)
    if os.path.exists(caps):
        shutil.rmtree(caps)
    os.makedirs(caps)
    meta = dict(label=label, side=a.side, server_commit=s_commit, client_commit=c_commit, tests_from=tests_from,
                server_repo=srv_repo, client_repo=cli_repo, snapshot=snap, added_tests=added,
                helper_sha256=hashlib.sha256(open(helper, "rb").read()).hexdigest(),
                filter=a.filter, suites={}, started=datetime.datetime.utcnow().isoformat() + "Z")
    for suite in a.suites.split(","):
        pkg, pkg_dir, with_dir = (("dsOMOP", s_dir, c_dir) if suite == "server" else ("dsOMOPClient", c_dir, s_dir))
        remote = "%s/caps/%s/%s" % (POD_BASE, label, pkg)
        r = ssh("rm -rf %s && mkdir -p %s" % (remote, remote))
        if r.returncode != 0:
            raise SystemExit("pod unreachable: " + r.stderr)
        env = dict(os.environ)
        env["RTEST_WITH"] = with_dir
        env["RTEST_ENV"] = " ".join(["DSOMOP_RESULT_CAPTURE_DIR=" + remote, "DSOMOP_RESULT_CAPTURE_LABEL=" + label,
                                     "DSOMOP_RESULT_CAPTURE_PKG=" + pkg])
        t0 = datetime.datetime.utcnow()
        run = subprocess.run([RTEST, pkg_dir, a.filter or "", str(a.timeout)], env=env, capture_output=True, text=True)
        t1 = datetime.datetime.utcnow()
        local = os.path.join(caps, pkg)
        fetched = rsync_from_pod(remote, local)
        name = pod_name(pkg_dir)
        log = ssh("cat '/root/rt/%s.last.log' && rm -f '/root/rt/%s.last.log'" % (name, name))
        open(os.path.join(caps, pkg + ".log"), "w").write(log.stdout)
        if fetched.returncode == 0:
            ssh("rm -rf %s && rmdir %s/caps/%s 2>/dev/null || true" % (remote, POD_BASE, label))
        result = [l for l in run.stdout.splitlines() if l.startswith("RESULT")]
        meta["suites"][pkg] = dict(rtest_exit=run.returncode, result=result[-1] if result else None,
                                   seconds=(t1 - t0).total_seconds(), fetch_exit=fetched.returncode,
                                   files=sorted(os.listdir(local)) if os.path.isdir(local) else [],
                                   rtest_tail=run.stdout.splitlines()[-60:], rtest_stderr=run.stderr[-4000:])
        print("[%s] %s: %s (%.0f s)" % (label, pkg, meta["suites"][pkg]["result"], meta["suites"][pkg]["seconds"]))
    meta["finished"] = datetime.datetime.utcnow().isoformat() + "Z"
    json.dump(meta, open(os.path.join(caps, "meta.json"), "w"), indent=1)
    if not a.keep_snapshot:
        drop_snapshot(snap, (s_dir, c_dir))



# --------------------------------------------------------------------------- loading
REFUSED_AT_BASELINE = {"unchanged_invalid", "newly_permitted", "publication_refused_unchanged"}


def clean(x):
    """tools/ws2/build_ir_corpus.py's clean(): the corpus plan form."""
    if isinstance(x, dict):
        if '__frame__' in x and len(x) == 1:
            return {'__resolved_frame__': {'ncol': x['__frame__'].get('ncol')}}
        return {k: clean(v) for k, v in x.items() if k != '__class__'}
    if isinstance(x, list):
        return [clean(v) for v in x]
    return x


def plan_hash(plan):
    """sha256 of the corpus form: equals ir-v1-corpus.json's `sha256` for the same plan."""
    return hashlib.sha256(json.dumps(clean(plan), sort_keys=True, separators=(',', ':')).encode()).hexdigest()


def canon_json(x):
    return json.dumps(x, sort_keys=True, separators=(',', ':'))


def norm_volatile(x, pid):
    """Strings of a call's context (options, symbols, environment) without their
    volatile parts: session tempdir, tempfile hex, tokens and the capturing pid."""
    if isinstance(x, str):
        x = re.sub(r"stg_[0-9a-f]{32}", "<token>", x)
        x = re.sub(r"/tmp/Rtmp[A-Za-z0-9]+", "<tempdir>", x)
        x = re.sub(r"(/[^/<>\s]*?)[0-9a-f]{10,}(?=/|\b)", r"\1<rand>", x)
        # the capture helper's rule: a `_<pid>` suffix of a test symbol
        return re.sub(r"(?<=_)%d(?![0-9A-Za-z])" % pid, "<pid>", x) if pid else x
    if isinstance(x, dict):
        return {k: norm_volatile(v, pid) for k, v in x.items()}
    if isinstance(x, list):
        return [norm_volatile(v, pid) for v in x]
    return x


def call_context(r, with_db=True):
    """What, besides the plan, defines the input of a call: output mode, requested
    symbols, server options, DSOMOP_* environment and (with_db) the fixture data."""
    call = r.get("call") or {}
    opts = r.get("options") or {}
    plan_level = r["fn"] in PLAN_FNS
    ctx = dict(mode=call.get("output_mode") or ("memory" if plan_level else None),
               symbols=call.get("out_symbols") or call.get("out"),
               options=opts.get("options"), env=opts.get("env"), inputs=r.get("inputs"))
    if with_db:
        ctx["db"] = ((r.get("handle") or {}).get("db") or {}).get("sha256")
    return norm_volatile(ctx, r.get("_pid"))


def _ctx_hash(r, with_db):
    return hashlib.sha256(canon_json(call_context(r, with_db)).encode()).hexdigest()[:16]


EXECUTOR_FNS = (".cohortCreate", ".cohortCombine", ".extractTable", ".toFeatures")
ASSIGN_FNS = ("omopPlanExecuteDS", "omopDpPlanExecuteDS")
PLAN_FNS = (".planExecute",) + ASSIGN_FNS
TABLE_ARGS = ("cohort_table", "cohort_table_a", "cohort_table_b", "new_name")
_RAND_SUFFIX = re.compile(r"_(?:[0-9a-f]{8,}|[0-9]{5,}|<rand>)$")


def _normalise_table_names(node):
    """In a canonical result tree: the value of every `cohort_table` element (an internal
    TEMP table name) without a random-looking suffix."""
    if isinstance(node, dict):
        if node.get("kind") == "list" and node.get("names"):
            for n, item in zip(node["names"], node["items"]):
                if n == "cohort_table" and isinstance(item, dict) and item.get("type") == "character":
                    item["values"] = [_RAND_SUFFIX.sub("_<rand>", v) if isinstance(v, str) else v for v in item["values"]]
        for v in node.values():
            if isinstance(v, (dict, list)):
                _normalise_table_names(v)
    elif isinstance(node, list):
        for v in node:
            _normalise_table_names(v)


def normalise_record(r):
    """Compare-time normalisation, applied identically to every side: internal TEMP table
    names are identifiers whose content is what matters. Executor calls name their
    input/output tables by random schemes; their content hashes are in `inputs` (part of the
    call context) and the produced roster is in the result."""
    if r.get("fn") in EXECUTOR_FNS and isinstance(r.get("plan"), dict):
        for a in TABLE_ARGS:
            if isinstance(r["plan"].get(a), str):
                r["plan"][a] = "<table>"
    if r.get("fn") in EXECUTOR_FNS + (".planResolvePopulations",):
        _normalise_table_names(r.get("result"))


class Run:
    def __init__(self, work, label):
        self.label = label
        self.dir = os.path.join(work, "caps", label)
        self.meta = json.load(open(os.path.join(self.dir, "meta.json")))
        self.calls, self.other, self.bad_lines = [], [], 0
        self.processes = {}
        for pkg in ("dsOMOP", "dsOMOPClient"):
            files = sorted(glob.glob(os.path.join(self.dir, pkg, "*.jsonl")))
            counts = {}
            for f in files:
                pid = int(re.search(r"-(\d+)\.jsonl$", f).group(1))
                n = 0
                for line in open(f, encoding="utf-8"):
                    line = line.strip()
                    if not line:
                        continue
                    try:
                        r = json.loads(line)
                    except ValueError:
                        self.bad_lines += 1
                        continue
                    r["_suite"], r["_pid"] = pkg, pid
                    if r.get("kind") == "call":
                        self.calls.append(r)
                        n += 1
                    else:
                        self.other.append(r)
                counts[pid] = n
            # the suite's own process holds the most calls; child R processes (tests that
            # spawn R) write their own files and are ordered after it, by pid
            main = max(counts, key=lambda k: counts[k]) if counts else None
            self.processes[pkg] = dict(main=main, calls=counts)
        main_of = {pkg: v["main"] for pkg, v in self.processes.items()}
        for r in self.calls:
            normalise_record(r)
            r["_hash"] = plan_hash(r.get("plan")) if "plan" in r else None
            r["_key"] = (r.get("context") or {}).get("key") or "<no-test-block>"
            r["_top"] = r.get("parent") is None
            r["_order"] = (r["_suite"], r["_pid"] != main_of.get(r["_suite"]), r["_pid"], r["id"])
        # occurrence of (fn, key, plan hash) among TOP-LEVEL calls, in call order
        occ = collections.Counter()
        self.top = collections.OrderedDict()
        for r in sorted((r for r in self.calls if r["_top"]), key=lambda r: r["_order"]):
            r["_ctx"] = _ctx_hash(r, True)
            r["_ctx_nodb"] = _ctx_hash(r, False)
            k = (r["fn"], r["_key"], r["_hash"], r["_ctx"])
            occ[k] += 1
            r["_join"] = k + (occ[k],)
            self.top[r["_join"]] = r
        self.capture_errors = [r for r in self.calls if r.get("capture_error") or r.get("serialization_error")]

    def children(self, r):
        return [c for c in self.calls if c["_suite"] == r["_suite"] and c["_pid"] == r["_pid"] and c.get("parent") == r["id"]]


def load_classes(path):
    rows = list(csv.DictReader(open(path, newline="", encoding="utf-8")))
    by_block = collections.OrderedDict()
    for r in rows:
        by_block.setdefault(r["plan_id"].partition("#")[0], []).append(r)
    return rows, by_block


def binding(by_block, key):
    rows = by_block.get(key)
    if not rows:
        return dict(label="unclassified", classes=[], mixed=False, rows=[])
    cls = sorted(set(r["class"] for r in rows))
    mixed = any("#" in r["plan_id"] for r in rows)
    label = cls[0] if len(cls) == 1 else "mixed:" + "+".join(cls)
    return dict(label=label, classes=cls, mixed=mixed, rows=[r["plan_id"] for r in rows])


# --------------------------------------------------------------------------- structural diff
def _short(x, n=160):
    t = x if isinstance(x, str) else json.dumps(x, sort_keys=True)
    return t if len(t) <= n else t[:n] + "..."


def _col_type(c):
    return dict(cls=c.get("class"), type=c.get("type"), levels=c.get("levels"))


def diff_nodes(a, b, path, out):
    """Append {path, aspect, detail} for every difference between two canonical nodes."""
    if a == b:
        return
    if not isinstance(a, dict) or not isinstance(b, dict) or a.get("kind") != b.get("kind"):
        out.append(dict(path=path, aspect="structure",
                        detail="baseline %s / current %s" % (_short((a or {}).get("kind") if isinstance(a, dict) else a, 60),
                                                             _short((b or {}).get("kind") if isinstance(b, dict) else b, 60)),
                        baseline=_short(a, 300), current=_short(b, 300)))
        return
    # Classification must never call unequal canonical trees identical, even
    # when a new payload field is absent from the human-readable diff below.
    before = len(out)
    k = a["kind"]
    if k == "data.frame":
        if a.get("class") != b.get("class"):
            out.append(dict(path=path, aspect="class", detail="%s -> %s" % (a.get("class"), b.get("class"))))
        if a["names"] != b["names"]:
            out.append(dict(path=path, aspect="names",
                            detail="baseline %s / current %s" % (_short(a["names"]), _short(b["names"])),
                            only_baseline=[n for n in a["names"] if n not in b["names"]],
                            only_current=[n for n in b["names"] if n not in a["names"]]))
        ca = dict(zip(a["names"], a["columns"]))
        cb = dict(zip(b["names"], b["columns"]))
        common = [n for n in a["names"] if n in cb]
        for n in common:
            if _col_type(ca[n]) != _col_type(cb[n]):
                out.append(dict(path=path + "." + n, aspect="types",
                                detail="%s -> %s" % (_short(_col_type(ca[n]), 120), _short(_col_type(cb[n]), 120))))
            for f in ("names", "attrs", "numeric_payload"):
                if ca[n].get(f) != cb[n].get(f):
                    out.append(dict(path=path + "." + n, aspect="column_" + f,
                                    detail="%s -> %s" % (_short(ca[n].get(f), 100), _short(cb[n].get(f), 100))))
        if a["nrow"] != b["nrow"]:
            out.append(dict(path=path, aspect="nrow", detail="%s -> %s" % (a["nrow"], b["nrow"])))
        else:
            diffcols = [n for n in common if ca[n].get("values") != cb[n].get("values")]
            if diffcols:
                rows_a = sorted(canon_json([ca[n].get("values")[i] for n in common]) for i in range(a["nrow"]))
                rows_b = sorted(canon_json([cb[n].get("values")[i] for n in common]) for i in range(b["nrow"]))
                if rows_a == rows_b:
                    out.append(dict(path=path, aspect="row_order",
                                    detail="same rows in a different order (columns differing by position: %s)" % ", ".join(diffcols)))
                else:
                    det = []
                    for n in diffcols:
                        va, vb = ca[n]["values"], cb[n]["values"]
                        pos = [i for i in range(len(va)) if va[i] != vb[i]]
                        det.append("%s: %d/%d rows (first row %d: %s -> %s)" % (n, len(pos), len(va), pos[0] + 1,
                                                                             _short(va[pos[0]], 40), _short(vb[pos[0]], 40)))
                    out.append(dict(path=path, aspect="values", detail="; ".join(det)))
        if a.get("row_names") != b.get("row_names"):
            out.append(dict(path=path, aspect="row_names",
                            detail="%s -> %s" % (_short(a.get("row_names"), 80), _short(b.get("row_names"), 80))))
        if a.get("attrs") != b.get("attrs"):
            diff_attrs(a.get("attrs"), b.get("attrs"), path, out)
        if len(out) == before:
            out.append(dict(path=path, aspect="canonical_payload",
                            detail="complete canonical forms differ beyond the displayed fields"))
        return
    if k == "list":
        if a.get("class") != b.get("class"):
            out.append(dict(path=path, aspect="class", detail="%s -> %s" % (a.get("class"), b.get("class"))))
        na = a.get("names") or ["[%d]" % (i + 1) for i in range(len(a["items"]))]
        nb = b.get("names") or ["[%d]" % (i + 1) for i in range(len(b["items"]))]
        if a.get("names") != b.get("names"):
            out.append(dict(path=path, aspect="inventory", detail="baseline %s / current %s" % (_short(na), _short(nb)),
                            only_baseline=[n for n in na if n not in nb], only_current=[n for n in nb if n not in na]))
        ib = dict(zip(nb, b["items"]))
        for n, item in zip(na, a["items"]):
            if n in ib:
                diff_nodes(item, ib[n], path + "." + n, out)
        if a.get("attrs") != b.get("attrs"):
            diff_attrs(a.get("attrs"), b.get("attrs"), path, out)
        if len(out) == before:
            out.append(dict(path=path, aspect="canonical_payload",
                            detail="complete canonical forms differ beyond the displayed fields"))
        return
    aspect = "value"
    if path.endswith(".descriptor") or ".descriptor." in path:
        aspect = "staged_descriptor"
    out.append(dict(path=path, aspect=aspect, detail="%s -> %s" % (_short(a.get("values"), 120), _short(b.get("values"), 120)),
                    baseline=_short(a, 300), current=_short(b, 300)))


def diff_attrs(a, b, path, out):
    a, b = a or {}, b or {}
    for n in sorted(set(a) | set(b)):
        if a.get(n) != b.get(n):
            if n in a and n in b:
                sub = []
                diff_nodes(a[n], b[n], path + "@" + n, sub)
                for d in sub:
                    d["aspect"] = "attrs:" + d["aspect"]
                out.extend(sub)
            else:
                out.append(dict(path=path + "@" + n, aspect="attrs",
                                detail="only in %s" % ("baseline" if n in a else "current")))


def _list_get(node, name):
    if isinstance(node, dict) and node.get("kind") == "list" and node.get("names"):
        return dict(zip(node["names"], node["items"])).get(name)
    return None


def effective(r):
    """(status, message, code) of a record. A 2.10.3 omopPlanExecuteDS() can refuse through its
    returned status object (status "refused") instead of an error: that is a refusal too."""
    o = r["outcome"]
    if o["status"] != "ok":
        return "error", o.get("message"), o.get("code")
    if r["fn"] in ASSIGN_FNS:
        res = r.get("result") or {}
        if "dsomop_plan_status" in (res.get("class") or []):
            st = _list_get(res, "status")
            st = (st or {}).get("values", [None])[0]
            if st == "refused":
                rej = _list_get(res, "rejected")
                codes = []
                for item in (rej or {}).get("items") or []:
                    c = _list_get(item, "code")
                    codes.append((c or {}).get("values", ["?"])[0])
                return "error", "plan status refused: " + ",".join(str(c) for c in codes), ",".join(str(c) for c in codes)
    return "ok", None, None


def result_node(r):
    """The compared value of a top-level record: .planExecute's return value, or
    omopPlanExecuteDS's / omopDpPlanExecuteDS's created/replaced bindings (their return
    value is reported apart)."""
    if r["fn"] in ASSIGN_FNS:
        return (r.get("assigned") or {}).get("values")
    return r.get("result")


def warnings_of(r):
    return [w.get("message") for w in r.get("warnings") or []]


def norm_msg(m):
    """An error message without its volatile parts (session tempdir, tokens, tempfile hex)."""
    if not isinstance(m, str):
        return m
    m = re.sub(r"stg_[0-9a-f]{32}", "<token>", m)
    m = re.sub(r"/tmp/Rtmp[A-Za-z0-9]+", "<tempdir>", m)
    return re.sub(r"(/[^/<>\s]*?)[0-9a-f]{10,}(?=/|\b)", r"\1<rand>", m)


def compare_pair(b, c):
    (sb, mb, _), (sc, mc, _) = effective(b), effective(c)
    rec = dict(outcome=None, diffs=[], notes=[])
    if sb == "ok" and sc == "ok":
        sub = []
        diff_nodes(result_node(b), result_node(c), "result", sub)
        rec["diffs"] = sub
        rec["outcome"] = "identical" if not sub else "different"
        st = _list_get(c.get("result") or {}, "status")
        if b["fn"] in ASSIGN_FNS and st and (st.get("values") or [None])[0] == "partial":
            rec["notes"].append("current plan status partial (some roots rejected)")
        if b["fn"] in ASSIGN_FNS and b.get("result") != c.get("result"):
            rec["notes"].append("return value differs (not compared): %s -> %s" % (
                _short((b.get("result") or {}).get("class"), 60), _short((c.get("result") or {}).get("class"), 60)))
    elif sb == "error" and sc == "ok":
        rec["outcome"] = "refused_baseline_only"
    elif sb == "ok" and sc == "error":
        rec["outcome"] = "refused_current_only"
    else:
        rec["outcome"] = "both_refused"
        rec["same_message"] = norm_msg(mb) == norm_msg(mc)
        rec["same_condition_class"] = b["outcome"].get("class") == c["outcome"].get("class")
    if warnings_of(b) != warnings_of(c):
        rec["notes"].append("warnings differ: %s -> %s" % (_short(warnings_of(b), 200), _short(warnings_of(c), 200)))
    xb, xc = call_context(b, False), call_context(c, False)
    ob, oc = xb.get("options") or {}, xc.get("options") or {}
    if ob != oc:
        names = sorted(set(ob) ^ set(oc) | {n for n in set(ob) & set(oc) if ob[n] != oc[n]})
        rec["notes"].append("options differ at the call: " + ", ".join(names))
    if xb.get("env") != xc.get("env"):
        rec["notes"].append("DSOMOP_* environment differs at the call")
    hb, hc = b.get("handle") or {}, c.get("handle") or {}
    if (hb.get("db") or {}).get("sha256") != (hc.get("db") or {}).get("sha256"):
        rec["notes"].append("fixture data differ (db %s / %s)" % (((hb.get("db") or {}).get("sha256") or "-")[:12],
                                                                   ((hc.get("db") or {}).get("sha256") or "-")[:12]))
    if bool(hb.get("default_fixture")) != bool(hc.get("default_fixture")):
        rec["notes"].append("default-fixture flag differs: %s -> %s" % (hb.get("default_fixture"), hc.get("default_fixture")))
    return rec


def leak_scan(node, found, path="result"):
    """Strings that look like random internal names (temporary tables, tokens) in a canonical tree."""
    if isinstance(node, dict):
        if node.get("kind") == "atomic" and node.get("type") == "character":
            for v in node.get("values") or []:
                if isinstance(v, str) and (re.search(r"dsomop_[a-z0-9_]*_[0-9a-f]{12}\b", v) or re.search(r"stg_[0-9a-f]{32}", v)):
                    found.append((path, v[:120]))
        for k, v in node.items():
            if isinstance(v, (dict, list)):
                leak_scan(v, found, path + "." + k)
    elif isinstance(node, list):
        for i, v in enumerate(node):
            leak_scan(v, found, path)


def match_records(base, cur):
    """Pairs of top-level records with the same block, function and plan. Level 1: the
    same complete input (output mode, symbols, options, DSOMOP_* environment and fixture
    data), in call order. Level 2, for the rest: the same input except the fixture data
    (e.g. a test inserting rows with random primary keys), in call order; such pairs are
    flagged `fixture_differs`."""
    pairs, used_b, used_c = [], set(), set()
    for k, b in base.top.items():
        c = cur.top.get(k)
        if c is not None:
            pairs.append((b, c, False))
            used_b.add(id(b)); used_c.add(id(c))
    rest_c = collections.defaultdict(list)
    for c in cur.top.values():
        if id(c) not in used_c:
            rest_c[(c["fn"], c["_key"], c["_hash"], c["_ctx_nodb"])].append(c)
    for b in base.top.values():
        if id(b) in used_b:
            continue
        lst = rest_c.get((b["fn"], b["_key"], b["_hash"], b["_ctx_nodb"]))
        if lst:
            c = lst.pop(0)
            pairs.append((b, c, True))
            used_b.add(id(b)); used_c.add(id(c))
    base_only = [b for b in base.top.values() if id(b) not in used_b]
    cur_only = [c for c in cur.top.values() if id(c) not in used_c]
    return pairs, base_only, cur_only


# DP provenance capsule fields keyed by the DP noise root: a test that creates a fresh
# random root per run (test-privacy-dp.R's .dp_local_state()) makes them differ between
# two runs of the same code while every value is identical.
KEYED_DP_FIELDS = ("dataset_id", "lineage_id", "mac")


def mask_keyed_dp(node):
    """A copy of a canonical tree with the keyed fields of every dsomop_dp_provenance capsule masked."""
    if isinstance(node, list):
        return [mask_keyed_dp(v) for v in node]
    if not isinstance(node, dict):
        return node
    out = {k: mask_keyed_dp(v) for k, v in node.items()}
    cap = (out.get("attrs") or {}).get("dsomop_dp_provenance") if isinstance(out.get("attrs"), dict) else None
    if isinstance(cap, dict) and cap.get("kind") == "list" and cap.get("names"):
        cap["items"] = [{"kind": "masked"} if n in KEYED_DP_FIELDS else it for n, it in zip(cap["names"], cap["items"])]
    return out


def masked_record(r):
    r = dict(r)
    for f in ("result", "assigned"):
        if r.get(f) is not None:
            r[f] = mask_keyed_dp(r[f])
    return r


def join_runs(base, cur, nondet, masked=None):
    joined = []
    masked = masked or {}
    pairs, base_only, cur_only = match_records(base, cur)
    for b, c, fixture_differs in pairs:
        if b["_join"] in masked:
            pair = compare_pair(masked_record(b), masked_record(c))
            pair["notes"].append("keyed DP provenance fields (%s) masked: per-run noise root" % ", ".join(KEYED_DP_FIELDS))
        else:
            pair = compare_pair(b, c)
        pair.update(b=b, c=c, key=b["_key"], join=b["_join"], excluded=b["_join"] in nondet,
                    fixture_differs=fixture_differs, masked=b["_join"] in masked)
        if fixture_differs and pair["outcome"] != "identical":
            pair["outcome"] += "_fixture_differs"
        joined.append(pair)
    return joined, base_only, cur_only


def determinism(a, b, masked=None):
    """Top-level records of two runs of the same side: keys whose outcome or result differ.
    With `masked` (a dict), records that agree once the keyed DP provenance fields are masked
    go there instead (compared later under that weaker guarantee)."""
    bad = {}
    pairs, a_only, b_only = match_records(a, b)
    for r in a_only:
        bad[r["_join"]] = "absent in the repeat run"
    for r in b_only:
        bad[r["_join"]] = "absent in the first run"
    for ra, rb, _ in pairs:
        k = ra["_join"]
        if effective(ra)[0] != effective(rb)[0]:
            bad[k] = "outcome %s / %s" % (effective(ra)[0], effective(rb)[0])
            continue
        if ra["outcome"]["status"] == "ok":
            sub = []
            diff_nodes(result_node(ra), result_node(rb), "result", sub)
            if sub and masked is not None:
                rest = []
                diff_nodes(mask_keyed_dp(result_node(ra)), mask_keyed_dp(result_node(rb)), "result", rest)
                if not rest:
                    masked[k] = "; ".join("%s %s" % (d["path"], d["aspect"]) for d in sub)
                    continue
            if sub:
                bad[k] = "; ".join("%s %s: %s" % (d["path"], d["aspect"], d["detail"]) for d in sub[:3])
        elif norm_msg(ra["outcome"].get("message")) != norm_msg(rb["outcome"].get("message")):
            bad[k] = "error message differs"
    return bad


def corpus_check(corpus, base):
    """Each ir-v1-corpus.json entry against the baseline capture: did the plan reach
    .planExecute() (ok / refused) or .planResolvePopulations() at baseline (any depth)?"""
    seen = collections.defaultdict(collections.Counter)
    for r in base.calls:
        if is_evidence(r):
            continue
        if r["fn"] in (".planExecute", ".planResolvePopulations") and r.get("_hash"):
            seen[(r["fn"], r["_hash"])][r["outcome"]["status"]] += 1

    def state(fn, h):
        s = seen.get((fn, h), collections.Counter())
        return ("ok" if s.get("ok") else ("refused" if s.get("error") else "not_reached")), sum(s.values())
    rows = []
    for e in corpus:
        ex, nx = state(".planExecute", e["sha256"])
        pr, npr = state(".planResolvePopulations", e["sha256"])
        rows.append(dict(id=e["id"], corpus_executed=e.get("executed"), baseline=ex, baseline_calls=nx,
                         resolve_populations=pr, resolve_calls=npr,
                         sources=["%s:%s:%s" % (x.get("package"), x.get("file"), x.get("test")) for x in e["sources"]]))
    summary = collections.Counter((r["corpus_executed"], r["baseline"]) for r in rows)
    resolve_summary = collections.Counter(r["resolve_populations"] for r in rows)
    executed_hashes = {h for (fn, h), s in seen.items() if fn == ".planExecute" and s.get("ok")}
    corpus_hashes = {e["sha256"] for e in corpus}
    return dict(rows=rows,
                summary=[dict(corpus_executed=k[0], baseline=k[1], n=v) for k, v in sorted(summary.items(), key=str)],
                resolve_populations=dict(resolve_summary),
                baseline_ok_plans_not_in_corpus=len(executed_hashes - corpus_hashes),
                baseline_ok_plans=len(executed_hashes))


def base_outcome(o):
    return o.replace("_fixture_differs", "") if isinstance(o, str) else o


OUTCOMES = ["identical", "different", "refused_baseline_only", "refused_current_only", "both_refused",
            "different_fixture_differs", "refused_baseline_only_fixture_differs",
            "refused_current_only_fixture_differs", "both_refused_fixture_differs",
            "excluded_nondeterministic", "baseline_only_unmatched", "current_only_unmatched"]


def class_tables(by_block, joined, base_only, cur_only, base):
    pair_counts = collections.defaultdict(collections.Counter)
    for p in joined:
        lab = binding(by_block, p["key"])["label"]
        pair_counts[lab]["excluded_nondeterministic" if p["excluded"] else p["outcome"]] += 1
    for r in base_only:
        pair_counts[binding(by_block, r["_key"])["label"]]["baseline_only_unmatched"] += 1
    for r in cur_only:
        pair_counts[binding(by_block, r["_key"])["label"]]["current_only_unmatched"] += 1
    executed_keys = {r["_key"] for r in base.top.values()}
    blocks = collections.defaultdict(collections.Counter)
    for key, rows in by_block.items():
        lab = binding(by_block, key)["label"]
        blocks[lab]["blocks"] += 1
        blocks[lab]["executed_at_baseline" if key in executed_keys else "not_executed"] += 1
    # block-level outcome: worst outcome among the block's joined pairs
    order = ["different", "refused_current_only", "refused_baseline_only", "different_fixture_differs",
             "refused_current_only_fixture_differs", "refused_baseline_only_fixture_differs", "baseline_only_unmatched",
             "both_refused", "both_refused_fixture_differs", "identical"]
    per_block = collections.defaultdict(set)
    for p in joined:
        if not p["excluded"]:
            per_block[p["key"]].add(p["outcome"])
    for r in base_only:
        per_block[r["_key"]].add("baseline_only_unmatched")
    for key, outs in per_block.items():
        lab = binding(by_block, key)["label"]
        worst = next(o for o in order if o in outs)
        blocks[lab]["block_" + worst] += 1
    labels = sorted(set(pair_counts) | set(blocks), key=lambda l: (l.startswith("mixed"), l == "unclassified", l))
    return [dict(label=l, calls={o: pair_counts[l].get(o, 0) for o in OUTCOMES}, blocks=dict(blocks[l])) for l in labels]


# The documented outcome of each frozen class (v1-corpus-classes.csv intended_behaviour)
# expressed as the baseline -> current call outcomes it allows. Calls are bound per
# block, and a block also makes setup calls (e.g. the .cohortCreate() a refused
# analysis consumes), so an `identical` call is consistent with every class; any
# other outcome must be the class's documented change. non_plan blocks have no plan
# outcome: their (executor) calls are consistent when unchanged.
EXPECTED = {
    "safe": set(),
    "corrected": {"different"},
    "newly_permitted": {"refused_baseline_only"},
    "newly_refused_structural": {"refused_current_only"},
    "newly_refused_publication": {"refused_current_only"},
    "publication_refused_unchanged": {"both_refused"},
    "unchanged_invalid": {"both_refused"},
    "non_plan": {"both_refused"},
}
PUBLICATION_MESSAGE = ("Disclosive: operation blocked - insufficient individuals to meet disclosure threshold. "
                       "No further details available.")


def expectation_check(by_block, joined):
    """Per class: calls whose observed outcome is outside the class's documented outcomes.
    A mixed block admits the union of its rows' outcomes (calls are bound per block)."""
    out = collections.OrderedDict()
    for p in joined:
        if p["excluded"]:
            continue
        bind = binding(by_block, p["key"])
        lab = bind["label"]
        allowed = {"identical"} | (set().union(*[EXPECTED.get(c, set()) for c in bind["classes"]]) if bind["classes"] else set())
        e = out.setdefault(lab, dict(label=lab, allowed=sorted(allowed), calls=0, as_documented=0, outside=[],
                                     same_message=0, message_changed=0, constant_publication_message=0))
        e["calls"] += 1
        if base_outcome(p["outcome"]) in allowed:
            e["as_documented"] += 1
        else:
            e["outside"].append(dict(key=p["key"], outcome=p["outcome"], fn=p["b"]["fn"], occurrence=p["join"][-1],
                                     baseline_error=_short(effective(p["b"])[1], 200) if effective(p["b"])[0] == "error" else None,
                                     current_error=_short(effective(p["c"])[1], 200) if effective(p["c"])[0] == "error" else None,
                                     diffs=["%s %s" % (d["aspect"], d["path"]) for d in p["diffs"][:6]]))
        if base_outcome(p["outcome"]) == "both_refused":
            e["same_message" if p.get("same_message") else "message_changed"] += 1
            if effective(p["c"])[1] == PUBLICATION_MESSAGE:
                e["constant_publication_message"] += 1
    return list(out.values())


def describe_pair(p, by_block):
    b, c = p["b"], p["c"]
    ctx = b.get("context") or {}
    bind = binding(by_block, p["key"])
    return dict(key=p["key"], fn=b["fn"], plan_sha256=b["_hash"], occurrence=p["join"][-1], cls=bind["label"],
                mixed=bind["mixed"], file=ctx.get("file"), line=ctx.get("line"), output_mode=(b.get("call") or {}).get("output_mode"),
                default_fixture=(b.get("handle") or {}).get("default_fixture"),
                outcome=p["outcome"], diffs=p["diffs"], notes=p["notes"],
                baseline_error=effective(b)[1] if effective(b)[0] == "error" else None,
                current_error=effective(c)[1] if effective(c)[0] == "error" else None,
                current_error_code=effective(c)[2] if effective(c)[0] == "error" else None,
                baseline_id=b["id"], current_id=c["id"])


def check_canonical_copies():
    """The shared `b10-canonical` block of capture_results.R and its verbatim copies in
    tests/testthat/test-ir-results*.R must be identical (comment lines and indentation aside)."""
    def block(path):
        if not os.path.exists(path):
            return None
        txt = open(path, encoding="utf-8").read()
        m = re.search(r"# >>> b10-canonical[^\n]*\n(.*?)# <<< b10-canonical", txt, re.S)
        if not m:
            return None
        return "\n".join(l.strip() for l in m.group(1).splitlines() if l.strip() and not l.strip().startswith("#"))
    a = block(os.path.join(HERE, "capture_results.R"))
    copies = {}
    for f in sorted(glob.glob(os.path.join(SERVER_ROOT, "tests", "testthat", "test-ir-results*.R"))):
        b = block(f)
        copies[os.path.basename(f)] = "missing" if b is None else ("identical" if a is not None and a == b else "DIFFERENT")
    return dict(capture=a is not None, copies=copies,
                identical=a is not None and bool(copies) and all(v == "identical" for v in copies.values()))


def is_evidence(r):
    """A record of the A-19 evidence test or of the open-item (i) probe (not corpus blocks)."""
    return (r.get("context") or {}).get("file") in EVIDENCE_FILES


def a19_item(key):
    m = re.search(r":A-19 item (\d+)", key or "")
    return int(m.group(1)) if m else None


def evidence_section(base, cur, joined, base_only, cur_only):
    """A-19 evidence blocks: every call of test-ir-results-a19.R, baseline vs current, and the
    open-item (i) probe lines of both suite logs."""
    rows = []
    index = {}
    for key, recs in a19_blocks(base).items():
        for i, r in enumerate(recs, 1):
            index[id(r)] = i
    for p in joined:
        if (p["b"].get("context") or {}).get("file") != A19_FILE:
            continue
        b, c = p["b"], p["c"]
        rows.append(dict(item=a19_item(p["key"]), key=p["key"], fn=b["fn"], occurrence=index.get(id(b)),
                         outcome=p["outcome"],
                         baseline="%s %s" % (effective(b)[0], _short(effective(b)[1] or "", 140)),
                         current="%s %s" % (effective(c)[0], _short(effective(c)[1] or "", 140)),
                         baseline_warnings=warnings_of(b), current_warnings=warnings_of(c),
                         diffs=["%s %s: %s" % (d["aspect"], d["path"], _short(d["detail"], 160)) for d in p["diffs"][:8]],
                         notes=p["notes"]))
    for side, recs in (("baseline only", base_only), ("current only", cur_only)):
        for r in recs:
            if (r.get("context") or {}).get("file") == A19_FILE:
                rows.append(dict(item=a19_item(r["_key"]), key=r["_key"], fn=r["fn"], occurrence=None,
                                 outcome=side, baseline="", current="", diffs=[], notes=[]))
    rows.sort(key=lambda x: (x["item"] or 0, x["key"], x["occurrence"] or 0))

    def probe_lines(run):
        f = os.path.join(run.dir, "dsOMOP.log")
        if not os.path.exists(f):
            return []
        return [l.split("[b10-probe] ", 1)[1].strip() for l in open(f, encoding="utf-8", errors="replace")
                if "[b10-probe] " in l]
    return dict(a19=rows, probe=dict(baseline=probe_lines(base), current=probe_lines(cur)))


def comparison(work, base_label, cur_label, by_block, nondet, title, masked=None):
    base, cur = Run(work, base_label), Run(work, cur_label)
    joined_all, base_only_all, cur_only_all = join_runs(base, cur, nondet, masked)
    evidence = evidence_section(base, cur, joined_all, base_only_all, cur_only_all)
    # the corpus tables cover the frozen blocks only
    joined = [p for p in joined_all if not is_evidence(p["b"])]
    base_only = [r for r in base_only_all if not is_evidence(r)]
    cur_only = [r for r in cur_only_all if not is_evidence(r)]
    tables = class_tables(by_block, joined, base_only, cur_only, base)
    safe_issues = []
    for p in joined:
        bind = binding(by_block, p["key"])
        if "safe" in bind["classes"] and not p["excluded"] and base_outcome(p["outcome"]) in ("different", "refused_current_only", "refused_baseline_only"):
            safe_issues.append(describe_pair(p, by_block))
    unmatched_safe = []
    for r in base_only:
        bind = binding(by_block, r["_key"])
        if "safe" in bind["classes"]:
            unmatched_safe.append(dict(key=r["_key"], fn=r["fn"], status=effective(r)[0], cls=bind["label"],
                                       plan_sha256=r["_hash"], line=(r.get("context") or {}).get("line")))
    leaks = []
    for p in joined:
        for side in ("b", "c"):
            found = []
            leak_scan(result_node(p[side]), found)
            for f in found:
                leaks.append(dict(key=p["key"], side=side, path=f[0], value=f[1]))
    other = collections.Counter()
    for p in joined:
        if not p["excluded"] and base_outcome(p["outcome"]) in ("different", "refused_current_only", "refused_baseline_only", "both_refused"):
            other[(binding(by_block, p["key"])["label"], p["outcome"])] += 1
    expectations = expectation_check(by_block, joined)
    by_fn = collections.defaultdict(collections.Counter)
    for p in joined:
        by_fn[p["b"]["fn"]]["excluded_nondeterministic" if p["excluded"] else p["outcome"]] += 1
    for r in base_only:
        by_fn[r["fn"]]["baseline_only_unmatched"] += 1
    for r in cur_only:
        by_fn[r["fn"]]["current_only_unmatched"] += 1
    by_fn = [dict(fn=f, calls={o: by_fn[f].get(o, 0) for o in OUTCOMES}) for f in sorted(by_fn)]
    return dict(title=title, baseline=base.meta, current=cur.meta, expectations=expectations, by_function=by_fn,
                evidence=evidence,
                processes=dict(baseline=base.processes, current=cur.processes),
                records=dict(baseline_top=len(base.top), current_top=len(cur.top), baseline_calls=len(base.calls),
                             current_calls=len(cur.calls), joined=len(joined), baseline_only=len(base_only),
                             current_only=len(cur_only), capture_errors=[len(base.capture_errors), len(cur.capture_errors)],
                             bad_lines=[base.bad_lines, cur.bad_lines]),
                classes=tables, safe_issues=safe_issues, safe_unmatched=unmatched_safe, leaks=leaks,
                pairs=[describe_pair(p, by_block) for p in joined],
                baseline_only=[dict(key=r["_key"], fn=r["fn"], status=effective(r)[0], plan_sha256=r["_hash"],
                                    cls=binding(by_block, r["_key"])["label"]) for r in base_only],
                current_only=[dict(key=r["_key"], fn=r["fn"], status=effective(r)[0], plan_sha256=r["_hash"],
                                   cls=binding(by_block, r["_key"])["label"]) for r in cur_only])


def md_table(rows, cols):
    out = ["| " + " | ".join(cols) + " |", "|" + "---|" * len(cols)]
    for r in rows:
        out.append("| " + " | ".join(str(r.get(c, "")) for c in cols) + " |")
    return "\n".join(out)


def write_markdown(rep, path):
    L = ["# B10 result-fidelity report", "",
         "Generated by `tools/ws2/compare_results.py compare` on %s." % rep["generated"], ""]
    d = rep["determinism"]
    if len(rep.get("capture_helper_sha256") or []) != 1:
        L += ["**WARNING: the compared captures used different capture_results.R versions: %s**" % rep.get("capture_helper_sha256"), ""]
    else:
        L += ["All compared captures used capture_results.R sha256 %s." % rep["capture_helper_sha256"][0], ""]
    L += ["## Runs", "", md_table([dict(label=m["label"], side=m["side"], server=m["server_commit"][:12], client=m["client_commit"][:12],
                                         tests=("2.10.2 pins" if m.get("tests_from") or m["side"] == "baseline" else "own"),
                                         server_suite=(m["suites"].get("dsOMOP") or {}).get("result"),
                                         client_suite=(m["suites"].get("dsOMOPClient") or {}).get("result"))
                                    for m in rep["runs"]],
                                   ["label", "side", "server", "client", "tests", "server_suite", "client_suite"]), ""]
    L += ["## Determinism (%s vs %s, 17 significant digits)" % (d["runs"][0], d["runs"][1]), "",
          "%d top-level records compared; %d nondeterministic (excluded from every comparison):" % (d["compared"], len(d["nondeterministic"])), ""]
    for x in d["nondeterministic"]:
        L.append("- `%s` %s (occurrence %s): %s" % (x["key"], x["fn"], x["occurrence"], x["reason"]))
    if d.get("masked"):
        L += ["", "%d record(s) deterministic only with the keyed DP provenance fields (%s) masked -- the test draws a "
              "fresh random DP noise root per run; compared below under that weaker guarantee (every value and every "
              "other capsule field exact):" % (len(d["masked"]), ", ".join(KEYED_DP_FIELDS)), ""]
        for x in d["masked"]:
            L.append("- `%s` %s (occurrence %s): %s" % (x["key"], x["fn"], x["occurrence"], x["differing"]))
    L.append("")
    for cmp in rep["comparisons"]:
        L += ["## %s" % cmp["title"], "", "Records: %s" % json.dumps(cmp["records"]), "",
              "Per class, top-level calls (a `.planExecute()` call made directly by a test, or an `omopPlanExecuteDS()` / "
              "`omopDpPlanExecuteDS()` call; calls nested in them are not counted). `blocks` counts classification rows "
              "per block (mixed `#sub` blocks under their combined label), `not_executed` = no captured call at baseline; "
              "`block_<o>` = the worst outcome among a block's calls.", ""]
        rows = []
        for t in cmp["classes"]:
            r = dict(cls=t["label"])
            r.update(t["calls"])
            r.update({k: t["blocks"].get(k, 0) for k in ("blocks", "executed_at_baseline", "not_executed")})
            r["block outcomes"] = ", ".join("%s=%d" % (k[6:], v) for k, v in sorted(t["blocks"].items()) if k.startswith("block_"))
            rows.append(r)
        L += [md_table(rows, ["cls"] + OUTCOMES + ["blocks", "executed_at_baseline", "not_executed", "block outcomes"]), ""]
        L += ["Per function (plan level: `.planExecute`, `omopPlanExecuteDS`, `omopDpPlanExecuteDS`; internal: population resolution and the "
              "executor entry points called directly by tests):", ""]
        frows = []
        for t in cmp["by_function"]:
            r = dict(fn=t["fn"])
            r.update(t["calls"])
            frows.append(r)
        L += [md_table(frows, ["fn"] + OUTCOMES), ""]
        L += ["### Safe-class (incl. mixed blocks with a safe row) calls that differ or are refused on one side only (%d)" % len(cmp["safe_issues"]), ""]
        for x in cmp["safe_issues"]:
            L.append("- **%s** `%s` [%s, %s, line %s, occurrence %s, plan %s, fixture default=%s]" % (
                x["outcome"], x["key"], x["fn"], x["cls"], x["line"], x["occurrence"], x["plan_sha256"][:12], x["default_fixture"]))
            for df in x["diffs"][:12]:
                L.append("  - %s `%s`: %s" % (df["aspect"], df["path"], df["detail"]))
            if len(x["diffs"]) > 12:
                L.append("  - ... %d more" % (len(x["diffs"]) - 12))
            if x["baseline_error"]:
                L.append("  - baseline error: %s" % _short(x["baseline_error"], 300))
            if x["current_error"]:
                L.append("  - current error%s: %s" % (" (" + x["current_error_code"] + ")" if x["current_error_code"] else "", _short(x["current_error"], 300)))
            for n in x["notes"]:
                L.append("  - note: %s" % n)
        L += ["", "### Safe-class calls executed at baseline without a current counterpart (%d)" % len(cmp["safe_unmatched"]), ""]
        for x in cmp["safe_unmatched"]:
            L.append("- `%s` %s %s (%s, line %s, plan %s)" % (x["key"], x["fn"], x["status"], x["cls"], x["line"], x["plan_sha256"][:12]))
        L += ["", "### Other classes: calls that differ or are refused (%d)" % sum(1 for p in cmp["pairs"] if "safe" not in p["cls"] and p["outcome"] != "identical"), ""]
        for x in cmp["pairs"]:
            if "safe" in x["cls"].split(":")[-1].split("+") or x["outcome"] == "identical":
                continue
            line = "- %s `%s` [%s, %s]" % (x["outcome"], x["key"], x["fn"], x["cls"])
            if base_outcome(x["outcome"]) == "both_refused":
                line += " same message" if norm_msg(x["baseline_error"]) == norm_msg(x["current_error"]) else " message changed: %s -> %s" % (
                    _short(x["baseline_error"], 120), _short(x["current_error"], 120))
            elif base_outcome(x["outcome"]) == "refused_current_only":
                line += " current: %s" % _short(x["current_error"], 160)
            elif base_outcome(x["outcome"]) == "refused_baseline_only":
                line += " baseline: %s" % _short(x["baseline_error"], 160)
            elif base_outcome(x["outcome"]) == "different":
                line += " " + "; ".join("%s %s" % (df["aspect"], df["path"]) for df in x["diffs"][:4])
            L.append(line)
        L += ["", "### Documented class outcomes (calls outside the documented outcome are listed)", "",
              md_table([dict(cls=e["label"], allowed="/".join(e["allowed"]) or "-", calls=e["calls"], as_documented=e["as_documented"],
                             outside=len(e["outside"]), both_refused_same_message=e["same_message"],
                             both_refused_message_changed=e["message_changed"],
                             constant_publication_message=e["constant_publication_message"]) for e in cmp["expectations"]],
                       ["cls", "allowed", "calls", "as_documented", "outside", "both_refused_same_message",
                        "both_refused_message_changed", "constant_publication_message"]), ""]
        for e in cmp["expectations"]:
            for x in e["outside"]:
                if "safe" in e["label"].split(":")[-1].split("+"):
                    continue   # listed above
                L.append("- %s: %s `%s` (%s, occurrence %s)%s%s%s" % (
                    e["label"], x["outcome"], x["key"], x["fn"], x["occurrence"],
                    " baseline: " + x["baseline_error"] if x["baseline_error"] else "",
                    " current: " + x["current_error"] if x["current_error"] else "",
                    " diffs: " + "; ".join(x["diffs"]) if x["diffs"] else ""))
        if cmp["leaks"]:
            L += ["", "### Internal-name strings found in compared values (%d)" % len(cmp["leaks"]), ""]
            for x in cmp["leaks"][:50]:
                L.append("- `%s` %s %s: %s" % (x["key"], x["side"], x["path"], x["value"]))
        ev = cmp.get("evidence") or {}
        if ev.get("a19"):
            L += ["", "### A-19 evidence (tests/testthat/test-ir-results-a19.R; declared fixtures)", "",
                  "Each call of the evidence test, baseline vs this capture. The test itself asserts each item's "
                  "reviewed disposition; `different` / `refused_*` here are the documented changes of items 3, 4, 5 "
                  "and 8 (see the test header), `identical` the preserved results.", ""]
            for x in ev["a19"]:
                L.append("- item %s **%s** `%s` [%s, call %s]: baseline %s; current %s" % (
                    x["item"], x["outcome"], x["key"].split(":", 2)[-1], x["fn"], x["occurrence"],
                    x["baseline"].strip() or "-", x["current"].strip() or "-"))
                for d in x["diffs"]:
                    L.append("  - %s" % d)
                for w in (x.get("baseline_warnings") or [])[:2]:
                    L.append("  - baseline warning: %s" % _short(w, 200))
                for w in (x.get("current_warnings") or [])[:2]:
                    L.append("  - current warning: %s" % _short(w, 200))
        if ev.get("probe") and (ev["probe"].get("baseline") or ev["probe"].get("current")):
            L += ["", "### Open item (i): set-op below nfilter (tools/ws2/test-b10-probe-setop.R, evidence only)", ""]
            for side in ("baseline", "current"):
                for l in ev["probe"].get(side) or []:
                    L.append("- %s: %s" % (side, l))
        L.append("")
    cc = rep["corpus"]
    L += ["## ir-v1-corpus.json entries vs the baseline capture", "",
          "%d distinct plans executed successfully at baseline (%d not in the corpus)." % (cc["baseline_ok_plans"], cc["baseline_ok_plans_not_in_corpus"]), "",
          md_table(cc["summary"], ["corpus_executed", "baseline", "n"]), "",
          "Corpus ids executed successfully at baseline: " + ", ".join(r["id"] for r in cc["rows"] if r["baseline"] == "ok"), "",
          "Corpus ids reaching `.planExecute()` at baseline but refused there: " + ", ".join(r["id"] for r in cc["rows"] if r["baseline"] == "refused"), "",
          "`.planResolvePopulations()` at baseline (any depth): %s; corpus ids resolved: %s" % (
              json.dumps(cc.get("resolve_populations")),
              ", ".join(r["id"] for r in cc["rows"] if r.get("resolve_populations") == "ok")), ""]
    cov = rep.get("coverage") or {}
    if cov.get("by_class"):
        cats = sorted({c for v in cov["by_class"].values() for c in v})
        L += ["## Coverage: what each classified block does (WS-0 inventory of the pinned baseline)", "",
              "Only blocks counted under `captured at baseline` contribute result comparisons above.", ""]
        for lab in sorted(cov["by_class"]):
            L.append("- **%s**: " % lab + "; ".join("%s = %d" % (c, n) for c, n in sorted(cov["by_class"][lab].items(), key=lambda x: -x[1])))
        L.append("")
    elif cov.get("error"):
        L += ["## Coverage", "", "inventory unavailable: " + cov["error"], ""]
    L += ["## Canonical serialiser copies", "", json.dumps(rep["canonical_copies"]), ""]
    open(path, "w", encoding="utf-8").write("\n".join(L) + "\n")


def coverage_breakdown(by_block, base, server_repo, client_repo):
    """What the classified blocks call, from the WS-0 inventory of the pinned baseline
    (tools/ws0/corpus_inventory.py): which classified blocks reached .planExecute() /
    omopPlanExecuteDS() in the baseline capture, and what the others call instead."""
    sys.dont_write_bytecode = True
    sys.path.insert(0, os.path.join(SERVER_ROOT, "tools", "ws0"))
    try:
        from corpus_inventory import inventory
        inv = {b["key"]: b for b in inventory(server_repo, client_repo)}
    except Exception as e:  # git history unavailable
        return dict(error=str(e))
    finally:
        sys.path.pop(0)
    executed = {r["_key"] for r in base.top.values()}
    out = collections.defaultdict(collections.Counter)
    examples = collections.defaultdict(list)
    for key in by_block:
        lab = binding(by_block, key)["label"]
        b = inv.get(key)
        if key in executed:
            cat = "captured at baseline"
        elif b is None:
            cat = "not in the inventory"
        else:
            calls = set(b["plan_calls"]) | set(v.split(" -> ")[-1] for v in b["plan_helpers"].values())
            if b["package"] == "dsOMOPClient":
                cat = "client suite (DSI mocked; plans built/sent, not executed)" if calls else "client suite, gate helpers only"
            elif any(c.startswith((".planExecute", "omopPlanExecuteDS")) for c in calls):
                cat = "server: calls .planExecute/omopPlanExecuteDS but no call captured (skipped or failed before the call)"
            elif any(c.startswith((".planResolvePopulations", ".planResolveIndexEvent", ".resolvePopulation")) for c in calls):
                cat = "server: population resolution only (.planResolvePopulations & co.)"
            elif any(c.startswith((".planValidate", "omopPlanValidateDS", "omopPlanPreviewDS", ".planPreview")) for c in calls):
                cat = "server: plan validation/preview only"
            elif any("cohort" in c.lower() for c in calls):
                cat = "server: cohort endpoints/helpers"
            elif any(c.startswith((".compile", ".extract", ".toFeatures", ".windowPredicateSql")) for c in calls):
                cat = "server: SQL compilers / extraction helpers"
            elif calls:
                cat = "server: other plan entry points"
            else:
                cat = "server: gate helpers only"
        out[lab][cat] += 1
        if len(examples[(lab, cat)]) < 3:
            examples[(lab, cat)].append(key)
    return dict(by_class={k: dict(v) for k, v in out.items()},
                examples={"%s | %s" % k: v for k, v in examples.items()})


def cmd_compare(a):
    work = os.path.abspath(a.work)
    rows, by_block = load_classes(a.classes)
    base = Run(work, a.baseline)
    nondet, masked = {}, {}
    det = dict(runs=[a.baseline, a.baseline_repeat], compared=0, nondeterministic=[], masked=[])
    if a.baseline_repeat:
        rep_run = Run(work, a.baseline_repeat)
        nondet = determinism(base, rep_run, masked)
        det["compared"] = len(base.top)
        det["nondeterministic"] = [dict(key=k[1], fn=k[0], plan_sha256=k[2], context=k[3], occurrence=k[4], reason=v,
                                        cls=binding(by_block, k[1])["label"]) for k, v in nondet.items()]
        det["masked"] = [dict(key=k[1], fn=k[0], occurrence=k[4], differing=v, cls=binding(by_block, k[1])["label"])
                         for k, v in masked.items()]
    comps = []
    for lab in a.current:
        comps.append(comparison(work, a.baseline, lab, by_block, nondet,
                                "Baseline %s vs %s (%s)" % (a.baseline, lab, Run(work, lab).meta["side"]), masked))
    helpers = sorted({Run(work, l).meta.get("helper_sha256") for l in
                      [a.baseline] + ([a.baseline_repeat] if a.baseline_repeat else []) + list(a.current)})
    if len(helpers) != 1:
        print("WARNING: the compared captures used different capture_results.R versions:", helpers)
    corpus = json.load(open(a.corpus, encoding="utf-8"))
    rep = dict(generated=datetime.datetime.utcnow().isoformat() + "Z", classes_csv=a.classes, corpus_json=a.corpus,
               runs=[Run(work, l).meta for l in [a.baseline] + ([a.baseline_repeat] if a.baseline_repeat else []) + list(a.current)],
               determinism=det, comparisons=comps, corpus=corpus_check(corpus, base),
               capture_helper_sha256=helpers,
               canonical_copies=check_canonical_copies(),
               coverage=coverage_breakdown(by_block, base, a.server_repo, a.client_repo))
    out = os.path.abspath(a.out or os.path.join(work, "report"))
    os.makedirs(out, exist_ok=True)
    json.dump(rep, open(os.path.join(out, "b10-report.json"), "w", encoding="utf-8"), indent=1, default=str)
    write_markdown(rep, os.path.join(out, "b10-report.md"))
    for cmp in comps:
        print(cmp["title"])
        for t in cmp["classes"]:
            print("  %-60s %s | blocks %s" % (t["label"], " ".join("%s=%d" % (k, v) for k, v in t["calls"].items() if v), dict(t["blocks"])))
        print("  safe issues:", len(cmp["safe_issues"]), " safe unmatched:", len(cmp["safe_unmatched"]), " leaks:", len(cmp["leaks"]))
    print("nondeterministic:", len(det["nondeterministic"]))
    print("report:", os.path.join(out, "b10-report.md"))


OPTION_PATTERN = r"^(default\.)?(nfilter|dsomop|dsstaging|datashield)\."
CANONICAL_DIGITS = 17
CORE_FIELDS = ("dbms", "target_dialect", "cdm_schema", "vocab_schema", "results_schema",
               "temp_schema", "resource_client", "config", "temp_tables")
A19_FILE = "test-ir-results-a19.R"
PROBE_FILE = "test-b10-probe-setop.R"
EVIDENCE_FILES = (A19_FILE, PROBE_FILE)

# A mixed block (#sub rows) is classified per row, but calls are bound per block.
# A successful call of a mixed block is pinned only with a reviewed attribution to
# its safe (or reviewed corrected) row (from the 2.10.2 test code and the frozen rows).
MIXED_ATTRIBUTION = {
    "dsOMOP:test-output-contracts.R:baseline output produces one row per cohort member":
        "the block's only call; row #2 (one row per cohort member with age_group: safe, corrected by "
        "F-26 after A-19 item 1) is its result contract; row #1 (newly_permitted, F-25: year_of_birth "
        "returned when requested) concerns a column this plan does not request, so it leaves this "
        "call's documented result unchanged",
    "dsOMOP:test-output-contracts.R:baseline accepts only a public common age-grid coarsening":
        "the .planExecute() call with the public common grid is row #1 (safe: accepted); row #2 "
        "(newly_permitted: off-grid grid) is a .planValidate() call, not executed and not pinned",
    "dsOMOP:test-output-contracts.R:cohort_membership absolute dates require explicit authorization":
        "the call with dsomop.allow_absolute_dates = TRUE is row #2 (safe); the unauthorised call is "
        "row #1 (unchanged_invalid), refused at baseline and not pinned",
}
# Reviewed corrections the replay applies (tests/testthat/test-ir-results.R
# .B10_CORRECTIONS): a block corrected by them stays pinnable.
REVIEWED_CORRECTIONS = {
    "dsOMOP:test-output-contracts.R:baseline output produces one row per cohort member":
        "F-26 demo.age_group (A-19 item 1)",
    "dsOMOP:test-output-contracts.R:single plan with all output types executes successfully":
        "F-26 demo.age_group (A-19 item 1)",
}


def output_types(plan_exact):
    """Output types declared by a plan (from its exact canonical form)."""
    def get(node, name):
        if not isinstance(node, dict) or node.get("kind") != "list" or not node.get("names"):
            return None
        return dict(zip(node["names"], node["items"])).get(name)
    outs = get(plan_exact, "outputs")
    types = []
    if outs and outs.get("names"):
        for o in outs["items"]:
            t = get(o, "type")
            types.append(t["values"][0] if t and t.get("kind") == "atomic" and t.get("values") else "event_level")
    return types


def pinnable_class(by_block, key):
    """A block whose call may be pinned: class safe, or corrected by a reviewed correction
    (replayed with .B10_CORRECTIONS); a mixed block needs a reviewed attribution."""
    bind = binding(by_block, key)
    cls = set(bind["classes"])
    if not ("safe" in cls or ("corrected" in cls and key in REVIEWED_CORRECTIONS)):
        return "class " + bind["label"]
    if bind["mixed"] and len(cls) > 1 and key not in MIXED_ATTRIBUTION:
        return "mixed block without a reviewed attribution to its safe row: " + bind["label"]
    return None


def deterministic(r, rep):
    """The repeat baseline run has the same outcome, value and raw person tokens for r."""
    if rep is None:
        return True
    o = rep.top.get(r["_join"])
    if o is None or o["outcome"]["status"] != r["outcome"]["status"]:
        return False
    if canon_json(result_node(o)) != canon_json(result_node(r)):
        return False
    for f in ("volatile", "assigned_volatile"):
        if ((o.get(f) or {}).get("raw_token_sha256")) != ((r.get(f) or {}).get("raw_token_sha256")):
            return False
    return True


def pin_candidates(base, rep, by_block):
    pins, rejected = [], collections.Counter()
    for k, r in base.top.items():
        if r["_suite"] != "dsOMOP" or r["fn"] != ".planExecute" or is_evidence(r):
            continue
        if ((r.get("call") or {}).get("output_mode") or "memory") == "staged":
            continue   # staged_candidates()
        why = pinnable_class(by_block, r["_key"])
        if why:
            pass
        elif r["outcome"]["status"] != "ok":
            why = "refused at baseline"
        elif r.get("warnings"):
            why = "warnings at baseline (permissive output failure possible)"
        elif not (r.get("handle") or {}).get("default_fixture"):
            why = "not the default fixture: " + ",".join((r.get("handle") or {}).get("diffs") or [])[:120]
        elif ((r.get("call") or {}).get("output_mode") or "memory") != "memory":
            why = "output_mode " + str((r.get("call") or {}).get("output_mode"))
        elif r.get("scope_frames"):
            why = "plan carries data frames"
        elif not r.get("plan_exact") or not (r.get("call") or {}).get("out_symbols"):
            why = "no exact plan form"
        elif not deterministic(r, rep):
            why = "nondeterministic across the two baseline runs"
        if why:
            rejected[why.split(":")[0]] += 1
            continue
        pins.append(r)
    return pins, rejected


def staged_components(node, path="result", out=None):
    """Every staged descriptor of a canonical tree: path, file state, rows, distinct persons."""
    out = [] if out is None else out
    if not isinstance(node, dict):
        return out
    if node.get("kind") == "list" and "b10_staged" in (node.get("class") or []):
        sf = _list_get(node, "staged_file") or {}
        content = _list_get(sf, "content")
        persons = None
        if isinstance(content, dict) and content.get("kind") == "data.frame":
            for n, c in zip(content["names"], content["columns"]):
                if n.lower() in ("person_id", "subject_id"):
                    persons = len(set(v for v in c.get("values") or [] if v is not None))
        out.append(dict(path=path, state=((_list_get(sf, "state") or {}).get("values") or [None])[0],
                        rows=content.get("nrow") if isinstance(content, dict) else None, persons=persons))
        return out
    if node.get("kind") == "list":
        names = node.get("names") or [str(i + 1) for i in range(len(node.get("items") or []))]
        for n, it in zip(names, node.get("items") or []):
            staged_components(it, path + "." + str(n), out)
    return out


def scalar_option(r, name, default=None):
    o = ((r.get("options") or {}).get("options") or {}).get(name)
    if isinstance(o, dict) and o.get("values"):
        try:
            return float(o["values"][0])
        except (TypeError, ValueError):
            return default
    return default


def rebuildable_fixture(h):
    """None when a replay can rebuild the call's fixture exactly from the record (changed-table
    rows, person-key fields), else the reason."""
    diffs = list(h.get("diffs") or [])
    content, exact = h.get("table_content") or {}, h.get("exact_fields") or {}
    for d in diffs:
        if d.startswith("table:"):
            c = content.get(d[6:])
            if not c or c.get("kind") == "unrecordable":
                return "changed table %s not recorded (%s)" % (d[6:], (c or {}).get("reason"))
        elif d.startswith("field:"):
            f = d[6:]
            if f in CORE_FIELDS or f == "blueprint":
                return "handle field " + f
            if not exact.get(f) or exact[f].get("kind") == "unrecordable":
                return "handle field %s not recorded" % f
        else:
            return d
    return None


TMP_PATH = re.compile(r"^/tmp/Rtmp[A-Za-z0-9]+/")


def fresh_tempfile_options(r):
    """Options naming a temporary path of the baseline session: the replay gives them a fresh
    tempfile() with the same prefix."""
    out = {}
    for n, v in ((r.get("options") or {}).get("options") or {}).items():
        vals = v.get("values") if isinstance(v, dict) else None
        if v.get("type") == "character" and vals and len(vals) == 1 and isinstance(vals[0], str) and TMP_PATH.match(vals[0]):
            out[n] = re.sub(r"[0-9a-f]{10,}$", "", os.path.basename(vals[0].rstrip("/")))
    return out


def staged_candidates(base, rep, by_block):
    pins, rejected, admissibility = [], collections.Counter(), []
    for k, r in base.top.items():
        if r["_suite"] != "dsOMOP" or r["fn"] not in (".planExecute", "omopPlanExecuteDS") or is_evidence(r):
            continue
        call = r.get("call") or {}
        if (call.get("output_mode") or "memory") != "staged":
            continue
        h = r.get("handle") or {}
        ok = r["outcome"]["status"] == "ok"
        comps = staged_components(result_node(r)) if ok else []
        k_subset = scalar_option(r, "nfilter.subset", scalar_option(r, "default.nfilter.subset", 3.0))
        roots = call.get("out_symbols") if r["fn"] == ".planExecute" else call.get("out")
        reasons = []
        why = pinnable_class(by_block, r["_key"])
        if why:
            reasons.append(why)
        if not ok:
            reasons.append("refused at baseline")
        if r.get("warnings"):
            reasons.append("warnings at baseline")
        if ok and (not roots or not ((roots.get("items") if roots.get("kind") == "list" else roots.get("values")) or [])):
            reasons.append("no requested root (A-19 item 4: 2.10.3 publishes nothing)")
        if r.get("scope_frames"):
            reasons.append("plan carries data frames")
        if not r.get("plan_exact"):
            reasons.append("no exact plan form")
        if rebuildable_fixture(h):
            reasons.append("fixture not rebuildable: " + rebuildable_fixture(h))
        if any(isinstance(v, str) and TMP_PATH.match(v) for v in ((r.get("options") or {}).get("env") or {}).values()):
            reasons.append("environment names a session temporary path")
        if ok and (not comps or any(c["state"] != "read" for c in comps)):
            reasons.append("staged file not readable")
        if any(c["persons"] is not None and c["persons"] < k_subset for c in comps):
            reasons.append("not admissible: a component below nfilter.subset (%s)" % ", ".join(
                "%s=%s" % (c["path"].split(".")[-1], c["persons"]) for c in comps if c["persons"] is not None))
        if not reasons and not deterministic(r, rep):
            reasons.append("nondeterministic across the two baseline runs")
        admissibility.append(dict(key=r["_key"], fn=r["fn"], nfilter_subset=k_subset, components=comps,
                                  pinned=not reasons, reasons=reasons))
        if reasons:
            rejected[reasons[0].split(":")[0]] += 1
            continue
        pins.append(r)
    return pins, rejected, admissibility


def _pin_common(i, prefix, r, by_block, corpus):
    ctx = r.get("context") or {}
    return dict(id="%s%03d" % (prefix, i), key=r["_key"], cls=binding(by_block, r["_key"])["label"],
                line=ctx.get("line"), occurrence=r["_join"][-1], also=[],
                attribution={r["_key"]: MIXED_ATTRIBUTION[r["_key"]]} if r["_key"] in MIXED_ATTRIBUTION else {},
                correction=REVIEWED_CORRECTIONS.get(r["_key"]),
                corpus_id=corpus.get(r["_hash"]), plan_sha256=r["_hash"],
                output_types=output_types(r["plan_exact"]), fn=r["fn"],
                plan=r["plan_exact"], options=(r.get("options") or {}).get("options") or {},
                env=(r.get("options") or {}).get("env") or {})


def _write_fixture(fx, out):
    txt = json.dumps(fx, indent=None, separators=(",", ":"), sort_keys=False)
    open(out, "w", encoding="utf-8").write(txt + "\n")
    return len(txt)


def cmd_pin(a):
    work = os.path.abspath(a.work)
    rows, by_block = load_classes(a.classes)
    base = Run(work, a.baseline)
    rep = Run(work, a.baseline_repeat) if a.baseline_repeat else None
    if base.meta["side"] != "baseline" or (rep and rep.meta["side"] != "baseline"):
        raise SystemExit("pins come from BASELINE captures only")
    corpus = {e["sha256"]: e["id"] for e in json.load(open(a.corpus, encoding="utf-8"))}
    ref = next((o["reference"] for o in base.other if o.get("kind") == "reference" and o.get("pkg") == "dsOMOP"), {})
    provenance = dict(dsOMOP=base.meta["server_commit"], dsOMOPClient=base.meta["client_commit"],
                      captures=[base.label] + ([rep.label] if rep else []),
                      capture_helper_sha256=base.meta.get("helper_sha256"))
    common_rules = ["block class safe in v1-corpus-classes.csv, or corrected by a reviewed correction replayed "
                    "by test-ir-results.R (.B10_CORRECTIONS; tools/ws2/compare_results.py REVIEWED_CORRECTIONS); "
                    "a mixed block only with a reviewed attribution of the call to that row (`attribution`; "
                    "MIXED_ATTRIBUTION)",
                    "succeeded at baseline without warnings, with no data frame inside the plan",
                    "identical in two independent baseline runs at 17 significant digits (values, raw person "
                    "tokens); duplicates (same plan, symbols, options, environment) pinned once"]
    # memory pins
    cands, rejected = pin_candidates(base, rep, by_block)
    groups = collections.OrderedDict()
    for r in cands:
        sig = canon_json([r["plan_exact"], r["call"]["out_symbols"], (r.get("options") or {}).get("options"),
                          (r.get("options") or {}).get("env")])
        groups.setdefault(sig, []).append(r)
    pins = []
    for i, (sig, rs) in enumerate(groups.items(), 1):
        r = rs[0]
        res = r["result"]
        for o in rs[1:]:
            if canon_json(o["result"]) != canon_json(res):
                raise SystemExit("same plan and options, different baseline results: %s / %s" % (r["_key"], o["_key"]))
        p = _pin_common(i, "r", r, by_block, corpus)
        p["also"] = [dict(key=o["_key"], occurrence=o["_join"][-1]) for o in rs[1:]]
        p["attribution"] = {o["_key"]: MIXED_ATTRIBUTION[o["_key"]] for o in rs if o["_key"] in MIXED_ATTRIBUTION}
        p.update(output_mode="memory", out_symbols=r["call"]["out_symbols"],
                 fresh_tempfile_options=fresh_tempfile_options(r),
                 result_sha256=hashlib.sha256(canon_json(res).encode()).hexdigest(), result=res,
                 volatile=r.get("volatile"))
        pins.append(p)
    fx = dict(
        version=2, canonical_digits=CANONICAL_DIGITS,
        description=("BASELINE (dsOMOP 2.10.2) canonical results of safe-class v1 plans, re-executed by "
                     "tests/testthat/test-ir-results.R with the current code (review B10); doubles at 17 "
                     "significant digits (lossless)."),
        generated_by="tools/ws2/compare_results.py pin (capture: tools/ws2/capture_results.R)",
        baseline=provenance,
        selection=["a .planExecute() call made directly by a test block of the 2.10.2 dsOMOP suite, in memory mode"] +
                  common_rules +
                  ["on an unmodified default create_test_handle() fixture (data, TEMP schema and handle fields equal to a fresh one)"],
        option_pattern=OPTION_PATTERN, reference_fixture_db_sha256=ref.get("db_sha256"),
        rejected_candidates=dict(rejected), pins=pins)
    out = a.out or os.path.join(SERVER_ROOT, "tests", "testthat", "fixtures", "ir-v1-results.json")
    size = _write_fixture(fx, out)
    types = collections.Counter(t for p in pins for t in p["output_types"])
    print("%d memory pins (%d candidate calls) -> %s (%.0f KB)" % (len(pins), len(cands), out, size / 1024.0))
    print("  output types:", dict(types))
    print("  rejected:", dict(rejected))
    # staged pins
    scands, srejected, admissibility = staged_candidates(base, rep, by_block)
    spins = []
    for i, r in enumerate(scands, 1):
        p = _pin_common(i, "s", r, by_block, corpus)
        call = r["call"]
        h = r.get("handle") or {}
        p.update(output_mode="staged", fresh_tempfile_options=fresh_tempfile_options(r),
                 fixture=dict(db_sha256=(h.get("db") or {}).get("sha256"), diffs=h.get("diffs"),
                              exact_fields=h.get("exact_fields") or {}, table_content=h.get("table_content") or {}),
                 support=dict(nfilter_subset=scalar_option(r, "nfilter.subset", scalar_option(r, "default.nfilter.subset", 3.0)),
                              components=staged_components(result_node(r))))
        if r["fn"] == ".planExecute":
            p.update(out_symbols=call["out_symbols"], result=r["result"], volatile=r.get("volatile"),
                     result_sha256=hashlib.sha256(canon_json(r["result"]).encode()).hexdigest())
        else:
            asg = r.get("assigned") or {}
            p.update(omop_symbol=call.get("omop_symbol"), out=call.get("out"),
                     assigned=dict(created=asg.get("created"), values=asg.get("values")),
                     assigned_volatile=r.get("assigned_volatile"),
                     result_sha256=hashlib.sha256(canon_json(asg.get("values")).encode()).hexdigest())
        spins.append(p)
    sfx = dict(
        version=1, canonical_digits=CANONICAL_DIGITS,
        description=("BASELINE (dsOMOP 2.10.2) canonical STAGED results of safe-class v1 plans with requested "
                     "roots on admissible fixtures, re-executed by tests/testthat/test-ir-results.R with the "
                     "current code (review B10, Astra R5)."),
        generated_by="tools/ws2/compare_results.py pin (capture: tools/ws2/capture_results.R)",
        baseline=provenance,
        selection=["a .planExecute() or omopPlanExecuteDS() call made directly by a test block of the 2.10.2 dsOMOP "
                   "suite, in staged mode, with at least one requested root"] + common_rules +
                  ["admissible: every staged component with a person column holds >= nfilter.subset distinct "
                   "persons (`support`)",
                   "a fixture the replay rebuilds exactly: a fresh create_test_handle() with the recorded rows of "
                   "every changed table and the recorded non-core handle fields (person key); no TEMP tables, no "
                   "core field change; the rebuilt content hash must equal `fixture$db_sha256`",
                   "options naming a temporary directory of the baseline session (`fresh_tempfile_options`) get a "
                   "fresh tempfile() in the replay"],
        compared=("the complete canonical form of the returned value (.planExecute) or of the assigned bindings "
                  "(omopPlanExecuteDS): descriptors field by field and the staged file content and byte hash; "
                  "excluded volatile parts only: staging tokens, the session tempdir and tempfile hex of "
                  "metadata$file, expires_at; person tokens by linkage plus raw_token_sha256"),
        option_pattern=OPTION_PATTERN, rejected_candidates=dict(srejected), admissibility=admissibility,
        pins=spins)
    sout = a.out_staged or os.path.join(SERVER_ROOT, "tests", "testthat", "fixtures", "ir-v1-results-staged.json")
    size = _write_fixture(sfx, sout)
    print("%d staged pins -> %s (%.0f KB)" % (len(spins), sout, size / 1024.0))
    print("  rejected:", dict(srejected))
    for x in admissibility:
        print("  %s %s %s: %s" % ("PIN " if x["pinned"] else "skip", x["fn"], x["key"][:90], "; ".join(x["reasons"]) or
                                  ", ".join("%s persons=%s rows=%s" % (c["path"], c["persons"], c["rows"]) for c in x["components"])))


def a19_blocks(run, file=A19_FILE):
    out = collections.OrderedDict()
    for r in sorted((r for r in run.calls if r["_top"] and (r.get("context") or {}).get("file") == file),
                    key=lambda r: r["_order"]):
        out.setdefault(r["_key"], []).append(r)
    return out


def a19_pin_record(r):
    o = r["outcome"]
    outcome = dict(status=o["status"])
    if o["status"] != "ok":
        outcome.update({k: o.get(k) for k in ("class", "message", "code")})
    rec = dict(fn=r["fn"], entry=r.get("entry"), line=(r.get("context") or {}).get("line"),
               plan_sha256=r["_hash"], plan=r.get("plan_exact"), call=r.get("call"),
               db_sha256=((r.get("handle") or {}).get("db") or {}).get("sha256"),
               outcome=outcome, warnings=[w.get("message") for w in r.get("warnings") or []],
               result=r.get("result"), volatile=r.get("volatile"))
    if r["fn"] in ASSIGN_FNS:
        asg = r.get("assigned") or {}
        rec["assigned"] = dict(created=sorted(set((asg.get("created") or []) + (asg.get("replaced") or []))),
                               values=asg.get("values"))
        rec["assigned_volatile"] = r.get("assigned_volatile")
    return rec


def a19_differences(ra, rb):
    """Why two baseline records of the same A-19 call differ (empty when deterministic)."""
    bad = []
    if ra["fn"] != rb["fn"]:
        bad.append("function")
    if ra["outcome"]["status"] != rb["outcome"]["status"]:
        bad.append("outcome")
    if norm_msg(ra["outcome"].get("message")) != norm_msg(rb["outcome"].get("message")):
        bad.append("message")
    if warnings_of(ra) != warnings_of(rb):
        bad.append("warnings")
    sub = []
    diff_nodes(result_node(ra), result_node(rb), "result", sub)
    bad += ["%s %s" % (d["aspect"], d["path"]) for d in sub[:5]]
    for f in ("volatile", "assigned_volatile"):
        if (ra.get(f) or {}).get("raw_token_sha256") != (rb.get(f) or {}).get("raw_token_sha256"):
            bad.append(f + ".raw_token_sha256")
    if ((ra.get("handle") or {}).get("db") or {}).get("sha256") != ((rb.get("handle") or {}).get("db") or {}).get("sha256"):
        bad.append("fixture content")
    return bad


def cmd_pin_a19(a):
    work = os.path.abspath(a.work)
    base = Run(work, a.baseline)
    rep = Run(work, a.baseline_repeat) if a.baseline_repeat else None
    if base.meta["side"] != "baseline" or (rep and rep.meta["side"] != "baseline"):
        raise SystemExit("pins come from BASELINE captures only")
    bb = a19_blocks(base)
    rb = a19_blocks(rep) if rep else {}
    pins, nondet = collections.OrderedDict(), {}
    for key, recs in bb.items():
        if rep is not None:
            other = rb.get(key) or []
            problems = [] if len(other) == len(recs) else ["%d / %d calls" % (len(recs), len(other))]
            for i, (ra, rr) in enumerate(zip(recs, other), 1):
                problems += ["call %d: %s" % (i, d) for d in a19_differences(ra, rr)]
            if problems:
                nondet[key] = problems
                continue
        pins[key] = [a19_pin_record(r) for r in recs]
    fx = dict(
        version=1, canonical_digits=CANONICAL_DIGITS,
        description=("BASELINE (dsOMOP 2.10.2) records of the A-19 evidence calls of "
                     "tests/testthat/test-ir-results-a19.R (declared deterministic fixtures), replayed by that "
                     "test with the expectation of each A-19 item (review B10, CR-19)."),
        generated_by="tools/ws2/compare_results.py pin-a19 (capture: tools/ws2/capture_results.R with "
                     "--add-tests tests/testthat/test-ir-results-a19.R)",
        baseline=dict(dsOMOP=base.meta["server_commit"], dsOMOPClient=base.meta["client_commit"],
                      captures=[base.label] + ([rep.label] if rep else []),
                      capture_helper_sha256=base.meta.get("helper_sha256"),
                      a19_test=[t for t in base.meta.get("added_tests") or [] if t["path"].endswith(A19_FILE)]),
        selection=["every top-level captured call of a test block of test-ir-results-a19.R, in call order",
                   "identical in two independent baseline runs at 17 significant digits (outcome, message, "
                   "warnings, values, raw person tokens, fixture content)"],
        nondeterministic=nondet, pins=pins)
    out = a.out or os.path.join(SERVER_ROOT, "tests", "testthat", "fixtures", "ir-v1-results-a19.json")
    size = _write_fixture(fx, out)
    print("%d A-19 blocks (%d calls) -> %s (%.0f KB); nondeterministic: %d" % (
        len(pins), sum(len(v) for v in pins.values()), out, size / 1024.0, len(nondet)))
    for k, v in pins.items():
        print("  %s: %s" % (k.split(":", 2)[-1], ", ".join("%s %s" % (c["fn"], c["outcome"]["status"]) for c in v)))
    for k, v in nondet.items():
        print("  NONDETERMINISTIC %s: %s" % (k, "; ".join(v[:4])))


def cmd_check(a):
    print(json.dumps(check_canonical_copies(), indent=1))


def cmd_validate(a):
    """Run tests/testthat/test-ir-results*.R (+ their fixtures) from this worktree against a
    snapshot: --side baseline must pass (the pins reproduce 2.10.2 itself); --side current
    is the B10 check of the code."""
    work = os.path.abspath(a.work)
    srv_repo, cli_repo = os.path.abspath(a.server_repo), os.path.abspath(a.client_repo)
    pins = json.load(open(os.path.join(srv_repo, "tools", "ws0", "corpus_baseline.json")))
    if a.side == "baseline":
        s_commit, c_commit = pins["dsOMOP"], pins["dsOMOPClient"]
    else:
        s_commit, c_commit = git(srv_repo, "rev-parse", a.server_commit), git(cli_repo, "rev-parse", a.client_commit)
    snap = os.path.join(work, "snap", a.label)
    s_dir, c_dir = os.path.join(snap, "dsOMOP"), os.path.join(snap, "dsOMOPClient")
    snapshot(srv_repo, s_commit, s_dir)
    snapshot(cli_repo, c_commit, c_dir)
    tt = os.path.join(s_dir, "tests", "testthat")
    os.makedirs(os.path.join(tt, "fixtures"), exist_ok=True)
    for f in glob.glob(os.path.join(SERVER_ROOT, "tests", "testthat", "test-ir-results*.R")):
        shutil.copy(f, tt)
    for f in glob.glob(os.path.join(SERVER_ROOT, "tests", "testthat", "fixtures", "ir-v1-results*.json")):
        shutil.copy(f, os.path.join(tt, "fixtures"))
    classes = os.path.join(tt, "fixtures", "v1-corpus-classes.csv")
    if not os.path.exists(classes):   # the 2.10.2 tree predates the frozen classes
        shutil.copy(os.path.join(SERVER_ROOT, "tests", "testthat", "fixtures", "v1-corpus-classes.csv"), classes)
    env = dict(os.environ)
    env["RTEST_WITH"] = c_dir
    t0 = datetime.datetime.utcnow()
    run = subprocess.run([RTEST, s_dir, "^ir-results", str(a.timeout)], env=env, capture_output=True, text=True)
    secs = (datetime.datetime.utcnow() - t0).total_seconds()
    name = pod_name(s_dir)
    log = ssh("cat '/root/rt/%s.last.log' && rm -f '/root/rt/%s.last.log'" % (name, name)).stdout
    out = os.path.join(work, "validate-%s.log" % a.label)
    open(out, "w").write(log)
    res = [l for l in run.stdout.splitlines() if l.startswith("RESULT")]
    print("[validate %s] server %s client %s: %s (%.0f s); log %s" % (
        a.side, s_commit[:12], c_commit[:12], res[-1] if res else run.stdout[-2000:], secs, out))
    if run.stdout and (not res or not res[-1].startswith("RESULT failed: 0 errors: 0")):
        print(run.stdout[-6000:])
    if not a.keep_snapshot:
        drop_snapshot(snap, (s_dir, c_dir))


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("capture")
    c.add_argument("--side", choices=["baseline", "current", "hybrid"], required=True)
    c.add_argument("--label", required=True)
    c.add_argument("--work", required=True)
    c.add_argument("--server-repo", default=SERVER_ROOT)
    c.add_argument("--client-repo", default=os.path.join(os.path.dirname(SERVER_ROOT), "dsOMOPClient"))
    c.add_argument("--server-commit", default="HEAD")
    c.add_argument("--client-commit", default="HEAD")
    c.add_argument("--suites", default="server,client")
    c.add_argument("--filter", default="")
    c.add_argument("--timeout", type=int, default=5400)
    c.add_argument("--add-tests", action="append", default=[],
                   help="a test file of this worktree to copy into the server snapshot's tests/testthat/")
    c.add_argument("--keep-snapshot", action="store_true")
    m = sub.add_parser("compare")
    m.add_argument("--work", required=True)
    m.add_argument("--baseline", required=True, help="label of the baseline capture")
    m.add_argument("--baseline-repeat", default=None, help="label of a second baseline capture (determinism)")
    m.add_argument("--current", required=True, action="append", help="label(s) of the current / hybrid captures")
    m.add_argument("--classes", default=os.path.join(SERVER_ROOT, "tests", "testthat", "fixtures", "v1-corpus-classes.csv"))
    m.add_argument("--corpus", default=os.path.join(SERVER_ROOT, "tests", "testthat", "fixtures", "ir-v1-corpus.json"))
    m.add_argument("--out", default=None)
    m.add_argument("--server-repo", default=SERVER_ROOT)
    m.add_argument("--client-repo", default=os.path.join(os.path.dirname(SERVER_ROOT), "dsOMOPClient"))
    q = sub.add_parser("pin")
    q.add_argument("--work", required=True)
    q.add_argument("--baseline", required=True)
    q.add_argument("--baseline-repeat", default=None)
    q.add_argument("--classes", default=os.path.join(SERVER_ROOT, "tests", "testthat", "fixtures", "v1-corpus-classes.csv"))
    q.add_argument("--corpus", default=os.path.join(SERVER_ROOT, "tests", "testthat", "fixtures", "ir-v1-corpus.json"))
    q.add_argument("--out", default=None)
    q.add_argument("--out-staged", default=None)
    q2 = sub.add_parser("pin-a19")
    q2.add_argument("--work", required=True)
    q2.add_argument("--baseline", required=True)
    q2.add_argument("--baseline-repeat", default=None)
    q2.add_argument("--out", default=None)
    v = sub.add_parser("validate")
    v.add_argument("--side", choices=["baseline", "current"], required=True)
    v.add_argument("--label", required=True)
    v.add_argument("--work", required=True)
    v.add_argument("--server-repo", default=SERVER_ROOT)
    v.add_argument("--client-repo", default=os.path.join(os.path.dirname(SERVER_ROOT), "dsOMOPClient"))
    v.add_argument("--server-commit", default="HEAD")
    v.add_argument("--client-commit", default="HEAD")
    v.add_argument("--timeout", type=int, default=1500)
    v.add_argument("--keep-snapshot", action="store_true")
    sub.add_parser("check", help="check that the b10-canonical copies agree")
    a = ap.parse_args()
    {"capture": cmd_capture, "compare": cmd_compare, "pin": cmd_pin, "pin-a19": cmd_pin_a19,
     "validate": cmd_validate, "check": cmd_check}[a.cmd](a)


if __name__ == "__main__":
    main()
