#!/usr/bin/env python3
"""Mandatory independent check of the WS-0 contract artefacts (B2 of the WS-0 review).

Runs in CI (.github/workflows/ws0-contracts.yaml) and before every freeze
resubmission. It uses the Python `jsonschema` package as a second, independent
Draft 2020-12 oracle for everything the packaged R validator checks:

  * the three schemas are valid under the 2020-12 metaschema and use only the
    keywords the R validator supports;
  * the two canonical examples validate under plan-v2 and ir-v2;
  * every probe verdict of probes/*.json holds (plan-v2, ir-v2, mutate-v1);
    text probes (non-finite numbers, duplicate members) are checked with
    parser hooks, since a conforming JSON document cannot carry them;
  * every acceptance plan validates under plan-v2 (and ir-v2);
  * every digest vector reproduces (RFC 8785 canonicalisation, ECMAScript
    numbers) and the documented relations between vectors hold;
  * SHA256SUMS matches the files, in the server and the client copies.

Usage: python3 tools/ws0/check_contracts.py <dsOMOP dir> [<dsOMOPClient dir>]
Exit status 0 means every check passed.
"""
import copy, hashlib, json, os, sys
from jsonschema import Draft202012Validator

S = sys.argv[1]
C = sys.argv[2] if len(sys.argv) > 2 else None
SCHEMA = os.path.join(S, "inst", "schema")
SUPPORTED = {"$ref", "type", "const", "enum", "properties", "required", "additionalProperties", "propertyNames",
             "minProperties", "maxProperties", "items", "minItems", "maxItems", "uniqueItems", "oneOf", "anyOf",
             "allOf", "not", "if", "then", "else", "pattern", "minLength", "maxLength", "minimum", "maximum",
             "exclusiveMinimum", "exclusiveMaximum",
             "$schema", "$id", "$comment", "title", "description", "default", "examples", "$defs", "deprecated",
             "readOnly", "writeOnly"}
failures = []
def check(cond, msg):
    if not cond:
        failures.append(msg)

def load(path):
    with open(path, encoding="utf-8") as f:
        return json.load(f)

def lint(node, path="#"):
    if isinstance(node, bool):
        return
    if not isinstance(node, dict):
        return
    for k, v in node.items():
        check(k in SUPPORTED, f"unsupported keyword {k} at {path}")
        if k in ("properties", "$defs"):
            for n, sub in v.items(): lint(sub, f"{path}/{k}/{n}")
        elif k in ("oneOf", "anyOf", "allOf"):
            for i, sub in enumerate(v): lint(sub, f"{path}/{k}/{i}")
        elif k in ("propertyNames", "additionalProperties", "items", "not", "if", "then", "else"):
            lint(v, f"{path}/{k}")

schemas = {}
for name in ("plan-v2", "ir-v2", "mutate-v1"):
    sch = load(os.path.join(SCHEMA, name + ".json"))
    Draft202012Validator.check_schema(sch)
    lint(sch)
    schemas[name] = Draft202012Validator(sch)

for ex in ("plan-v2-example-A.json", "plan-v2-example-B.json"):
    inst = load(os.path.join(SCHEMA, "examples", ex))
    for name in ("plan-v2", "ir-v2"):
        check(schemas[name].is_valid(inst), f"{ex} does not validate under {name}")

# --- probes ---------------------------------------------------------------
class DuplicateKey(Exception):
    pass
def no_duplicates(pairs):
    seen = set()
    for k, _ in pairs:
        if k in seen:
            raise DuplicateKey(k)
        seen.add(k)
    return dict(pairs)
def strict_parse(text):
    """Parse a JSON text refusing duplicate members and non-finite numbers."""
    def bad_constant(name):
        raise ValueError(f"non-finite constant {name}")
    obj = json.loads(text, object_pairs_hook=no_duplicates, parse_constant=bad_constant)
    def walk(o):
        if isinstance(o, float) and (o != o or o in (float("inf"), float("-inf"))):
            raise ValueError("non-finite number")
        if isinstance(o, dict):
            for v in o.values(): walk(v)
        if isinstance(o, list):
            for v in o: walk(v)
    walk(obj)
    return obj

n_probes = 0
for fn in ("plan-v2-probes.json", "mutate-v1-probes.json"):
    for p in load(os.path.join(SCHEMA, "probes", fn)):
        n_probes += 1
        expect_valid = p["expect"] == "valid"
        if p.get("instance") is None and "instance_text" in p:
            try:
                inst = strict_parse(p["instance_text"])
                valid = schemas[p["schema"]].is_valid(inst)
            except (DuplicateKey, ValueError):
                valid = False
        else:
            valid = schemas[p["schema"]].is_valid(p["instance"])
        check(valid == expect_valid, f"probe {p['id']} ({p['label']}): expected {p['expect']}")
for p in load(os.path.join(SCHEMA, "probes", "plan-v1-probes.json")):
    n_probes += 1
    check(p["schema"] == "v1-validator" and p["owner"] == "WS-2" and p["expect"] in ("valid", "invalid"), f"v1 probe {p['id']} malformed")

# --- acceptance plans ---------------------------------------------------------
corpus = os.path.join(S, "inst", "plans", "acceptance")
n_plans = 0
for d in sorted(os.listdir(corpus)):
    path = os.path.join(corpus, d, "plan.json")
    if not os.path.exists(path):
        continue
    n_plans += 1
    plan = load(path)
    check(schemas["plan-v2"].is_valid(plan), f"acceptance plan {d} does not validate under plan-v2")
    check(schemas["ir-v2"].is_valid(plan), f"acceptance plan {d} does not validate under ir-v2")
    var_dir = os.path.join(corpus, d, "variants")
    if os.path.isdir(var_dir):
        manifest = load(os.path.join(var_dir, "variants.json"))
        check(len(manifest) == 12, f"{d}: twelve parametrised variants expected")
        for m in manifest:
            vplan = load(os.path.join(var_dir, m["bound_plan"]))
            check(schemas["plan-v2"].is_valid(vplan), f"variant {d}/{m['bound_plan']} does not validate")
            base = load(os.path.join(corpus, m["equals_plan_of"], "plan.json"))
            check(vplan == base, f"variant {d}/{m['bound_plan']} differs from {m['equals_plan_of']}/plan.json")
            check(os.path.exists(os.path.join(var_dir, m["variant"])), f"variant recipe {m['variant']} missing")

# --- digest vectors (independent canonicaliser) --------------------------------------
import importlib.util
spec = importlib.util.spec_from_file_location("gen", os.path.join(S, "tools", "ws0", "jcs_reference.py"))
gen = importlib.util.module_from_spec(spec); spec.loader.exec_module(gen)
vec = load(os.path.join(SCHEMA, "digest-vectors.json"))
by = {}
for v in vec["vectors"]:
    payload, h = gen.digest(v["instance"])
    check(payload == v["payload"], f"digest vector {v['label']}: payload differs")
    check(h == v["payload_sha256"], f"digest vector {v['label']}: sha256 differs")
    by[v["label"]] = h
check(by["base"] != by["semantic_version_change"], "semantic version change must change the digest")
check(by["base"] == by["result_symbol_change"], "result_symbol change must not change the digest")
check(by["base"] == by["population_label_text_change"], "population label text must not change the digest")
for a, b in (("event_named_description_mi", "event_named_description_t2dm"), ("event_named_label_mi", "event_named_label_t2dm"),
             ("event_named_result_symbol_mi", "event_named_result_symbol_t2dm")):
    check(by[a] != by[b], f"{a}/{b}: a definition named like an advisory field must stay in the payload")
# ECMAScript number corpus (RFC 8785 Appendix B plus the review's counterexamples)
import struct
def dbl(hexstr):
    return struct.unpack(">d", bytes.fromhex(hexstr))[0]
NUMBERS = [("0000000000000000", "0"), ("8000000000000000", "0"), ("0000000000000001", "5e-324"), ("8000000000000001", "-5e-324"),
           ("7fefffffffffffff", "1.7976931348623157e+308"), ("ffefffffffffffff", "-1.7976931348623157e+308"),
           ("4340000000000000", "9007199254740992"), ("c340000000000000", "-9007199254740992"),
           ("4430000000000000", "295147905179352830000"), ("44b52d02c7e14af5", "9.999999999999997e+22"),
           ("44b52d02c7e14af6", "1e+23"), ("44b52d02c7e14af7", "1.0000000000000001e+23"),
           ("444b1ae4d6e2ef4e", "999999999999999700000"), ("444b1ae4d6e2ef4f", "999999999999999900000"),
           ("444b1ae4d6e2ef50", "1e+21"), ("3eb0c6f7a0b5ed8c", "9.999999999999997e-7"), ("3eb0c6f7a0b5ed8d", "0.000001"),
           ("41b3de4355555553", "333333333.3333332"), ("41b3de4355555554", "333333333.33333325"),
           ("41b3de4355555555", "333333333.3333333"), ("41b3de4355555556", "333333333.3333334"),
           ("41b3de4355555557", "333333333.33333343"), ("becbf647612f3696", "-0.0000033333333333333333"),
           ("43143ff3c1cb0959", "1424953923781206.2"), ("e17cd1f192002619", "-4.051847215643804e+161")]
for hx, want in NUMBERS:
    got = gen.es6_number(dbl(hx))
    check(got == want, f"number {hx}: got {got}, want {want}")
for x, want in ((1e-7, "1e-7"), (1000000000000000100.0, "1000000000000000100"), (1.2345678901234568e18, "1234567890123456800"),
                (0.0915, "0.0915"), (2.15, "2.15"), (365.0, "365"), (1e21, "1e+21"), (0.000001, "0.000001"), (100.0, "100")):
    check(gen.es6_number(x) == want, f"number {x!r}: got {gen.es6_number(x)}, want {want}")

# --- checksums ----------------------------------------------------------------------
def check_sums(dirname, label):
    sums = os.path.join(dirname, "SHA256SUMS")
    for line in open(sums):
        h, rel = line.split()
        path = os.path.join(dirname, rel)
        check(os.path.exists(path), f"{label}: missing {rel}")
        if os.path.exists(path):
            check(hashlib.sha256(open(path, "rb").read()).hexdigest() == h, f"{label}: checksum mismatch {rel}")
check_sums(SCHEMA, "server")
if C:
    CS = os.path.join(C, "inst", "schema")
    check_sums(CS, "client")
    for line in open(os.path.join(CS, "SHA256SUMS")):
        h, rel = line.split()
        check(open(os.path.join(SCHEMA, rel), "rb").read() == open(os.path.join(CS, rel), "rb").read(), f"client copy differs: {rel}")
    for f in ("json_schema.R", "semantic_digest.R"):
        check(open(os.path.join(S, "R", f), "rb").read() == open(os.path.join(C, "R", f), "rb").read(), f"vendored R file differs: {f}")
    check(open(os.path.join(S, "inst", "column_classes.csv"), "rb").read() == open(os.path.join(C, "inst", "column_classes.csv"), "rb").read(),
          "column corpus differs between packages")

print(f"checked 3 schemas, 2 examples, {n_probes} probes, {n_plans} acceptance plans, {len(vec['vectors'])} digest vectors, {len(NUMBERS) + 9} numbers")
if failures:
    print("FAILURES:"); [print(" -", f) for f in failures]
    sys.exit(1)
print("all contract checks passed")
