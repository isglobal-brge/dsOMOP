#!/usr/bin/env python3
"""WS-0 generator: schema artefacts of dsOMOP 2.10.3 from the endorsed design.

Reads design-2.10.3.endorsed.md (SHA-256 0988b2da...7940), extracts the normative
JSON blocks of section 4 and writes, under inst/schema/:
  plan-v2.json            section 4.1 verbatim (wire schema)
  ir-v2.json              section 4.5: wire schema + closed compatibility variants
  mutate-v1.json          section 4.6 with the predicate grammar copied in
  examples/*.json         canonical examples A and B (sections 4.2, 4.3)
  probes/*.json           the probe corpus (expected verdict per instance)
  digest-vectors.json     the three semantic-digest vectors (section 4.4)
  SHA256SUMS              checksums of every file above
and validates everything with the jsonschema package (2020-12).

Usage: PYTHONPATH=<dir with jsonschema> python3 tools/ws0/gen_schema_artefacts.py DESIGN.md OUT_DIR
"""
import copy, hashlib, json, math, os, re, sys

DESIGN, OUT = sys.argv[1], sys.argv[2]
txt = open(DESIGN, encoding="utf-8").read()
blocks = [json.loads(b) for b in re.findall(r"```json\n(.*?)```", txt, re.S)]
by_id = {b.get("$id"): b for b in blocks if "$id" in b}
plan = by_id["https://isglobal-brge.github.io/dsOMOP/schema/plan-v2.json"]
mutate = by_id["https://isglobal-brge.github.io/dsOMOP/schema/mutate-v1.json"]
examples = [b for b in blocks if b.get("version") == 2]
assert len(examples) == 2, "expected the two canonical examples"
example_a, example_b = examples

def dump(obj, path):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        json.dump(obj, f, indent=2, ensure_ascii=False)
        f.write("\n")

# --------------------------------------------------------------------------
# 1. wire schema, verbatim
# --------------------------------------------------------------------------
dump(plan, os.path.join(OUT, "plan-v2.json"))

# --------------------------------------------------------------------------
# 2. compatibility IR: wire schema + closed legacy variants (section 4.5)
# --------------------------------------------------------------------------
ir = copy.deepcopy(plan)
ir["$id"] = "https://isglobal-brge.github.io/dsOMOP/schema/ir-v2.json"
ir["title"] = "dsOMOP internal compatibility IR, language 2.0 (wire schema plus closed legacy variants; never on the wire)"
D = ir["$defs"]
D["legacy_window"] = {
    "type": "object", "additionalProperties": False,
    "properties": {"start": {"type": "integer"}, "end": {"type": "integer"}},
    "anyOf": [{"required": ["start"]}, {"required": ["end"]}]
}
D["legacy_nullable_date"] = {"oneOf": [{"type": "null"}, {"$ref": "#/$defs/date"}]}
D["legacy_concept_params"] = {
    "concept_id": {"$ref": "#/$defs/concept_id"},
    "concept_ids": {"type": "array", "minItems": 1, "items": {"$ref": "#/$defs/concept_id"}}
}
def leaf(type_name, params, required=()):
    props = dict(params)
    props["reference_date"] = {"$ref": "#/$defs/legacy_nullable_date"}
    return {
        "type": "object", "additionalProperties": False, "required": ["type", "params"],
        "properties": {
            "type": {"const": type_name},
            "params": {"type": "object", "additionalProperties": False,
                       "required": list(required), "properties": props}
        }
    }
concept_scoped = dict(D["legacy_concept_params"])
concept_scoped.update({"table": {"$ref": "#/$defs/table_name"}, "window": {"$ref": "#/$defs/legacy_window"}})
with_count = dict(concept_scoped); with_count["min_count"] = {"type": "integer", "minimum": 0}
D["legacy_leaf"] = {"oneOf": [
    leaf("sex", {"value": {"type": "string", "minLength": 1}}, ["value"]),
    leaf("age_range", {"min": {"$ref": "#/$defs/nullable_int"}, "max": {"$ref": "#/$defs/nullable_int"}}),
    leaf("age_group", {"groups": {"type": "array", "minItems": 1,
                                   "items": {"type": "string", "pattern": "^[0-9]+(-[0-9]+|\\+)$"}}}, ["groups"]),
    leaf("cohort", {"cohort_definition_id": {"type": "integer", "minimum": 0}}, ["cohort_definition_id"]),
    leaf("has_concept", with_count),
    leaf("not_has_concept", concept_scoped),
    leaf("concept_count", with_count),
    leaf("prior_observation", {"min_days": {"type": "integer", "minimum": 0}}),
    leaf("followup", {"min_days": {"type": "integer", "minimum": 0}}),
    leaf("visit_count", {"visit_concept_id": {"$ref": "#/$defs/concept_id"},
                         "visit_concept_ids": {"type": "array", "minItems": 1, "items": {"$ref": "#/$defs/concept_id"}},
                         "min_count": {"type": "integer", "minimum": 0},
                         "window": {"$ref": "#/$defs/legacy_window"}}),
    leaf("has_measurement", dict(concept_scoped, **{
        "min_value": {"type": "number"}, "max_value": {"type": "number"},
        "safe_scope": {"type": "object", "additionalProperties": False,
                       "properties": {"concept_id": {"oneOf": [{"$ref": "#/$defs/concept_id"},
                                                               {"type": "array", "items": {"$ref": "#/$defs/concept_id"}}]},
                                      "concept_col": {"$ref": "#/$defs/column_name"}}}})),
    leaf("missing_measurement", concept_scoped),
]}
D["criteria"]["oneOf"].append({
    "type": "object", "additionalProperties": False, "required": ["legacy"],
    "properties": {"legacy": {"$ref": "#/$defs/legacy_leaf"}}
})
D["legacy_inclusion_criterion"] = {
    "type": "object", "additionalProperties": False, "required": ["table"],
    "properties": {
        "table": {"$ref": "#/$defs/table_name"},
        "concept_set": {"$ref": "#/$defs/concept_set_ref"},
        "temporal": {"type": "object", "additionalProperties": False,
                     "properties": {"index_window": {"$ref": "#/$defs/legacy_window"},
                                    "calendar": {"type": "object", "additionalProperties": False,
                                                 "properties": {"start": {"$ref": "#/$defs/date"}, "end": {"$ref": "#/$defs/date"}}}}},
        "occurrence": {"type": "object", "additionalProperties": False,
                       "properties": {"min": {"type": "integer", "minimum": 0}, "max": {"type": "integer", "minimum": 0}}}
    }
}
D["legacy_spec"] = {
    "type": "object", "additionalProperties": False,
    "properties": {
        "type": {"enum": ["condition", "drug", "measurement", "observation", "procedure"], "default": "condition"},
        "concept_set": {"$ref": "#/$defs/concept_set_ref"},
        "value_bin": {"type": "object", "additionalProperties": False, "required": ["lower", "upper", "safe_scope"],
                      "properties": {"lower": {"type": "number"}, "upper": {"type": "number"},
                                     "safe_scope": {"type": "object"}}},
        "inclusion_criteria": {"type": "object", "propertyNames": {"$ref": "#/$defs/name"},
                               "additionalProperties": {"$ref": "#/$defs/legacy_inclusion_criterion"}}
    }
}
src = D["index"]["properties"]["source"]["oneOf"]
src.append({"type": "object", "additionalProperties": False, "required": ["legacy_spec"],
            "properties": {"legacy_spec": {"$ref": "#/$defs/legacy_spec"}}})
D["index"]["properties"]["legacy_end_strategy"] = {
    "oneOf": [{"type": "null"},
              {"type": "object", "additionalProperties": False, "required": ["DateOffset"],
               "properties": {"DateOffset": {"type": "object", "additionalProperties": False, "required": ["DateField", "Offset"],
                                             "properties": {"DateField": {"enum": ["StartDate", "EndDate"]},
                                                            "Offset": {"type": "integer"}}}}}],
    "default": None
}
D["options_survival"]["properties"]["legacy_cohort_end_cap"] = {"type": "boolean", "default": False}
for opt in ("options_person_period", "options_temporal"):
    D[opt]["properties"]["legacy_composite"] = {"type": "boolean", "default": False}
    D[opt]["properties"]["legacy_covariate_ids"] = {"type": "boolean", "default": False}
D["output"]["properties"]["legacy_split"] = {"type": "boolean", "default": False}
dump(ir, os.path.join(OUT, "ir-v2.json"))

# --------------------------------------------------------------------------
# 3. mutation schema with the predicate grammar copied in
# --------------------------------------------------------------------------
mut = copy.deepcopy(mutate)
md = mut["$defs"]
for k in ("predicate", "predicate_leaf", "scalar", "column_name"):
    md[k] = copy.deepcopy(plan["$defs"][k])
# an assigned column may be referenced by later expressions: widen `var` to the assigned-name pattern
assigned = {"type": "string", "pattern": "^[A-Za-z][A-Za-z0-9_]{0,63}$"}
for branch in md["predicate_leaf"]["oneOf"]:
    branch["properties"]["var"] = copy.deepcopy(assigned)
md["predicate"]["$comment"] = ("copied from plan-v2.json#/$defs/predicate by tools/ws0/gen_schema_artefacts.py; "
                               "`var` additionally accepts an earlier assigned column name")
dump(mut, os.path.join(OUT, "mutate-v1.json"))

# --------------------------------------------------------------------------
# 4. examples
# --------------------------------------------------------------------------
dump(example_a, os.path.join(OUT, "examples", "plan-v2-example-A.json"))
dump(example_b, os.path.join(OUT, "examples", "plan-v2-example-B.json"))

# --------------------------------------------------------------------------
# 5. probes (materialised instances with expected verdicts)
# --------------------------------------------------------------------------
A, B = example_a, example_b
probes = []
def probe(pid, label, expect, base, mutator, schema="plan-v2"):
    inst = copy.deepcopy(base); mutator(inst)
    probes.append({"id": pid, "label": label, "schema": schema, "expect": expect, "instance": inst})
R, OK = "invalid", "valid"
probe("P01", "canonical example A", OK, A, lambda b: None)
probe("P02", "canonical example B", OK, B, lambda b: None)
probe("P03", "forged roster field on an output", R, A, lambda b: b["outputs"]["baseline"].__setitem__("roster", "new_users"))
probe("P04", "mixed population shape (index + setop)", R, A, lambda b: b["populations"]["new_users"].__setitem__("setop", {"op": "union", "members": ["a", "b"]}))
probe("P05", "unknown survival option", R, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("censoring", {"death": True}))
probe("P06", "survival without censor", R, A, lambda b: b["outputs"]["tte"]["options"].pop("censor"))
probe("P07", "survival without options", R, A, lambda b: b["outputs"]["tte"].pop("options"))
probe("P08", "negative matching seed", R, A, lambda b: b["populations"].__setitem__("cc", {"kind": "match", "match": {"cases": "new_users", "controls": "new_users", "outcome": {"event": "mi"}, "seed": -1}}))
probe("P09", "legacy criteria leaf on the wire", R, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"legacy": {"type": "sex", "params": {"value": "F"}}}))
probe("P10", "legacy_spec index source on the wire", R, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("source", {"legacy_spec": {"type": "condition"}}))
probe("P11", "legacy_end_strategy on the wire", R, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("legacy_end_strategy", {"DateOffset": {"DateField": "StartDate", "Offset": 1}}))
probe("P12", "legacy_cohort_end_cap on the wire", R, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("legacy_cohort_end_cap", True))
probe("P13", "array operand on a scalar comparison", R, A, lambda b: b["events"]["hba1c"].__setitem__("where", {"var": "value", "op": ">", "value": [1, 2]}))
probe("P14", "comparison without a value", R, A, lambda b: b["events"]["hba1c"].__setitem__("where", {"var": "value", "op": ">"}))
probe("P15", "null test with an operand", R, A, lambda b: b["events"]["hba1c"].__setitem__("where", {"var": "value", "op": "is_null", "value": 1}))
probe("P16", "has(between) with a scalar n", R, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"has": {"source": {"event": "mi"}, "op": "between", "n": 2}}))
probe("P17", "has(>=) with an array n", R, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"has": {"source": {"event": "mi"}, "op": ">=", "n": [1, 2]}}))
probe("P18", "binned date handling without bin_width", R, A, lambda b: b["outputs"]["baseline"]["options"].__setitem__("date_handling", {"mode": "binned"}))
probe("P19", "bin_width with relative mode", R, A, lambda b: b["outputs"]["baseline"]["options"].__setitem__("date_handling", {"mode": "relative", "bin_width": "week"}))
probe("P20", "string date_handling shortcut on the wire", R, A, lambda b: b["outputs"]["baseline"]["options"].__setitem__("date_handling", "relative"))
probe("P21", "obs_duration with an unknown period policy", R, A, lambda b: b["variables"].__setitem__("od", {"derived": {"kind": "obs_duration", "period_policy": "index_period"}}))
probe("P22", "prior_obs without reference_date", R, A, lambda b: b["variables"].__setitem__("po", {"derived": {"kind": "prior_obs"}}))
probe("P23", "age_breaks with one bound", R, A, lambda b: b["variables"].__setitem__("ag", {"derived": {"kind": "age", "bands": True, "age_breaks": [0]}}))
probe("P24", "has(between) with two bounds", OK, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"has": {"source": {"event": "mi"}, "op": "between", "n": [1, 3]}}))
probe("P25", "binned date handling by month", OK, A, lambda b: b["outputs"]["baseline"]["options"].__setitem__("date_handling", {"mode": "binned", "bin_width": "month"}))
probe("P26", "obs_duration longest", OK, A, lambda b: b["variables"].__setitem__("od", {"derived": {"kind": "obs_duration", "period_policy": "longest"}}))
probe("P27", "age with off-grid breaks", OK, A, lambda b: b["variables"].__setitem__("ag", {"derived": {"kind": "age", "bands": True, "age_breaks": [0, 18, 19, 65]}}))
probe("P28", "observed gap explicit", OK, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"observed": {"before": 30, "gap": 7}}))
probe("P29", "episode_members event", OK, A, lambda b: b["events"].__setitem__("members", {"episode_members": {"episode": {"event": "mi"}, "tables": ["drug_exposure"]}}))
probe("P30", "cost event", OK, A, lambda b: b["events"].__setitem__("costs", {"cost": {"event": {"event": "mi"}, "field": "total_cost", "currency": 44818568}}))
probe("P31", "supply_days reducer with clipped fills", OK, A, lambda b: b["variables"].__setitem__("sd", {"source": {"event": "metf_rx"}, "reducer": {"fn": "supply_days", "fills": "clipped", "window": {"start": 1, "end": 365}}}))
probe("P32", "covered_days with effective supply", OK, A, lambda b: b["variables"].__setitem__("cd", {"source": {"interval": "metf_era"}, "reducer": {"fn": "covered_days", "coverage": "effective_supply", "window": {"start": 1, "end": 365}}}))
probe("P33", "censor object with which omitted", OK, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("censor", ["observation_end", {"censor": {"source": {"interval": "metf_era"}}}]))
probe("P34", "covered_days observed_only", OK, A, lambda b: b["variables"].__setitem__("cdo", {"source": {"interval": "metf_era"}, "reducer": {"fn": "covered_days", "coverage": "effective_supply", "observed_only": True, "window": {"start": 1, "end": 365}}}))
probe("P35", "observed_only on count", R, A, lambda b: b["variables"].__setitem__("c1", {"source": {"event": "mi"}, "reducer": {"fn": "count", "observed_only": True}}))
probe("P36", "coverage on count", R, A, lambda b: b["variables"].__setitem__("c2", {"source": {"event": "mi"}, "reducer": {"fn": "count", "coverage": "effective_supply"}}))
probe("P37", "unknown root field", R, A, lambda b: b.__setitem__("already_gated", True))
probe("P38", "unknown field inside a dictionary output", R, A, lambda b: b["outputs"].__setitem__("dict", {"rows": "new_users", "format": "dictionary", "already_gated": True}))
probe("P39", "publish naming (schema-level) accepted", OK, A, lambda b: b.__setitem__("publish", ["baseline"]))
probe("P40", "episode_members with an unknown field", R, A, lambda b: b["events"].__setitem__("members", {"episode_members": {"episode": {"event": "mi"}, "join": "free"}}))
probe("P41", "cost without field", R, A, lambda b: b["events"].__setitem__("costs", {"cost": {"event": {"event": "mi"}}}))
probe("P42", "wire plan carrying a legacy_split flag", R, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_split", True))
# IR probes: the same compatibility variants are valid under ir-v2.json
def ir_probe(pid, label, expect, base, mutator):
    probe(pid, label, expect, base, mutator, schema="ir-v2")
ir_probe("I01", "legacy sex leaf in the IR", OK, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"legacy": {"type": "sex", "params": {"value": "F"}}}))
ir_probe("I02", "legacy has_concept leaf with window in the IR", OK, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"legacy": {"type": "has_concept", "params": {"concept_ids": [201820], "table": "condition_occurrence", "window": {"start": -365, "end": 0}, "min_count": 1}}}))
ir_probe("I03", "legacy leaf with an unknown param", R, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"legacy": {"type": "sex", "params": {"value": "F", "already_gated": True}}}))
ir_probe("I04", "legacy leaf with an unknown type", R, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"legacy": {"type": "condition", "params": {}}}))
ir_probe("I05", "legacy_spec source in the IR", OK, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("source", {"legacy_spec": {"type": "drug", "concept_set": {"concepts": [1503297], "include_descendants": True}, "inclusion_criteria": {"dm": {"table": "condition_occurrence", "concept_set": {"concepts": [201820]}, "temporal": {"index_window": {"start": -365, "end": 0}}, "occurrence": {"min": 1}}}}}))
ir_probe("I06", "legacy_spec with value_threshold (not executable since 2.9)", R, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("source", {"legacy_spec": {"type": "measurement", "value_threshold": {"op": ">", "value": 7}}}))
ir_probe("I07", "legacy_end_strategy DateOffset in the IR", OK, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("legacy_end_strategy", {"DateOffset": {"DateField": "EndDate", "Offset": 30}}))
ir_probe("I08", "legacy_cohort_end_cap in the IR", OK, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("legacy_cohort_end_cap", True))
ir_probe("I09", "forged roster field still refused by the IR", R, A, lambda b: b["outputs"]["baseline"].__setitem__("roster", "new_users"))
dump(probes, os.path.join(OUT, "probes", "plan-v2-probes.json"))

mreq = {"version": 1, "assign": [
    {"name": "pdc", "expr": {"op": "/", "args": [{"col": "covered"}, {"col": "observed"}]}},
    {"name": "band", "expr": {"cut": {"arg": {"col": "age"}, "breaks": [0, 18, 65, 120]}}},
    {"name": "flag", "expr": {"ifelse": {"test": {"var": "pdc", "op": ">=", "value": 0.8}, "yes": {"lit": 1}, "no": {"lit": 0}}}}]}
mprobes = [
    {"id": "M01", "label": "mutation request with arithmetic, cut and ifelse over an assigned column", "schema": "mutate-v1", "expect": OK, "instance": mreq},
    {"id": "M02", "label": "free-form expression rejected", "schema": "mutate-v1", "expect": R,
     "instance": {"version": 1, "assign": [{"name": "x", "expr": {"fn": "eval", "code": "system('x')"}}]}},
    {"id": "M03", "label": "unknown top-level field rejected", "schema": "mutate-v1", "expect": R,
     "instance": {"version": 1, "assign": [{"name": "x", "expr": {"lit": 1}}], "already_gated": True}},
    {"id": "M04", "label": "case_when with coalesce and typed literal", "schema": "mutate-v1", "expect": OK,
     "instance": {"version": 1, "assign": [{"name": "grp", "expr": {"case_when": {"cases": [{"when": {"var": "age", "op": "<", "value": 18}, "then": {"lit": "child"}}], "else": {"coalesce": [{"col": "label"}, {"lit": "adult", "as": "text"}]}}}}]}},
    {"id": "M05", "label": "arithmetic with three operands rejected", "schema": "mutate-v1", "expect": R,
     "instance": {"version": 1, "assign": [{"name": "x", "expr": {"op": "+", "args": [{"col": "a"}, {"col": "b"}, {"col": "c"}]}}]}},
]
dump(mprobes, os.path.join(OUT, "probes", "mutate-v1-probes.json"))

# --------------------------------------------------------------------------
# 6. semantic digest vectors (RFC 8785 canonicalisation; section 4.4)
# --------------------------------------------------------------------------
def jcs_number(x):
    if isinstance(x, bool):
        raise TypeError
    if isinstance(x, int):
        return str(x)
    if x != x or x in (float("inf"), float("-inf")):
        raise ValueError("non-finite number")
    if x == 0:
        return "0"
    if x == int(x) and abs(x) < 1e21:
        return str(int(x))
    r = repr(x)  # shortest round-trip
    if "e" in r or "E" in r:
        mant, exp = r.lower().split("e"); exp = int(exp)
        digits = mant.replace(".", "").replace("-", "")
        sign = "-" if x < 0 else ""
        k = len(digits); n = exp + (len(mant.replace("-", "").split(".")[0]))
        if 1e-7 <= abs(x) < 1e21:
            if n <= 0:
                return sign + "0." + "0" * (-n) + digits
            if n >= k:
                return sign + digits + "0" * (n - k)
            return sign + digits[:n] + "." + digits[n:]
        m = digits[0] + ("." + digits[1:] if k > 1 else "")
        return sign + m + "e" + ("+" if n - 1 > 0 else "-") + str(abs(n - 1))
    return r
def jcs(obj):
    if obj is None: return "null"
    if obj is True: return "true"
    if obj is False: return "false"
    if isinstance(obj, (int, float)): return jcs_number(obj)
    if isinstance(obj, str): return json.dumps(obj, ensure_ascii=False)
    if isinstance(obj, list): return "[" + ",".join(jcs(v) for v in obj) + "]"
    if isinstance(obj, dict):
        keys = sorted(obj.keys(), key=lambda k: k.encode("utf-16-be"))
        return "{" + ",".join(json.dumps(k, ensure_ascii=False) + ":" + jcs(obj[k]) for k in keys) + "}"
    raise TypeError(type(obj))
ADVISORY_META = ("generated_by", "client_version", "recipe_hash")
ADVISORY_KEYS = ("label", "description", "result_symbol")
def digest_payload(ir_obj):
    p = copy.deepcopy(ir_obj)
    meta = p.get("meta", {})
    p["meta"] = {k: v for k, v in meta.items() if k not in ADVISORY_META}
    def strip(o):
        if isinstance(o, dict):
            return {k: strip(v) for k, v in o.items() if k not in ADVISORY_KEYS}
        if isinstance(o, list):
            return [strip(v) for v in o]
        return o
    return strip(p)
def digest(ir_obj):
    payload = jcs(digest_payload(ir_obj))
    return payload, hashlib.sha256(payload.encode("utf-8")).hexdigest()
v_base = copy.deepcopy(A)
v_version = copy.deepcopy(A); v_version["meta"]["semantic_version"] = "2.1.0"
v_symbol = copy.deepcopy(A); v_symbol["outputs"]["baseline"]["result_symbol"] = "D_other"
vectors = []
for label, obj, note in (("base", v_base, "canonical example A"),
                         ("semantic_version_change", v_version, "meta.semantic_version = 2.1.0: a different digest"),
                         ("result_symbol_change", v_symbol, "outputs.baseline.result_symbol = D_other: the identical digest")):
    payload, h = digest(obj)
    vectors.append({"label": label, "note": note, "instance": obj, "payload_sha256": h, "payload": payload})
assert vectors[0]["payload_sha256"] != vectors[1]["payload_sha256"]
assert vectors[0]["payload_sha256"] == vectors[2]["payload_sha256"]
dump({"canonicalisation": "RFC 8785 (JSON Canonicalization Scheme) of the normalised IR; advisory fields excluded individually: meta.generated_by, meta.client_version, meta.recipe_hash, every label, description and result_symbol; version and meta.language_version/meta.semantic_version retained",
      "vectors": vectors}, os.path.join(OUT, "digest-vectors.json"))

# --------------------------------------------------------------------------
# 7. validate everything with jsonschema (2020-12) and write SHA256SUMS
# --------------------------------------------------------------------------
from jsonschema import Draft202012Validator
Draft202012Validator.check_schema(plan); Draft202012Validator.check_schema(ir); Draft202012Validator.check_schema(mut)
validators = {"plan-v2": Draft202012Validator(plan), "ir-v2": Draft202012Validator(ir), "mutate-v1": Draft202012Validator(mut)}
bad = []
for p in probes + mprobes:
    ok = validators[p["schema"]].is_valid(p["instance"])
    if ok != (p["expect"] == OK):
        bad.append(p["id"])
assert not bad, "probe verdict mismatch: %s" % bad
assert validators["ir-v2"].is_valid(A) and validators["ir-v2"].is_valid(B)
print("validated %d plan/ir probes and %d mutate probes" % (len(probes), len(mprobes)))

sums = []
for root, _, files in os.walk(OUT):
    for fn in sorted(files):
        if fn == "SHA256SUMS": continue
        path = os.path.join(root, fn)
        rel = os.path.relpath(path, OUT)
        sums.append((hashlib.sha256(open(path, "rb").read()).hexdigest(), rel))
sums.sort(key=lambda t: t[1])
with open(os.path.join(OUT, "SHA256SUMS"), "w") as f:
    for h, rel in sums:
        f.write("%s  %s\n" % (h, rel))
print("wrote", len(sums), "files + SHA256SUMS under", OUT)
