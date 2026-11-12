"""Reference normaliser of a version-2 plan (amendment A-11 to design-2.10.3).

The normal form is what the semantic digest hashes and what explain renders.
Rules, applied in this order and nowhere else:
  1. an absent root `options` is `{}`; absent registries (`concept_sets`,
     `events`, `intervals`, `populations`, `variables`) are `{}`;
  2. the `default` keywords of plan-v2.json are applied in the schema branch
     each instance matches (properties, additionalProperties, items, allOf,
     the first matching oneOf/anyOf branch, if/then/else);
  3. an absent `publish` is the output names in declaration order;
  4. an `index` or `criteria` population without `coverage` takes
     `options.coverage`, else {unique, drop, drop, 0}, with on_missing = "keep"
     when its `index.restrict_to_observation` is false;
  5. an output without `grain` takes "episode" when its rows population is
     indexed (kind index or match; a criteria population inherits from its
     parent; a union is indexed if any member is; intersect and setdiff
     inherit from their first member) and "person" otherwise;
  6. a `wide` output with `columns: null` takes every variable in declaration
     order;
  7. a `survival` output without `event_order` takes "first" for survival and
     competing_risk and "all" otherwise, and without `outcome_mode` takes
     "composite" for survival and "named" otherwise.
No other field is added and none is removed. Optional fields with neither a
schema default nor a semantic default stay absent. Wire validation precedes
normalisation; invalid input is never repaired. R/plan_normalize_v2.R is the
byte-identical R implementation vendored in both packages.
"""
import copy, json, sys
from jsonschema import Draft202012Validator

DEFAULT_COVERAGE = {"policy": "unique", "on_ambiguous": "drop", "on_missing": "drop", "gap": 0}
REGISTRIES = ("concept_sets", "events", "intervals", "populations", "variables")


def resolve_ref(ref, root):
    if not ref.startswith("#/"):
        raise ValueError("only local references are supported: %s" % ref)
    node = root
    for part in ref[2:].split("/"):
        node = node[part.replace("~1", "/").replace("~0", "~")]
    return node


def _valid(branch, inst, root):
    if isinstance(branch, bool):
        return branch
    sub = dict(branch)
    sub["$defs"] = root["$defs"]
    return Draft202012Validator(sub).is_valid(inst)


def apply_defaults(schema, x, root):
    """Add every absent defaulted property of the branch `x` matches (recursively)."""
    if not isinstance(schema, dict):
        return x
    if "$ref" in schema:
        x = apply_defaults(resolve_ref(schema["$ref"], root), x, root)
    if isinstance(x, dict):
        props = schema.get("properties") or {}
        for name, sub in props.items():
            if name not in x and isinstance(sub, dict) and "default" in sub:
                x[name] = copy.deepcopy(sub["default"])
        for name in list(x):
            if name in props:
                x[name] = apply_defaults(props[name], x[name], root)
        extra = schema.get("additionalProperties")
        if isinstance(extra, dict):
            for name in list(x):
                if name not in props:
                    x[name] = apply_defaults(extra, x[name], root)
    if isinstance(x, list) and "items" in schema:
        x = [apply_defaults(schema["items"], item, root) for item in x]
    for sub in schema.get("allOf", []):
        x = apply_defaults(sub, x, root)
    for key in ("oneOf", "anyOf"):
        branches = schema.get(key, [])
        if branches:
            hits = [b for b in branches if _valid(b, x, root)]
            if hits:
                x = apply_defaults(hits[0], x, root)
    if "if" in schema:
        branch = "then" if _valid(schema["if"], x, root) else "else"
        if branch in schema:
            x = apply_defaults(schema[branch], x, root)
    return x


def population_indexed(populations, pid, seen=()):
    p = populations.get(pid)
    if not isinstance(p, dict) or pid in seen:
        return False
    kind = p.get("kind")
    if kind in ("index", "match"):
        return True
    seen = seen + (pid,)
    if kind == "setop":
        setop = p.get("setop") or {}
        members = list(setop.get("members") or [])
        if not members:
            return False
        if setop.get("op") == "union":
            return any(population_indexed(populations, m, seen) for m in members)
        return population_indexed(populations, members[0], seen)
    if kind == "criteria" and p.get("parent") is not None:
        return population_indexed(populations, p["parent"], seen)
    return False


def default_event_order(fmt):
    return "all" if fmt in ("recurrent_events", "counting_process", "multi_state") else "first"


def normalize(plan, schema):
    x = copy.deepcopy(plan)
    if not isinstance(x, dict) or x.get("version") != 2:
        raise ValueError("only version 2 plans are normalised")
    if x.get("options") is None:
        x["options"] = {}
    for slot in REGISTRIES:
        if x.get(slot) is None:
            x[slot] = {}
    x = apply_defaults(schema, x, schema)
    if x.get("publish") is None:
        x["publish"] = list(x["outputs"].keys())
    plan_cov = x["options"].get("coverage")
    for pid, p in x["populations"].items():
        if p.get("kind") in ("index", "criteria") and p.get("coverage") is None:
            cov = copy.deepcopy(plan_cov) if plan_cov is not None else dict(DEFAULT_COVERAGE)
            if p["kind"] == "index" and p.get("index", {}).get("restrict_to_observation") is False:
                cov["on_missing"] = "keep"
            p["coverage"] = cov
    for name, o in x["outputs"].items():
        if o.get("grain") is None:
            o["grain"] = "episode" if population_indexed(x["populations"], o.get("rows")) else "person"
        if o.get("format") == "wide" and o.get("columns") is None:
            o["columns"] = list(x["variables"].keys())
        if o.get("format") == "survival":
            opts = o.setdefault("options", {})
            fmt = opts.get("format", "survival")
            if opts.get("event_order") is None:
                opts["event_order"] = default_event_order(fmt)
            if opts.get("outcome_mode") is None:
                opts["outcome_mode"] = "composite" if fmt == "survival" else "named"
    return x


if __name__ == "__main__":
    schema = json.load(open(sys.argv[1]))
    plan = json.load(open(sys.argv[2]))
    json.dump(normalize(plan, schema), sys.stdout, indent=2, sort_keys=True)
    print()
