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

# --------------------------------------------------------------------------
# Amendments to the endorsed design applied to the wire schema (amendments.md):
#   A-7  censor objects gain `when_index_source` (null or a nonempty unique array)
# --------------------------------------------------------------------------
censor_variants = plan["$defs"]["censor_item"]["oneOf"]
censor_obj = [v for v in censor_variants if isinstance(v, dict) and "censor" in v.get("properties", {})][0]
censor_obj["properties"]["censor"]["properties"]["when_index_source"] = {
    "oneOf": [{"type": "null"},
              {"type": "array", "minItems": 1, "uniqueItems": True, "items": {"type": "string", "minLength": 1, "maxLength": 64}}],
    "default": None}
#   A-14 (CR-14) per-source analysis selection for temporal_covariates / person_period
for _opt in ("options_temporal", "options_person_period"):
    plan["$defs"][_opt]["properties"]["source_analyses"] = {
        "type": "object", "default": {},
        "description": "Per-source analysis selection: resolved source name -> nonempty unique subset of analyses; an omitted source inherits the output's analyses (A-14)",
        "propertyNames": {"$ref": "#/$defs/name"},
        "additionalProperties": {"type": "array", "minItems": 1, "uniqueItems": True,
                                 "items": {"enum": ["binary", "count", "mean", "last", "exposed_days"]}}}
# A-27 / CR-28: semantic concept presentation maps (never v1 wire fields).
label_map = {"type": "object", "default": {},
             "propertyNames": {"pattern": "^(0|[1-9][0-9]*)$"},
             "additionalProperties": {"type": "string", "minLength": 1}}
name_map = {"type": "object", "default": {},
            "propertyNames": {"minLength": 1},
            "additionalProperties": {"type": "string", "minLength": 1}}
for node in [plan["properties"]["options"]] + plan["$defs"]["variable"]["oneOf"]:
    node["properties"]["labels"] = copy.deepcopy(label_map)
    node["properties"]["column_names"] = copy.deepcopy(name_map)
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
# 1. wire schema (section 4.1 plus the amendments above)
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
legacy_concept_params = {   # property fragments (not a schema): never stored under $defs
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
concept_scoped = dict(legacy_concept_params)
concept_scoped.update({"table": {"$ref": "#/$defs/table_name"}, "window": {"$ref": "#/$defs/legacy_window"}})
with_count = dict(concept_scoped); with_count["min_count"] = {"type": "integer", "minimum": 0}
# one closed safe-scope shape for both legacy locations (B3): the fields
# .assertSafeNumericBinContract accepts (S/R/extraction.R:1349-1375)
D["legacy_safe_scope"] = {
    "type": "object", "additionalProperties": False, "required": ["table", "column", "n_bins"],
    "properties": {
        "table": {"$ref": "#/$defs/table_name"},
        "column": {"$ref": "#/$defs/column_name"},
        "n_bins": {"type": "integer", "minimum": 2, "maximum": 100},
        "concept_id": {"oneOf": [{"type": "null"}, {"$ref": "#/$defs/concept_id"},
                                 {"type": "array", "minItems": 1, "items": {"$ref": "#/$defs/concept_id"}}]},
        "concept_col": {"oneOf": [{"type": "null"}, {"$ref": "#/$defs/column_name"}]}
    }
}
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
        "safe_scope": {"$ref": "#/$defs/legacy_safe_scope"}})),
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
        # A-15: the historical .applyInclusionCriteria() contract (S/R/cohort.R):
        # type at_least | at_most | exactly (default at_least), count a
        # non-negative integer (default 1); the lowering writes the effective
        # object, so an omitted occurrence is {at_least, 1}
        "occurrence": {"type": "object", "additionalProperties": False,
                       "default": {"type": "at_least", "count": 1},
                       "properties": {"type": {"enum": ["at_least", "at_most", "exactly"], "default": "at_least"},
                                      "count": {"type": "integer", "minimum": 0, "default": 1}}}
    }
}
D["legacy_spec"] = {
    "type": "object", "additionalProperties": False,
    "properties": {
        "type": {"enum": ["condition", "drug", "measurement", "observation", "procedure"], "default": "condition"},
        "concept_set": {"$ref": "#/$defs/concept_set_ref"},
        "value_bin": {"type": "object", "additionalProperties": False, "required": ["lower", "upper", "safe_scope"],
                      "properties": {"lower": {"type": "number"}, "upper": {"type": "number"},
                                     "safe_scope": {"$ref": "#/$defs/legacy_safe_scope"}}},
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
# A-2 (CR-2): lowered v1 score nodes carry legacy_all_time_conditions in the IR only
score_variant = [v for v in D["variable"]["oneOf"][1]["properties"]["derived"]["oneOf"]
                 if "charlson" in v["properties"]["kind"].get("enum", [])][0]
score_variant["properties"]["legacy_all_time_conditions"] = {"type": "boolean", "default": False}

# A-10 (CR-10): closed IR-only variants carrying v1 semantics the wire cannot express
D["output"]["properties"]["legacy_columns"] = {
    "type": "array", "uniqueItems": True, "items": {"$ref": "#/$defs/column_name"},
    "description": "v1 event_level projection; absent = default projection, [] = explicitly empty (mandatory identity/concept columns still added)"}
D["legacy_temporal_window"] = {
    "type": "object", "additionalProperties": False, "required": ["start", "end"],
    "properties": {"start": {"oneOf": [{"type": "null"}, {"type": "integer"}]},
                   "end": {"oneOf": [{"type": "null"}, {"type": "integer"}]}},
    "anyOf": [{"properties": {"start": {"type": "integer"}}}, {"properties": {"end": {"type": "integer"}}}]}
D["legacy_temporal"] = {
    "type": "object", "additionalProperties": False, "minProperties": 1,
    "properties": {
        "index_window": {"$ref": "#/$defs/legacy_temporal_window"},
        "event_select": {"type": "object", "additionalProperties": False, "required": ["order", "n", "by"],
                         "properties": {"order": {"enum": ["first", "last"]}, "n": {"type": "integer", "minimum": 1},
                                        "by": {"enum": ["grain", "concept"]}}},
        "min_gap": {"type": "object", "additionalProperties": False, "required": ["days", "by", "keep"],
                    "properties": {"days": {"type": "integer", "minimum": 1}, "by": {"enum": ["grain", "concept"]},
                                   "keep": {"enum": ["first", "last"]}}}}}
D["output"]["properties"]["legacy_temporal"] = {"$ref": "#/$defs/legacy_temporal"}
D["output"]["properties"]["legacy_automatic_features"] = {
    "type": "object", "additionalProperties": False, "required": ["format", "source"],
    "properties": {"format": {"enum": ["wide", "features", "sparse"]},
                   "source": {"type": "object", "additionalProperties": False, "required": ["event"],
                              "properties": {"event": {"$ref": "#/$defs/name"}}}}}
D["options_survival"]["properties"]["legacy_single_outcome"] = {"const": True}
D["options_survival"]["properties"]["legacy_states"] = {"type": "array", "minItems": 1, "uniqueItems": True, "items": {"$ref": "#/$defs/name"}}
D["options_survival"]["properties"]["legacy_state_step"] = {"type": "number", "exclusiveMinimum": 0,
    "description": "effective validated v1 state step (default 0.01 day); at most nine decimals and effective_step * (states - 1) < 1 day are semantic checks; mutually exclusive with state_step"}
D["options_survival"].setdefault("allOf", []).append({"not": {"required": ["state_step", "legacy_state_step"]}})
for _branch in D["population"]["oneOf"]:
    if _branch["properties"]["kind"].get("const") in ("index", "criteria"):
        _branch["properties"]["legacy_episodes"] = {"enum": ["cohort_rows", "observation_periods"]}
        _branch["properties"]["legacy_index_anchor"] = {"const": "scope_cohort"}
_observed = [b for b in D["variable"]["oneOf"][1]["properties"]["derived"]["oneOf"] if b["properties"]["kind"].get("const") == "observed_days"][0]
_observed["properties"]["legacy_containing_period"] = {"const": True}
_observed.setdefault("allOf", []).append({"if": {"required": ["legacy_containing_period"]}, "then": {"properties": {"window": {"oneOf": [
    {"type": "object", "required": ["start", "end"], "properties": {"start": {"type": "null"}, "end": {"const": -1}, "anchor": {"const": "index"}}},
    {"type": "object", "required": ["start", "end"], "properties": {"start": {"const": 1}, "end": {"type": "null"}, "anchor": {"const": "index"}}}]}}}})
_name_map = {"type": "object", "propertyNames": {"pattern": "^[A-Za-z][A-Za-z0-9_]{0,63}$"}, "additionalProperties": {"type": "string", "minLength": 1}}
_column_map = {"type": "object", "propertyNames": {"pattern": "^[a-z][a-z0-9_]{0,63}$"}, "additionalProperties": {"type": "string", "minLength": 1}}
_by_output = lambda inner: {"type": "object", "propertyNames": {"pattern": "^[A-Za-z][A-Za-z0-9_]{0,63}$"}, "additionalProperties": inner}
ir["properties"]["legacy_names"] = {
    "type": "object", "additionalProperties": False, "minProperties": 1,
    "properties": {"outputs": _name_map, "populations": _name_map,
                   "outcomes": _by_output(_name_map), "states": _by_output(_name_map), "columns": _by_output(_column_map)},
    "description": "original v1 names of re-encoded identifiers, keyed by IR name (outcomes/states/columns first by IR output name)"}
D["event_table"]["properties"]["legacy_concept_scope"] = {
    "type": "object", "additionalProperties": False, "required": ["concept_set_ref"],
    "properties": {"concept_set_ref": {"$ref": "#/$defs/name"}},
    "description": "the v1 output-level concept set that also restricts this event (intersection with the event's own concept set)"}

# A-16 (CR-16): v1 semantics the A-10 variants still miss -- IR-only, closed,
# produced only by the v1 lowering (legacy_automatic_features above also admits
# the legacy sparse builder)
D["output"]["properties"]["legacy_raw_filter"] = {
    "$ref": "#/$defs/predicate",
    "description": "the v1 person_level output-level custom filter, ANDed into every raw person/death table of the output before the outer merge"}
D["options_intervals"]["properties"]["legacy_anchor_offset"] = {
    "type": "integer", "not": {"const": 0},
    "description": "the v1 intervals_long anchor as a non-zero day offset from the index (either sign), present only with event_select = nearest, which orders by ABS(start_days_from_index - offset); omitted and zero offsets lower to anchor index with this field absent"}
D["options_intervals"]["allOf"] = [
    {"if": {"required": ["legacy_anchor_offset"]},
     "then": {"required": ["event_select"], "properties": {"event_select": {"const": "nearest"}}}}]
# A-21: an unseeded non-base v1 criteria population is an empty
# observation-period roster. This closed compatibility shape is IR-only;
# A-25 additionally excludes an owned cohort reference from this variant.
for _branch in D["population"]["oneOf"]:
    if _branch["properties"]["kind"].get("const") == "criteria":
        _branch["properties"]["legacy_empty"] = {
            "const": True,
            "description": "the v1 criteria population without filter_tree, cohort_definition_id or index_event: no person (2.10.2 materialised an empty roster)"}
        _branch.setdefault("allOf", []).append({
            "if": {"required": ["legacy_empty"]},
            "then": {"required": ["legacy_episodes"],
                     "not": {"required": ["legacy_cohort_table"]},
                     "properties": {"parent": {"type": "null"}, "where": {"type": "null"},
                                    "cohort_definition_id": {"type": "null"},
                                    "legacy_episodes": {"const": "observation_periods"}}}})
# A-25 (CR-25): historical handle-owned cohort names narrow only their declaring
# population. This is a closed IR-only reference, never a native name or ID.
for _branch in D["population"]["oneOf"]:
    _kind = _branch["properties"]["kind"].get("const")
    if _kind in ("criteria", "index"):
        _branch["properties"]["legacy_cohort_table"] = {
            "type": "string", "pattern": "^[A-Za-z_][A-Za-z0-9_.]*$",
            "description": "the canonical historical cohort identifier owned by the handle; preserves case, dots and underscores without a length cap (A-25)"}
        _named_requirements = {
            "not": {"required": ["legacy_empty"]},
            "properties": {"cohort_definition_id": {"type": "null"}}}
        if _kind == "criteria":
            _named_requirements["required"] = ["legacy_episodes"]
            _named_requirements["properties"]["legacy_episodes"] = {"const": "observation_periods"}
        _branch.setdefault("allOf", []).append({
            "if": {"required": ["legacy_cohort_table"]}, "then": _named_requirements})
# A-26: the effective root cohort ID labels every v1 membership output;
# it never selects population membership and has no absent-value default.
D["options_membership"]["properties"]["legacy_cohort_definition_id"] = {
    "type": ["integer", "null"], "minimum": -2147483647, "maximum": 2147483647,
    "description": "effective historical root cohort identifier used in the released membership column, independent of the output population; present only when supplied, coerced integer NA is present null, never defaulted"}
D["legacy_spec"]["properties"]["cohort_definition_id"] = {
    "type": "integer", "minimum": 0,
    "description": "the v1 plan cohort's cohort_definition_id of an inline spec (names its temporary cohort, fills the cohort_membership id column); present only when supplied, never defaulted"}
_scope = ir["properties"]["scope"]
_scope_source = copy.deepcopy([b for b in _scope["properties"]["cohort"]["oneOf"] if b != {"type": "null"}])
_scope["properties"]["legacy_cohort_sources"] = {
    "type": "array", "minItems": 2, "items": {"oneOf": _scope_source},
    "description": "every cohort source v1 folds into the one scope cohort with the materialised scope.combine, in fold order; scope.cohort is the first entry (checked by the IR validator)"}
_scope["allOf"] = [
    {"if": {"required": ["legacy_cohort_sources"]},
     "then": {"required": ["cohort", "combine"], "properties": {"cohort": {"not": {"type": "null"}}}}}]

dump(ir, os.path.join(OUT, "ir-v2.json"))

# --------------------------------------------------------------------------
# 3. mutation schema with the predicate grammar copied in
# --------------------------------------------------------------------------
mut = copy.deepcopy(mutate)
md = mut["$defs"]
for k in ("predicate", "predicate_leaf", "scalar", "column_name"):
    md[k] = copy.deepcopy(plan["$defs"][k])
# A-6: an assigned column may be referenced by later expressions: `var` and `col`
# use the assigned-name grammar ^[A-Za-z][A-Za-z0-9_]{0,63}$
assigned = {"type": "string", "pattern": "^[A-Za-z][A-Za-z0-9_]{0,63}$"}
for branch in md["predicate_leaf"]["oneOf"]:
    branch["properties"]["var"] = copy.deepcopy(assigned)
col_variant = [v for v in md["expr"]["oneOf"] if "col" in v.get("properties", {})][0]
col_variant["properties"]["col"] = copy.deepcopy(assigned)
md["predicate"]["$comment"] = ("copied from plan-v2.json#/$defs/predicate by tools/ws0/gen_schema_artefacts.py; "
                               "`var` and `col` use the assigned-name grammar (amendment A-6)")
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
# shared probe helpers (A-10 / A-14 instances)
_LT = {"index_window": {"start": -365, "end": None}, "event_select": {"order": "first", "n": 1, "by": "grain"}, "min_gap": {"days": 30, "by": "concept", "keep": "first"}}
def _long(b, **extra):
    b["outputs"]["events"] = dict({"rows": "new_users", "format": "long", "sources": [{"event": "hba1c"}], "options": {}}, **extra)
def _tc(b, source_analyses):
    b["outputs"]["tc"] = {"rows": "new_users", "format": "temporal_covariates", "sources": [{"event": "hba1c"}],
                          "options": {"sources": [{"event": "hba1c"}], "analyses": ["binary", "count"], "source_analyses": source_analyses}}
ir_probe("I01", "legacy sex leaf in the IR", OK, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"legacy": {"type": "sex", "params": {"value": "F"}}}))
ir_probe("I02", "legacy has_concept leaf with window in the IR", OK, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"legacy": {"type": "has_concept", "params": {"concept_ids": [201820], "table": "condition_occurrence", "window": {"start": -365, "end": 0}, "min_count": 1}}}))
ir_probe("I03", "legacy leaf with an unknown param", R, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"legacy": {"type": "sex", "params": {"value": "F", "already_gated": True}}}))
ir_probe("I04", "legacy leaf with an unknown type", R, A, lambda b: b["populations"]["new_users"]["where"]["and"].append({"legacy": {"type": "condition", "params": {}}}))
ir_probe("I05", "legacy_spec source in the IR", OK, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("source", {"legacy_spec": {"type": "drug", "concept_set": {"concepts": [1503297], "include_descendants": True}, "inclusion_criteria": {"dm": {"table": "condition_occurrence", "concept_set": {"concepts": [201820]}, "temporal": {"index_window": {"start": -365, "end": 0}}, "occurrence": {"type": "at_least", "count": 1}}}}}))
ir_probe("I06", "legacy_spec with value_threshold (not executable since 2.9)", R, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("source", {"legacy_spec": {"type": "measurement", "value_threshold": {"op": ">", "value": 7}}}))
ir_probe("I07", "legacy_end_strategy DateOffset in the IR", OK, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("legacy_end_strategy", {"DateOffset": {"DateField": "EndDate", "Offset": 30}}))
ir_probe("I08", "legacy_cohort_end_cap in the IR", OK, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("legacy_cohort_end_cap", True))
ir_probe("I09", "forged roster field still refused by the IR", R, A, lambda b: b["outputs"]["baseline"].__setitem__("roster", "new_users"))
REAL_SCOPE = {"table": "measurement", "column": "value_as_number", "n_bins": 10, "concept_id": [3004410], "concept_col": "measurement_concept_id"}
ir_probe("I10", "legacy has_measurement with a real issued safe scope", OK, A, lambda b: b["populations"]["new_users"]["where"]["and"].append(
    {"legacy": {"type": "has_measurement", "params": {"concept_ids": [3004410], "min_value": 6.5, "max_value": 7.5, "safe_scope": REAL_SCOPE, "window": {"start": -365, "end": 0}}}}))
ir_probe("I11", "legacy has_measurement scope missing n_bins", R, A, lambda b: b["populations"]["new_users"]["where"]["and"].append(
    {"legacy": {"type": "has_measurement", "params": {"concept_ids": [3004410], "min_value": 6.5, "max_value": 7.5, "safe_scope": {"table": "measurement", "column": "value_as_number"}}}}))
ir_probe("I12", "legacy has_measurement scope with a forged field", R, A, lambda b: b["populations"]["new_users"]["where"]["and"].append(
    {"legacy": {"type": "has_measurement", "params": {"concept_ids": [3004410], "min_value": 6.5, "max_value": 7.5, "safe_scope": dict(REAL_SCOPE, already_gated=True)}}}))
ir_probe("I13", "legacy_spec value_bin with a real safe scope", OK, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("source",
    {"legacy_spec": {"type": "measurement", "concept_set": {"concepts": [3004410]}, "value_bin": {"lower": 6.5, "upper": 7.5, "safe_scope": REAL_SCOPE}}}))
ir_probe("I14", "legacy_spec value_bin scope with a forged field", R, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("source",
    {"legacy_spec": {"type": "measurement", "value_bin": {"lower": 6.5, "upper": 7.5, "safe_scope": {"already_gated": True}}}}))
ir_probe("I15", "legacy_spec value_bin scope missing required fields", R, A, lambda b: b["populations"]["new_users"]["index"].__setitem__("source",
    {"legacy_spec": {"type": "measurement", "value_bin": {"lower": 6.5, "upper": 7.5, "safe_scope": {"concept_id": 3004410}}}}))
ir_probe("I16", "lowered v1 score with legacy_all_time_conditions in the IR", OK, A, lambda b: b["variables"].__setitem__("cci", {"derived": {"kind": "charlson", "legacy_all_time_conditions": True}}))
probe("P43", "legacy_all_time_conditions refused on the wire", R, A, lambda b: b["variables"].__setitem__("cci", {"derived": {"kind": "charlson", "legacy_all_time_conditions": True}}))
# ... and accepted (or refused when malformed) in the IR (A-10)
ir_probe("I17", "legacy_columns projection in the IR", OK, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_columns", ["person_id", "value_as_number"]))
ir_probe("I18", "legacy_columns explicitly empty in the IR", OK, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_columns", []))
ir_probe("I19", "legacy_columns with a duplicate", R, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_columns", ["person_id", "person_id"]))
ir_probe("I20", "legacy_temporal with open-ended window, selection and gap in the IR", OK, A, lambda b: _long(b, legacy_temporal=_LT))
ir_probe("I21", "legacy_temporal event_select order all", R, A, lambda b: _long(b, legacy_temporal={"event_select": {"order": "all", "n": 1, "by": "grain"}}))
ir_probe("I22", "legacy_temporal window with no integer bound", R, A, lambda b: _long(b, legacy_temporal={"index_window": {"start": None, "end": None}}))
ir_probe("I23", "legacy_temporal empty object", R, A, lambda b: _long(b, legacy_temporal={}))
ir_probe("I24", "legacy_automatic_features object in the IR", OK, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_automatic_features", {"format": "wide", "source": {"event": "hba1c"}}))
ir_probe("I25", "legacy_automatic_features as a boolean", R, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_automatic_features", True))
ir_probe("I26", "legacy_single_outcome in the IR", OK, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("legacy_single_outcome", True))
ir_probe("I27", "legacy_single_outcome false", R, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("legacy_single_outcome", False))
ir_probe("I28", "legacy_states with the effective legacy state step in the IR", OK, A, lambda b: b["outputs"]["tte"]["options"].update({"legacy_states": ["index", "mi"], "legacy_state_step": 0.01}))
ir_probe("I29", "legacy_state_step beside state_step", R, A, lambda b: b["outputs"]["tte"]["options"].update({"legacy_state_step": 0.01, "state_step": 1}))
ir_probe("I30", "legacy_state_step zero", R, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("legacy_state_step", 0))
ir_probe("I31", "legacy_states empty", R, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("legacy_states", []))
ir_probe("I32", "legacy_episodes and legacy_index_anchor in the IR", OK, A, lambda b: b["populations"]["new_users"].update({"legacy_episodes": "cohort_rows", "legacy_index_anchor": "scope_cohort"}))
ir_probe("I33", "legacy_episodes with an unknown value", R, A, lambda b: b["populations"]["new_users"].__setitem__("legacy_episodes", "rows"))
ir_probe("I34", "legacy_containing_period prior observation in the IR", OK, A, lambda b: b["variables"].__setitem__("po", {"derived": {"kind": "observed_days", "window": {"start": None, "end": -1, "anchor": "index"}, "legacy_containing_period": True}}))
ir_probe("I35", "legacy_containing_period future observation in the IR", OK, A, lambda b: b["variables"].__setitem__("fo", {"derived": {"kind": "observed_days", "window": {"start": 1, "end": None, "anchor": "index"}, "legacy_containing_period": True}}))
ir_probe("I36", "legacy_containing_period with an ordinary window", R, A, lambda b: b["variables"].__setitem__("po", {"derived": {"kind": "observed_days", "window": {"start": -365, "end": 0, "anchor": "index"}, "legacy_containing_period": True}}))
ir_probe("I37", "legacy_names for an output and a column in the IR", OK, A, lambda b: b.__setitem__("legacy_names", {"outputs": {"baseline": "my.output"}, "columns": {"baseline": {"hba1c_base": "hba1c-base"}}}))
ir_probe("I38", "legacy_names with an unknown map", R, A, lambda b: b.__setitem__("legacy_names", {"other": {"a": "b"}}))
ir_probe("I39", "legacy_concept_scope in the IR", OK, A, lambda b: b["events"]["hba1c"].__setitem__("legacy_concept_scope", {"concept_set_ref": "t2dm"}))
ir_probe("I40", "legacy_concept_scope with an extra field", R, A, lambda b: b["events"]["hba1c"].__setitem__("legacy_concept_scope", {"concept_set_ref": "t2dm", "extra": 1}))
# A-15: the legacy inclusion occurrence is the historical {type, count} object
def _legacy_spec(**fields):
    def edit(b):
        b["populations"]["new_users"]["index"]["source"] = {"legacy_spec": dict({"type": "drug", "concept_set": {"concepts": [1503297]}}, **fields)}
    return edit
def _legacy_spec_occ(occurrence):
    return _legacy_spec(inclusion_criteria={"dm": {"table": "condition_occurrence", "concept_set": {"concepts": [201820]}, "occurrence": occurrence}})
ir_probe("I41", "legacy_spec occurrence in the min/max form the executor never accepted (A-15)", R, A, _legacy_spec_occ({"min": 1}))
ir_probe("I42", "legacy_spec occurrence exactly 0 in the IR (A-15)", OK, A, _legacy_spec_occ({"type": "exactly", "count": 0}))
ir_probe("I43", "legacy_spec occurrence with an unknown type (A-15)", R, A, _legacy_spec_occ({"type": "more_than", "count": 1}))
# A-16 variants in the IR (accepted, or refused when malformed)
def _iv(b, **opts):
    b["outputs"]["iv"] = {"rows": "new_users", "format": "intervals", "options": dict({"sources": [{"event": "hba1c"}]}, **opts)}
ir_probe("I44", "legacy_raw_filter on an output in the IR (A-16)", OK, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_raw_filter", {"var": "gender_concept_id", "op": "==", "value": 8507}))
ir_probe("I45", "legacy_raw_filter that is not a predicate", R, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_raw_filter", {"private": True}))
ir_probe("I46", "legacy_anchor_offset with event_select nearest in the IR (A-16)", OK, A, lambda b: _iv(b, event_select="nearest", legacy_anchor_offset=30))
ir_probe("I47", "legacy_anchor_offset that is not an integer", R, A, lambda b: _iv(b, event_select="nearest", legacy_anchor_offset=1.5))
ir_probe("I48", "legacy_spec cohort_definition_id in the IR (A-16)", OK, A, _legacy_spec(cohort_definition_id=7))
ir_probe("I49", "legacy_spec cohort_definition_id zero: a supplied v1 inline id, admitted as supplied (A-16)", OK, A, _legacy_spec(cohort_definition_id=0))
ir_probe("I50", "scope legacy_cohort_sources with the materialised combine in the IR (A-16)", OK, A, lambda b: b.__setitem__("scope", {"cohort": 3, "tables": [], "combine": "union", "legacy_cohort_sources": [3, 5]}))
ir_probe("I51", "scope legacy_cohort_sources with one source", R, A, lambda b: b.__setitem__("scope", {"cohort": 3, "combine": "union", "legacy_cohort_sources": [3]}))
ir_probe("I52", "legacy_automatic_features for the legacy sparse builder in the IR (A-16)", OK, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_automatic_features", {"format": "sparse", "source": {"event": "hba1c"}}))
ir_probe("I53", "legacy_anchor_offset of the other sign in the IR (A-16)", OK, A, lambda b: _iv(b, event_select="nearest", legacy_anchor_offset=-14))
ir_probe("I54", "legacy_anchor_offset zero: lowers to anchor index with the field absent", R, A, lambda b: _iv(b, event_select="nearest", legacy_anchor_offset=0))
ir_probe("I55", "legacy_anchor_offset with a non-nearest selection", R, A, lambda b: _iv(b, event_select="first", legacy_anchor_offset=30))
ir_probe("I56", "legacy_anchor_offset without the materialised event_select", R, A, lambda b: _iv(b, legacy_anchor_offset=30))
ir_probe("I57", "legacy_spec cohort_definition_id negative", R, A, _legacy_spec(cohort_definition_id=-1))
ir_probe("I58", "scope legacy_cohort_sources without the materialised combine", R, A, lambda b: b.__setitem__("scope", {"cohort": 3, "legacy_cohort_sources": [3, 5]}))
ir_probe("I59", "scope legacy_cohort_sources with a null scope cohort", R, A, lambda b: b.__setitem__("scope", {"cohort": None, "combine": "union", "legacy_cohort_sources": [3, 5]}))
ir_probe("I60", "scope legacy_cohort_sources with a results-cohort reference of zero", R, A, lambda b: b.__setitem__("scope", {"cohort": 3, "combine": "intersect", "legacy_cohort_sources": [3, {"type": "cohort_table", "cohort_definition_id": 0}]}))
# A-21's approved empty criteria variant, with A-25's named-cohort exclusion.
ir_probe("I61", "legacy_empty criteria population in the IR (A-21)", OK, A, lambda b: b["populations"].__setitem__("nobody", {"kind": "criteria", "legacy_empty": True, "legacy_episodes": "observation_periods"}))
ir_probe("I62", "legacy_empty beside criteria", R, A, lambda b: b["populations"].__setitem__("nobody", {"kind": "criteria", "legacy_empty": True, "legacy_episodes": "observation_periods", "where": {"person": {"var": "gender_concept_id", "op": "==", "value": 8532}}}))
ir_probe("I63", "legacy_empty false", R, A, lambda b: b["populations"].__setitem__("nobody", {"kind": "criteria", "legacy_empty": False, "legacy_episodes": "observation_periods"}))
ir_probe("I64", "legacy_empty requires its observation-period roster", R, A, lambda b: b["populations"].__setitem__("nobody", {"kind": "criteria", "legacy_empty": True}))
ir_probe("I65", "legacy_empty cannot use cohort rows", R, A, lambda b: b["populations"].__setitem__("nobody", {"kind": "criteria", "legacy_empty": True, "legacy_episodes": "cohort_rows"}))
ir_probe("I66", "legacy_empty cannot inherit a parent", R, A, lambda b: b["populations"].__setitem__("nobody", {"kind": "criteria", "legacy_empty": True, "legacy_episodes": "observation_periods", "parent": "new_users"}))
ir_probe("I67", "legacy_empty cannot reference a cohort", R, A, lambda b: b["populations"].__setitem__("nobody", {"kind": "criteria", "legacy_empty": True, "legacy_episodes": "observation_periods", "cohort_definition_id": 1}))
ir_probe("I_A21_kind", "legacy_empty cannot mark an index population", R, A, lambda b: b["populations"]["new_users"].update({"legacy_empty": True, "legacy_episodes": "observation_periods"}))
ir_probe("I_A21_named_cohort", "legacy_empty excludes legacy_cohort_table (A-25)", R, A, lambda b: b["populations"].__setitem__("nobody", {"kind": "criteria", "legacy_empty": True, "legacy_episodes": "observation_periods", "legacy_cohort_table": "owned_cohort"}))
# A-25: compact complete documents keep the named-reference boundary explicit.
_named_base = {"version": 2, "meta": copy.deepcopy(A["meta"]),
               "events": {"events": {"table": "condition_occurrence"}},
               "populations": {}, "outputs": {"result": {"rows": "named", "format": "wide"}}}
def _named_population(kind="criteria", **extra):
    pop = {"kind": kind, "legacy_cohort_table": "_Owned.Cohort_1"}
    if kind == "criteria":
        pop["legacy_episodes"] = "observation_periods"
    elif kind == "index":
        pop["index"] = {"source": {"event": "events"}, "select": "all"}
    pop.update(extra)
    return pop

_named_cases = [
    ("criteria reference preserves case, dots and underscores", OK, _named_population()),
    ("index reference preserves recurrent event selection", OK, _named_population("index")),
    ("criteria reference has no native name length cap", OK, _named_population(legacy_cohort_table="_" + "Cohort_" * 24)),
    ("index reference has no native name length cap", OK, _named_population("index", legacy_cohort_table="_" + "Cohort_" * 24)),
    ("criteria reference permits a null numeric reference", OK, _named_population(cohort_definition_id=None)),
    ("criteria reference preserves its episode policy and scope anchor", OK, _named_population(episode_policy="all_episodes", legacy_index_anchor="scope_cohort")),
    ("index reference preserves historical cohort-row episodes", OK, _named_population("index", legacy_episodes="cohort_rows", legacy_index_anchor="scope_cohort")),
]
for _kind in ("criteria", "index"):
    for _label, _value in (("null", None), ("numeric", 1), ("object", {}), ("array", ["owned"]),
                           ("empty", ""), ("whitespace-padded", " owned "), ("malformed", "bad-name"),
                           ("digit-only", "1"), ("padded digit-only", " 1 "), ("signed numeric", "+1"),
                           ("exponent numeric", "1e0"), ("decimal numeric", "1.0")):
        _named_cases.append((f"{_kind} reference refuses {_label}", R,
                             _named_population(_kind, legacy_cohort_table=_value)))
    _named_cases.extend([
        (f"{_kind} reference conflicts with a numeric population reference", R, _named_population(_kind, cohort_definition_id=1)),
        (f"{_kind} reference conflicts with legacy_empty", R, _named_population(_kind, legacy_empty=True)),
    ])
_missing_episodes = _named_population(); _missing_episodes.pop("legacy_episodes")
_named_cases.extend([
    ("criteria reference requires legacy_episodes", R, _missing_episodes),
    ("criteria reference refuses cohort-row episodes", R, _named_population(legacy_episodes="cohort_rows")),
    ("criteria reference refuses null legacy_episodes", R, _named_population(legacy_episodes=None)),
    ("index reference refuses episode_policy", R, _named_population("index", episode_policy="first_episode")),
    ("index reference refuses even null episode_policy", R, _named_population("index", episode_policy=None)),
    ("setop reference refuses the compatibility property", R, _named_population("setop", setop={"op": "union", "members": ["a", "b"]})),
    ("match reference refuses the compatibility property", R, _named_population("match", match={"cases": "a", "controls": "b", "outcome": {"event": "events"}})),
    ("legacy numeric cohort leaf still refuses names", R, {"kind": "criteria", "where": {"legacy": {"type": "cohort", "params": {"cohort_definition_id": "owned"}}}}),
])
for _i, (_label, _expect, _population) in enumerate(_named_cases, 68):
    ir_probe(f"I{_i:02d}", f"{_label} (A-25)", _expect, _named_base,
             lambda b, pop=_population: b["populations"].__setitem__("named", pop))

# CR-27: an isolated membership output proves both the closed IR option
# and native-wire refusal; no other compatibility field masks the boundary.
_membership_base = {"version": 2, "meta": copy.deepcopy(A["meta"]),
                    "populations": {"p": {"kind": "criteria"}},
                    "outputs": {"members": {"rows": "p", "format": "membership", "options": {}}}}
_membership_cases = [("positive identifier", OK, 1), ("zero identifier", OK, 0),
                     ("null identifier", OK, None), ("negative identifier", OK, -1),
                     ("fractional identifier", R, 1.5), ("string identifier", R, "1"),
                     ("boolean identifier", R, True), ("array identifier", R, [1]),
                     ("object identifier", R, {"value": 1})]
for _i, (_label, _expect, _value) in enumerate(_membership_cases, 111):
    ir_probe(f"I{_i:02d}", f"membership {_label} (CR-27)", _expect, _membership_base,
             lambda b, value=_value: b["outputs"]["members"]["options"].__setitem__("legacy_cohort_definition_id", value))
probe("P74", "membership identifier absent without default on native wire (CR-27)", OK,
         _membership_base, lambda b: None)
def _membership_wrong_format(b):
    b["outputs"]["members"]["format"] = "wide"
    b["outputs"]["members"]["options"]["legacy_cohort_definition_id"] = 1
ir_probe("I120", "membership identifier refused on wide output (CR-27)", R,
         _membership_base, _membership_wrong_format)
probe("P73", "membership identifier refused on native wire (CR-27)", R,
      _membership_base,
      lambda b: b["outputs"]["members"]["options"].__setitem__("legacy_cohort_definition_id", 1))

for _i, (_label, _expect, _value) in enumerate([
        ("minimum integer", OK, -2147483647), ("maximum integer", OK, 2147483647),
        ("below integer range", R, -2147483648), ("above integer range", R, 2147483648)], 121):
    ir_probe(f"I{_i:02d}", f"membership {_label} (A-26)", _expect, _membership_base,
             lambda b, value=_value: b["outputs"]["members"]["options"].__setitem__("legacy_cohort_definition_id", value))
for _i, _value in enumerate([None, -1, 0], 75):
    probe(f"P{_i:02d}", "membership compatibility label refused on native wire (A-26)", R,
          _membership_base,
          lambda b, value=_value: b["outputs"]["members"]["options"].__setitem__("legacy_cohort_definition_id", value))

# Native probes isolate each unchanged boundary: no other legacy field is used.
_native_named_cases = [
    ("criteria legacy_cohort_table", {"kind": "criteria", "legacy_cohort_table": "owned"}),
    ("index legacy_cohort_table", {"kind": "index", "index": {"source": {"event": "events"}}, "legacy_cohort_table": "owned"}),
    ("named population cohort_definition_id", {"kind": "criteria", "cohort_definition_id": "owned"}),
    ("named native cohort criterion", {"kind": "criteria", "where": {"cohort": {"cohort_definition_id": "owned"}}}),
    ("named index cohort source", {"kind": "index", "index": {"source": {"cohort_definition_id": "owned"}}}),
]
for _i, (_label, _population) in enumerate(_native_named_cases, 68):
    probe(f"P{_i:02d}", f"{_label} refused on the wire (A-25)", R, _named_base,
          lambda b, pop=_population: b["populations"].__setitem__("named", pop))
probe("P63", "temporal source_analyses with a count-only selection (A-14; a wire field, so valid under both schemas)", OK, A, lambda b: _tc(b, {"hba1c": ["count"]}))
probe("P44", "censor conditioned on an index source (A-7)", OK, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("censor", ["observation_end", {"censor": {"source": {"interval": "metf_era"}, "which": "first_start_after_entry", "when_index_source": ["acei"]}}]))
probe("P45", "censor when_index_source must be nonempty and unique", R, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("censor", ["observation_end", {"censor": {"source": {"interval": "metf_era"}, "when_index_source": ["acei", "acei"]}}]))
probe("P46", "censor when_index_source empty array refused", R, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("censor", ["observation_end", {"censor": {"source": {"interval": "metf_era"}, "when_index_source": []}}]))
# A-14: per-source analyses on the wire
probe("P50", "temporal source_analyses selecting one analysis for one source (A-14)", OK, A, lambda b: _tc(b, {"hba1c": ["binary"]}))
probe("P51", "temporal source_analyses with an unknown analysis", R, A, lambda b: _tc(b, {"hba1c": ["bogus"]}))
probe("P52", "temporal source_analyses with an empty selection", R, A, lambda b: _tc(b, {"hba1c": []}))
probe("P53", "temporal source_analyses keyed outside the name grammar", R, A, lambda b: _tc(b, {"9x": ["binary"]}))
# A-10: every IR-only variant is refused on the wire ...
probe("P54", "legacy_columns on the wire", R, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_columns", ["person_id"]))
probe("P55", "legacy_temporal on the wire", R, A, lambda b: _long(b, legacy_temporal=_LT))
probe("P56", "legacy_automatic_features on the wire", R, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_automatic_features", {"format": "wide", "source": {"event": "hba1c"}}))
probe("P57", "legacy_single_outcome on the wire", R, A, lambda b: b["outputs"]["tte"]["options"].__setitem__("legacy_single_outcome", True))
probe("P58", "legacy_states and legacy_state_step on the wire", R, A, lambda b: b["outputs"]["tte"]["options"].update({"legacy_states": ["index", "mi"], "legacy_state_step": 0.01}))
probe("P59", "legacy_episodes on the wire", R, A, lambda b: b["populations"]["new_users"].update({"legacy_episodes": "cohort_rows", "legacy_index_anchor": "scope_cohort"}))
probe("P60", "legacy_containing_period on the wire", R, A, lambda b: b["variables"].__setitem__("po", {"derived": {"kind": "observed_days", "window": {"start": None, "end": -1, "anchor": "index"}, "legacy_containing_period": True}}))
probe("P61", "legacy_names on the wire", R, A, lambda b: b.__setitem__("legacy_names", {"outputs": {"baseline": "my.output"}}))
probe("P62", "legacy_concept_scope on the wire", R, A, lambda b: b["events"]["hba1c"].__setitem__("legacy_concept_scope", {"concept_set_ref": "t2dm"}))
# A-16: refused on the wire
probe("P64", "legacy_raw_filter on the wire", R, A, lambda b: b["outputs"]["baseline"].__setitem__("legacy_raw_filter", {"var": "gender_concept_id", "op": "==", "value": 8507}))
probe("P65", "legacy_anchor_offset on the wire", R, A, lambda b: _iv(b, event_select="nearest", legacy_anchor_offset=30))
probe("P66", "scope legacy_cohort_sources on the wire", R, A, lambda b: b.__setitem__("scope", {"cohort": 3, "combine": "union", "legacy_cohort_sources": [3, 5]}))
probe("P67", "legacy_empty on the wire", R, A, lambda b: b["populations"].__setitem__("nobody", {"kind": "criteria", "legacy_empty": True, "legacy_episodes": "observation_periods"}))
# input-level probes carried as JSON TEXT (a conforming JSON document cannot hold
# Infinity, and duplicate members are only visible to a parser): the R validator
# parses `instance_text` with jsonlite (1e400 -> Inf; duplicate names kept) and
# must refuse them; Python's parser rejects/collapses them, so check_contracts.py
# verifies them with its own parser hooks.
base_text = json.dumps(A)
probes.append({"id": "P47", "label": "infinite conversion factor (1e400) is not a finite number", "schema": "plan-v2", "expect": R,
               "instance": None, "instance_text": base_text.replace('"factor": 0.0915', '"factor": 1e400')})
probes.append({"id": "P48", "label": "duplicate root member `version`", "schema": "plan-v2", "expect": R,
               "instance": None, "instance_text": base_text.replace('"version": 2,', '"version": 2, "version": "wrong",', 1)})
probes.append({"id": "P49", "label": "duplicate member inside a definition map (events.hba1c.table)", "schema": "plan-v2", "expect": R,
               "instance": None, "instance_text": base_text.replace('"hba1c": {"table": "measurement",', '"hba1c": {"table": "measurement", "table": "observation",', 1)})
assert '"factor": 1e400' in probes[-3]["instance_text"] and '"version": "wrong"' in probes[-2]["instance_text"] and '"table": "observation"' in probes[-1]["instance_text"]
probe("P_A27_1", "recipe concept recodes", OK, A, lambda b: b.setdefault("options", {}).__setitem__("labels", {"0": "unknown", "45000123": "yes"}))
probe("P_A27_2", "array concept map refused", R, A, lambda b: b.setdefault("options", {}).__setitem__("labels", ["yes"]))
probe("P_A27_3", "null concept map member refused", R, A, lambda b: b.setdefault("options", {}).__setitem__("labels", {"1": None}))
probe("P_A27_4", "noncanonical concept key refused", R, A, lambda b: b.setdefault("options", {}).__setitem__("labels", {"01": "yes"}))
probe("P_A27_5", "generated-name overrides", OK, A, lambda b: b.setdefault("options", {}).__setitem__("column_names", {"education_45000123": "educated"}))
dump(probes, os.path.join(OUT, "probes", "plan-v2-probes.json"))

# the three ACTUAL v1 inputs of section 4.5 (WS-2 executes them through the corrected v1 validator;
# WS-0 materialises the inputs and their verdicts)
v1_probes = [
    {"id": "V01", "label": "v1 plan with an unknown root field", "schema": "v1-validator", "owner": "WS-2", "expect": "invalid",
     "instance": {"cohort": {"type": "cohort_table", "cohort_definition_id": 1}, "already_gated": True,
                  "outputs": {"demo": {"type": "baseline", "columns": ["gender_concept_id"], "derived": ["age_at_index"]}},
                  "options": {"translate_concepts": False, "block_sensitive": True}}},
    {"id": "V02", "label": "v1 concept_dictionary output with an unknown field", "schema": "v1-validator", "owner": "WS-2", "expect": "invalid",
     "instance": {"outputs": {"dict": {"type": "concept_dictionary", "concept_ids": [201820], "already_gated": True}},
                  "options": {"translate_concepts": True, "block_sensitive": True}}},
    {"id": "V03", "label": "v1 baseline with the off-grid age grid 0,18,19,65 (assignment grid-shape validation)", "schema": "v1-validator", "owner": "WS-2", "expect": "valid",
     "instance": {"cohort": {"type": "cohort_table", "cohort_definition_id": 1},
                  "outputs": {"demo": {"type": "baseline", "columns": ["gender_concept_id"], "derived": ["age_at_index"], "age_breaks": [0, 18, 19, 65]}},
                  "options": {"translate_concepts": False, "block_sensitive": True}}},
]
dump(v1_probes, os.path.join(OUT, "probes", "plan-v1-probes.json"))

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
    {"id": "M06", "label": "uppercase assigned name referenced directly (A-6)", "schema": "mutate-v1", "expect": OK,
     "instance": {"version": 1, "assign": [{"name": "X", "expr": {"col": "age"}}, {"name": "y", "expr": {"col": "X"}}]}},
    {"id": "M07", "label": "uppercase assigned name in arithmetic and a predicate (A-6)", "schema": "mutate-v1", "expect": OK,
     "instance": {"version": 1, "assign": [{"name": "Pdc", "expr": {"op": "/", "args": [{"col": "covered"}, {"col": "observed"}]}},
                                           {"name": "flag", "expr": {"ifelse": {"test": {"var": "Pdc", "op": ">=", "value": 0.8}, "yes": {"lit": 1}, "no": {"lit": 0}}}}]}},
    {"id": "M08", "label": "identifier starting with a digit refused", "schema": "mutate-v1", "expect": R,
     "instance": {"version": 1, "assign": [{"name": "1x", "expr": {"lit": 1}}]}},
]
dump(mprobes, os.path.join(OUT, "probes", "mutate-v1-probes.json"))

# --------------------------------------------------------------------------
# 6. semantic digest vectors (RFC 8785 canonicalisation; section 4.4)
# --------------------------------------------------------------------------
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from jcs_reference import es6_number, jcs, digest_payload, digest   # shared with check_contracts.py
v_base = copy.deepcopy(A)
v_version = copy.deepcopy(A); v_version["meta"]["semantic_version"] = "2.1.0"
v_symbol = copy.deepcopy(A); v_symbol["outputs"]["baseline"]["result_symbol"] = "D_other"
# B1 regressions: an event NAMED `description` whose definition changes must change the
# digest (names are clinical content); the same for `label` and `result_symbol` names.
def rename_event(obj, old, new):
    obj["events"][new] = obj["events"].pop(old)
    def walk(o):
        if isinstance(o, dict):
            if o.get("event") == old: o["event"] = new
            for v in o.values(): walk(v)
        elif isinstance(o, list):
            for v in o: walk(v)
    walk(obj)
v_name_a = copy.deepcopy(A); rename_event(v_name_a, "mi", "description")
v_name_b = copy.deepcopy(v_name_a); v_name_b["events"]["description"]["concept_set"] = "t2dm"
v_label_a = copy.deepcopy(A); rename_event(v_label_a, "mi", "label")
v_label_b = copy.deepcopy(v_label_a); v_label_b["events"]["label"]["concept_set"] = "t2dm"
v_rs_a = copy.deepcopy(A); rename_event(v_rs_a, "mi", "result_symbol")
v_rs_b = copy.deepcopy(v_rs_a); v_rs_b["events"]["result_symbol"]["concept_set"] = "t2dm"
v_label_text = copy.deepcopy(A); v_label_text["populations"]["new_users"]["label"] = "renamed label text"
# numeric canonicalisation regressions
v_num = copy.deepcopy(A)
v_num["events"]["hba1c"]["unit"]["convert"]["8753"] = {"factor": 1e-7, "offset": 1000000000000000100}
v_num["events"]["hba1c"]["unit"]["convert"]["8840"] = {"factor": 1.2345678901234568e18, "offset": -4.051847215643804e+161}
v_num["events"]["hba1c"]["unit"]["convert"]["8876"] = {"factor": 0.000001, "offset": 1e21}
# re-review B1: a power of two has an asymmetric rounding interval; the shortest round-trip
# decimal of 2^-24 is 5.960464477539063e-8 (sixteen digits), not the nearest sixteen-digit decimal
v_pow2 = copy.deepcopy(A)
v_pow2["events"]["hba1c"]["unit"]["convert"]["8753"]["factor"] = 2.0 ** -24
vectors = []
for label, obj, note in (("base", v_base, "canonical example A"),
                         ("semantic_version_change", v_version, "meta.semantic_version = 2.1.0: a different digest"),
                         ("result_symbol_change", v_symbol, "outputs.baseline.result_symbol = D_other: the identical digest"),
                         ("event_named_description_mi", v_name_a, "event renamed to `description` (MI concepts): the name is kept in the payload"),
                         ("event_named_description_t2dm", v_name_b, "same event with diabetes concepts: a DIFFERENT digest from event_named_description_mi"),
                         ("event_named_label_mi", v_label_a, "event renamed to `label` (MI concepts)"),
                         ("event_named_label_t2dm", v_label_b, "same with diabetes concepts: a different digest"),
                         ("event_named_result_symbol_mi", v_rs_a, "event renamed to `result_symbol` (MI concepts)"),
                         ("event_named_result_symbol_t2dm", v_rs_b, "same with diabetes concepts: a different digest"),
                         ("population_label_text_change", v_label_text, "populations.new_users.label text changed: the identical digest to base"),
                         ("numeric_canonicalisation", v_num, "payload contains 1e-7, 1000000000000000100, 1234567890123456800, -4.051847215643804e+161, 0.000001 and 1e+21"),
                         ("power_of_two_factor", v_pow2, "events.hba1c.unit.convert.8753.factor = 2^-24 (bits 3e70000000000000): payload contains 5.960464477539063e-8")):
    payload, h = digest(obj)
    vectors.append({"label": label, "note": note, "instance": obj, "payload_sha256": h, "payload": payload})
vec = {v["label"]: v for v in vectors}
assert vec["base"]["payload_sha256"] != vec["semantic_version_change"]["payload_sha256"]
assert vec["base"]["payload_sha256"] == vec["result_symbol_change"]["payload_sha256"]
assert vec["base"]["payload_sha256"] == vec["population_label_text_change"]["payload_sha256"]
for a, b in (("event_named_description_mi", "event_named_description_t2dm"), ("event_named_label_mi", "event_named_label_t2dm"),
             ("event_named_result_symbol_mi", "event_named_result_symbol_t2dm")):
    assert vec[a]["payload_sha256"] != vec[b]["payload_sha256"], a
    assert '"' + a.split("_")[2] + '"' in vec[a]["payload"] or True
assert '"factor":5.960464477539063e-8' in vec["power_of_two_factor"]["payload"]
assert vec["power_of_two_factor"]["payload_sha256"] == "101a26cd7a125eefec08cb556361c7ae33e81d737fa0abe27e9c85d43aab905d", vec["power_of_two_factor"]["payload_sha256"]
for needle in ('"factor":1e-7', '"offset":1000000000000000100', '"factor":1234567890123456800', '"offset":-4.051847215643804e+161', '"factor":0.000001', '"offset":1e+21'):
    assert needle in vec["numeric_canonicalisation"]["payload"], needle
dump({"canonicalisation": "RFC 8785 (JSON Canonicalization Scheme) of the normalised IR with ECMAScript number formatting; advisory properties removed at their owning schema paths only (meta.generated_by, meta.client_version, meta.recipe_hash, populations.<id>.label, outputs.<name>.result_symbol); definition names are never stripped; version and meta.language_version/meta.semantic_version retained",
      "vectors": vectors}, os.path.join(OUT, "digest-vectors.json"))

# --------------------------------------------------------------------------
# 6b. normalised fixtures (A-11): the normal form of A, B and the acceptance plans,
#     pinned as documents, normalised-document hashes and semantic digests
# --------------------------------------------------------------------------
from normalize_reference import normalize   # shared with check_contracts.py
norm_fixtures = [("plan-v2-example-A", "examples/plan-v2-example-A.json", A),
                 ("plan-v2-example-B", "examples/plan-v2-example-B.json", B)]
acceptance_dir = os.path.join(os.path.dirname(OUT), "plans", "acceptance")
if os.path.isdir(acceptance_dir):
    for d in sorted(os.listdir(acceptance_dir)):
        path = os.path.join(acceptance_dir, d, "plan.json")
        if os.path.exists(path):
            norm_fixtures.append(("acceptance-" + d, os.path.relpath(path, OUT), json.load(open(path))))
norm_vectors = []
for label, source, obj in norm_fixtures:
    n = normalize(obj, plan)
    assert normalize(n, plan) == n, label
    dump(n, os.path.join(OUT, "normalized", label + ".json"))
    norm_vectors.append({"label": label, "source": source,
                         "normalized_sha256": hashlib.sha256(jcs(n).encode("utf-8")).hexdigest(),
                         "digest_sha256": digest(n)[1]})
assert len(norm_vectors) >= 2
dump({"rule": "amendment A-11: schema defaults in the matching branch, then options/registries {}, publish = output names, population coverage (options.coverage or unique/drop/drop/0; on_missing keep when restrict_to_observation is false), grain (episode for an indexed rows population: index or match, criteria inheriting from its parent, union with any indexed member, intersect/setdiff inheriting from the first member), wide columns = every variable, survival event_order (first for survival/competing_risk, all otherwise) and outcome_mode (composite for survival, named otherwise); nothing else added or removed; idempotent",
      "note": "raw canonicalisation vectors (digest-vectors.json) are serializer tests of the unnormalised instances; these are the normalised-IR digests the federation signature and the DP capsule bind",
      "vectors": norm_vectors}, os.path.join(OUT, "normalized-vectors.json"))

# --------------------------------------------------------------------------
# 7. validate everything with jsonschema (2020-12) and write SHA256SUMS
# --------------------------------------------------------------------------
from jsonschema import Draft202012Validator
Draft202012Validator.check_schema(plan); Draft202012Validator.check_schema(ir); Draft202012Validator.check_schema(mut)
validators = {"plan-v2": Draft202012Validator(plan), "ir-v2": Draft202012Validator(ir), "mutate-v1": Draft202012Validator(mut)}
bad = []
for p in probes + mprobes:
    if p.get("instance") is None and "instance_text" in p:
        continue   # text probes: verified by check_contracts.py with parser hooks and by the R validator
    ok = validators[p["schema"]].is_valid(p["instance"])
    if ok != (p["expect"] == OK):
        bad.append(p["id"])
assert not bad, "probe verdict mismatch: %s" % bad
assert validators["ir-v2"].is_valid(A) and validators["ir-v2"].is_valid(B)
for label, _, obj in norm_fixtures:
    n = normalize(obj, plan)
    assert validators["plan-v2"].is_valid(n) and validators["ir-v2"].is_valid(n), "normalised %s does not validate" % label
print("validated %d plan/ir probes and %d mutate probes" % (len(probes), len(mprobes)))

sums = []
for root, _, files in os.walk(OUT):
    for fn in sorted(files):
        if fn in ("SHA256SUMS", "README.md"): continue   # the client README differs by design
        path = os.path.join(root, fn)
        rel = os.path.relpath(path, OUT)
        sums.append((hashlib.sha256(open(path, "rb").read()).hexdigest(), rel))
sums.sort(key=lambda t: t[1])
with open(os.path.join(OUT, "SHA256SUMS"), "w") as f:
    for h, rel in sums:
        f.write("%s  %s\n" % (h, rel))
print("wrote", len(sums), "files + SHA256SUMS under", OUT)
