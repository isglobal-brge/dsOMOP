"""Reference RFC 8785 canonicaliser and semantic digest (Python), shared by
gen_schema_artefacts.py (which writes the vectors) and check_contracts.py (the
independent CI oracle). Mirrors R/semantic_digest.R: ECMAScript Number::toString
numbers, UTF-16 key order, advisory properties stripped at their owning paths only.
"""
import copy, decimal, hashlib, json

def es6_number(x):
    """ECMAScript Number::toString (RFC 8785 section 3.2.2.3) of a finite double."""
    if isinstance(x, bool):
        raise TypeError("booleans are not numbers")
    x = float(x)
    if x != x or x in (float("inf"), float("-inf")):
        raise ValueError("non-finite number")
    if x == 0:
        return "0"
    t = decimal.Decimal(repr(abs(x))).as_tuple()      # repr = shortest round-trip digits
    digits_full = "".join(str(d) for d in t.digits)
    lead = len(digits_full) - len(digits_full.lstrip("0"))
    digits = digits_full.lstrip("0").rstrip("0")
    n = len(digits_full) + t.exponent - lead           # value = 0.digits * 10**n
    k = len(digits)
    if k <= n <= 21:
        body = digits + "0" * (n - k)
    elif 0 < n <= 21:
        body = digits[:n] + "." + digits[n:]
    elif -6 < n <= 0:
        body = "0." + "0" * (-n) + digits
    else:
        e10 = n - 1
        m = digits[0] + ("." + digits[1:] if k > 1 else "")
        body = m + "e" + ("+" if e10 > 0 else "-") + str(abs(e10))
    return ("-" if x < 0 else "") + body

def jcs(obj):
    if obj is None: return "null"
    if obj is True: return "true"
    if obj is False: return "false"
    if isinstance(obj, (int, float)): return es6_number(obj)
    if isinstance(obj, str): return json.dumps(obj, ensure_ascii=False)
    if isinstance(obj, list): return "[" + ",".join(jcs(v) for v in obj) + "]"
    if isinstance(obj, dict):
        keys = sorted(obj.keys(), key=lambda k: k.encode("utf-16-be"))
        return "{" + ",".join(json.dumps(k, ensure_ascii=False) + ":" + jcs(obj[k]) for k in keys) + "}"
    raise TypeError(type(obj))

# advisory properties are removed at their owning schema paths only (B1)
ADVISORY_PATHS = [(("meta",), ("generated_by", "client_version", "recipe_hash")),
                  (("populations", "*"), ("label", "description")),
                  (("outputs", "*"), ("result_symbol", "description"))]

def digest_payload(ir_obj):
    p = copy.deepcopy(ir_obj)
    for path, keys in ADVISORY_PATHS:
        if len(path) == 1:
            if isinstance(p.get(path[0]), dict):
                p[path[0]] = {k: v for k, v in p[path[0]].items() if k not in keys}
        else:
            container = p.get(path[0])
            if isinstance(container, dict):
                for name, obj in container.items():
                    if isinstance(obj, dict):
                        container[name] = {k: v for k, v in obj.items() if k not in keys}
    return p

def digest(ir_obj):
    payload = jcs(digest_payload(ir_obj))
    return payload, hashlib.sha256(payload.encode("utf-8")).hexdigest()
