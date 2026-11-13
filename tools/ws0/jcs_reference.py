"""Reference RFC 8785 canonicaliser and semantic digest (Python), shared by
gen_schema_artefacts.py (which writes the vectors) and check_contracts.py (the
independent CI oracle). Mirrors R/semantic_digest.R: ECMAScript Number::toString
numbers, UTF-16 key order, advisory properties stripped at their owning paths only.
"""
import copy, decimal, hashlib, json, os

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


# ---------------------------------------------------------------------------
# Differential number corpus (tests/testthat/fixtures/es6-number-corpus.csv):
# the R serialiser is compared against this reference row by row. Groups:
#   random        seeded random bit patterns (finite doubles)
#   appendix_b    RFC 8785 Appendix B and the design-review counterexamples
#   power_of_two  every 2^k, k = -1074..1023 (asymmetric rounding intervals)
#   adjacent      the doubles just below and just above every power of two
#   near_pow2     seeded random doubles within 64 ulp of a power of two
#   review        the re-review's R/ECMAScript mismatches (B1)
# Usage: python3 tools/ws0/jcs_reference.py --corpus <out.csv> [<existing.csv>]
# An existing corpus keeps its random rows (and their order) so the file only
# grows; expectations are always recomputed here.
# ---------------------------------------------------------------------------
import math, random, struct

def _hex(x):
    return struct.pack(">d", x).hex()

def _dbl(hexstr):
    return struct.unpack(">d", bytes.fromhex(hexstr))[0]

REVIEW_HEX = ["0060000000000000", "0100000000000000", "0420000000000000", "0660000000000000", "3e70000000000000"]

def number_corpus(existing=None, seed=20261112, n_random=2102, n_near=600):
    rows, seen = [], set()
    def add(group, x):
        h = _hex(x)
        if h in seen or x != x or x in (float("inf"), float("-inf")):
            return
        seen.add(h); rows.append((h, es6_number(x), group))
    if existing and os.path.exists(existing):
        import csv
        for r in csv.DictReader(open(existing, newline="", encoding="utf-8")):
            if r.get("group", "random") == "random":
                add("random", _dbl(r["hex"]))
    rng = random.Random(seed)
    while sum(1 for r in rows if r[2] == "random") < n_random:
        add("random", _dbl("%016x" % rng.getrandbits(64)))
    rng = random.Random(seed + 1)   # the near-power-of-two sample never depends on how many random rows were kept
    for h in ("0000000000000000", "8000000000000000", "0000000000000001", "8000000000000001", "7fefffffffffffff", "ffefffffffffffff",
              "4340000000000000", "c340000000000000", "4430000000000000", "44b52d02c7e14af5", "44b52d02c7e14af6", "44b52d02c7e14af7",
              "444b1ae4d6e2ef4e", "444b1ae4d6e2ef4f", "444b1ae4d6e2ef50", "3eb0c6f7a0b5ed8c", "3eb0c6f7a0b5ed8d", "41b3de4355555553",
              "41b3de4355555554", "41b3de4355555555", "41b3de4355555556", "41b3de4355555557", "becbf647612f3696", "43143ff3c1cb0959",
              "e17cd1f192002619"):
        add("appendix_b", _dbl(h))
    for k in range(-1074, 1024):
        x = math.ldexp(1.0, k)
        add("power_of_two", x)
        add("power_of_two", -x)
    for k in range(-1074, 1024):
        x = math.ldexp(1.0, k)
        add("adjacent", math.nextafter(x, math.inf))
        add("adjacent", math.nextafter(x, -math.inf))
    for _ in range(n_near):
        k = rng.randint(-1074, 1023); x = math.ldexp(1.0, k); steps = rng.randint(2, 64); sign = rng.choice((1, -1))
        for _s in range(steps):
            x = math.nextafter(x, math.inf if sign > 0 else -math.inf)
        add("near_pow2", x)
    for h in REVIEW_HEX:
        add("review", _dbl(h))
    return rows

if __name__ == "__main__":
    import csv, os, sys
    if len(sys.argv) >= 3 and sys.argv[1] == "--corpus":
        out = sys.argv[2]; existing = sys.argv[3] if len(sys.argv) > 3 else out
        rows = number_corpus(existing)
        with open(out, "w", newline="", encoding="utf-8") as fh:
            w = csv.writer(fh); w.writerow(["hex", "expected", "group"]); w.writerows(rows)
        from collections import Counter
        print(len(rows), "rows", dict(Counter(r[2] for r in rows)))
    else:
        print(__doc__)
