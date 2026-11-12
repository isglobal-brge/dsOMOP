"""WS-2 tooling: build the IR golden corpus from plan captures.

usage: build_ir_corpus.py SERVER_CAPTURE_DIR CLIENT_CAPTURE_DIR OUT.json

Reads the JSON lines written by tools/ws2/plan_capture_helper.R during the
dsOMOP (with its sibling dsOMOPClient installed) and dsOMOPClient suites,
keeps every plan that reached a v1 server entry point or was built or sent by
the client, drops capture artefacts (S3 class tags), replaces resolved
workspace frames by the marker the server's .irJsonForm() uses, deduplicates
by canonical content and records up to six provenance sources per plan plus
whether a .planExecute call with it succeeded.
"""
import json, glob, hashlib, sys, collections
def load(pattern):
    recs=[]
    for f in sorted(glob.glob(pattern)):
        for l in open(f):
            l=l.strip()
            if l: recs.append(json.loads(l))
    return recs
def clean(x):
    if isinstance(x, dict):
        if '__frame__' in x and len(x) == 1:
            return {'__resolved_frame__': {'ncol': x['__frame__'].get('ncol')}}
        return {k: clean(v) for k, v in x.items() if k != '__class__'}
    if isinstance(x, list):
        return [clean(v) for v in x]
    return x
SERVER_FNS = {'.planExecute', '.planPreview', '.planValidate', '.planResolvePopulations'}
entries = []
for label, pat in (('dsOMOP', sys.argv[1] + '/*.jsonl'), ('dsOMOPClient', sys.argv[2] + '/*.jsonl')):
    recs = load(pat)
    exits = {r['id']: r.get('ok') for r in recs if r['kind'] == 'exit'}
    for r in recs:
        if r['kind'] not in ('entry', 'value') or not isinstance(r.get('plan'), dict):
            continue
        fn = r.get('fn')
        if r['kind'] == 'entry' and fn not in SERVER_FNS:
            continue
        ok = exits.get(r['id']) if r['kind'] == 'entry' else None
        entries.append(dict(package=label, file=r.get('file'), line=r.get('line'), test=r.get('test'),
                            fn=fn, ok=ok, plan=clean(r['plan'])))
by_hash = collections.OrderedDict()
for e in entries:
    h = hashlib.sha256(json.dumps(e['plan'], sort_keys=True, separators=(',', ':')).encode()).hexdigest()
    rec = by_hash.setdefault(h, dict(id='', plan=e['plan'], sources=[], executed=None))
    src = dict(package=e['package'], file=e['file'], line=e['line'], test=e['test'], fn=e['fn'])
    if src not in rec['sources']:
        rec['sources'].append(src)
    if e['fn'] == '.planExecute':
        rec['executed'] = bool(rec['executed']) or bool(e['ok'])
out = []
for i, (h, rec) in enumerate(by_hash.items(), 1):
    rec['id'] = 'v1-%03d' % i
    rec['sha256'] = h
    rec['sources'] = sorted(rec['sources'], key=lambda s: (s['package'], s['file'] or '', s['line'] or 0, s['fn'] or ''))[:6]
    out.append(rec)
json.dump(out, open(sys.argv[3], 'w'), indent=None, separators=(',', ':'), sort_keys=False)
print(len(entries), 'records ->', len(out), 'distinct plans')
print(collections.Counter(r['executed'] for r in out))
