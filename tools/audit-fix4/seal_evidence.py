#!/usr/bin/env python3
"""Inventory the saved audit evidence; never reads a database or network."""
from pathlib import Path
import gzip
import hashlib
import json
import re

root = Path(__file__).resolve().parent
runs = []
for path in sorted(root.glob('*.full.log')):
    text = path.read_text(errors='replace')
    files = list(dict.fromkeys(re.findall(r'^([a-z][a-z0-9-]+):', text, re.M)))
    results = re.findall(r'RESULT failed: (\d+) errors: (\d+) skipped: (\d+) passed: (\d+)', text)
    runs.append(dict(log=path.name, files=['test-' + x + '.R' for x in files],
        result=dict(zip(['failed', 'errors', 'skipped', 'passed'], map(int, results[-1]))) if results else None))
(root / 'run-inventory.json').write_text(json.dumps(runs, indent=2) + '\n')
entries = []
for path in sorted(root.iterdir()):
    if not path.is_file() or path.name == 'evidence-manifest.json':
        continue
    data = path.read_bytes()
    entry = dict(path=path.name, bytes=len(data), sha256=hashlib.sha256(data).hexdigest())
    if path.suffix == '.gz':
        raw = gzip.decompress(data)
        entry.update(uncompressed_bytes=len(raw), uncompressed_sha256=hashlib.sha256(raw).hexdigest())
    entries.append(entry)
(root / 'evidence-manifest.json').write_text(json.dumps(dict(schema_version=1, files=entries), indent=2) + '\n')
print('Sealed', len(entries), 'evidence files and', len(runs), 'full test logs')
