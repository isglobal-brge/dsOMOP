#!/usr/bin/env python3
"""Remove only this task's disposable pod copies, after saving evidence locally."""
from pathlib import Path
import json
import os
import shutil

roots = [Path(p) for p in (
    '/root/rt/wt/audit/dsOMOP', '/root/rt/wt/audit/dsOMOPClient',
    '/root/rt/lib/wt_audit_dsOMOP', '/root/rt/lib/wt_audit_dsOMOPClient',
    '/root/rt/audit-fix4-fixtures', '/root/rt/audit-fix4-deps',
    '/root/rt/audit-fix4-artifacts')]
for proc in Path('/proc').iterdir():
    if not proc.name.isdigit():
        continue
    try:
        if proc.joinpath('comm').read_text().strip() not in ('R', 'Rscript'):
            continue
        text = proc.joinpath('cmdline').read_bytes() + proc.joinpath('environ').read_bytes()
        cwd = os.readlink(proc / 'cwd')
    except (OSError, PermissionError):
        continue
    if any(str(root).encode() in text or cwd.startswith(str(root)) for root in roots):
        raise SystemExit('Refusing cleanup: active R process uses task resources, PID ' + proc.name)
results = []
for root in roots:
    if root.is_symlink():
        raise SystemExit('Refusing symlink root: ' + str(root))
    if not root.exists():
        results.append(dict(path=str(root), status='already absent', bytes=0))
        continue
    size = sum(p.lstat().st_size for p in root.rglob('*') if p.is_file() or p.is_symlink())
    shutil.rmtree(root)
    results.append(dict(path=str(root), status='deleted', bytes=size))
print(json.dumps(results, indent=2))
