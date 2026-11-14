#!/usr/bin/env python3
"""Regenerate client Rd on the pod with the required roxygen version."""
from pathlib import Path
import hashlib
import json
import os
import subprocess

root = Path('/root/rt/wt/audit/dsOMOPClient')
def snapshot():
    paths = list(root.joinpath('man').glob('*.Rd')) + [root / 'NAMESPACE', root / 'DESCRIPTION']
    return {str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}
before = snapshot()
env = dict(os.environ, R_LIBS_USER='/root/rt/numeric-deps:/root/rt/labels-deps', R_LIBS='/root/rt/lib/wt_audit_dsOMOPClient')
subprocess.run(['Rscript', '-e', 'stopifnot(as.character(packageVersion("roxygen2")) == "7.3.3"); roxygen2::roxygenise("/root/rt/wt/audit/dsOMOPClient")'], env=env, check=True)
after = snapshot()
changes = {key: dict(before=before.get(key), after=after.get(key)) for key in sorted(before.keys() | after.keys()) if before.get(key) != after.get(key)}
print(json.dumps(dict(roxygen_version='7.3.3', changes=changes), indent=2))
if changes:
    raise SystemExit('Documentation changed; inspect and pull only named Rd/NAMESPACE/DESCRIPTION changes.')
