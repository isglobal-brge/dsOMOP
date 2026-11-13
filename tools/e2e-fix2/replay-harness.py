#!/usr/bin/env python3
"""Run only the retained harness probes through the authorized pod runner."""
import os, pathlib, shutil, subprocess
base = pathlib.Path(__file__).resolve().parent
harness = pathlib.Path(os.environ.get('E2E_HARNESS', '/tmp/claude-1000/-workspace-app/a9e21344-5d10-4451-a8b6-63b53826c6ca/scratchpad/dsomop-e2e'))
p = base / 'harness-probe'
if p.exists():
    raise SystemExit('Remove the previous owned harness-probe only after retaining its logs')
(p / 'tests/testthat').mkdir(parents=True)
(p / 'DESCRIPTION').write_text('Package: e2efix2probe\nVersion: 0.0.1\nTitle: Targeted Harness Probe\nDescription: Targeted harness assertions.\nLicense: MIT\nAuthor: Test\nMaintainer: Test <test@example.org>\nSuggests: testthat, withr, DBI, RPostgres\nConfig/testthat/edition: 3\n')
for name in ['scripts/g_contract.R', 'scripts/common.R', 'scripts/g_lib.R', 'scripts/scenario_G.R', 'scripts/tests/g-contract.R', 'scripts/omopbig.sh', 'pkgs/dsOMOP/DESCRIPTION']:
    dst = p / name
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(harness / name, dst)
shutil.copyfile(base / 'harness-test.R', p / 'tests/testthat/test-harness.R')
subprocess.run(['/workspace/dsomop/tools/rtest.sh', str(p), '^harness$', '180'], check=True)
print('Retain the pod detail log and remove this owned probe after inspection:', p)
