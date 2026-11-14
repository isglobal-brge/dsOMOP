#!/usr/bin/env python3
"""Run a command on the existing test pod; never creates or stops it."""
import subprocess, sys
host,port = subprocess.check_output(['python3','/workspace/astra/runpod/deckpod.py','ssh-target'],text=True).split()
ssh=['ssh','-i','/home/dev/.ssh/runpod_cover','-o','StrictHostKeyChecking=no','-o','UserKnownHostsFile=/dev/null','-o','LogLevel=ERROR','-p',port,'root@'+host]
raise SystemExit(subprocess.call(ssh+[sys.argv[1]]))
