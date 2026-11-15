#!/usr/bin/env python3
"""Time already-compiled book-test modules (one fresh Racket process per sample).
Run separately under the v2 and v3 PLTUSERHOME; no optimizations or retries.
"""
import argparse, json, os, pathlib, subprocess, time
p = argparse.ArgumentParser()
p.add_argument('--racket', required=True)
p.add_argument('--repo', default='.')
p.add_argument('--timeout', type=int, default=180)
p.add_argument('--repeats', type=int, default=1)
a = p.parse_args()
for edition in ('trs1', 'trs2'):
    module = pathlib.Path(a.repo).resolve() / f'miniKanren/amb/{edition}/tests/main.rkt'
    expression = '(define-values (v cpu real gc) (time-apply dynamic-require (list (string->path (vector-ref (current-command-line-arguments) 0)) #f))) (printf "TIMING ~s\\n" (list cpu real gc))'
    for i in range(a.repeats):
        start = time.perf_counter()
        try:
            r = subprocess.run([a.racket, '-e', expression, '--', str(module)],
                               capture_output=True, text=True, timeout=a.timeout)
            print(json.dumps(dict(edition=edition, sample=i, exit=r.returncode,
                wall_seconds=time.perf_counter()-start, stdout=r.stdout, stderr=r.stderr)), flush=True)
        except subprocess.TimeoutExpired:
            print(json.dumps(dict(edition=edition, sample=i, timeout_seconds=a.timeout)), flush=True)
