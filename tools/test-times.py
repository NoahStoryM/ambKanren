#!/usr/bin/env python3
"""Time already-compiled book-test modules (one fresh Racket process per sample).
Run separately under the v2 and v3 PLTUSERHOME; no optimizations or retries.
"""
import argparse, json, pathlib, subprocess, time
p = argparse.ArgumentParser()
p.add_argument('--racket', required=True)
p.add_argument('--repo', default='.')
p.add_argument('--timeout', type=int, default=180)
p.add_argument('--repeats', type=int, default=1)
p.add_argument('--implementation', choices=('amb', 'native'), default='amb')
p.add_argument('--warmups', type=int, default=0)
a = p.parse_args()
for edition in ('trs1', 'trs2'):
    root = pathlib.Path(a.repo).resolve()
    module = root / (f'tools/native-{edition}.rkt' if a.implementation == 'native'
                     else f'miniKanren/amb/{edition}/tests/main.rkt')
    target = '(list \'submod path \'test)' if a.implementation == 'native' else 'path'
    expression = '(define path (string->path (vector-ref (current-command-line-arguments) 0))) (define-values (v cpu real gc) (time-apply dynamic-require (list ' + target + ' #f))) (printf "TIMING ~s\\n" (list cpu real gc))'
    for i in range(-a.warmups, a.repeats):
        start = time.perf_counter()
        try:
            r = subprocess.run([a.racket, '-e', expression, '--', str(module)],
                               capture_output=True, text=True, timeout=a.timeout)
            print(json.dumps(dict(implementation=a.implementation, edition=edition, sample=i, warmup=i < 0, exit=r.returncode,
                wall_seconds=time.perf_counter()-start, stdout=r.stdout, stderr=r.stderr)), flush=True)
        except subprocess.TimeoutExpired:
            print(json.dumps(dict(implementation=a.implementation, edition=edition, sample=i, warmup=i < 0, timeout_seconds=a.timeout)), flush=True)
