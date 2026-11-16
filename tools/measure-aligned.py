#!/usr/bin/env python3
"""Alternate stream/amb book tests; keep raw samples and median/range summaries.
Precompile the four targets with raco make. Do not run other benchmarks or
compilation concurrently. Allocations include module loading and test checks.
"""
import argparse
import datetime
import json
import hashlib
import os
from pathlib import Path
import platform
import statistics
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--racket', required=True)
parser.add_argument('--reference', required=True, type=Path)
parser.add_argument('--output', required=True, type=Path)
parser.add_argument('--repeats', type=int, default=3)
parser.add_argument('--timeout', type=int, default=240)
args = parser.parse_args()
if args.repeats < 3:
    parser.error('at least three samples per implementation are required')
repo = Path(__file__).resolve().parent.parent
args.output.mkdir(parents=True, exist_ok=True)
env = dict(os.environ, LC_ALL='C.UTF-8')

def command(argv):
    return subprocess.check_output(argv, text=True, env=env).strip()

def commit(path):
    return command(['git', '-C', str(path), 'rev-parse', 'HEAD'])

cpu = next((line.split(':', 1)[1].strip() for line in
            Path('/proc/cpuinfo').read_text().splitlines()
            if line.startswith('model name')), 'unknown')
metadata = dict(date=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                racket=command([args.racket, '--version']),
                system=platform.platform(), cpu=cpu,
                visible_cpu_count=os.cpu_count(),
                cpu_affinity=sorted(os.sched_getaffinity(0)),
                ambkanren_base=commit(repo),
                ambkanren_working_tree_patch=command(['git', '-C', str(repo), 'diff', '--', 'miniKanren', 'microKanren', 'private', 'tools/native-trs1.rkt']),
                source_sha256={str(path.relative_to(repo)): hashlib.sha256(path.read_bytes()).hexdigest()
                               for path in sorted(set(repo.glob('miniKanren/amb/trs*/**/*.rkt')) |
                                                  set(repo.glob('tools/native-trs*.rkt')) |
                                                  {repo / 'private/search.rkt'})},
                reference=commit(args.reference),
                amb=command(['git', '-C', str(repo.parent / 'amb'), 'rev-parse', 'HEAD']),
                samples_per_implementation=args.repeats,
                order='trs1: native, amb, repeated; then trs2: native, amb, repeated',
                units=dict(cpu_ms='ms', real_ms='ms', gc_ms='ms', allocated_bytes='bytes'),
                package_links=command([str(Path(args.racket).with_name('raco')), 'pkg', 'show', '-l',
                                       'amb', 'typed-amb', 'miniKanren-typed', 'ambKanren']))
(args.output / 'environment.json').write_text(json.dumps(metadata, ensure_ascii=False, indent=2) + '\n')
rows = []
with (args.output / 'samples.jsonl').open('w') as output:
    for edition in ('trs1', 'trs2'):
        for sample in range(1, args.repeats + 1):
            for implementation in ('native', 'amb'):
                if implementation == 'native':
                    module = repo / f'tools/native-{edition}.rkt'
                    spec = f'(submod (file {json.dumps(str(module))}) test)'
                else:
                    module = repo / f'miniKanren/amb/{edition}/tests/main.rkt'
                    spec = f'(file {json.dumps(str(module))})'
                argv = [args.racket, str(repo / 'tools/measure-aligned.rkt'), spec]
                result = subprocess.run(argv, capture_output=True, text=True,
                                        timeout=args.timeout, env=env)
                if result.returncode or result.stderr:
                    raise RuntimeError(f'{edition} {implementation}: {result.stdout}\n{result.stderr}')
                metrics = json.loads(result.stdout.splitlines()[-1])
                row = dict(edition=edition, implementation=implementation, sample=sample,
                           command=argv, stdout=result.stdout, stderr=result.stderr,
                           returncode=result.returncode, **metrics)
                rows.append(row)
                output.write(json.dumps(row, ensure_ascii=False) + '\n')
                output.flush()
                print(json.dumps({k: v for k, v in row.items()
                                  if k not in ('command', 'stdout', 'stderr')}) , flush=True)
summary = {}
for edition in ('trs1', 'trs2'):
    summary[edition] = {}
    for implementation in ('native', 'amb'):
        samples = [r for r in rows if r['edition'] == edition and r['implementation'] == implementation]
        summary[edition][implementation] = {
            key: dict(median=statistics.median(r[key] for r in samples),
                      min=min(r[key] for r in samples), max=max(r[key] for r in samples))
            for key in ('cpu_ms', 'real_ms', 'gc_ms', 'allocated_bytes')}
    summary[edition]['cpu_ratio'] = (summary[edition]['amb']['cpu_ms']['median'] /
                                     summary[edition]['native']['cpu_ms']['median'])
(args.output / 'summary.json').write_text(json.dumps(summary, ensure_ascii=False, indent=2) + '\n')
