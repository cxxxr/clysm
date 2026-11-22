#!/usr/bin/env python3
"""Ticks of the V8 profiler placed on the machine code of Wasm functions.

  tools/iprof.py run OUT.wasm [DIR [ARG...]]   run the module under
        node --prof --print-wasm-code; the log and the code go to DIR
        (default build/iprof).  With the REPL image as OUT.wasm and a file
        to load as ARG, the code of a script or of fasls (the listing of
        an image is some hundred megabytes)
  tools/iprof.py top [DIR] [N]          the N functions with the most ticks
  tools/iprof.py show NAME [DIR] [MIN]  the code of the function whose name
        contains NAME (its TurboFan code with the most ticks), each
        instruction with its ticks; with MIN, only the instructions with at
        least MIN ticks and two lines around them

The ticks are those of the thread that runs the program (the isolate with
the most ticks).  A tick is charged to the instruction after the one that
was executing (the pc is the next instruction's).
"""
import os, re, subprocess, sys, bisect, collections

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def run(wasm, d, args=()):
    os.makedirs(d, exist_ok=True)
    for f in os.listdir(d):
        if f.endswith('.log') or f == 'code.txt':
            os.remove(os.path.join(d, f))
    with open(os.path.join(d, 'code.txt'), 'w') as out:
        subprocess.call(['node', '--prof', '--prof-sampling-interval=100', '--logfile=v8-%p.log',
                         '--print-wasm-code', '--max-semi-space-size=256',
                         os.path.join(ROOT, 'host', 'run.mjs'), os.path.abspath(wasm)]
                        + [os.path.abspath(x) if os.path.exists(x) else x for x in args],
                        stdout=out, stderr=subprocess.STDOUT, cwd=d)


def load(d):
    logs = [os.path.join(d, f) for f in os.listdir(d) if f.endswith('.log')]
    best = None
    for f in logs:
        n = 0
        with open(f, errors='replace') as h:
            for line in h:
                if line.startswith('tick,'):
                    n += 1
        if best is None or n > best[0]:
            best = (n, f)
    codes = []          # (start, size, name, optimized, order)
    ticks = []
    with open(best[1], errors='replace') as h:
        for i, line in enumerate(h):
            if line.startswith('code-creation,'):
                p = line.rstrip('\n').split(',')
                try:
                    start = int(p[4], 16); size = int(p[5])
                except ValueError:
                    continue
                codes.append((start, size, p[6], line.rstrip('\n').endswith('*'), i))
            elif line.startswith('tick,'):
                p = line.split(',')
                ticks.append((int(p[1], 16), i))
    return codes, ticks


def place(codes, ticks):
    codes_sorted = sorted(codes)
    starts = [c[0] for c in codes_sorted]
    per = collections.defaultdict(lambda: collections.Counter())
    other = 0
    for pc, order in ticks:
        i = bisect.bisect_right(starts, pc) - 1
        hit = None
        # (several code objects may have had the address: the last made before the tick)
        j = i
        while j >= 0 and starts[j] + 0x100000 > pc:
            c = codes_sorted[j]
            if c[0] <= pc < c[0] + c[1] and c[4] < order:
                if hit is None or c[4] > hit[4]:
                    hit = c
            j -= 1
        if hit is None:
            other += 1
        else:
            per[hit][pc - hit[0]] += 1
    return per, other


def listing(d, start):
    """The instructions of the code object at START: [(offset, text)]."""
    want = '0x%x' % start
    out = []
    inside = False
    with open(os.path.join(d, 'code.txt'), errors='replace') as h:
        for line in h:
            if not inside:
                if line.startswith(want + ' '):
                    inside = True
                else:
                    continue
            m = re.match(r'0x[0-9a-f]+\s+([0-9a-f]+)\s+[0-9a-f]+\s+(.*)', line)
            if not m:
                break
            out.append((int(m.group(1), 16), m.group(2).rstrip()))
    return out


def main():
    a = sys.argv[1:]
    if not a:
        print(__doc__); return
    if a[0] == 'run':
        run(a[1], a[2] if len(a) > 2 else os.path.join(ROOT, 'build', 'iprof'), a[3:])
        return
    if a[0] == 'top':
        d = a[1] if len(a) > 1 else os.path.join(ROOT, 'build', 'iprof')
        n = int(a[2]) if len(a) > 2 else 30
        per, other = place(*load(d))
        total = sum(sum(c.values()) for c in per.values()) + other
        rows = sorted(((sum(c.values()), k) for k, c in per.items()), reverse=True)[:n]
        print('%7d ticks, %d outside the code objects of the log' % (total, other))
        for t, k in rows:
            print('%7d %5.1f%%  %s%s' % (t, 100.0 * t / total, k[2], ' *' if k[3] else ''))
        return
    if a[0] == 'show':
        name = a[1]
        d = a[2] if len(a) > 2 else os.path.join(ROOT, 'build', 'iprof')
        low = int(a[3]) if len(a) > 3 else None
        per, other = place(*load(d))
        cands = [(sum(c.values()), k) for k, c in per.items() if name in k[2]]
        if not cands:
            print('no ticks in a function named like', name); return
        t, k = max(cands)
        ins = listing(d, k[0])
        print('%s%s: %d ticks, %d bytes' % (k[2], ' *' if k[3] else '', t, k[1]))
        c = per[k]
        if low is None:
            for off, text in ins:
                print('%6s %5x  %s' % (c[off] or '', off, text))
        else:
            keep = set()
            for i, (off, text) in enumerate(ins):
                if c[off] >= low:
                    keep.update(range(max(0, i - 3), min(len(ins), i + 2)))
            last = None
            for i, (off, text) in enumerate(ins):
                if i in keep:
                    if last is not None and i != last + 1:
                        print('   ...')
                    print('%6s %5x  %s' % (c[off] or '', off, text))
                    last = i
        return
    print(__doc__)


main()
