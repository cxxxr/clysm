#!/usr/bin/env python3
"""Fail when a function, macro or generic function of the runtime is defined
twice at toplevel: the definition loaded later (tools/runtime-files.lisp gives
the order) silently replaces the earlier one, as a stub of
UPGRADED-COMPLEX-PART-TYPE in misc.lisp once replaced the real one in
subtypep.lisp.  A name that is meant to be defined again is listed in
ALLOWED with the reason."""
import collections, os, re, sys

ALLOWED = {
    # (kind, name): reason
}

root = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..')
order = re.findall(r'"([a-z0-9-]+)"', open(os.path.join(root, 'tools/runtime-files.lisp')).read())
defs = collections.defaultdict(list)
for name in order:
    path = os.path.join(root, 'runtime', name + '.lisp')
    with open(path, encoding='utf-8') as f:
        for i, line in enumerate(f, 1):
            m = re.match(r'\((defun|defmacro|defgeneric) +(\(setf [^)\s]+\)|[^\s()]+)', line)
            # a name with a comma is a template inside a macro
            if m and ',' not in m.group(2):
                defs[(m.group(1), m.group(2).lower())].append(f'runtime/{name}.lisp:{i}')
twice = {k: v for k, v in defs.items() if len(v) > 1 and k not in ALLOWED}
for (kind, name), places in sorted(twice.items()):
    print(f'{kind} {name} is defined more than once: {" ".join(places)}')
if twice:
    sys.exit(1)
print(f'defs: ok ({sum(len(v) for v in defs.values())} definitions, none twice)')
