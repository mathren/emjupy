# Copyright (C) 2026 Mathieu Renzo
# SPDX-License-Identifier: GPL-3.0-or-later
"""uses.py [SRC]: does each file require what it uses, and use what it requires?

Reports, per file: names used from files not below it (only keymaps and
the menu may), files used without being required, files required without
being used.  Compare its output before and after a change: nothing new.
"""
import re, glob, os, sys
SRC = sys.argv[1] if len(sys.argv) > 1 else '/tmp/RF/src'
files = sorted(f for f in glob.glob(f'{SRC}/emjupy*.el') if 'pkg' not in f)
name = lambda f: os.path.basename(f)[:-3]
defs, text, reqs = {}, {}, {}
def strip(s):   # drop comments and strings, roughly
    s = re.sub(r'"(?:[^"\\]|\\.)*"', '""', s, flags=re.S)
    return re.sub(r';[^\n]*', '', s)
for f in files:
    s = open(f, encoding='utf-8').read(); n = name(f)
    for m in re.finditer(r'^\((?:cl-)?def[a-z-]* \(?([^\s()]+)', s, re.M): defs.setdefault(m.group(1), n)
    for m in re.finditer(r"^\(transient-define-prefix ([^\s()]+)", s, re.M): defs.setdefault(m.group(1), n)
    text[n] = strip(s); reqs[n] = set(re.findall(r"^\(require '(emjupy[a-z-]*)\)", s, re.M))
uses = {}
for n, t in text.items():
    u = set()
    for tok in set(re.findall(r'(?<![\w-])(emjupy-[a-z0-9-]*[a-z0-9])', t)):
        d = defs.get(tok)
        if d and d != n: u.add(d)
    uses[n] = u
def closure(n, seen=None):
    seen = seen or set()
    for r in reqs.get(n, ()):
        if r not in seen: seen.add(r); closure(r, seen)
    return seen
problems = 0
for n in sorted(text):
    if n == 'emjupy': continue
    below = closure(n)
    up = sorted(u for u in uses[n] if u not in below)
    undeclared = sorted(u for u in uses[n] if u in below and u not in reqs[n])
    unused = sorted(r for r in reqs[n] if r not in uses[n])
    if up or undeclared or unused:
        problems += 1
        print(f"  {n}: " + '; '.join(x for x in [
            up and f"uses files not below it: {', '.join(up)}",
            undeclared and f"uses without requiring: {', '.join(undeclared)}",
            unused and f"requires without using: {', '.join(unused)}"] if x))
print(f"USES {len(text)} files; {problems} with a problem")
