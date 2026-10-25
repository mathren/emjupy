#!/usr/bin/env python3
# Copyright (C) 2026 Mathieu Renzo
# SPDX-License-Identifier: GPL-3.0-or-later
"""orgkeep.py OLD NEW: what NEW lost of OLD's code spans, links and page links.

For a docs rewrite meant to lose no information.  A reported loss is not
always real: a span pattern can match the text BETWEEN two adjacent
=spans=; check each against NEW by hand.  Exits 1 if anything is lost.
"""
import re, sys
def tokens(t):
    out = set()
    out |= set(re.findall(r'=([^=\n]+?)=', t))
    out |= set(re.findall(r'~([^~\n]+?)~', t))
    out |= set(re.findall(r'\[\[([^\]]+)\]', t))
    out |= set(re.findall(r'^#\+(?:PREVIOUS|NEXT)_PAGE: *(\S+)', t, re.M))
    return out
old = open(sys.argv[1], encoding='utf-8').read()
new = open(sys.argv[2], encoding='utf-8').read()
lost = sorted(tokens(old) - tokens(new))
print(f"{sys.argv[2]}: {old.count(chr(10))} -> {new.count(chr(10))} lines, "
      f"{len(old.split())} -> {len(new.split())} words; tokens lost: {len(lost)}")
for t in lost:
    print("   lost:", t[:100])
sys.exit(1 if lost else 0)
