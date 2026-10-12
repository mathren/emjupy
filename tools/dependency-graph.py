#!/usr/bin/env python3
# Copyright (C) 2026 Mathieu Renzo
# SPDX-License-Identifier: GPL-3.0-or-later
"""Draw how emjupy's files depend on each other, read from the code.

  tools/dependency-graph.py OUTDIR

Reads each package file's `(require 'emjupy-...)' forms and writes
OUTDIR/dependencies.dot, for Graphviz, and OUTDIR/dependencies.svg,
drawn here: each file in a row by layer, above every file it requires,
so no install is needed.  The drawing leaves out a require that a longer
path already implies; the .dot file has every one.  A require from a file to one in its own or
a higher row, a cycle, would be drawn red; there should be none.
"""
import re
import sys
from pathlib import Path

SRC = Path(__file__).resolve().parent.parent / "src"
SKIP = {"emjupy-pkg", "emjupy-run-tests"}


def read_requires():
    files = {}
    for path in sorted(SRC.glob("emjupy*.el")):
        name = path.stem
        if name in SKIP or name.endswith("-test"):
            continue
        text = path.read_text(encoding="utf-8")
        files[name] = sorted(set(
            m.group(1) for m in re.finditer(r"^\(require '(emjupy[a-z0-9-]*)\)",
                                            text, re.M)))
    return files


def layers(files):
    """Each file's layer: one above the highest layer it requires."""
    depth = {}

    def walk(name, seen):
        if name in depth:
            return depth[name]
        if name in seen:            # a cycle: drawn red, not recursed
            return 0
        seen = seen | {name}
        depth[name] = 1 + max([walk(r, seen) for r in files.get(name, ())
                               if r in files] or [-1])
        return depth[name]

    for name in files:
        walk(name, frozenset())
    return depth


def reduced(files):
    """FILES with each require that a longer path implies left out."""
    def reaches(start, goal, skip_edge):
        stack, seen = [start], set()
        while stack:
            node = stack.pop()
            for nxt in files.get(node, ()):
                if (node, nxt) == skip_edge or nxt not in files or nxt in seen:
                    continue
                if nxt == goal:
                    return True
                seen.add(nxt)
                stack.append(nxt)
        return False
    return {name: [r for r in reqs if r in files and not reaches(name, r, (name, r))]
            for name, reqs in files.items()}


def short(name):
    return name if name == "emjupy" else name[len("emjupy-"):]


def dot(files, depth):
    out = ["digraph emjupy {", "  rankdir=BT;",
           '  node [shape=box, style="rounded", fontname="sans-serif"];']
    for layer in sorted(set(depth.values())):
        names = sorted(n for n in files if depth[n] == layer)
        out.append("  { rank=same; " + " ".join(f'"{short(n)}";' for n in names) + " }")
    for name, reqs in sorted(files.items()):
        for r in reqs:
            if r in files:
                upward = depth[r] >= depth[name]
                out.append(f'  "{short(name)}" -> "{short(r)}"'
                           + (' [color=red];' if upward else ';'))
    out.append("}")
    return "\n".join(out) + "\n"


def svg(files, depth):
    box_w, box_h, gap_x, gap_y, pad = 128, 30, 18, 62, 20
    rows = {}
    for name in files:
        rows.setdefault(depth[name], []).append(name)
    for layer in rows:
        rows[layer].sort()
    widest = max(len(r) for r in rows.values())
    width = pad * 2 + widest * box_w + (widest - 1) * gap_x
    top = max(rows)
    height = pad * 2 + (top + 1) * box_h + top * gap_y + 40
    place = {}
    for layer, names in rows.items():
        row_w = len(names) * box_w + (len(names) - 1) * gap_x
        x0 = (width - row_w) / 2
        y = pad + (top - layer) * (box_h + gap_y)
        for i, name in enumerate(names):
            place[name] = (x0 + i * (box_w + gap_x), y)
    parts = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{width:.0f}" height="{height:.0f}" '
             f'viewBox="0 0 {width:.0f} {height:.0f}" font-family="sans-serif" font-size="12">',
             '<defs><marker id="arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" '
             'markerHeight="7" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" '
             'fill="#888"/></marker></defs>',
             f'<rect width="100%" height="100%" fill="white"/>']
    edges = 0
    for name, reqs in sorted(files.items()):
        x1, y1 = place[name]
        for r in reqs:
            if r not in files:
                continue
            edges += 1
            x2, y2 = place[r]
            colour = "#c00" if depth[r] >= depth[name] else "#888"
            sx, sy = x1 + box_w / 2, y1 + box_h
            ex, ey = x2 + box_w / 2, y2
            parts.append(f'<path d="M {sx:.1f} {sy:.1f} C {sx:.1f} {sy + gap_y / 2:.1f} '
                         f'{ex:.1f} {ey - gap_y / 2:.1f} {ex:.1f} {ey:.1f}" fill="none" '
                         f'stroke="{colour}" stroke-opacity="0.55" marker-end="url(#arrow)"/>')
    for name, (x, y) in place.items():
        parts.append(f'<rect x="{x:.1f}" y="{y:.1f}" width="{box_w}" height="{box_h}" rx="6" '
                     f'fill="#f4f6fa" stroke="#456"/>')
        parts.append(f'<text x="{x + box_w / 2:.1f}" y="{y + box_h / 2 + 4:.1f}" '
                     f'text-anchor="middle">{short(name)}</text>')
    parts.append(f'<text x="{pad}" y="{height - 24:.0f}" fill="#555">{len(files)} files, '
                 f'each above the files it requires.</text>')
    parts.append(f'<text x="{pad}" y="{height - 8:.0f}" fill="#555">Lines a longer path '
                 f'implies are left out.</text>')
    parts.append("</svg>")
    return "\n".join(parts) + "\n", edges


def main():
    if len(sys.argv) != 2:
        sys.exit(__doc__.strip().split("\n\n")[1])
    out = Path(sys.argv[1])
    out.mkdir(parents=True, exist_ok=True)
    files = read_requires()
    depth = layers(files)
    (out / "dependencies.dot").write_text(dot(files, depth), encoding="utf-8")
    drawing, _ = svg(reduced(files), depth)
    edges = sum(1 for rs in files.values() for r in rs if r in files)
    (out / "dependencies.svg").write_text(drawing, encoding="utf-8")
    upward = sum(1 for n, rs in files.items() for r in rs
                 if r in files and depth[r] >= depth[n])
    print(f"dependency-graph: {len(files)} files, {edges} requires, "
          f"{upward} against the layers -> {out}/dependencies.svg")
    sys.exit(1 if upward else 0)


if __name__ == "__main__":
    main()
