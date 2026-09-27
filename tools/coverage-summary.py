#!/usr/bin/env python3
"""Summarise an lcov file: coverage per source file and in total.

Written as Markdown, so the same output reads in a terminal and in a
GitHub job summary.  Exits non-zero if the file holds no data, since a
report of nothing usually means the instrumentation never ran.
"""
import os
import sys


def parse(path):
    files, current = {}, None
    for line in open(path, encoding="utf-8"):
        line = line.strip()
        if line.startswith("SF:"):
            current = os.path.basename(line[3:])
            files.setdefault(current, [0, 0])
        elif line.startswith("DA:") and current:
            hits = int(line[3:].split(",")[1])
            files[current][1] += 1
            if hits > 0:
                files[current][0] += 1
        elif line == "end_of_record":
            current = None
    return files


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "coverage/lcov.info"
    if not os.path.exists(path):
        sys.exit(f"no coverage report at {path}")
    files = parse(path)
    if not files:
        sys.exit(f"{path} holds no coverage data -- was anything instrumented?")
    rows, hit, total = [], 0, 0
    for name, (h, t) in sorted(files.items()):
        hit, total = hit + h, total + t
        rows.append(f"| `{name}` | {h} / {t} | {100 * h / t:.1f}% |" if t else f"| `{name}` | 0 / 0 | -- |")
    print("| File | Lines run | Coverage |")
    print("|------|-----------|----------|")
    print("\n".join(rows))
    print(f"| **Total** | **{hit} / {total}** | **{100 * hit / total:.1f}%** |")


if __name__ == "__main__":
    main()
