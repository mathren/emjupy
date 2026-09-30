#!/usr/bin/env python3
"""Compare a notebook with emjupy's save of it; exit 1 on any loss.

    check-roundtrip.py ORIGINAL SAVED

The saved copy must validate, and hold what the original held: every
cell's type, source, metadata and attachments, and every output.  Cell
ids are compared only where the original had them -- a 4.4 notebook has
none, and saving it as 4.5 adds them, as the format requires.
"""
import json, sys
import nbformat

def text(v):
    return "".join(v) if isinstance(v, list) else v

def norm_output(o):
    o = dict(o)
    for k in ("text", "traceback"):
        if k in o and k == "text":
            o[k] = text(o[k])
    if "data" in o:
        o["data"] = {m: text(v) for m, v in o["data"].items()}
    return o

def cells(path):
    with open(path, encoding="utf-8") as f:
        nb = json.load(f)
    out = []
    for c in nb["cells"]:
        out.append({
            "cell_type": c["cell_type"],
            "id": c.get("id"),
            "source": text(c["source"]),
            "metadata": c.get("metadata", {}),
            "attachments": c.get("attachments"),
            "outputs": [norm_output(o) for o in c.get("outputs", [])],
            "execution_count": c.get("execution_count"),
        })
    return nb.get("metadata", {}), out

def main(orig, saved):
    problems = []
    try:
        nbformat.validate(nbformat.read(saved, as_version=4))
    except Exception as e:
        problems.append(f"saved copy does not validate: {e}")
    om, oc = cells(orig)
    sm, sc = cells(saved)
    if om != sm:
        problems.append(f"notebook metadata differs: {om} != {sm}")
    if len(oc) != len(sc):
        problems.append(f"{len(oc)} cells became {len(sc)}")
    for i, (a, b) in enumerate(zip(oc, sc)):
        for field in ("cell_type", "source", "metadata", "attachments", "outputs", "execution_count"):
            if a[field] != b[field]:
                problems.append(f"cell {i} {field}: {a[field]!r:.80} != {b[field]!r:.80}")
        if a["id"] is not None and a["id"] != b["id"]:
            problems.append(f"cell {i} id changed: {a['id']} -> {b['id']}")
    for p in problems:
        print(p)
    return 1 if problems else 0

if __name__ == "__main__":
    sys.exit(main(sys.argv[1], sys.argv[2]))
