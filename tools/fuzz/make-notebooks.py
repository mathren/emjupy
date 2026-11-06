#!/usr/bin/env python3
"""Write fresh fuzzing notebooks: make-notebooks.py DIR KIND.

KIND is sci (numpy, a matplotlib figure), plain (the standard library
only) or py311 (numpy below 2).  A fuzz run saves over them: make them
again before each run.
"""
import json, os, sys

def cell(kind, source):
    c = {"cell_type": kind, "id": os.urandom(4).hex(), "metadata": {},
         "source": source.splitlines(keepends=True)}
    if kind == "code":
        c.update(execution_count=None, outputs=[])
    return c

def notebook(kind):
    cells = [cell("markdown", f"# A {kind} notebook\n\nFuzzed: anything may happen."),
             cell("code", "import sys\nprint(sys.version.split()[0])"),
             cell("code", "total = 0\nfor i in range(5):\n    total += i\n    print(i)"),
             cell("markdown", "Some *emphasis*, and `code`.")]
    if kind == "sci":
        cells += [cell("code", "import numpy as np\nx = np.linspace(0, 6, 200)\nprint(np.__version__)"),
                  cell("code", "import matplotlib\nmatplotlib.use('module://matplotlib_inline.backend_inline')\n"
                               "import matplotlib.pyplot as plt\nplt.figure(figsize=(3, 2)); plt.plot(x, np.sin(x)); plt.show()")]
    elif kind == "py311":
        cells += [cell("code", "import numpy as np\nprint(np.__version__, np.arange(4).sum())")]
    else:
        cells += [cell("code", "import json\nprint(json.dumps({'total': total}))")]
    cells.append(cell("code", "print('done')"))
    return {"cells": cells, "nbformat": 4, "nbformat_minor": 5,
            "metadata": {"kernelspec": {"display_name": "Python 3", "language": "python", "name": "python3"}}}

if __name__ == "__main__":
    directory, kind = sys.argv[1], sys.argv[2]
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, {"sci": "a", "plain": "b", "py311": "c"}[kind] + ".ipynb")
    with open(path, "w") as f:
        json.dump(notebook(kind), f, indent=1)
    print(path)
