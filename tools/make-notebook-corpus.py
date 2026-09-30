#!/usr/bin/env python3
"""Write the notebook corpus the round-trip tests read: one notebook per
shape a real .ipynb comes in.  Built with nbformat itself, so each is
valid by Jupyter's own definition.  Run from the repository root:

    python3 tools/make-notebook-corpus.py
"""
import base64, os
import nbformat
from nbformat import v4

OUT = os.path.join(os.path.dirname(__file__), "..", "src", "test-corpus")
PNG = base64.b64encode(bytes.fromhex(
    "89504e470d0a1a0a0000000d4948445200000001000000010806000000"
    "1f15c4890000000d49444154789c6360000002000154a24f5d0000000049454e44ae426082")).decode()

def save(name, nb, minor=5):
    nb.nbformat_minor = minor
    if minor < 5:                       # ids only exist from 4.5
        for c in nb.cells:
            c.pop("id", None)
    nbformat.validate(nb)
    with open(os.path.join(OUT, name + ".ipynb"), "w", encoding="utf-8") as f:
        nbformat.write(nb, f)

def kernel_metadata(nb):
    nb.metadata = {"kernelspec": {"name": "python3", "display_name": "Python 3", "language": "python"},
                   "language_info": {"name": "python", "version": "3.12.0"}}

os.makedirs(OUT, exist_ok=True)

nb = v4.new_notebook(); save("empty", nb)

nb = v4.new_notebook(); kernel_metadata(nb)
nb.cells = [v4.new_markdown_cell("# Title\n\nSome *text*."),
            v4.new_code_cell("x = 1\ny = 2", execution_count=1),
            v4.new_raw_cell("raw\ncontent")]
save("every-cell-type", nb)

nb = v4.new_notebook()
c = v4.new_code_cell("print('hi')\n1/0", execution_count=3)
c.outputs = [v4.new_output("stream", name="stdout", text="hi\n"),
             v4.new_output("stream", name="stderr", text="warn\n"),
             v4.new_output("execute_result", data={"text/plain": "42"}, execution_count=3),
             v4.new_output("display_data", data={"image/png": PNG, "text/plain": "<Figure>"},
                           metadata={"image/png": {"width": 1, "height": 1}}),
             v4.new_output("error", ename="ZeroDivisionError", evalue="division by zero",
                           traceback=["\x1b[31mTraceback\x1b[0m", "ZeroDivisionError"])]
nb.cells = [c]
save("every-output-type", nb)

nb = v4.new_notebook()
m = v4.new_markdown_cell("An image: ![p](attachment:p.png)")
m.attachments = {"p.png": {"image/png": PNG}}
nb.cells = [m]
save("attachments", nb)

nb = v4.new_notebook()
c = v4.new_code_cell("x = 1")
c.metadata = {"tags": ["parameters"], "collapsed": True, "jupyter": {"source_hidden": True},
              "scrolled": False}
nb.cells = [c]
save("cell-metadata", nb)

nb = v4.new_notebook()
nb.cells = [v4.new_markdown_cell("naïve café — ✓ 日本語 😀"),
            v4.new_code_cell("s = 'é'  # ünïcödé\nprint(s)")]
save("unicode", nb)

nb = v4.new_notebook()
nb.cells = [v4.new_code_cell("a = 1\r\nb = 2\r\n"), v4.new_code_cell("trailing newline\n")]
save("line-endings", nb)

nb = v4.new_notebook()
nb.cells = [v4.new_markdown_cell("no ids here"), v4.new_code_cell("x = 1")]
save("no-cell-ids-4.4", nb, minor=4)

nb = v4.new_notebook()
nb.cells = [v4.new_code_cell(f"x{i} = {i}", execution_count=i + 1) for i in range(1000)]
save("thousand-cells", nb)

# A notebook with very large output is built by the test itself, in memory:
# at 3 MB it is too heavy to keep in the repository.

print("wrote", len(os.listdir(OUT)), "notebooks to", os.path.normpath(OUT))
