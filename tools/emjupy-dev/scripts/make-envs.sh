#!/bin/bash
# Three Python environments for the remote user: different packages, and a second Python version.
set -x
base="jupyter-server ipykernel jupyter-lsp python-lsp-server"
python3 -m venv ~/envs/sci && ~/envs/sci/bin/pip install -q $base numpy matplotlib
python3 -m venv ~/envs/plain && ~/envs/plain/bin/pip install -q $base
python3 -m venv ~/envs/tools && ~/envs/tools/bin/pip install -q uv
# A second Python: uv's standalone builds.  Run as a user without the
# sandbox's certificate settings, pass them: SSL_CERT_FILE=... make-envs.sh
~/envs/tools/bin/uv python install 3.11 && ~/envs/tools/bin/uv venv -q -p 3.11 ~/envs/py311 && \
  ~/envs/tools/bin/uv pip install -q --python ~/envs/py311/bin/python $base "numpy<2"
for e in sci plain py311; do echo "ENV $e: $(~/envs/$e/bin/python --version 2>&1); numpy $(~/envs/$e/bin/python -c 'import numpy; print(numpy.__version__)' 2>&1 | tail -1); jupyter $(test -x ~/envs/$e/bin/jupyter && echo yes || echo NO)"; done
echo ENVS-DONE
