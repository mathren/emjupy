# Verifying emjupy changes

## Environment, once per session

The sandbox may have been reset: check before relying on any path.

```bash
# Emacs 30.1 is the target; Ubuntu 24.04's apt has only 29.3.  Build it
# (several minutes), with X for the graphical tests:
apt-get install -y -q build-essential autoconf texinfo libgnutls28-dev \
  libxml2-dev libncurses-dev libx11-dev libxaw7-dev libxpm-dev libjpeg-dev \
  libpng-dev libgif-dev libtiff-dev librsvg2-dev xvfb imagemagick
git clone -q --depth 1 --branch emacs-30.1 https://github.com/emacs-mirror/emacs.git /tmp/emacs-src
cd /tmp/emacs-src && ./autogen.sh && ./configure -q --prefix=/opt/emacs301 \
  --with-x-toolkit=lucid --without-native-compilation && make -j"$(nproc)" && make install
EMACS=/opt/emacs301/bin/emacs
# websocket.el, the one dependency
git clone --depth 1 https://github.com/ahyatt/emacs-websocket ~/vendor/websocket
export EMJUPY_WEBSOCKET_DIR=~/vendor/websocket
# Python for live tests: server, kernel, language server, and tooling
apt-get install -y -q texlive-latex-base dvipng   # LaTeX previews: the render test skips without
python3 -m venv /opt/jenv && /opt/jenv/bin/pip install -q \
  jupyter-server ipykernel jupyter-lsp 'python-lsp-server[all]' \
  matplotlib plotly ipywidgets anywidget tqdm nbformat nbstripout
# package-lint, the real one (MELPA is not reachable: use GitHub)
git clone --depth 1 https://github.com/purcell/package-lint ~/package-lint
```

`package-lint` checks dependencies are installable, so it needs an
archive listing `websocket`.  Build one with MELPA's own tools (see
`testbeds.md`, "MELPA's real build") and point `EMJUPY_PACKAGE_ARCHIVE`
at its `packages/`; in CI it reads MELPA.

## The checks

`scripts/check.sh REPO [full]` runs them -- `full` takes longer than a
sandbox command's 300 seconds, so start it in the background and poll:

```bash
(setsid nohup bash scripts/check.sh REPO full > /tmp/check.out 2>&1 < /dev/null &)
for i in $(seq 1 13); do grep -q "make check:" /tmp/check.out && break; sleep 20; done
```

Three tests need more than one server: the two of a second server
(`EMJUPY_TEST_URL2`, `EMJUPY_TEST_TOKEN2`) and the TRAMP one
(`EMJUPY_TEST_SSH_NOTEBOOK`, `EMJUPY_TEST_SSH_ENV`, run as another user);
`testbeds.md` sets both up.  Without them they skip.

A test skipped for something missing where the kernel runs ("tqdm is not
installed", "anywidget is not installed", "No python on PATH") is the
environment, not the change: install it and run that test alone.  Three
always skip without a second server or an SSH notebook configured.

By hand:

| Check | Command | Expect |
|---|---|---|
| compile | `make compile EMACS=$EMACS` | 0 warnings |
| unit tests | `make test EMACS=$EMACS` | 0 unexpected |
| lint | `make lint PACKAGE_LINT_DIR=~/package-lint EMJUPY_PACKAGE_ARCHIVE=<archive>` | exit 0 |
| full suites | `EMJUPY_TEST_URL=127.0.0.1:PORT EMJUPY_TEST_TOKEN=abc EMJUPY_TEST_ROOT=DIR make check` | 0 unexpected |
| recipe | `make recipe-check` | all listed, builds alone |
| version | `make check-version` | one version everywhere |
| layers | `python3 tools/dependency-graph.py OUT` | 0 against the layers |
| docs | `make docs && python3 tools/check-links.py html-content` | 0 broken |
| workflows | `actionlint -shellcheck= .github/workflows/*.yml` | clean |
| notebooks | `nbstripout --keep-id --verify test_notebooks/*.ipynb`; regenerate the corpus and `git diff --exit-code test_notebooks/test-corpus` | clean, no diff |
| each module alone | `$EMACS -batch -Q -L src -L $EMJUPY_WEBSOCKET_DIR --eval "(require 'emjupy-X)"` | all load |

A live server for `make check`:

```bash
mkdir -p /tmp/nbroot && cd /tmp/nbroot && setsid nohup /opt/jenv/bin/jupyter server \
  --allow-root --no-browser --port=18888 --ip=127.0.0.1 \
  --IdentityProvider.token=abc --ServerApp.root_dir=/tmp/nbroot \
  > /tmp/jupyter.log 2>&1 < /dev/null &
curl -sf "http://127.0.0.1:18888/api/status?token=abc"   # check it answers
```

## One test

`scripts/one_test.el` runs a single ERT test by name and prints
`ONE name PASS|FAIL time`:

```bash
REPO=/path/to/clone T=emjupy-test-some-name $EMACS -batch -Q -l scripts/one_test.el
```

Add the `EMJUPY_TEST_*` variables for a live test.

## Specialised checks

- **Same code** (refactors, prose passes):
  `BEFORE=dir/ AFTER=dir/ FILES="a.el b.el" $EMACS -batch -Q -l scripts/same_code.el`
  compares every file's forms with docstrings (including `ert-deftest`'s)
  set aside.  A reordering (options gathered at the top) needs the
  multiset comparison instead: sort the printed forms.
- **Docs tokens**: `python3 scripts/orgkeep.py OLD.org NEW.org`.
- **Differential**: load the old file in a scratch tree, run old and new
  on the same inputs (requests sent, rows produced, pages built, random
  writes to a terminal emulator), compare.  Under `cl-letf`, stub what
  reaches out (`emjupy--http-request`, `url-retrieve-synchronously`,
  `run-at-time` to run at once).
- **Old behaviour shown**: when fixing a bug, demonstrate it on the old
  code first (a script calling the old function), then the fix.
- **Under load**: `for k in 1 2 3 4; do (timeout 240 sh -c 'while :; do :; done' &); done`,
  run the test six times, then `pkill -f "^sh -c while"`.
- **Graphical behaviour** (menus, rendering): see `testbeds.md`.

## Reading results

`make check` prints `Ran N tests, M results as expected, K unexpected`.
Quote those numbers.  A live test failing once in a full run but passing
alone 5 times out of 5 is flaky: say so, and look for the race (drawing
from a timer, replies overtaking output, a timer settling things
early) before lengthening a timeout.

## Fuzzing

Fuzz on the standard testbed (testbeds.md): through TRAMP, several
environments, Eglot on, latency.  A run checks the buffer after every
step (sync first: typed text reaches the cells lazily), stops at the
first failure with the step's action and what came before, and ends by
restarting each kernel and running every cell, then saving and comparing
the server's copy with the buffer.  Fresh notebooks each run: a run
saves them.  Seeds fixed, so a failure replays.

## Skipped tests are results

A test skips when what it needs is missing -- a language server not
attached in time, no TRAMP environment.  Compare the skipped count with
the last full check's: a rise is a regression until explained, as a
failure is.  Six Eglot tests once went from passing to skipping, a
change having left their notebooks with no language server: the check
said 0 unexpected.
