#!/bin/bash
# Copyright (C) 2026 Mathieu Renzo
# SPDX-License-Identifier: GPL-3.0-or-later
# check.sh REPO [full] -- verify an emjupy clone.
# quick: compile (0 warnings), unit tests, real lint.
# full:  also the recipe check and the full suites against a live server.
# Environment: EMACS (Emacs 30.1+), EMJUPY_WEBSOCKET_DIR, PACKAGE_LINT_DIR,
# EMJUPY_PACKAGE_ARCHIVE (an archive listing websocket), and for full,
# JUPYTER (default jupyter) and PORT (default 18888).  EMJUPY_TEST_URL2 and
# EMJUPY_TEST_TOKEN2, if set, reach the tests of a second server; see
# references/testbeds.md.  The TRAMP test runs separately, as another user.
set -u
REPO=${1:?usage: check.sh REPO [full]}; MODE=${2:-quick}
EMACS=${EMACS:-emacs}; PORT=${PORT:-18888}; JUPYTER=${JUPYTER:-jupyter}
cd "$REPO" || exit 2
v=$("$EMACS" -batch -Q --eval '(princ emacs-version)' 2>/dev/null)
case "$v" in 30.*|3[1-9].*) ;; *) echo "check.sh: Emacs $v; emjupy needs 30.1 or later -- set EMACS"; exit 2;; esac
: "${EMJUPY_WEBSOCKET_DIR:?set EMJUPY_WEBSOCKET_DIR to a websocket.el checkout}"
export EMJUPY_WEBSOCKET_DIR
# A UTF-8 locale, as a user's terminal: format-message then curls quotes,
# which tests matching messages must allow for.
export LANG=C.UTF-8 LC_ALL=C.UTF-8
status=0
make -s clean >/dev/null 2>&1
make compile EMACS="$EMACS" > /tmp/check-compile.log 2>&1; s=$?
w=$(grep -aci warning /tmp/check-compile.log)
echo "compile: exit $s, $w warning(s)"; [ $s -ne 0 ] || [ "$w" -ne 0 ] && { grep -a -A2 -i "warning\|error" /tmp/check-compile.log | head -8; status=1; }
make -s clean >/dev/null 2>&1
lhome=$(mktemp -d)
HOME=$lhome make -s lint EMACS="$EMACS" ${PACKAGE_LINT_DIR:+PACKAGE_LINT_DIR="$PACKAGE_LINT_DIR"} \
  ${EMJUPY_PACKAGE_ARCHIVE:+EMJUPY_PACKAGE_ARCHIVE="$EMJUPY_PACKAGE_ARCHIVE"} > /tmp/check-lint.log 2>&1; s=$?
echo "lint (checkdoc + package-lint): exit $s"; [ $s -ne 0 ] && { grep -a "^  emjupy\|: error:\|: warning:\|not installed\|No package archive" /tmp/check-lint.log | head -8; status=1; }
rm -rf "$lhome"
if [ "$MODE" = full ]; then
  make -s recipe-check EMACS="$EMACS" || status=1
  root=$(mktemp -d)
  if curl -sf -o /dev/null "http://127.0.0.1:$PORT/api/status?token=abc"; then
    # A server already on PORT: the tests must be told ITS root, or those
    # reading saved files there skip.
    root=$("$JUPYTER" server list --json 2>/dev/null | python3 -c "import sys, json
for l in sys.stdin:
    d = json.loads(l)
    if str(d.get('port')) == '$PORT': print(d.get('root_dir')); break")
    [ -n "$root" ] || { echo "check.sh: a server answers on $PORT but jupyter server list does not name its root; stop it"; exit 2; }
  else
    (cd "$root" && setsid nohup "$JUPYTER" server --allow-root --no-browser --port="$PORT" --ip=127.0.0.1 \
       --IdentityProvider.token=abc --ServerApp.root_dir="$root" > /tmp/check-jupyter.log 2>&1 < /dev/null &)
    for i in $(seq 1 40); do curl -sf -o /dev/null "http://127.0.0.1:$PORT/api/status?token=abc" && break; sleep 1; done
  fi
  EMJUPY_TEST_URL=127.0.0.1:$PORT EMJUPY_TEST_TOKEN=abc EMJUPY_TEST_ROOT="$root" EMJUPY_SKIP_UNSTABLE=1 \
    timeout 600 make check EMACS="$EMACS" > /tmp/check-suite.log 2>&1; s=$?
else
  timeout 600 make test EMACS="$EMACS" > /tmp/check-suite.log 2>&1; s=$?
fi
echo "$([ "$MODE" = full ] && echo "make check" || echo "make test"): exit $s"
grep -aE "^Ran |FAILED  " /tmp/check-suite.log | head -8
[ $s -ne 0 ] && status=1
# Skipped tests are results: one skipped now that was not last time is a
# regression until explained -- six Eglot tests once went from passing to
# skipping, and the suite said 0 unexpected.
base="${XDG_CACHE_HOME:-$HOME/.cache}/emjupy-check/skipped-$MODE.txt"
mkdir -p "$(dirname "$base")"
grep -aE "^ +skipped +[0-9]+/[0-9]+ " /tmp/check-suite.log | awk '{print $3}' | sort -u > /tmp/check-skipped.txt
if [ -f "$base" ]; then
  new=$(comm -13 "$base" /tmp/check-skipped.txt)
  if [ -n "$new" ]; then
    echo "skipped, newly -- a regression until explained (if meant: rm $base):"
    echo "$new" | sed 's/^/  /'; status=1
  else
    echo "skipped: $(wc -l < /tmp/check-skipped.txt), none newly"; cp /tmp/check-skipped.txt "$base"
  fi
else
  echo "skipped: $(wc -l < /tmp/check-skipped.txt), recorded in $base for the next run"; cp /tmp/check-skipped.txt "$base"
fi
make -s clean >/dev/null 2>&1
exit $status
