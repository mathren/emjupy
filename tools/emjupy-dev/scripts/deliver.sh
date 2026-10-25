#!/bin/bash
# Copyright (C) 2026 Mathieu Renzo
# SPDX-License-Identifier: GPL-3.0-or-later
# deliver.sh BASE OUT.mbox [REPO] -- export REPO's BASE..HEAD as an mbox,
# apply it to a fresh clone of the upstream at BASE, and compare the
# result with REPO's tree.  Present OUT.mbox only if this says "matches".
set -u
BASE=${1:?usage: deliver.sh BASE OUT.mbox [REPO]}; OUT=${2:?usage}; REPO=${3:-.}
UPSTREAM=${UPSTREAM:-https://github.com/mathren/emjupy.git}
REPO=$(cd "$REPO" && pwd) || exit 2
cd "$REPO" || exit 2
n=$(git log --oneline "$BASE..HEAD" | wc -l)
[ "$n" -gt 0 ] || { echo "deliver.sh: no commits in $BASE..HEAD"; exit 2; }
[ -z "$(git status --porcelain --untracked-files=no)" ] || { echo "deliver.sh: uncommitted changes in $REPO"; exit 2; }
git format-patch --stdout "$BASE..HEAD" > "$OUT"
tmp=$(mktemp -d)
git clone -q "$UPSTREAM" "$tmp/fresh" && cd "$tmp/fresh" && git checkout -q "$BASE" || { echo "deliver.sh: cannot clone $UPSTREAM at $BASE"; exit 2; }
if git -c user.email=t@t -c user.name=t am "$OUT" > "$tmp/am.log" 2>&1; then
  echo "applied on $BASE: $n commit(s), $(grep -c warning "$tmp/am.log") warning(s)"
else
  echo "FAILED to apply on $BASE:"; head -5 "$tmp/am.log"; exit 1
fi
# The tested tree is the committed one (no uncommitted changes, checked
# above).  Compared by tree hash, which covers contents, file modes and
# names alike -- diff -rq sees contents only, and passed a lost executable
# bit -- and not against the working directory, whose ignored build output
# is no part of a commit.
want=$(git -C "$REPO" rev-parse "HEAD^{tree}")
got=$(git rev-parse "HEAD^{tree}")
if [ "$want" = "$got" ]; then
  echo "matches the tested tree: $OUT"; rm -rf "$tmp"
else
  git fetch -q "$REPO" HEAD
  echo "DIFFERS from the tested tree:"; git diff --stat --summary HEAD FETCH_HEAD | head -8; exit 1
fi
