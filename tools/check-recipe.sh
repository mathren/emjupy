#!/bin/sh
# Check that the MELPA recipe builds the package -- run by `make recipe-check'
# and by CI.  Two checks:
#
# 1. The recipe lists every file the package is made of -- the Makefile's
#    SOURCES and the other files PKGFILES ships -- and nothing that is not
#    there.  A file left out is not in the package MELPA builds.
# 2. Built as MELPA builds it -- only the recipe's files, copied into an
#    empty directory -- every one byte-compiles with nothing else on the
#    load path.  That is how the review caught "Cannot open load file":
#    the recipe lacked files the code requires.
#
#   tools/check-recipe.sh [EMACS]
set -u
emacs=${1:-emacs}
recipe=.github/melpa-recipe
status=0

listed=$(grep -o '"src/[^"]*"' "$recipe" | tr -d '"')
wanted=$(make -s print-package-files)

for f in $wanted; do
  echo "$listed" | grep -qx "$f" || { echo "recipe-check: $f is not in $recipe"; status=1; }
done
for f in $listed; do
  [ -f "$f" ] || { echo "recipe-check: $recipe lists $f, which does not exist"; status=1; }
done

out=$(mktemp -d)
for f in $listed; do [ -f "$f" ] && cp "$f" "$out/"; done
for f in "$out"/*.el; do
  problems=$("$emacs" -Q --batch -L "$out" ${EMJUPY_WEBSOCKET_DIR:+-L "$EMJUPY_WEBSOCKET_DIR"} \
               -f batch-byte-compile "$f" 2>&1 | grep -E "Error|Warning")
  [ -n "$problems" ] && { echo "$problems" | sed "s|[^ ]*/\(emjupy[a-z-]*\.el\)|\1|"; status=1; }
done
rm -rf "$out"

[ $status -eq 0 ] && echo "recipe-check: $(echo "$listed" | wc -l) files, all in the recipe; built from it alone, no problems"
exit $status
