#!/usr/bin/env bash
# Set the package version everywhere the CURRENT version is written:
#
#   src/emjupy.el      the ";; Version:" header and `emjupy-version'
#   src/emjupy-pkg.el  the define-package form
#   docs/usage.org     the sample output of M-x emjupy-version
#
# A version mentioned as history -- "fixed in 0.1.2" -- is deliberately not
# touched: it says when something happened, and stays true.
#
#   tools/set-version.sh 0.1.3
set -euo pipefail
cd "$(dirname "$0")/.."

new="${1:-}"
if ! [[ "$new" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "usage: $0 X.Y.Z   (got '${new}')" >&2
  exit 2
fi

old="$(sed -n 's/^;; Version: *//p' src/emjupy.el | head -1)"
if [ "$new" = "$old" ]; then
  echo "set-version: already $old" >&2
  exit 1
fi
# refuse to go backwards: a lower version would sort before tags already out
if [ "$(printf '%s\n%s\n' "$old" "$new" | sort -V | tail -1)" != "$new" ]; then
  echo "set-version: $new is lower than the current $old" >&2
  exit 1
fi

sed -i "s/^;; Version: .*/;; Version: $new/" src/emjupy.el
sed -i "s/^(defconst emjupy-version \"[^\"]*\"/(defconst emjupy-version \"$new\"/" src/emjupy.el
sed -i "s/^(define-package \"emjupy\" \"[^\"]*\"/(define-package \"emjupy\" \"$new\"/" src/emjupy-pkg.el
# the sample output: a line beginning "emjupy X.Y.Z, " inside the example
sed -i -E "s/^emjupy [0-9]+\.[0-9]+\.[0-9]+, /emjupy $new, /" docs/usage.org

# every place must now say NEW; say which one did not, if any
fail=0
check() { grep -q "$2" "$1" || { echo "set-version: $1 does not show $new" >&2; fail=1; }; }
check src/emjupy.el     "^;; Version: $new\$"
check src/emjupy.el     "^(defconst emjupy-version \"$new\""
check src/emjupy-pkg.el "^(define-package \"emjupy\" \"$new\""
check docs/usage.org    "^emjupy $new, "
[ "$fail" = 0 ] || exit 1
echo "set-version: $old -> $new"
