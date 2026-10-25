---
name: "emjupy-dev"
description: Maintain and develop emjupy, the Emacs Jupyter notebook package (github.com/mathren/emjupy) -- fixes, features, refactors, reviews, docs, tests, CI and MELPA packaging, delivered as git mbox patches on the exact current main. Use this skill whenever the user mentions emjupy, its Elisp files (emjupy-*.el), its docs (docs/*.org), its tests, its CI or release workflows, or asks for a patch, mbox, review, fuzz test, refactor or docs pass on it -- even when the request is short, such as "fix the docs", "get the last commit and ...", or "continue".
---

# emjupy: maintenance and development

emjupy edits and runs Jupyter notebooks in Emacs 30.1+, over a Jupyter
server's HTTP and WebSocket API (works through an SSH tunnel), with
Eglot language support through `jupyter-lsp`.  The maintainer works on
GitHub; Claude works in a sandbox clone and delivers `git format-patch`
mbox files the maintainer applies with `git am`.

The maintainer wants brief, factual answers; critical in a
constructive, actionable way; nothing done that was not asked for.  When
a request has a weak spot -- a refactor that would lose information, a
setting that is a security risk -- say so, briefly, with the evidence.

## Start of every task

1. **Clone the latest `main`** and look at its last commits.  The
   maintainer often applies earlier patches, then adds commits of their
   own ("code cleanup", "fix docs", a version bump) that may break
   things.  Never assume main is where you left it.
2. **Find out what is already applied**: compare `git log`, or patch-ids
   (`git show C --format= | git patch-id --stable`) of yours against
   main's.  Build on main plus whatever of yours is not yet there.
3. **Read before deciding.**  Measure (scripts, `grep`, the dependency
   graph) and read the code a change touches.  Base claims on what the
   code does, not on memory: functions move, docstrings go stale.
4. Work on a branch in the clone.  Commit with
   `git -c user.email=dev@example.com -c user.name=emjupy commit`
   (the sandbox has no git identity).

## The rules of the code

Read `references/architecture.md` for the layers and the main flows
before changing behaviour.  The rules that matter most:

- **Dependencies run one way.**  Each file requires only files below it
  (core < terminal, ipynb, latex, undo, faces < http < render < cells <
  redraw < commands < kernel < figures, widgets < remote < lsp < shadow <
  eglot < mode < save < notebook < dashboard, tramp-open < emjupy), and
  requires what it uses: `scripts/uses.py` says.  No file names anything
  above it -- except a keymap, which names its commands with a plain
  quote, `'command` (resolved when the key is pressed; `#'` would make
  the compiler warn).  Otherwise a lower layer reaches up only through
  hook lists (`emjupy-*-functions`) wired when `emjupy-mode` starts.
  `tools/dependency-graph.py` must report 0 requires against the layers.
- **Loading a file only defines things.**  Hooks are added in an
  `-enable` function or by `emjupy-mode`, never at top level.  A keymap
  is a `defvar` whose value builds it whole --
  `(defvar MAP (let ((map (make-sparse-keymap))) (define-key map ...) map))`
  -- where it is made; never a `define-key` at top level.
- **The cells are the notebook; the buffer is a drawing.**  Edits sync
  back with `emjupy--sync-all-cells`; redraws go through
  `emjupy--atomic-redraw`; a cell command records one undo step with
  `emjupy--as-one-undo-step`.
- **Nothing on the per-keystroke path touches the network or TRAMP**
  (completion, eldoc, redraws).  Remote work is asynchronous or backed off.
- **Each layer wires its hooks in its `-enable` function**, which
  `emjupy-mode` calls in an order that keeps each hook's order.
- **Undo is in two files on purpose**: positions in emjupy-undo.el (low),
  the undo step with the full redraw in emjupy-redraw.el (they call each
  other).  Do not "consolidate" them.
- **Tests replace functions with `cl-letf`.**  A refactor may move code
  into helpers freely, but must keep every call a test replaces, keep
  every message's text, and never `defsubst` a helper.

## Style

Follow `docs/development.org` (Style section) and the
[Emacs Lisp style guide](https://github.com/bbatsov/emacs-lisp-style-guide).
In short: SPDX headers; requires at the top, the compiler's declarations
(`declare-function`, `defvar` of another package's variable) right after
them, never mid-file; `;;;; Options` first in each file, then a `;;;;`
heading per part; messages start `[emjupy]`; questions use `y-or-n-p`;
`;;;###autoload` only on entry points (login, list, open, create, menu,
version); user options in their subject's group (defined in
`emjupy-core.el`), `:safe` only for display/export ones; 80 columns,
except long strings, regexps and messages.

Comments and docstrings give the **reason, briefly**; history ("used
to", "was found by") belongs in commit messages.  See
`references/writing.md` for the voice, docstring escapes and checkdoc
traps.

## Moving code between files

1. Map sections (`;;;;`) and their line ranges; for each piece to move,
   what it uses and who uses it.
2. Before splitting a file, compute its function call graph: a group of
   functions calling each other must stay together; files of a split
   must not call each other both ways.  Options defined in one file and
   read only by the moved code move with it -- they are the usual cause
   of a cycle.
3. Move whole sections or definitions (with the comments above them);
   register a new file everywhere: Makefile SOURCES, emjupy.el's
   requires, .github/melpa-recipe and its copy in development.org, the
   tables in development.org and reading-the-code.org.
4. Prove it: `scripts/moves.el` (only moved), the compiler (0 warnings:
   a declaration left behind turns a special binding lexical),
   `scripts/uses.py` (nothing new), `make recipe-check`, the dependency
   graph, the unit suite, and the autoloads of moved entry points.

## Verify, in proportion to the change

Every change: compile with 0 warnings, the unit suite, and the **real**
lint.  Larger changes: the full suites against a live Jupyter server,
the recipe check, every module loading alone, the dependency graph, the
docs build with its link check, `actionlint` for workflows.
Commit only if the checks pass -- chain them with `&&`, never `;`.
`scripts/check.sh` runs these; `references/verification.md` has setup,
commands, and the specialised checks.  A sandbox command stops after 300
seconds: run `check.sh REPO full`, and an Emacs build, in the background
(`setsid nohup ... &`) and poll their log.

- **Refactors and prose passes**: `scripts/same_code.el` proves every
  file reads back as the same code, docstrings set aside.
- **Docs rewrites**: `scripts/orgkeep.py OLD NEW` lists code spans and
  link targets lost; review each report by hand (many are artifacts of
  adjacent spans).
- **Behaviour that is easy to get subtly wrong** (output, ordering,
  requests sent, drawing): compare the old and new code on the same
  inputs -- byte for byte, or on thousands of random inputs.
- **A new check**: run a negative control too -- break something on
  purpose and confirm it fails.
- **Flaky tests**: reproduce under CPU load before "fixing" with longer
  timeouts; the cause is often a real race (see `references/pitfalls.md`).
- **Remote behaviour**: the two-hop SSH testbed in
  `references/testbeds.md`.

Report test counts and what passed or failed, honestly: a test that
failed once and passed on rerun is named as flaky, with its cause if
known.

## Deliver

1. One commit per concern, message in the project's voice: what was
   wrong, what changed, how it was checked.
2. `scripts/deliver.sh BASE OUT.mbox` exports `BASE..HEAD`, applies it to
   a fresh clone at BASE, and diffs the result against the tested tree.
   Only then present the file.
3. Say which earlier mbox files it replaces, and remove superseded ones
   from the outputs folder.
4. If the turn ends before export, say plainly that nothing was
   delivered, what is committed where, and what remains.

## Reference files

- `references/architecture.md` -- files, layers, the main flows, hard
  functions.  Read before changing behaviour.
- `references/verification.md` -- environment setup, every check, and
  how to run one test.  Read before the first test run of a session.
- `references/testbeds.md` -- SSH two-hop testbed, low-rate Jupyter
  server, graphical Emacs for menus and screenshots, MELPA's real build.
- `references/writing.md` -- comments, docstrings, Org docs, commit
  messages.
- `references/pitfalls.md` -- traps met before, in the sandbox and in
  the code.  Skim at the start of a session.
