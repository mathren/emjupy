# Pitfalls met before

## The sandbox and the shell

- **`pkill -f PATTERN` kills your own shell** when your command line
  contains the pattern -- including through a later argument (starting
  `--port=18985` after killing `port=1898[5]`).  Anchor the pattern to
  the target's command line start: `pkill -f "^ssh -N -L 18990"`,
  `pkill -f "^/opt/jenv/bin/python[0-9.]* /opt/jenv/bin/jupyter.*PORT"`.
  The same goes for `pgrep` checks: they find themselves and report
  "already running".  Check a server by asking its port (`curl -sf`).
- **The tool's shell is `sh`**: `${PIPESTATUS}`, `<(...)` and arrays fail
  with "Bad substitution" or a syntax error.  Put such code in a file and
  run it with `bash`.
- **Background processes** started with `cmd &` or `ssh -f` may end with
  the command.  Use `(setsid nohup cmd < /dev/null > log 2>&1 &)` and a
  foreground mode (`sshd -D`, `ssh -N` without `-f`).
- **`ssh host 'cmd &'` can hang** holding the session: wrap in `timeout`,
  use `ssh -n`, redirect everything.
- **A moved binding loses its declaration**: code binding another
  package's variable (`python-indent-guess-indent-offset-verbose`) needs
  `(defvar NAME)` in its own file; moved without it, the binding turns
  lexical and silently does nothing.  Only the compiler sees it ("Unused
  lexical variable"); a moves check does not.
- **Scanning Lisp by regexp**: a docstring line can begin with "(".  To
  find top-level forms, read them with Emacs (`read`, positions from the
  reader); scripts that cut forms out must start at the form's first line
  and write destinations before cutting sources, so a crash loses nothing.
- **A commit after a failed check**: `check; git commit` commits anyway.
  Chain with `&&`.
- **Counting a test's calls** counts the functions it stubs with `cl-letf`:
  file a test by its subject.
- **A side effect at load time** hides from checks that look at
  definitions: a bare top-level call -- advising another package, adding
  to a global hook -- is one.  `emjupy-test-loading-changes-nothing`
  loads emjupy in a fresh Emacs to catch it; such wiring goes in an
  `-enable` function.
- **Re-indenting in batch** uses tabs (set `indent-tabs-mode` nil) and
  does not know emjupy's macros' indentation: whole-file re-indent
  rewrites thousands of correct lines.  Realign only what a change moved.
- **Renaming a user option**: `define-obsolete-variable-alias` before the
  new `defcustom`, so a value set under the old name carries over.
- **Coverage finds the source files** by pattern (every `src/emjupy*.el`
  but the pkg file): a new file is measured with nothing to list.
- **The docs name files**: "=NAME= (file)" in reading-the-code.org rots
  when code moves; `emjupy-test-the-docs-say-where-things-are` checks it.
- **A command stops after 300 seconds**, output lost: run builds and the
  full suites in the background, logging to a file, and poll it.
- **Realigning Org tables** with a loop over the whole file realigns
  every table, churning the diff: edit a row within its column's width,
  or align only the table changed.
- **No git identity** in the sandbox: pass `-c user.email=... -c
  user.name=...` to commit, cherry-pick, rebase.
- **Long work spans turns**: the clone and scratch files usually survive;
  check (`git status`, `git log`) before continuing.
- **Python patching Elisp**: write replacements in raw strings and check
  escapes survived (`grep -c` the exact regexp before and after); assert
  each old text occurs exactly once.
- **Cutting a form's body out by text** can drop a closing parenthesis
  that belonged to an enclosing form: check balance after every edit
  (`check-parens`), restore and redo rather than patch the patch.

## emjupy and Emacs

- **TRAMP rewrites a file name without its hops** once connected:
  `/ssh:bastion|ssh:node:` becomes `/ssh:node:`.  Get the route from
  `tramp-compute-multi-hops`, not the name.
- **A "local" server can be remote**: behind a tunnel the address is
  127.0.0.1; never treat a kernel's directory as local without checking.
- **Replies overtake output** on the kernel protocol; a request is done
  only when its reply and its closing idle have both come.
- **Output is drawn from a timer**: a test reading the buffer right after
  a run races it; wait for the text (`emjupy-int--shows`).
- **Settling after a reconnect** must touch only the runs adopted: a run
  sent since can finish with its output still in flight.
- **jupyter-server's output limit** is per request, reset on idle; set it
  with `ZMQChannelsWebsocketConnection.`, not `ServerApp.`.
- **Org title links** broke because publish.el's id map was never filled;
  now every heading gets its slug before export.  Do not reintroduce
  CUSTOM_ID links.
- **transient computes menu titles in a temporary buffer**: read the
  notebook through `transient--original-buffer`.
- **`#'command` in a keymap** of a file below the command's makes the
  compiler warn the function is not known to be defined; name it with a
  plain quote.
- **A toggle command must not `setq` a user option**: it changes every
  buffer and shows in Customize as changed outside it; use `setq-local`.
- **package-lint in batch** resolves `.dir-locals.el`'s relative main
  file against the directory Emacs started in: run it from `src/`.
- **`make lint` used to pass without package-lint**: it is required now;
  a missing package-lint or archive fails it, on purpose.

## Judgement calls the maintainer has made

- Tests are not changed by a refactor; behaviour fixes may add tests.
- Do not deduplicate `README.org` and `docs/index.org`.
- Unit tests live in one file per source file (`test/emjupy-FILE-test.el`),
  each with the source file of what it tests -- its subject, not the
  functions it stubs.
- Keep `emjupy-figure-window.py` in `src/`.
- Remote servers emjupy starts are left running for reconnection.
- Questions use `y-or-n-p`, even before destructive actions.
- Generated files (`html-content/*.html`, the dependency graph) are not
  committed.
