# Fuzzing findings

What `emjupy-fuzz.el` found, newest first.  Each: what a user saw, the
cause, the fix and its test, and how it replays.  The testbed and how to
run: `README.md`.

## 2026-11: the driver rebuilt; Emacs 30.1, TRAMP over two slowed hops, three environments, Eglot on

Seeds 4711, 90210, 31337, 1, 2, 3, 42, 1234; 200 steps each, over three
notebooks: `sci` (numpy 2, matplotlib), `plain` (no numpy), `py311`
(Python 3.11, numpy 1.26).

### Fixed: completion raised "No current JSON-RPC connection"

- **Seen**: asking for completion in a notebook ended in an error, the
  candidate inserted -- seeds 31337, 1 and 2, nearly every completion
  that reached its end.
- **Cause**: Eglot's completion answer carries functions called later,
  once the candidates are listed or one chosen -- its collection, its
  `:exit-function`, annotations, documentation.  emjupy passed them on;
  called in the notebook's buffer, where Eglot has no server, each asked
  for one and raised.  The guard caught the error only in the first call.
  A backtrace from a replay: `completion--done` < the exit function <
  `eglot-server-capable` < `eglot--current-server-or-lose`.
- **Fix**: the collection and the other functions run in the shadow
  buffer, where the server is; the exit function, which may edit the
  completed text, where it is; a `jsonrpc-error` in any means no
  candidates, or nothing more done -- not an error.
- **Test**: `emjupy-test-completion-with-its-server-gone-offers-nothing`,
  failing on the code before.

### Fixed: a notebook would not open after the remote machine restarted

- **Seen**: "The Jupyter server on rbox did not answer", after a minute,
  opening a notebook whose folder a live server served -- every time.
- **Cause**: a server gone leaves its runtime file, `jpserver-PID.json`.
  emjupy listed the servers whose process is alive, `kill -0 PID`; after
  a restart, PIDs start low again, and the stale file's number was then
  a process of TRAMP's own connection -- alive, for that moment.  Listed
  first, its port was tunnelled to and waited on; nothing served it.
- **Fix**: a file is listed if its process is alive and a Jupyter's:
  `ps -p PID -o args=` names jupyter (Linux and macOS); without `ps`,
  alive is all that is asked.
- **Test**: `emjupy-test-servers-are-listed-from-their-runtime-files`,
  now with a live process that is not Jupyter's -- Emacs's own -- not
  listed; failing on the code before.
- **Replays**: a stale runtime file whose PID a process of the remote
  user's holds when the list is taken.

### Fixed in part: opening the first notebook froze Emacs for about 7 s

- **Seen**: in every run, the longest Emacs went without running a timer
  was 6 to 7 s, opening the first notebook.
- **Where the time goes**, its server stopped first, timing each remote
  command (12 s in all, over two hops slowed to 1.4 s a command):

  | Stage | Time | Whose |
  |---|---|---|
  | TRAMP's connection through both hops, 22 setup commands | 3.5 s | Emacs's |
  | `find-file`: the file read; version control probed, bzr, git, hg | 2.9 s | Emacs's |
  | The environment remembered for the folder, checked | 0.4 s | emjupy |
  | The servers listed; one started | 0.5 s | emjupy |
  | The tunnel, through both hops | 1.3 s | emjupy |
  | The new server waited for: asked, `sleep-for` 0.5 s, five times | 2.8 s | emjupy |

- **Half is Emacs's**, for any remote file; version control's probing a
  user can turn off for remote files (`vc-handled-backends`).
- **emjupy's, taken**: the wait for a server it started blocked Emacs --
  `sleep-for` in a loop, no keys read.  Opened by a key now, it is not
  waited for: the notebook's buffer shows at once, saying the server is
  starting, and fills when it answers.  A server answering in 6 s,
  simulated: the command returns after 2.3 s, where it took 6.9 s; the
  notebook drawn in the same buffer, its kernel connected at 7 s.  From
  Lisp, the notebook is still returned ready.  Tests:
  `emjupy-test-a-server-is-awaited-without-holding-emacs`,
  `emjupy-test-a-notebook-shows-at-once-and-fills-when-ready`.
- **Here too**: a notebook on this machine waited the same way for the
  server started for it.  One way now, `emjupy--server-ready`, for both:
  by a key, the command returns in 0.12 s, where it took 1.3 s.  Asked
  too early, a server here refuses the connection, and the url library
  retried it nested until Emacs ran out of Lisp depth: its port is asked
  first now, `emjupy--server-listening-p`.  Tests:
  `emjupy-test-a-server-is-made-ready-one-way-here-and-there`,
  `emjupy-test-a-local-notebook-shows-at-once-too`.
- **Found measuring it**: with no `~/.emacs.d`, every opening said
  "Environments not saved": the file's folder is made now.  Test:
  `emjupy-test-environments-are-saved-into-a-new-folder`.

### Not emjupy's: what the driver got wrong, and fixed

- Typing at a separator `undo` left point on: a batch Emacs never
  redraws, so `cursor-intangible-mode` never moved point off it.  The
  driver now runs the command loop's hooks around each step.
- Typing at the end of an output line: `emjupy--cell-at-point` answers
  which cell a command acts on -- the nearest one from an output -- not
  where text can go.  The driver now checks point is in a cell's source.
- `args-out-of-range` after a restart, a run, a save: the driver ran one
  notebook's redisplay functions for another's window.  It now runs them
  as redisplay does, for each window with its own buffer.

### Expected: `NameError` in the final run

A cell moved, at random, below the one defining what it uses raises it
when every cell is run in order.  The driver logs it as a problem, not
a failure.
