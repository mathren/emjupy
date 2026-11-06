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

### Open: opening the first notebook freezes Emacs for about 7 s

- **Seen**: in every run, the longest Emacs went without running a timer
  was 6 to 7 s, opening the first notebook: a server started on the
  remote, over two hops slowed to 1.4 s a command.
- **Not yet looked at**: which of those round trips must block.  Later
  notebooks open in 3 to 5 s, and steps never block more than a fraction
  of a second.

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
