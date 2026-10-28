# emjupy architecture

The repository's own guide is `docs/reading-the-code.org`; this is the
short form, plus mechanisms that have bitten before.

## Files, bottom layer first

| File | Holds |
|---|---|
| `src/emjupy-core.el` | structs (server, kernel, cell, notebook) -- a server parses its address once, `emjupy--parse-base-url` -- the customization groups, cell-at-position lookups, `emjupy--private-directory`, `emjupy--timed`, what this Emacs can display |
| `src/emjupy-terminal.el` | stream text as a terminal shows it (`emjupy--terminal-apply`): string to string |
| `src/emjupy-ipynb.el` | nbformat JSON read and written; the .py percent format |
| `src/emjupy-latex.el` | LaTeX previews, by emjupy's own process (`emjupy--latex-process`: no shell escape) into a private folder; org declared, loaded lazily |
| `src/emjupy-undo.el` | undo positions shifted by a redraw (`emjupy--undo-adjust`) |
| `src/emjupy-faces.el` | appearance options, faces, theme colours |
| `src/emjupy-http.el` | REST requests, `emjupy--server-root-url`, errors (`emjupy-http-error`, `-status`, `-xsrf`); a WebSocket's token in a header (`emjupy--ws-token-param`, `emjupy--note-ws-refusal`) |
| `src/emjupy-render.el` | drawing a cell and its output, rules, fontifying |
| `src/emjupy-cells.el` | syncing the buffer back into the cells, structure checks, `emjupy-cells-enable` |
| `src/emjupy-redraw.el` | every redraw (`emjupy--atomic-redraw`), the undo step (`emjupy--as-one-undo-step`), repair after a redraw |
| `src/emjupy-commands.el` | the commands on cells |
| `src/emjupy-kernel.el` | WebSocket, execution, `emjupy--handle-ws-message`; reconnecting (`emjupy-reconnect-functions`, cells run meanwhile held); asking the server whether a kernel is still there, quiet ones every `emjupy-kernel-check-interval` |
| `src/emjupy-debug.el` | the debugger: DAP in debug_request on the control channel; dumpCell names a cell's file, breakpoints set there before each run (`emjupy-before-execute-functions`); events through `emjupy-debug-functions`, handled from a timer |
| `src/emjupy-figures.el`, `src/emjupy-widgets.el`, `src/emjupy-widget-page.el` | outputs opened apart, into a private folder -- an untrusted notebook's scripts not run (`emjupy--cell-trusted-p`, in core); widgets |
| `src/emjupy-remote.el` | server roots, SSH tunnel hosts, TRAMP names |
| `src/emjupy-lsp.el` | Eglot over the Jupyter WebSocket, shadow document text |
| `src/emjupy-shadow.el` | the shadow file and buffer, positions, edit buffers |
| `src/emjupy-eglot.el` | attaching Eglot to the shadow, xref, Eglot commands, completion, eldoc, diagnosis |
| `src/emjupy-mode.el` | `emjupy-mode`, its keymap: its own setup, then each layer's `-enable`, `emjupy-eglot-completion-enable` last |
| `src/emjupy-save.el` | saving, auto-saving, recovery copies |
| `src/emjupy-notebook.el` | login, opening and creating, switching, export command, the menu |
| `src/emjupy-dashboard.el` | the server dashboard |
| `src/emjupy-tramp-open.el` | an .ipynb opened as a notebook, here or through TRAMP (`emjupy-open-this-notebook`); servers started with the token in `JUPYTER_TOKEN`; tunnels, reopened when they die; the same machine, port and path the same notebook |
| `src/emjupy.el` | package header, version, requiring everything |
| `src/quicktry.el` | try-it harness the README points to; not shipped |
| `test/emjupy-FILE-test.el` | the unit tests of src/emjupy-FILE.el; `emjupy-package-test.el` for the package as a whole, the docs' names and file references |
| `test/emjupy-test-helpers.el`, `test/emjupy-test.el` | what the tests share; the file loading them all |
| `test/emjupy-integration-test.el`, `test/emjupy-robustness-test.el`, `test/emjupy-run-tests.el` | live tests, generated inputs, the runner |
| `tools/` | `dependency-graph.py`, `check-links.py`, `check-recipe.sh`, `lint-package.el`, `checkdoc.el`, `make-notebook-corpus.py`, `faultproxy.py` |
| `docs/*.org` + `publish.el` | the site, built into `html-content/` |
| `.github/melpa-recipe` | lists every shipped file explicitly |

## Main flows

- **Open**: `emjupy-login` → `emjupy--bind-server-kernel` (one port, one
  kernel) → `emjupy-open-notebook` → `emjupy--fetch-notebook` →
  `emjupy--draw-opened-notebook` → `emjupy--attach-bound-kernel`.
- **Run**: `emjupy-execute-cell-at-point` → `emjupy--ws-send`; replies in
  `emjupy--handle-ws-message`, outputs built by `emjupy--stream-output`,
  `emjupy--rich-output`, `emjupy--error-output`, appended by
  `emjupy--append-output-to-cell`; drawn from a timer
  (`emjupy--redraw-pending-output`), so tests must wait for drawing.
  A request finishes when both its reply and the idle after its last
  output came (`emjupy--request-half-seen`): replies can overtake output.
- **Save**: `emjupy--save-to-server` sends `emjupy--notebook-request`,
  refusing to overwrite a copy changed on the server; auto-save is
  asynchronous; recovery copies go to `emjupy-recovery-directory`.
- **Language support**: shadow buffer = all code cells as one Python
  file; Eglot attached over the Jupyter WebSocket; completion and eldoc
  delegate into it; `emjupy-language-support` nil disables every path.
- **Drop and reconnect**: `emjupy--kernel-connection-closed` keeps running
  cells, `emjupy--try-reconnect` retries under the same session id (the
  server replays missed output), `emjupy--adopt-kept-runs` passes the
  adopted ids to `emjupy--settle-adopted-runs`, which ends only those.
- **Notebook through TRAMP**: `emjupy-open-this-notebook` on
  `/ssh:a|ssh:b:/...` finds or starts a server there (left running on
  close), tunnels with `ssh -J`, records `emjupy-server-files-root`.  The
  route comes from `tramp-compute-multi-hops`: TRAMP rewrites file names
  without their hops once connected.

## Hard parts

`emjupy--ensure-shadow-buffer` (local/remote, back-off), the message
handler, `emjupy--reconcile-rules` (rules hung so line numbers fall on
code), `emjupy--undo-adjust` (a wrong position rewrites the wrong text; positions in emjupy-undo.el, the undo step with the full redraw in emjupy-redraw.el, as the two call each other),
`emjupy--terminal-apply` (state machine for progress bars).

## Mechanisms with sharp edges

- **Output rate limit**: jupyter-server counts output per request and
  resets when the kernel goes idle; the limit is
  `ZMQChannelsWebsocketConnection.iopub_data_rate_limit` (the
  `ServerApp.` name is deprecated and ignored).  plotly.js is fetched in
  pieces, halved when one comes back the wrong length.
- **Site anchors** (`publish.el`): every heading gets a CUSTOM_ID (slug of
  its title) in the export's copy before parsing, links use it
  (`org-html-prefer-user-labels`), and cross-page `*Title` links resolve
  to the slug.  Link by title; no CUSTOM_IDs to manage.
- **The figure window script** `emjupy-figure-window.py` stays in `src/`:
  shipped, and found beside the `.el` files in both a checkout and an
  install.
- **Generated files** are not committed: `html-content/*.html`,
  `html-content/dependencies.*`.
