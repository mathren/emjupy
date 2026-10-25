# Writing in emjupy: comments, docstrings, docs, commits

## Voice

Short sentences; the reason, not the story.  A comment says why the code
is as it is; what it used to do, and how a bug was found, belong in the
commit message.  Turn a story into a reason:

- "Leaving point at the end of the notebook, as it used to, loses your
  place" → "point at the notebook's end would lose your place".
- "An earlier version of this test passed while nothing was asked" →
  "a call failing at once would pass the timing having asked nothing".

A test's docstring still says what it protects against.  Public
docstrings (`C-h f`, `C-h v`) are user documentation: complete, but no
history.

## Docstrings

- First line a complete sentence, ending with a period, under 80
  columns; checkdoc rejects one that breaks mid-sentence.
- Every argument named in capitals; checkdoc asks for each.
- checkdoc reads a third-person verb at the start of the first line's
  object as a non-imperative verb ("End the runs IDS" fails on "runs"):
  reword.
- Disambiguate a symbol that is both a function and a variable: "the
  variable `temporary-file-directory'".
- Quote with backquote and apostrophe, `like-this'; never Org's `=x=`.
- An apostrophe needs no escape.  A straight quote that must stay
  straight before a Lisp form is `\\='` (two backslashes in the source).
  A single backslash, `\='`, reads as a bare `=`: C-h f shows "notebook='s".
  The compiler warns about an unescaped `'(` in a docstring.

## Org docs

- The opening paragraph is shared by `src/emjupy.el`'s Commentary,
  `README.org` and `docs/index.org`; the maintainer chooses its words.
- Link to sections by title: `[[*Title]]` within a page,
  `[[file:page.org::*Title]]` across pages.  No CUSTOM_IDs.  Keep a link
  on one line.
- Shortening a page: keep every instruction, rule, table, command, key,
  option and link; cut history and sentences restating a table.  Check
  with `scripts/orgkeep.py`, review each report (adjacent `=spans=`
  produce false ones), build the site and run the link check.
- `docs/fuzz-findings.org` is a log: fixed findings keep what fixed them,
  how it was checked, how to reproduce, what it was.

## Commit messages

Title: what changed, plainly ("Fix: after a reconnect, only the runs
adopted are settled").  Body, wrapped at 72: what was wrong and its
consequence, what changed, how it was checked (numbers: tests run, bytes
compared, cases tried).  Name the files and functions.  One concern per
commit; a fix found during a refactor is its own commit, marked "Fix:".
