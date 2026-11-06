# Fuzzing emjupy as a user works

`emjupy-fuzz.el` opens notebooks as a user does -- through TRAMP, each
in its own environment on another machine -- and works in them at
random, through the commands a user runs: moving by line and by cell,
typing, adding, moving, splitting, joining and deleting cells, undoing,
running, interrupting, restarting, saving, switching notebooks, asking
for completion.  The command loop's hooks run around each step, as
they do for a key: `cursor-intangible-mode` moving point off a
separator, among others.

After every step each notebook is checked: its cells' overlays sound,
the text outside them protected, no cell left marked running that is
gone, no error from the step -- a `user-error` aside, a refusal -- and
none from a timer or a process, which a user sees only in `*Messages*`.
The session ends by restarting each kernel and running every cell, then
saving and reading the saved file back, compared cell by cell with the
buffer.  It also records the longest Emacs was kept from running a
timer: how long it froze.

A seed fixes the session: a failure replays.  The log says what each
step did, and on a failure where point was and what was around it.

## The testbed: TRAMP over two hops, several environments, latency

The standard, as in `tools/emjupy-dev/references/testbeds.md`: Emacs
runs as a user, `local`, who cannot read the remote's files -- run as
root, a remote path taken for a local one goes unnoticed; the notebooks
are on `remote`, reached through `bastion`; the first hop slowed.

```bash
apt-get install -y openssh-server
for u in local bastion remote; do useradd -m -s /bin/bash $u; usermod -p '*' $u; done
chmod 700 /home/remote /home/bastion
```

`usermod -p '*'`: a new account is locked, and with PAM off `sshd`
refuses even a key to it.  Keys: `local` to `bastion` and `remote`,
`bastion` to `remote`.  `local`'s `~/.ssh/config` names `rbox` (port
2222, user remote) and `bhost` (port 2223, user bastion -- through the
latency proxy); `bastion`'s names `rbox`.  Then:

```bash
/usr/sbin/sshd -p 2222 -o PasswordAuthentication=no -o UsePAM=no
python3 tools/emjupy-dev/scripts/latency.py 2223 2222 0.04 &
su - local -c 'ssh -J bhost rbox whoami'          # remote, in about 1.5 s
su - remote -c tools/emjupy-dev/scripts/make-envs.sh   # ~/envs/sci, plain, py311
```

`sshd` and the proxy stop now and then -- check both answer before a
run.  Fresh notebooks before each run, which saves over them:

```bash
su - remote -c 'python3 tools/fuzz/make-notebooks.py ~/proj-sci sci'
su - remote -c 'python3 tools/fuzz/make-notebooks.py ~/proj-plain plain'
su - remote -c 'python3 tools/fuzz/make-notebooks.py ~/proj-311 py311'
```

## A run

As `local`, from the repository:

```bash
EMJUPY_FUZZ_NOTEBOOKS='/ssh:bhost|ssh:rbox:/home/remote/proj-sci/a.ipynb=/home/remote/envs/sci /ssh:bhost|ssh:rbox:/home/remote/proj-plain/b.ipynb=/home/remote/envs/plain /ssh:bhost|ssh:rbox:/home/remote/proj-311/c.ipynb=/home/remote/envs/py311' \
EMJUPY_FUZZ_SEED=4711 EMJUPY_FUZZ_STEPS=200 EMJUPY_FUZZ_LOG=/tmp/fuzz.log \
emacs -batch -Q -L src -L WEBSOCKET -l tools/fuzz/emjupy-fuzz.el
```

`EMJUPY_FUZZ_EGLOT=0` turns language support off; `EMJUPY_FUZZ_KEEP_GOING`
goes on after a failure.  The exit status is 1 if anything failed.  A
run of 200 steps over three notebooks takes about a minute and a half;
the seeds used so far: 4711, 90210, 31337, 1, 2, 3, 42, 1234.

## Reading a run

- `FAILURE`: emjupy's, or the driver's -- check which before fixing.
  The driver has been wrong three ways: typing where point was left
  without the command loop's hooks; trusting `emjupy--cell-at-point`,
  which answers which cell a command acts on, the nearest from an
  output; and running one buffer's redisplay functions for another's
  window.
- `PROBLEM`: what a user would see go wrong but is not emjupy's: a cell
  moved below the one it needs, raising `NameError` in the final run.
- `the longest Emacs was blocked`: opening the first notebook, a
  server started over two slowed hops, blocks about 7 s.

Findings so far are in `FINDINGS.md`.
