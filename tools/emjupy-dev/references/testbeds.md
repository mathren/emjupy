# Testbeds

## Remote: two SSH hops on one machine, made faithful

A notebook on another machine, behind a bastion, as on a cluster.  The
point is that Emacs's user must NOT be able to read the remote files:
running Emacs as root, which can, hides the bugs that matter (a remote
path taken for a local one).

```bash
apt-get install -y -q openssh-server openssh-client
useradd -m -s /bin/bash remote; useradd -m -s /bin/bash bastion; useradd -m -s /bin/bash local
chmod 700 /home/remote /home/bastion             # local cannot read them
# keys: local -> bastion and remote (for ssh -J); bastion -> remote (TRAMP's second hop)
su - local   -c 'ssh-keygen -q -t ed25519 -N "" -f ~/.ssh/id_ed25519'
su - bastion -c 'ssh-keygen -q -t ed25519 -N "" -f ~/.ssh/id_ed25519'
# append the public keys to the targets' ~/.ssh/authorized_keys (mode 600),
# and give local and bastion an ~/.ssh/config with
#   Host rbox  HostName 127.0.0.1  Port 2222  User remote
#   Host bhost HostName 127.0.0.1  Port 2222  User bastion   (local only)
#   StrictHostKeyChecking no, UserKnownHostsFile /dev/null, LogLevel ERROR
mkdir -p /run/sshd; ssh-keygen -A
(setsid nohup /usr/sbin/sshd -D -e -p 2222 -o PasswordAuthentication=no \
   -o PermitRootLogin=no < /dev/null > /tmp/sshd.log 2>&1 &)
su - local -c 'ssh -o BatchMode=yes -J bhost rbox whoami'   # prints: remote
```

Put a project in `/home/remote/proj` (copy `test_notebooks/`, owned by
`remote`).  Then, as `local`:

```bash
su - local -c 'cd REPO && EMJUPY_TEST_SSH_NOTEBOOK="/ssh:bhost|ssh:rbox:/home/remote/proj/test.ipynb" \
  EMJUPY_TEST_SSH_ENV=/opt/jenv EMJUPY_WEBSOCKET_DIR=... \
  T=emjupy-int-a-notebook-through-tramp-opens-on-its-machine $EMACS -batch -Q -l scripts/one_test.el'
```

Run it twice: first with no server on the remote (it must start one --
stop them with `pkill -u remote -f "[j]upyter-server"` over ssh), then
with one running (it must find it).  Paths through each differ.

`sshd` dies between commands now and then: check it answers before a run
(`ssh -o BatchMode=yes -o ConnectTimeout=3 rbox true`), restart if not.

## A second server, through a tunnel

The two tests of a second server want what a user has: another machine,
reached through an SSH tunnel.  With the testbed above, give root a key
in remote's authorized_keys (and a `Host rbox` entry), then:

```bash
chmod -R o+rX /opt/jenv /opt/emacs301 /path/to/websocket   # other users run them
su - remote -c 'mkdir -p ~/srv2 && cd ~/srv2 && (setsid nohup /opt/jenv/bin/jupyter server \
  --no-browser --port=18892 --ip=127.0.0.1 --IdentityProvider.token=def \
  --ServerApp.root_dir=$HOME/srv2 > /tmp/srv2.log 2>&1 < /dev/null &)'
(setsid nohup ssh -N -o BatchMode=yes -o ExitOnForwardFailure=yes \
   -L 18893:127.0.0.1:18892 rbox < /dev/null > /tmp/tunnel2.log 2>&1 &)
export EMJUPY_TEST_URL2=127.0.0.1:18893 EMJUPY_TEST_TOKEN2=def
```

The TRAMP test runs as `local`, the repository and scripts readable by
it (`chmod -R o+rX`): twice, with remote's server for the project stopped
(`pkill -u remote -f "[j]upyter-server.*proj"`) and running.

## A Jupyter server with a low output limit

```bash
jupyter server ... --ZMQChannelsWebsocketConnection.iopub_data_rate_limit=200000
```

`ServerApp.iopub_data_rate_limit` is deprecated and ignored in 2.x.  The
limit is counted per request and reset on idle, over
`rate_limit_window` (3 s): a single output over limit × window is cut.
Check the log for "IOPub data rate exceeded" to know the limit applied.

## Graphical Emacs, for menus and screenshots

```bash
(setsid Xvfb :98 -screen 0 1280x800x24 +extension GLX +render -noreset > /tmp/xvfb.log 2>&1 < /dev/null &)
DISPLAY=:98 $EMACS -Q --geometry 150x50 -l script.el
```

In the script, open a notebook offline (parse `test_notebooks/test.ipynb`
with `emjupy--parse-ipynb`, set the buffer, `emjupy-mode`, render), use
`run-at-time` to act after the frame shows: take a screenshot with
`(call-process "import" nil nil nil "-window" "root" "-crop" ...)`, press
keys with `unread-command-events`, record results to a file, then
`kill-emacs`.  Crop screenshots with `convert X +repage -crop ...`.

To test against the newest transient (as Magit users have it), clone
`magit/transient`, `emacs-compat/compat`, `tarsius/cond-let` and
`tarsius/llama` and put them first on `load-path`.

## MELPA's real build

```bash
git clone --depth 1 https://github.com/melpa/melpa && cd melpa
cp REPO/.github/melpa-recipe recipes/emjupy
# to build a local branch: :fetcher git :url "/path/to/clone" :branch "name"
make recipes/websocket recipes/emjupy archive-contents EMACS=$EMACS
```

package-build picks the newest commit changing a shipped file, so a
branch whose last commits touch only tests builds an earlier commit:
check the shipped files are identical.  Then install from it in a fresh
HOME:

```elisp
(setq package-user-dir "~/elpa"
      package-archives '(("local" . "/path/to/melpa/packages/")))
(package-initialize) (package-refresh-contents) (package-install 'emjupy)
```

It must pull websocket from the archive, use transient from Emacs, load
without warnings, and install no test or tool file.  The same
`packages/` serves as `EMJUPY_PACKAGE_ARCHIVE` for package-lint.
