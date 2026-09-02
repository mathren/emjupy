#!/bin/bash
# Bring up the full test stack: sshd -> jupyter -> ssh tunnel.
# Jupyter binds ONLY to 127.0.0.1:8888; emjupy talks to 127.0.0.1:18888,
# which is forwarded through ssh, so the tunnel is genuinely in the path.
set -u

JPORT=8888
TPORT=18888
SSHPORT=2222
TOKEN=abc
ROOT=/home/claude/nbroot

mkdir -p /run/sshd "$ROOT"

# --- sshd ---
if ! pgrep -f "sshd -f /etc/ssh/sshd_config" >/dev/null 2>&1; then
  setsid nohup /usr/sbin/sshd -f /etc/ssh/sshd_config -e > /tmp/sshd.log 2>&1 < /dev/null &
  sleep 2
fi

# --- jupyter ---
if ! curl -s -m 3 "http://127.0.0.1:$JPORT/api/status?token=$TOKEN" >/dev/null 2>&1; then
  cd "$ROOT"
  setsid nohup jupyter server --no-browser --port=$JPORT --ip=127.0.0.1 --allow-root \
    --IdentityProvider.token="$TOKEN" --ServerApp.root_dir="$ROOT" \
    > /tmp/jupyter.log 2>&1 < /dev/null &
  for i in $(seq 1 40); do
    curl -s -m 2 "http://127.0.0.1:$JPORT/api/status?token=$TOKEN" >/dev/null 2>&1 && break
    sleep 1
  done
fi

# --- ssh tunnel ---
if ! curl -s -m 3 "http://127.0.0.1:$TPORT/api/status?token=$TOKEN" >/dev/null 2>&1; then
  setsid nohup ssh -N -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
    -o ExitOnForwardFailure=yes -o ServerAliveInterval=30 -o BatchMode=yes \
    -L $TPORT:127.0.0.1:$JPORT -p $SSHPORT root@127.0.0.1 \
    > /tmp/tunnel.log 2>&1 < /dev/null &
  for i in $(seq 1 20); do
    curl -s -m 2 "http://127.0.0.1:$TPORT/api/status?token=$TOKEN" >/dev/null 2>&1 && break
    sleep 1
  done
fi

echo -n "direct  8888: "; curl -s -m 3 "http://127.0.0.1:$JPORT/api/status?token=$TOKEN" || echo FAIL
echo
echo -n "tunnel 18888: "; curl -s -m 3 "http://127.0.0.1:$TPORT/api/status?token=$TOKEN" || echo FAIL
echo

# --- second server + second tunnel (for the multi-server tests) ---
JPORT2=8899; TPORT2=19999; TOKEN2=def; ROOT2=/home/claude/nbroot2
mkdir -p "$ROOT2"
if ! curl -s -m 3 "http://127.0.0.1:$JPORT2/api/status?token=$TOKEN2" >/dev/null 2>&1; then
  cd "$ROOT2"
  setsid nohup jupyter server --no-browser --port=$JPORT2 --ip=127.0.0.1 --allow-root \
    --IdentityProvider.token="$TOKEN2" --ServerApp.root_dir="$ROOT2" \
    > /tmp/jupyter2.log 2>&1 < /dev/null &
  for i in $(seq 1 40); do
    curl -s -m 2 "http://127.0.0.1:$JPORT2/api/status?token=$TOKEN2" >/dev/null 2>&1 && break
    sleep 1
  done
fi
if ! curl -s -m 3 "http://127.0.0.1:$TPORT2/api/status?token=$TOKEN2" >/dev/null 2>&1; then
  setsid nohup ssh -N -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
    -o ExitOnForwardFailure=yes -o BatchMode=yes \
    -L $TPORT2:127.0.0.1:$JPORT2 -p $SSHPORT root@127.0.0.1 \
    > /tmp/tunnel2.log 2>&1 < /dev/null &
  for i in $(seq 1 20); do
    curl -s -m 2 "http://127.0.0.1:$TPORT2/api/status?token=$TOKEN2" >/dev/null 2>&1 && break
    sleep 1
  done
fi
echo -n "tunnel2 19999: "; curl -s -m 3 "http://127.0.0.1:$TPORT2/api/status?token=$TOKEN2" || echo FAIL
echo
