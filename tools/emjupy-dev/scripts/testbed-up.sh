#!/bin/bash
# Copyright (C) 2026 Mathieu Renzo
# Author: Mathieu Renzo <mrenzo@arizona.edu>
# Assisted-by: Claude:claude-opus-5.5
# SPDX-License-Identifier: GPL-3.0-or-later
# Start whatever of the testbed is down: sshd, the SSH latency proxy,
# server A here, server B on the remote and the tunnel to it.  The
# sandbox restarts now and then, and every process with it: run this
# before any live run.  Do not pipe it: what it starts keeps the pipe open.
up() { timeout 3 bash -c "echo > /dev/tcp/127.0.0.1/$1" 2>/dev/null; }
mkdir -p /run/sshd /tmp/fz/a
# sshd in the foreground under setsid nohup: started as a daemon, it died
# between turns.
up 2222 || { (setsid nohup /usr/sbin/sshd -D -e -p 2222 -o PasswordAuthentication=no \
               -o PermitRootLogin=no -o UsePAM=no < /dev/null > /tmp/sshd.log 2>&1 &); sleep 1; }
up 2223 || (setsid nohup python3 /tmp/fz/latency.py 2223 2222 0.04 < /dev/null > /tmp/fz/latency-ssh.log 2>&1 &)
up 18891 || (cd /tmp/fz/a && setsid nohup /opt/jenv/bin/jupyter server --allow-root --no-browser \
               --port=18891 --ip=127.0.0.1 --IdentityProvider.token=abc --ServerApp.root_dir=/tmp/fz/a \
               > /tmp/fz/serverA.log 2>&1 < /dev/null &)
su - remote -c 'curl -sf -o /dev/null "http://127.0.0.1:18892/api/status?token=def"' || \
  su - remote -c 'mkdir -p ~/srv2; (setsid nohup /opt/jenv/bin/jupyter server --no-browser --port=18892 --ip=127.0.0.1 --IdentityProvider.token=def --ServerApp.root_dir=$HOME/srv2 > /tmp/srv2-remote.log 2>&1 < /dev/null &)'
for i in $(seq 1 30); do up 18891 && su - remote -c 'curl -sf -o /dev/null "http://127.0.0.1:18892/api/status?token=def"' && break; sleep 1; done
up 18893 || (setsid nohup ssh -N -o BatchMode=yes -o ExitOnForwardFailure=yes -o ServerAliveInterval=2 \
               -L 18893:127.0.0.1:18892 rbox < /dev/null > /tmp/fz/tunnel.log 2>&1 &)
for i in $(seq 1 15); do curl -sf -o /dev/null "http://127.0.0.1:18893/api/status?token=def" && break; sleep 1; done
for p in 2222 2223; do up $p && echo "port $p up" || echo "port $p DOWN"; done
for u in "A 18891 abc" "B 18893 def"; do set -- $u
  echo "server $1: HTTP $(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$2/api/status?token=$3")"; done
grep -A3 "Host bhost" /home/local/.ssh/config 2>/dev/null | grep -q "Port 2223" && echo "first hop through the latency proxy" || echo "first hop direct"
