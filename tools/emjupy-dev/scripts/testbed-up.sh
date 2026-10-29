#!/bin/bash
# Start whatever of the testbed is down: sshd, the SSH latency proxy, server A.
up() { timeout 3 bash -c "echo > /dev/tcp/127.0.0.1/$1" 2>/dev/null; }
if ! up 2222; then mkdir -p /run/sshd; (setsid /usr/sbin/sshd -D -e -p 2222 -o PasswordAuthentication=no -o PermitRootLogin=no < /dev/null > /tmp/sshd.log 2>&1 &); sleep 2; echo "sshd started"; else echo "sshd up"; fi
if ! up 2223; then (setsid python3 /tmp/fz/latency.py 2223 2222 0.04 < /dev/null > /tmp/fz/latency-ssh.log 2>&1 &); sleep 1; echo "ssh latency proxy started"; else echo "ssh latency proxy up"; fi
if ! up 18891; then mkdir -p /tmp/fz/a; (cd /tmp/fz/a && setsid /opt/jenv/bin/jupyter server --allow-root --no-browser --port=18891 --ip=127.0.0.1 --IdentityProvider.token=abc --ServerApp.root_dir=/tmp/fz/a > /tmp/fz/serverA.log 2>&1 < /dev/null &); for k in $(seq 1 25); do up 18891 && break; sleep 1; done; echo "server A started"; else echo "server A up"; fi
grep -A3 "Host bhost" /home/local/.ssh/config | grep -q "Port 2223" && echo "first hop through the latency proxy" || echo "first hop direct"
