#!/bin/bash
# The fuzz session in one Emacs daemon, as one works, with the network
# lost under it: see README.org.  Run as root, from the repository, the
# testbed up: it pauses and restarts the SSH latency proxy and the
# daemon, which runs as `local'.  Each emacsclient call is timed.
#
#   daemon-fuzz.sh SEED [STEPS]
#
# Notebooks: EMJUPY_FUZZ_NOTEBOOKS, as for emjupy-fuzz.el; the first two
# in one folder and environment, for the second to share the first's
# kernel.  Logs in $LOGS (/tmp/fzlog): daemon-SEED.log, the session;
# daemon-SEED.times, each call's seconds; daemon-SEED.messages, every
# message, timed, with the buffer in view.

seed=${1:?seed}; steps=${2:-120}
LOGS=${LOGS:-/tmp/fzlog}; repo=$(pwd)
WEBSOCKET=${WEBSOCKET:-/home/claude/vendor/websocket}
EMACS=${EMACS:-/opt/emacs301/bin/emacs}; CLIENT=${CLIENT:-/opt/emacs301/bin/emacsclient}
export DISPLAY=${DISPLAY:-:99}
log=$LOGS/daemon-$seed.log times=$LOGS/daemon-$seed.times
: > "$times"

as_local() { su - local -c "$*"; }
# One call to the daemon, timed: NAME, then the Lisp.
call() {
  local name=$1 form=$2 t0 t1 out
  t0=$(date +%s.%N)
  out=$(timeout "${3:-180}" su - local -c "$CLIENT -s fzd -e '$form'" 2>&1)
  t1=$(date +%s.%N)
  printf '%s\t%.3f\t%s\n' "$name" "$(echo "$t1 - $t0" | bc)" "$out" >> "$times"
  echo "$out"
}
proxy_pid() { pgrep -f "latency.py 2223 2222" | head -1; }
proxy_start() {
  (setsid nohup python3 /tmp/fz/latency.py 2223 2222 0.04 < /dev/null \
     > /tmp/fz/latency-ssh.log 2>&1 &); sleep 1; }
# Probe: a small cell run in every notebook; how long until all answer.
recover() {
  local what=$1 t0 n left
  t0=$(date +%s.%N)
  call "probe start ($what)" "(fzd-probe-start)" > /dev/null
  for n in $(seq 1 150); do
    left=$(call "probe ($what)" "(fzd-probe-pending)" 20)
    [ "$left" = "0" ] && break
    sleep 1
  done
  printf 'RECOVERY\t%s\t%.1f s\t%s pending\n' "$what" \
         "$(echo "$(date +%s.%N) - $t0" | bc)" "$left" >> "$times"
}

# Xvfb for the daemon's frame
pgrep -f "Xvfb $DISPLAY" > /dev/null || {
  (setsid nohup Xvfb "$DISPLAY" -screen 0 1400x1000x24 -ac < /dev/null > /dev/null 2>&1 &); sleep 1; }

# The daemon, as `local': the notebooks fresh, then opened
as_local "$CLIENT -s fzd -e '(kill-emacs)'" > /dev/null 2>&1
for k in sci:proj-sci:a sci:proj-sci:a2 plain:proj-plain:b py311:proj-311:c; do
  IFS=: read kind dir name <<< "$k"
  su - remote -c "python3 $repo/tools/fuzz/make-notebooks.py ~/$dir $kind $name" > /dev/null
done
base=$(for i in 1 2 3 4 5; do s=$(date +%s.%N); su - local -c true; echo "$(date +%s.%N) - $s" | bc; done | sort -n | head -1)
printf 'BASELINE\tsu alone\t%.3f\n' "$base" >> "$times"
as_local "cd $repo && env DISPLAY=$DISPLAY PATH=/opt/jenv/bin:\$PATH \
  EMJUPY_FUZZ_NOTEBOOKS='$EMJUPY_FUZZ_NOTEBOOKS' EMJUPY_FUZZ_SEED=$seed \
  EMJUPY_FUZZ_LOG=$log EMJUPY_FUZZ_MESSAGES=$LOGS/daemon-$seed.messages \
  $EMACS --daemon=fzd -Q -L src -L tools/fuzz -L $WEBSOCKET \
  --eval \"(require 'emjupy-fuzz-daemon)\"" > "$LOGS/daemon-$seed.out" 2>&1
call setup "(fzd-setup)" 300

for i in $(seq 1 "$steps"); do
  case $i in
    10) call share "(fzd-share-kernel)" ;;
    20|60|100) call concurrent "(fzd-run-slow-everywhere)" ;;
    30) # an outage: the network frozen 25 s, work going on
        kill -STOP "$(proxy_pid)"; echo "EVENT	outage" >> "$times"
        for j in 1 2 3 4 5; do call "step $i.$j (outage)" "(fzd-step $i)" 120; done
        sleep 15; kill -CONT "$(proxy_pid)"; recover outage ;;
    70) # a network change: every connection broken at once
        kill "$(proxy_pid)"; echo "EVENT	network change" >> "$times"
        sleep 3; proxy_start; recover "network change" ;;
    105) # a laptop suspended: the daemon frozen, the network gone, then back
        echo "EVENT	suspend" >> "$times"
        kill -STOP "$(pgrep -u local -f 'daemon=fzd' | head -1)"
        kill "$(proxy_pid)"; sleep 30; proxy_start
        kill -CONT "$(pgrep -u local -f 'daemon=fzd' | head -1)"
        recover suspend ;;
  esac
  call "step $i" "(fzd-step $i)" > /dev/null
done
call end "(fzd-end)" 900
as_local "$CLIENT -s fzd -e '(kill-emacs)'" > /dev/null 2>&1
echo "DONE" >> "$times"
