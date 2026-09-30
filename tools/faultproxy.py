#!/usr/bin/env python3
"""A TCP proxy that misbehaves on command, for emjupy's network tests.

    faultproxy.py TARGET_HOST TARGET_PORT

Listens on a free port for clients, forwards to the target, and prints
"listening PORT control CPORT" once ready.  Commands, one per line, on
the control port:

    sever        close every connection now, as a dropped tunnel does
    stall        stop forwarding, but keep connections open
    resume       forward normally again
    delay MS     wait MS milliseconds before forwarding each chunk
    chunk N      forward in pieces of N bytes, splitting messages
    reset        resume, no delay, no chunking

Each command is answered "ok".
"""
import asyncio, sys

class Faults:
    def __init__(self):
        self.reset()
        self.conns = set()
    def reset(self):
        self.stalled = asyncio.Event(); self.stalled.set()   # set = flowing
        self.delay = 0.0
        self.chunk = 0

F = None

async def pipe(reader, writer):
    try:
        while True:
            data = await reader.read(65536)
            if not data:
                break
            await F.stalled.wait()
            pieces = [data] if not F.chunk else [data[i:i + F.chunk] for i in range(0, len(data), F.chunk)]
            for p in pieces:
                if F.delay:
                    await asyncio.sleep(F.delay)
                await F.stalled.wait()
                writer.write(p)
                await writer.drain()
    except (ConnectionError, asyncio.CancelledError):
        pass
    finally:
        try:
            writer.close()
        except Exception:
            pass

async def client(reader, writer, host, port):
    try:
        ur, uw = await asyncio.open_connection(host, port)
    except OSError:
        writer.close(); return
    tasks = (asyncio.ensure_future(pipe(reader, uw)), asyncio.ensure_future(pipe(ur, writer)))
    entry = (writer, uw, tasks)
    F.conns.add(entry)
    await asyncio.wait(tasks, return_when=asyncio.FIRST_COMPLETED)
    for t in tasks:
        t.cancel()
    for w in (writer, uw):
        try:
            w.close()
        except Exception:
            pass
    F.conns.discard(entry)

async def control(reader, writer):
    while True:
        line = await reader.readline()
        if not line:
            break
        cmd = line.decode().split()
        if not cmd:
            continue
        if cmd[0] == "sever":
            for w, uw, tasks in list(F.conns):
                for t in tasks: t.cancel()
                for x in (w, uw):
                    try:
                        x.transport.abort()
                    except Exception:
                        pass
            F.conns.clear()
        elif cmd[0] == "stall":
            F.stalled.clear()
        elif cmd[0] == "resume":
            F.stalled.set()
        elif cmd[0] == "delay":
            F.delay = float(cmd[1]) / 1000.0
        elif cmd[0] == "chunk":
            F.chunk = int(cmd[1])
        elif cmd[0] == "reset":
            F.reset()
        writer.write(b"ok\n"); await writer.drain()

async def main(host, port):
    global F
    F = Faults()
    srv = await asyncio.start_server(lambda r, w: client(r, w, host, port), "127.0.0.1", 0)
    ctl = await asyncio.start_server(control, "127.0.0.1", 0)
    print("listening", srv.sockets[0].getsockname()[1], "control", ctl.sockets[0].getsockname()[1], flush=True)
    await asyncio.gather(srv.serve_forever(), ctl.serve_forever())

if __name__ == "__main__":
    asyncio.run(main(sys.argv[1], int(sys.argv[2])))
