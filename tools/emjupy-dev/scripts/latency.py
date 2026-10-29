# A link with latency, not little bandwidth: each chunk arrives DELAY seconds
# after it was sent, while the ones behind it keep flowing.
import asyncio, sys, time
LISTEN, TARGET, DELAY = int(sys.argv[1]), int(sys.argv[2]), float(sys.argv[3])
async def pipe(reader, writer):
    queue = asyncio.Queue()
    async def read():
        try:
            while data := await reader.read(65536):
                await queue.put((time.monotonic() + DELAY, data))
        finally:
            await queue.put((None, None))
    async def write():
        try:
            while True:
                due, data = await queue.get()
                if data is None: break
                wait = due - time.monotonic()
                if wait > 0: await asyncio.sleep(wait)
                writer.write(data); await writer.drain()
        except Exception: pass
        finally:
            try: writer.close()
            except Exception: pass
    await asyncio.gather(read(), write(), return_exceptions=True)
async def handle(cr, cw):
    try: sr, sw = await asyncio.open_connection('127.0.0.1', TARGET)
    except Exception: cw.close(); return
    await asyncio.gather(pipe(cr, sw), pipe(sr, cw), return_exceptions=True)
async def main():
    server = await asyncio.start_server(handle, '127.0.0.1', LISTEN)
    async with server: await server.serve_forever()
asyncio.run(main())
