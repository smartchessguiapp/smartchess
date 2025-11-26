"""
Simple mock RPi server for local testing of the Tkinter GUI.
Provides minimal HTTP endpoints and a WebSocket that emits status.update events.

Usage:
  python host_client/mock_server.py

Dependencies:
  pip install aiohttp

Then run the GUI in another terminal (PC samples moved to `host_client/pc`):
    python host_client/pc/ws_tk_sample.py

The mock server listens on 0.0.0.0:8000 by default and exposes:
  - HTTP: /api/v1/di (GET), /api/v1/di/pending (PUT), /api/v1/di/start (POST), /api/v1/di/stop (POST)
  - WS: /ws/v1

This is intentionally tiny — good enough to demo the GUI.
"""
import asyncio
import json
from aiohttp import web, WSMsgType

# Shared state
STATE = {
    "di_byte": 0,
    "pending_di_byte": 0,
    "dac": {
        "cur_freq": 0.0,
        "target_freq": 0.0,
        "cmd_freq": 0.0,
        "acc_time": 0.0,
        "vf_enable": True,
        "vf_fbase": 50.0,
        "vf_vbase": 200.0,
        "vf_vboost": 0.0,
        "vmax_applied": 180.0,
        "output_enabled": False,
    },
    "ao1": 0.0,
    "ao2": 0.0,
}

WS_CLIENTS = set()

async def get_di(request):
    return web.json_response({"ok": True, "data": {"di_byte": STATE["di_byte"], "pending_di_byte": STATE["pending_di_byte"]}})

async def put_di_pending(request):
    try:
        body = await request.json()
        v = int(body.get("di_byte", 0))
        STATE["pending_di_byte"] = v & 0xFF
        return web.json_response({"ok": True, "data": {"pending_di_byte": STATE["pending_di_byte"]}})
    except Exception as e:
        return web.json_response({"ok": False, "error": {"code": "BAD_REQUEST", "message": str(e)}} , status=400)

async def post_di_start(request):
    # apply pending (or override if body contains di_byte)
    try:
        body = await request.json()
    except Exception:
        body = {}
    if "di_byte" in body:
        STATE["di_byte"] = int(body.get("di_byte", 0)) & 0xFF
    else:
        STATE["di_byte"] = STATE["pending_di_byte"]
    return web.json_response({"ok": True, "data": {"di_byte": STATE["di_byte"]}})

async def post_di_stop(request):
    STATE["di_byte"] = 0
    return web.json_response({"ok": True, "data": {"di_byte": STATE["di_byte"]}})

async def ws_handler(request):
    ws = web.WebSocketResponse()
    await ws.prepare(request)
    WS_CLIENTS.add(ws)
    try:
        async for msg in ws:
            if msg.type == WSMsgType.TEXT:
                # echo or ignore
                pass
            elif msg.type == WSMsgType.ERROR:
                print('ws connection closed with exception %s' % ws.exception())
    finally:
        WS_CLIENTS.discard(ws)
    return ws

async def broadcast_loop():
    while True:
        payload = {
            "type": "status.update",
            "timestamp": asyncio.get_event_loop().time(),
            "di_byte": STATE["di_byte"],
            "pending_di_byte": STATE["pending_di_byte"],
            "dac": STATE["dac"],
            "ao1": STATE.get("ao1"),
            "ao2": STATE.get("ao2"),
        }
        text = json.dumps(payload)
        to_remove = []
        for ws in list(WS_CLIENTS):
            try:
                await ws.send_str(text)
            except Exception:
                to_remove.append(ws)
        for ws in to_remove:
            WS_CLIENTS.discard(ws)
        # update a simple DAC frequency for demo
        STATE["dac"]["cur_freq"] = (STATE["dac"]["cur_freq"] + 0.5) % 60.0
        await asyncio.sleep(1.0)


async def put_io_ao(request):
    """Handle POST /api/v1/io/ao { channel: "ao1", value: <num>, mode: <str> }
    Stores ao1/ao2 in STATE and returns the stored value.
    """
    try:
        body = await request.json()
        ch = body.get("channel")
        val = body.get("value")
        if ch is None or val is None:
            raise ValueError("channel and value required")
        # normalize channel name (ao1/ao2)
        nch = str(ch).lower()
        if nch not in ("ao1", "ao2"):
            return web.json_response({"ok": False, "error": {"code": "BAD_CHANNEL", "message": f"Unsupported channel {ch}"}}, status=400)
        # store numeric value
        try:
            nv = float(val)
        except Exception:
            return web.json_response({"ok": False, "error": {"code": "BAD_VALUE", "message": f"Invalid value {val}"}}, status=400)
        STATE[nch] = nv
        return web.json_response({"ok": True, "data": {nch: nv}})
    except Exception as e:
        return web.json_response({"ok": False, "error": {"code": "BAD_REQUEST", "message": str(e)}}, status=400)

async def start_background(app):
    app['broadcaster'] = asyncio.create_task(broadcast_loop())

async def cleanup_background(app):
    app['broadcaster'].cancel()
    try:
        await app['broadcaster']
    except asyncio.CancelledError:
        pass

def create_app():
    app = web.Application()
    app.router.add_get('/api/v1/di', get_di)
    app.router.add_put('/api/v1/di/pending', put_di_pending)
    app.router.add_post('/api/v1/di/start', post_di_start)
    app.router.add_post('/api/v1/di/stop', post_di_stop)
    app.router.add_post('/api/v1/io/ao', put_io_ao)
    app.router.add_get('/ws/v1', ws_handler)
    app.on_startup.append(start_background)
    app.on_cleanup.append(cleanup_background)
    return app

if __name__ == '__main__':
    app = create_app()
    web.run_app(app, host='0.0.0.0', port=8000)
