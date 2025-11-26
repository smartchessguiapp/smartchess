import json
import time
import threading
import types

import pytest

import gui.pc.ws_tk_sample as ws_sample


class DummyResponse:
    def __init__(self, status_code=200):
        self.status_code = status_code


class DummyWSApp:
    def __init__(self, url, on_message=None, on_error=None, on_close=None, on_open=None):
        self.url = url
        self.on_message = on_message
        self.on_error = on_error
        self.on_close = on_close
        self.on_open = on_open
        self._closed = False

    def run_forever(self):
        # simulate open
        if self.on_open:
            try:
                self.on_open(self)
            except Exception:
                pass
        # simulate a status.update message
        msg = json.dumps({"type": "status.update", "timestamp": time.time(), "di_byte": 3, "pending_di_byte": 3, "dac": {"cur_freq": 12.34}})
        if self.on_message:
            try:
                self.on_message(self, msg)
            except Exception:
                pass

    def close(self):
        self._closed = True
        if self.on_close:
            try:
                self.on_close(self, 1000, "OK")
            except Exception:
                pass


def test_wsapp_dispatch(monkeypatch):
    received = []

    def on_status(data):
        received.append(data)

    # monkeypatch requests.get to simulate healthy service
    monkeypatch.setattr(ws_sample, 'requests', types.SimpleNamespace(get=lambda url, timeout=1: DummyResponse(200)))
    # monkeypatch WebSocketApp used in ws_sample
    monkeypatch.setattr(ws_sample, 'WebSocketApp', DummyWSApp)

    w = ws_sample.WSApp("ws://127.0.0.1:8000/ws/v1", on_status)
    w.start()
    # allow background thread to run and process
    time.sleep(0.2)
    # stop and join briefly
    w.stop()
    # wait for thread to stop
    time.sleep(0.1)

    assert len(received) >= 1
    assert received[0].get('type') == 'status.update'
    assert 'di_byte' in received[0]
