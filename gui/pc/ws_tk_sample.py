"""
WS Tk sample (PC package copy)

Wrapper that demonstrates connecting to the mock server and updating a small
Tk GUI with incoming status.update messages. Copied under `host_client/pc`.
"""
import json
import threading
import time
import tkinter as tk
from tkinter import ttk
import requests
import urllib.parse

from websocket import WebSocketApp
from host_client.openapi_client.smart_checker_host_raspberry_pi_api_client.client import Client as APIClient
from host_client.openapi_client.smart_checker_host_raspberry_pi_api_client.api.default import (
    put_di_pending as api_put_di_pending,
)
from host_client.openapi_client.smart_checker_host_raspberry_pi_api_client.models import (
    DISetPendingRequest,
)
from tkinter import messagebox


class WSApp:
    def __init__(self, ws_url: str, on_status):
        self.ws_url = ws_url
        self.on_status = on_status
        self.ws = None
        self.thread = None
        self.running = False
        # derive an HTTP base url for simple health checks
        try:
            p = urllib.parse.urlparse(ws_url)
            scheme = 'http' if p.scheme == 'ws' else p.scheme
            self.base_http = f"{scheme}://{p.hostname}:{p.port}/api/v1"
        except Exception:
            self.base_http = None

    def _on_message(self, ws, message):
        try:
            data = json.loads(message)
        except Exception:
            return
        t = data.get("type")
        if t == "status.update":
            # pass to GUI callback
            self.on_status(data)

    def _on_error(self, ws, err):
        print("WS error:", err)

    def _on_close(self, ws, close_status_code, close_msg):
        print("WS closed", close_status_code, close_msg)
        self.running = False

    def _on_open(self, ws):
        print("WS opened")

    def start(self):
        if self.running:
            return
        self.running = True
        # Start background thread that ensures connection with retry/backoff
        def run_loop():
            backoff = 0.5
            max_backoff = 30.0
            while self.running:
                # optional health-check before attempting WS connect
                if self.base_http:
                    try:
                        r = requests.get(self.base_http, timeout=1)
                        if r.status_code >= 400:
                            # service not ready
                            raise Exception(f"health-check {r.status_code}")
                    except Exception:
                        # wait and retry health-check
                        time.sleep(backoff)
                        backoff = min(max_backoff, backoff * 2)
                        continue

                try:
                    self.ws = WebSocketApp(self.ws_url,
                                           on_message=self._on_message,
                                           on_error=self._on_error,
                                           on_close=self._on_close,
                                           on_open=self._on_open)
                    # blocking call; will return when connection closed or error
                    self.ws.run_forever()
                except Exception as e:
                    print('WS run_forever error:', e)

                if not self.running:
                    break
                # connection lost — wait with backoff
                time.sleep(backoff)
                backoff = min(max_backoff, backoff * 2)

        self.thread = threading.Thread(target=run_loop, daemon=True)
        self.thread.start()

    def stop(self):
        if self.ws:
            try:
                self.ws.close()
            except Exception:
                pass
        self.running = False


class App(tk.Tk):
    def __init__(self, ws_url: str):
        super().__init__()
        self.title("SmartChecker WS Sample")
        self.geometry("480x240")

        self.status_label = ttk.Label(self, text="Disconnected", font=(None, 14))
        self.status_label.pack(pady=8)

        self.freq_var = tk.StringVar(value="-")
        ttk.Label(self, text="cur_freq (Hz):").pack()
        ttk.Label(self, textvariable=self.freq_var, font=(None, 12)).pack()

        # Note: connection controls removed per request; this sample only shows DI checkboxes and two buttons
        # DI controls
        frame = ttk.Frame(self)
        frame.pack(pady=6, fill="x")

        self.di_val_var = tk.IntVar(value=0)
        ttk.Label(frame, text="di_byte:").grid(row=0, column=0, sticky="w")
        self.di_label = ttk.Label(frame, textvariable=tk.StringVar(value="-"))
        self.di_label.grid(row=0, column=1, sticky="w")

        ttk.Label(frame, text="pending:").grid(row=1, column=0, sticky="w")
        self.pending_label = ttk.Label(frame, textvariable=tk.StringVar(value="-"))
        self.pending_label.grid(row=1, column=1, sticky="w")

        # Two buttons: Reflect (apply checkboxes) and STOP (set all off)
        btn_frame = ttk.Frame(frame)
        btn_frame.grid(row=3, column=0, columnspan=2, pady=6)
        ttk.Button(btn_frame, text="反映", command=self._do_reflect).grid(row=0, column=0, padx=8)
        ttk.Button(btn_frame, text="STOP", command=self._do_stop_button).grid(row=0, column=1, padx=8)

        # Bit checkboxes (DI bits 0..7)
        bits_frame = ttk.Frame(frame)
        bits_frame.grid(row=4, column=0, columnspan=2, pady=6)
        self.bit_vars = [tk.IntVar(value=0) for _ in range(8)]
        # To avoid sending PUTs when we programmatically update checkboxes, use a suspend flag
        self._suspend_bit_callbacks = False
        for i in range(8):
            var = self.bit_vars[i]
            cb = ttk.Checkbutton(bits_frame, text=f"CH{i+1}", variable=var)
            cb.grid(row=0, column=i, padx=2)

        # no extra buttons — reflect and STOP only

        self.ws_app = WSApp(ws_url, self.on_status_update)

        # HTTP client to call DI endpoints (use generated OpenAPI client for type safety)
        base_http = ws_url.replace("ws://", "http://").rsplit("/ws", 1)[0] + "/api/v1"
        self.api_client = APIClient(base_url=base_http)

        # periodic GUI update (safe from other thread)
        self.after(1000, self._tick)

    # connection toggle removed

    def on_status_update(self, data: dict):
        # Called from WS thread. Use `after` to marshal to GUI thread.
        self.after(0, lambda: self._apply_status(data))

    def _apply_status(self, data: dict):
        dac = data.get("dac") or {}
        cur = dac.get("cur_freq")
        if cur is not None:
            self.freq_var.set(f"{cur:.2f}")
        self.status_label.config(text=f"Last: {data.get('timestamp')}")
        # update DI labels if present
        di = data.get("di_byte")
        if di is not None:
            try:
                self.di_label.config(text=str(di))
            except Exception:
                pass
        pending = data.get("pending_di_byte")
        if pending is not None:
            try:
                self.pending_label.config(text=str(pending))
            except Exception:
                pass

    # --- DI control helpers (run HTTP calls in background) ---
    def _run_in_bg(self, fn, *args, on_done=None):
        def worker():
            try:
                res = fn(*args)
            except Exception as e:
                msg = str(e)
                self.after(0, lambda m=msg: messagebox.showerror("Error", m))
                return
            if on_done:
                self.after(0, lambda: on_done(res))

        threading.Thread(target=worker, daemon=True).start()

    def _do_reflect(self):
        # assemble bits into byte (bit0 = CH1, bit7 = CH8)
        v = 0
        for i in range(8):
            try:
                b = int(self.bit_vars[i].get())
            except Exception:
                b = 0
            if b:
                v |= (1 << i)
        # reflect into numeric entry
        self.di_val_var.set(v)
        body = DISetPendingRequest(di_byte=v)
        self._run_in_bg(lambda: api_put_di_pending.sync(client=self.api_client, body=body), on_done=lambda r: messagebox.showinfo("OK", f"pending set to {v}"))

    def _do_stop_button(self):
        # set all checkboxes off and send pending=0
        self._suspend_bit_callbacks = True
        for i in range(8):
            try:
                self.bit_vars[i].set(0)
            except Exception:
                pass
        self._suspend_bit_callbacks = False
        self.di_val_var.set(0)
        body = DISetPendingRequest(di_byte=0)
        self._run_in_bg(lambda: api_put_di_pending.sync(client=self.api_client, body=body), on_done=lambda r: messagebox.showinfo("OK", "all channels STOPPED"))

    # removed start/stop handlers; STOP is handled by _do_stop_button

    def _tick(self):
        # keep alive tick
        if self.ws_app.running:
            self.status_label.config(foreground="green")
        else:
            self.status_label.config(foreground="red")
        self.after(1000, self._tick)


if __name__ == "__main__":
    # Example usage - change host as needed
    host = "192.168.50.2"
    port = 8000
    ws_url = f"ws://{host}:{port}/ws/v1"
    app = App(ws_url)
    try:
        app.mainloop()
    finally:
        app.ws_app.stop()
