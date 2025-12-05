"""
MFDI Tkinter sample (PC package)

This is a lightweight indicator sample located under
`gui/pc` to make PC-targeted code explicit.
"""
import json
import threading
import tkinter as tk
from tkinter import ttk

from gui.pc.ws_tk_sample import WSApp


ITEMS = ["P1-PC", "P2-PC", "MA-MB-MC", "M1-M2", "M3-M4"]


class LEDIndicator(ttk.Frame):
    def __init__(self, parent, label: str):
        super().__init__(parent)
        self.canvas = tk.Canvas(self, width=24, height=24, highlightthickness=0)
        self.canvas.grid(row=0, column=0, padx=(0, 6))
        self.text = ttk.Label(self, text=label)
        self.text.grid(row=0, column=1)
        self._id = self.canvas.create_oval(2, 2, 22, 22, fill="#888888")

    def set_on(self, on: bool):
        color = "#33cc33" if on else "#888888"
        self.canvas.itemconfigure(self._id, fill=color)


class MFDIGUI(tk.Tk):
    def __init__(self, ws_url: str):
        super().__init__()
        self.title("MFDI Indicators")
        self.geometry("360x160")

        frame = ttk.Frame(self, padding=12)
        frame.pack(fill="both", expand=True)

        self.leds = []
        for i, name in enumerate(ITEMS):
            li = LEDIndicator(frame, name)
            li.grid(row=i, column=0, sticky="w", pady=4)
            self.leds.append(li)

        self.status_label = ttk.Label(self, text="waiting for data...")
        self.status_label.pack(pady=6)

        # WS client
        self.ws_app = WSApp(ws_url, self.on_status)
        self.ws_app.start()

        # safe shutdown
        self.protocol("WM_DELETE_WINDOW", self._on_close)

    def on_status(self, data: dict):
        # called from WS thread
        self.after(0, lambda: self._apply_status(data))

    def _apply_status(self, data: dict):
        di = data.get("di_byte")
        if di is None:
            self.status_label.config(text="no di_byte in status")
            return
        try:
            v = int(di) & 0xFF
        except Exception:
            self.status_label.config(text="invalid di_byte")
            return
        # update LEDs according to mapping
        for i in range(len(self.leds)):
            bit = (v >> i) & 1
            self.leds[i].set_on(bool(bit))
        self.status_label.config(text=f"di_byte={v}")

    def _on_close(self):
        try:
            self.ws_app.stop()
        except Exception:
            pass
        self.destroy()


if __name__ == "__main__":
    host = "127.0.0.1"
    port = 8000
    ws_url = f"ws://{host}:{port}/ws/v1"
    app = MFDIGUI(ws_url)
    app.mainloop()
