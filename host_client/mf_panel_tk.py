"""
Combined MF Panel

Shows MFDO controls (left) and MFDI indicators (right) in one window.

Run with:
  python host_client/mf_panel_tk.py

It reuses the existing minimal client and WS helper.
"""
import math
import threading
import tkinter as tk
from tkinter import ttk, messagebox

from host_client.client import SmartCheckerClient
from gui.pc.ws_tk_sample import WSApp


class LEDIndicator(ttk.Frame):
    def __init__(self, parent, label: str):
        super().__init__(parent)
        self.canvas = tk.Canvas(self, width=28, height=28, highlightthickness=0)
        self.canvas.grid(row=0, column=0, padx=(0, 8))
        self.text = ttk.Label(self, text=label)
        self.text.grid(row=0, column=1)
        self._id = self.canvas.create_oval(3, 3, 25, 25, fill="#888888")

    def set_on(self, on: bool):
        color = "#33cc33" if on else "#888888"
        self.canvas.itemconfigure(self._id, fill=color)


class MFDOFrame(ttk.Frame):
    def __init__(self, parent, base_url: str):
        super().__init__(parent, padding=8)
        self.client = SmartCheckerClient(base_url=base_url)

        ttk.Label(self, text="MFDO Controls", font=(None, 12, "bold")).grid(row=0, column=0, columnspan=3, sticky="w")

        ttk.Label(self, text="Frequency (Hz):").grid(row=1, column=0, sticky="w")
        self.freq_var = tk.DoubleVar(value=50.0)
        self.freq_entry = ttk.Entry(self, textvariable=self.freq_var, width=8)
        self.freq_entry.grid(row=1, column=1, sticky="w")
        self.freq_scale = ttk.Scale(self, from_=0.1, to=1000.0, orient="horizontal", command=self._on_scale)
        self.freq_scale.set(self.freq_var.get())
        self.freq_scale.grid(row=2, column=0, columnspan=3, sticky="we", pady=6)

        ttk.Label(self, text="Accel (s):").grid(row=3, column=0, sticky="w")
        self.acc_var = tk.DoubleVar(value=0.1)
        self.acc_entry = ttk.Entry(self, textvariable=self.acc_var, width=8)
        self.acc_entry.grid(row=3, column=1, sticky="w")

        btn_frame = ttk.Frame(self)
        btn_frame.grid(row=4, column=0, columnspan=3, pady=8)
        ttk.Button(btn_frame, text="Apply", command=self._do_apply).grid(row=0, column=0, padx=6)
        ttk.Button(btn_frame, text="Start", command=self._do_start).grid(row=0, column=1, padx=6)
        ttk.Button(btn_frame, text="STOP", command=self._do_stop).grid(row=0, column=2, padx=6)

        self.status_var = tk.StringVar(value="DAC: idle")
        ttk.Label(self, textvariable=self.status_var).grid(row=5, column=0, columnspan=3, sticky="w", pady=(6,0))

        self.canvas = tk.Canvas(self, height=100, bg="#111111")
        self.canvas.grid(row=6, column=0, columnspan=3, sticky="we", pady=8)
        self._draw_preview()

        # periodic update
        self.after(1000, self._tick)

    def _on_scale(self, v):
        try:
            self.freq_var.set(float(v))
            self._draw_preview()
        except Exception:
            pass

    def _draw_preview(self):
        w = self.canvas.winfo_width() or 360
        h = 100
        self.canvas.delete("all")
        freq = max(0.1, float(self.freq_var.get()))
        amp = 0.4
        samples = 160
        for i in range(samples - 1):
            x1 = i * (w / (samples - 1))
            x2 = (i + 1) * (w / (samples - 1))
            t1 = i / samples
            t2 = (i + 1) / samples
            y1 = h / 2 + math.sin(2 * math.pi * freq * t1) * (h * amp / 2)
            y2 = h / 2 + math.sin(2 * math.pi * freq * t2) * (h * amp / 2)
            self.canvas.create_line(x1, y1, x2, y2, fill="#33ff66")

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

    def _do_apply(self):
        try:
            freq = float(self.freq_var.get())
            acc = float(self.acc_var.get())
        except Exception:
            messagebox.showerror("Invalid", "Frequency/Accel must be numbers")
            return
        self._run_in_bg(lambda: self.client.put_dac_command(freq_hz=freq, acc_s=acc), on_done=lambda r: messagebox.showinfo("OK", "command applied"))

    def _do_start(self):
        try:
            freq = float(self.freq_var.get())
            acc = float(self.acc_var.get())
        except Exception:
            messagebox.showerror("Invalid", "Frequency/Accel must be numbers")
            return
        self._run_in_bg(lambda: self.client.post_dac_run(freq_hz=freq, acc_s=acc), on_done=lambda r: messagebox.showinfo("OK", "MFDO started"))

    def _do_stop(self):
        self._run_in_bg(lambda: self.client.post_dac_stop(acc_s=0.0), on_done=lambda r: messagebox.showinfo("OK", "MFDO stopped"))

    def _tick(self):
        def on_done(res):
            try:
                st = res.get("data") if isinstance(res, dict) else res
                if st:
                    status = st.get("status", "ok") if isinstance(st, dict) else str(st)
                else:
                    status = "idle"
                self.status_var.set(f"DAC: {status}")
            except Exception:
                pass

        self._run_in_bg(lambda: self.client.get_dac_status(), on_done=on_done)
        self._draw_preview()
        self.after(1000, self._tick)


class MFDIFrame(ttk.Frame):
    ITEMS = ["P1-PC", "P2-PC", "MA-MB-MC", "M1-M2", "M3-M4"]

    def __init__(self, parent, ws_url: str):
        super().__init__(parent, padding=8)
        ttk.Label(self, text="MFDI Indicators", font=(None, 12, "bold")).pack(anchor="w")
        self.leds = []
        grid = ttk.Frame(self)
        grid.pack(anchor="w", pady=6)
        for i, name in enumerate(self.ITEMS):
            li = LEDIndicator(grid, name)
            li.grid(row=i, column=0, sticky="w", pady=4)
            self.leds.append(li)

        self.status_label = ttk.Label(self, text="waiting for data...")
        self.status_label.pack(pady=6)

        # WS client
        self.ws_app = WSApp(ws_url, self.on_status)
        self.ws_app.start()

    def on_status(self, data: dict):
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
        for i in range(len(self.leds)):
            bit = (v >> i) & 1
            self.leds[i].set_on(bool(bit))
        self.status_label.config(text=f"di_byte={v}")

    def stop(self):
        try:
            self.ws_app.stop()
        except Exception:
            pass


class MFPanelApp(tk.Tk):
    def __init__(self, host: str = "127.0.0.1", port: int = 8000):
        super().__init__()
        self.title("MF Panel")
        self.geometry("820x420")

        base_url = f"http://{host}:{port}/api/v1"
        ws_url = f"ws://{host}:{port}/ws/v1"

        paned = ttk.Panedwindow(self, orient=tk.HORIZONTAL)
        paned.pack(fill="both", expand=True)

        left = MFDOFrame(paned, base_url=base_url)
        right = MFDIFrame(paned, ws_url=ws_url)

        paned.add(left, weight=1)
        paned.add(right, weight=0)

        self._mfdiframe = right

        self.protocol("WM_DELETE_WINDOW", self._on_close)

    def _on_close(self):
        try:
            self._mfdiframe.stop()
        except Exception:
            pass
        self.destroy()


if __name__ == "__main__":
    app = MFPanelApp()
    app.mainloop()
