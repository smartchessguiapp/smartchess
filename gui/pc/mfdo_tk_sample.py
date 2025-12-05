"""
MFDO control panel sample (PC package)

Placed under `gui/pc` for clarity: PC-targeted GUI samples live here.
"""
import math
import threading
import tkinter as tk
from tkinter import ttk, messagebox

from host_client.client import SmartCheckerClient


class MFDOGUI(tk.Tk):
    def __init__(self, base_url: str):
        super().__init__()
        self.title("MFDO Control")
        self.geometry("520x320")

        self.client = SmartCheckerClient(base_url=base_url)

        # Controls frame
        frm = ttk.Frame(self, padding=8)
        frm.pack(fill="x")

        # Frequency
        ttk.Label(frm, text="Frequency (Hz):").grid(row=0, column=0, sticky="w")
        self.freq_var = tk.DoubleVar(value=50.0)
        self.freq_entry = ttk.Entry(frm, textvariable=self.freq_var, width=8)
        self.freq_entry.grid(row=0, column=1, sticky="w")
        self.freq_scale = ttk.Scale(frm, from_=0.1, to=1000.0, orient="horizontal", command=self._on_scale)
        self.freq_scale.set(self.freq_var.get())
        self.freq_scale.grid(row=0, column=2, columnspan=3, sticky="we", padx=8)

        # Acceleration
        ttk.Label(frm, text="Accel (s):").grid(row=1, column=0, sticky="w")
        self.acc_var = tk.DoubleVar(value=0.1)
        self.acc_entry = ttk.Entry(frm, textvariable=self.acc_var, width=8)
        self.acc_entry.grid(row=1, column=1, sticky="w")

        # Buttons
        btn_frame = ttk.Frame(frm)
        btn_frame.grid(row=2, column=0, columnspan=4, pady=10)
        ttk.Button(btn_frame, text="Apply", command=self._do_apply).grid(row=0, column=0, padx=6)
        ttk.Button(btn_frame, text="Start", command=self._do_start).grid(row=0, column=1, padx=6)
        ttk.Button(btn_frame, text="STOP", command=self._do_stop).grid(row=0, column=2, padx=6)

        # Status
        self.status_var = tk.StringVar(value="idle")
        ttk.Label(self, textvariable=self.status_var, font=(None, 11)).pack(pady=4)

        # Canvas preview
        self.canvas = tk.Canvas(self, height=120, bg="#111111")
        self.canvas.pack(fill="x", padx=8, pady=6)
        self._draw_preview()

        # periodic status update
        self.after(1000, self._tick)

    def _on_scale(self, v):
        try:
            self.freq_var.set(float(v))
            self._draw_preview()
        except Exception:
            pass

    def _draw_preview(self):
        w = self.canvas.winfo_width() or 480
        h = 120
        self.canvas.delete("all")
        freq = max(0.1, float(self.freq_var.get()))
        amp = 0.4  # normalized
        points = []
        samples = 200
        for i in range(samples):
            x = (i / (samples - 1)) * w
            t = i / samples
            y = h / 2 + math.sin(2 * math.pi * freq * t) * (h * amp / 2)
            points.append((x, y))
        # draw polyline
        for i in range(len(points) - 1):
            x1, y1 = points[i]
            x2, y2 = points[i + 1]
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
        # send DAC command (put)
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
        # update status from device
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
        # redraw preview in case freq changed via entry
        self._draw_preview()
        self.after(1000, self._tick)


if __name__ == "__main__":
    host = "127.0.0.1"
    port = 8000
    base = f"http://{host}:{port}/api/v1"
    app = MFDOGUI(base_url=base)
    app.mainloop()
