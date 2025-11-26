"""
PC-specific MF Tab GUI (moved into `gui.pc`)

This file is the same GUI code but placed under `gui.pc` to make it explicit
that it targets a developer PC (Tkinter) rather than Raspberry Pi hardware.
"""
import tkinter as tk
from tkinter import ttk, messagebox
import threading

from host_client.client import SmartCheckerClient
from gui.pc.ws_tk_sample import WSApp
import time
import json
import os
import math
import random
try:
    import matplotlib
    matplotlib.use("TkAgg")
    from matplotlib.figure import Figure
    from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
    HAS_MATPLOTLIB = True
except Exception:
    HAS_MATPLOTLIB = False


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


class MFDIControls(ttk.Frame):
    def __init__(self, parent, base_url: str):
        super().__init__(parent, padding=8)
        ttk.Label(self, text="MFDI Controls", font=(None, 12, "bold")).pack(anchor="w")

        self.client = SmartCheckerClient(base_url=base_url)

        cb_frame = ttk.Frame(self)
        cb_frame.pack(anchor="w", pady=6)

        self.bit_vars = [tk.IntVar(value=0) for _ in range(8)]
        for i in range(8):
            cb = ttk.Checkbutton(cb_frame, text=f"CH{i+1}", variable=self.bit_vars[i])
            cb.grid(row=0, column=i, padx=4)

        btn_frame = ttk.Frame(self)
        btn_frame.pack(anchor="w", pady=8)
        ttk.Button(btn_frame, text="反映", command=self._do_reflect).grid(row=0, column=0, padx=6)
        ttk.Button(btn_frame, text="STOP", command=self._do_stop).grid(row=0, column=1, padx=6)

        # --- MFAI: Analog Input controls (AI1, AI2)
        ttk.Separator(self, orient=tk.HORIZONTAL).pack(fill="x", pady=(8,6))
        ttk.Label(self, text="MFAI (Analog Input to inverter)", font=(None, 10, "bold")).pack(anchor="w")
        self._aip_frame = ttk.Frame(self)
        self._aip_frame.pack(fill="x", pady=4)

        self._ai_param_vars = []
        self._ai_value_vars = []
        for i in range(2):
            row = ttk.Frame(self._aip_frame)
            row.pack(fill="x", pady=2)
            ttk.Label(row, text=f"AI{i+1}:").grid(row=0, column=0, sticky="w")
            param_var = tk.StringVar(value="0-10V")
            cb = ttk.Combobox(row, values=["0-10V", "-10-10V", "0-20mA", "4-20mA"], textvariable=param_var, width=10)
            cb.grid(row=0, column=1, padx=6)
            val_var = tk.StringVar(value="0.0")
            ttk.Entry(row, textvariable=val_var, width=10).grid(row=0, column=2, padx=6)
            ttk.Button(row, text="Set", command=(lambda idx=i, p=param_var, v=val_var: self._do_set_ai(idx+1, p.get(), v.get()))).grid(row=0, column=3, padx=6)
            lbl = ttk.Label(row, text=" ")
            lbl.grid(row=0, column=4, padx=6)
            self._ai_param_vars.append(param_var)
            self._ai_value_vars.append((val_var, lbl))

    def _run_in_bg(self, fn, *args, on_done=None):
        def worker():
            try:
                res = fn(*args)
            except Exception as e:
                self.after(0, lambda: messagebox.showerror("Error", str(e)))
                return
            if on_done:
                self.after(0, lambda: on_done(res))

        threading.Thread(target=worker, daemon=True).start()

    def _assemble_bits(self) -> int:
        v = 0
        for i in range(8):
            try:
                b = int(self.bit_vars[i].get())
            except Exception:
                b = 0
            if b:
                v |= (1 << i)
        return v

    def _do_reflect(self):
        v = self._assemble_bits()
        self._run_in_bg(lambda: self.client.put_di_pending(di_byte=v), on_done=lambda r: messagebox.showinfo("OK", f"pending set to {v}"))

    def _do_stop(self):
        for i in range(8):
            try:
                self.bit_vars[i].set(0)
            except Exception:
                pass
        self._run_in_bg(lambda: self.client.put_di_pending(di_byte=0), on_done=lambda r: messagebox.showinfo("OK", "all channels STOPPED"))

    def _do_set_ai(self, channel: int, mode: str, raw_value: str):
        try:
            if mode in ("0-10V", "-10-10V"):
                v = round(float(raw_value), 1)
            else:
                v = int(float(raw_value))
        except Exception:
            messagebox.showerror("Invalid", f"Invalid value for {mode}: {raw_value}")
            return

        def worker():
            try:
                payload = {"channel": f"ao{channel}", "mode": mode, "value": v}
                res = self.client._post("/io/ao", payload)
            except Exception as e:
                self.after(0, lambda: messagebox.showerror("Error", str(e)))
                return
            err = abs(v) * 0.0001
            import time as _time
            ts = _time.strftime("%Y-%m-%d %H:%M:%S")
            try:
                val_var, lbl = self._ai_value_vars[channel-1]
                self.after(0, lambda: lbl.config(text=f"err±{err:.4f} ts:{ts}"))
                self.after(0, lambda: messagebox.showinfo("OK", f"AO{channel} set: {res}"))
            except Exception:
                pass

        threading.Thread(target=worker, daemon=True).start()


class MFDOIndicators(ttk.Frame):
    ITEMS = ["P1-PC", "P2-PC", "MA-MB-MC", "M1-M2", "M3-M4"]

    def __init__(self, parent, ws_url: str):
        super().__init__(parent, padding=8)
        ttk.Label(self, text="MFDO Indicators", font=(None, 12, "bold")).pack(anchor="w")

        self.leds = []
        grid = ttk.Frame(self)
        grid.pack(anchor="w", pady=6)
        for i, name in enumerate(self.ITEMS):
            li = LEDIndicator(grid, name)
            li.grid(row=i, column=0, sticky="w", pady=4)
            self.leds.append(li)

        self.status_label = ttk.Label(self, text="waiting for data...")
        self.status_label.pack(pady=6)

        ctrl = ttk.Frame(self)
        ctrl.pack(fill="x", pady=(2,6))
        ttk.Label(ctrl, text="MFAO Update:").pack(side="left")
        self._mfao_interval_var = tk.StringVar(value="500 ms")
        self._mfao_interval_cb = ttk.Combobox(ctrl, values=["200 ms", "500 ms", "1000 ms"], textvariable=self._mfao_interval_var, width=8)
        self._mfao_interval_cb.pack(side="left", padx=6)
        ttk.Label(ctrl, text="(client throttle)").pack(side="left")
        self._mfao_interval_ms = 500
        self._last_mfao_update = 0.0
        def _on_interval_change(event=None):
            try:
                s = self._mfao_interval_var.get()
                self._mfao_interval_ms = int(s.split()[0])
            except Exception:
                self._mfao_interval_ms = 500
        self._mfao_interval_cb.bind("<<ComboboxSelected>>", _on_interval_change)

        mfaof = ttk.Frame(self)
        mfaof.pack(fill="x", pady=(4,0))
        ttk.Label(mfaof, text="MFAO (Analog Output)").grid(row=0, column=0, columnspan=3, sticky="w")
        self._mfao_vars = [tk.StringVar(value="-"), tk.StringVar(value="-")]
        self._mfao_pbars = []
        for i in range(2):
            ttk.Label(mfaof, text=f"MFAO{i+1}:").grid(row=i+1, column=0, sticky="w", padx=(0,6))
            ttk.Label(mfaof, textvariable=self._mfao_vars[i]).grid(row=i+1, column=1, sticky="w")
            pb = ttk.Progressbar(mfaof, orient=tk.HORIZONTAL, length=150, mode='determinate')
            pb.grid(row=i+1, column=2, sticky="w", padx=6)
            self._mfao_pbars.append(pb)

        self.ws_app = WSApp(ws_url, self.on_status)
        self.ws_app.start()

    def on_status(self, data: dict):
        self.after(0, lambda: self._apply_status(data))

    def _apply_status(self, data: dict):
        now_ms = time.time() * 1000.0
        mfao_allowed = (now_ms - getattr(self, '_last_mfao_update', 0.0)) >= getattr(self, '_mfao_interval_ms', 500)
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
        dac = data.get("dac") or {}
        cur = dac.get("cur_freq")
        if cur is not None and mfao_allowed:
            try:
                self._mfao_vars[0].set(f"{cur:.2f} Hz")
                pct = min(100, max(0, (float(cur) / 60.0) * 100.0))
                self._mfao_pbars[0]['value'] = pct
            except Exception:
                pass
        ao1 = data.get("ao1")
        ao2 = data.get("ao2")
        if ao1 is not None and mfao_allowed:
            try:
                self._mfao_vars[0].set(str(ao1))
                self._mfao_pbars[0]['value'] = min(100, float(ao1))
            except Exception:
                pass
        if ao2 is not None and mfao_allowed:
            try:
                self._mfao_vars[1].set(str(ao2))
                self._mfao_pbars[1]['value'] = min(100, float(ao2))
            except Exception:
                pass
        if mfao_allowed:
            self._last_mfao_update = now_ms

    def stop(self):
        try:
            self.ws_app.stop()
        except Exception:
            pass


class MemobusFrame(ttk.Frame):
    BAUDS = ["1.2", "2.4", "4.8", "9.6", "19.2", "38.4", "57.6", "76.8", "115.2"]

    def __init__(self, parent, base_url: str):
        super().__init__(parent, padding=8)
        self.client = SmartCheckerClient(base_url=base_url)
        ctrl = ttk.Frame(self)
        ctrl.pack(fill="x", pady=4)

        ttk.Label(ctrl, text="ボーレート:").grid(row=0, column=0, sticky="w")
        self.baud_cb = ttk.Combobox(ctrl, values=self.BAUDS, width=6)
        self.baud_cb.set(self.BAUDS[4])
        self.baud_cb.grid(row=0, column=1, sticky="w", padx=4)

        ttk.Label(ctrl, text="データ長:").grid(row=0, column=2, sticky="w", padx=(12,0))
        ttk.Label(ctrl, text="8 bit").grid(row=0, column=3, sticky="w")

        ttk.Label(ctrl, text="パリティ:").grid(row=0, column=4, sticky="w", padx=(12,0))
        self.parity_cb = ttk.Combobox(ctrl, values=["None", "Even", "Odd"], width=6)
        self.parity_cb.set("None")
        self.parity_cb.grid(row=0, column=5, sticky="w")

        ttk.Label(ctrl, text="ストップビット:").grid(row=0, column=6, sticky="w", padx=(12,0))
        ttk.Label(ctrl, text="1").grid(row=0, column=7, sticky="w")

        self._online = False
        self.online_btn = ttk.Button(ctrl, text="OFFLINE", command=self._toggle_online)
        self.online_btn.grid(row=0, column=8, padx=12)

        log_frame = ttk.Frame(self)
        log_frame.pack(fill="both", expand=False, pady=6)
        filt_frame = ttk.Frame(log_frame)
        filt_frame.pack(anchor="w")
        self.show_tx = tk.BooleanVar(value=True)
        self.show_rx = tk.BooleanVar(value=True)
        self.auto_scroll = tk.BooleanVar(value=True)
        ttk.Checkbutton(filt_frame, text="TX", variable=self.show_tx, command=self._render_logs).pack(side="left")
        ttk.Checkbutton(filt_frame, text="RX", variable=self.show_rx, command=self._render_logs).pack(side="left")

        self.log_text = tk.Text(log_frame, height=8, wrap="none")
        vs = ttk.Scrollbar(log_frame, orient="vertical", command=self.log_text.yview)
        self.log_text.configure(yscrollcommand=vs.set)
        self.log_text.pack(side="left", fill="x", expand=True)
        vs.pack(side="left", fill="y")
        try:
            self.log_text.tag_config('TX', foreground='#1f77b4')
            self.log_text.tag_config('RX', foreground='#2ca02c')
            self.log_text.tag_config('ERR', foreground='#d62728')
            self.log_text.tag_config('TS', foreground='#888888')
        except Exception:
            pass

        tbl_frame = ttk.Frame(self)
        tbl_frame.pack(fill="both", expand=True, pady=6)

        hdr = ttk.Frame(tbl_frame)
        hdr.pack(fill="x")
        ttk.Label(hdr, text="Address", width=12).grid(row=0, column=0)
        ttk.Label(hdr, text="Read", width=8).grid(row=0, column=1)
        ttk.Label(hdr, text="Write", width=8).grid(row=0, column=2)
        ttk.Label(hdr, text="Data", width=24).grid(row=0, column=3)

        canvas = tk.Canvas(tbl_frame, height=180)
        vs = ttk.Scrollbar(tbl_frame, orient="vertical", command=canvas.yview)
        canvas.configure(yscrollcommand=vs.set)
        canvas.pack(side="left", fill="both", expand=True)
        vs.pack(side="left", fill="y")

        self.rows_container = ttk.Frame(canvas)
        self._rows_window = canvas.create_window((0, 0), window=self.rows_container, anchor="nw")

        def _on_frame_config(event):
            canvas.configure(scrollregion=canvas.bbox("all"))
        self.rows_container.bind("<Configure>", _on_frame_config)

        def _on_canvas_config(event):
            canvas.itemconfig(self._rows_window, width=event.width)
        canvas.bind("<Configure>", _on_canvas_config)

        self._rows = []

        ttk.Button(self, text="行を追加", command=self._add_row).pack(anchor="w", pady=(4,0))

        self._logs = []
        self._log_capacity = 200

    def _insert_log_line(self, kind: str, text: str):
        ts = time.strftime("[%Y-%m-%d %H:%M:%S]")
        prefix = f"{ts} [{kind}] "
        body = text
        if kind == 'RX':
            try:
                j = json.loads(text)
                body = json.dumps(j, ensure_ascii=False, indent=2)
            except Exception:
                body = text
        self.log_text.insert('end', prefix, ('TS',))
        self.log_text.insert('end', body + "\n", (kind,))
        if self.auto_scroll.get():
            self.log_text.see('end')

    def _log(self, kind: str, text: str):
        self._logs.append((kind, text))
        if len(self._logs) > getattr(self, '_log_capacity', 1000):
            excess = len(self._logs) - self._log_capacity
            if excess > 0:
                del self._logs[0:excess]
        if (kind == "TX" and self.show_tx.get()) or (kind == "RX" and self.show_rx.get()):
            self._insert_log_line(kind, text)

    def _render_logs(self):
        self.log_text.delete("1.0", "end")
        for kind, text in self._logs:
            if (kind == "TX" and self.show_tx.get()) or (kind == "RX" and self.show_rx.get()):
                self._insert_log_line(kind, text)

    def _toggle_online(self):
        self._online = not self._online
        if self._online:
            self.online_btn.config(text="ONLINE")
            self.online_btn.config(style="Online.TButton")
            self._log("TX", "ONLINE pressed (simulated connect)")
        else:
            self.online_btn.config(text="OFFLINE")
            self.online_btn.config(style="TButton")
            self._log("TX", "OFFLINE pressed (simulated disconnect)")

    def _add_row(self):
        row = ttk.Frame(self.rows_container)
        row.pack(fill="x", pady=2, padx=2)
        self._rows.append(row)

        addr_var = tk.StringVar(value="0")
        data_var = tk.StringVar(value="-")
        addr_entry = ttk.Entry(row, textvariable=addr_var, width=12)
        addr_entry.grid(row=0, column=0, padx=4)
        data_entry = ttk.Entry(row, textvariable=data_var, width=24)
        data_entry.grid(row=0, column=3, padx=4)

        def do_read(event=None):
            try:
                addr = int(addr_var.get())
            except Exception:
                messagebox.showerror("Invalid", "Address must be integer")
                return
            self._log("TX", f"modbus_read addr={addr}")

            def on_done(res):
                try:
                    data = res.get("values") if isinstance(res, dict) else res
                    if isinstance(data, list) and data:
                        data_entry.delete(0, "end")
                        data_entry.insert(0, str(data[0]))
                    else:
                        data_entry.delete(0, "end")
                        data_entry.insert(0, str(data))
                    self._log("RX", json.dumps(res))
                except Exception:
                    self._log("RX", f"invalid read response: {res}")

            threading.Thread(target=lambda: self._call_read(addr, on_done), daemon=True).start()

        def do_write():
            try:
                addr = int(addr_var.get())
                val = int(data_var.get())
            except Exception:
                messagebox.showerror("Invalid", "Address and Data must be integers")
                return
            self._log("TX", f"modbus_write addr={addr} val={val}")

            def on_done(res):
                self._log("RX", json.dumps(res))

            threading.Thread(target=lambda: self._call_write(addr, val, on_done), daemon=True).start()

        read_btn = ttk.Button(row, text="Read", command=do_read)
        read_btn.grid(row=0, column=1, padx=4)
        write_btn = ttk.Button(row, text="Write", command=do_write)
        write_btn.grid(row=0, column=2, padx=4)

        def do_delete(r=row):
            try:
                r.destroy()
            except Exception:
                pass
            try:
                self._rows.remove(r)
            except ValueError:
                pass

        del_btn = ttk.Button(row, text="削除", command=do_delete)
        del_btn.grid(row=0, column=4, padx=4)

        addr_entry.bind("<Return>", do_read)

    def _call_read(self, addr: int, on_done):
        try:
            res = self.client.modbus_read(address=addr, quantity=1)
        except Exception as e:
            self.after(0, lambda: messagebox.showerror("Error", str(e)))
            return
        self.after(0, lambda: on_done(res))

    def _call_write(self, addr: int, val: int, on_done):
        try:
            res = self.client.modbus_write(address=addr, value=val)
        except Exception as e:
            self.after(0, lambda: messagebox.showerror("Error", str(e)))
            return
        self.after(0, lambda: on_done(res))


class TestFrame(ttk.Frame):
    # Profiles UI removed: persistent profile list/editor is not present in this variant.
    def __init__(self, parent, base_url: str):
        super().__init__(parent, padding=8)
        self.client = SmartCheckerClient(base_url=base_url)
        self._running = False
        self._paused = False
        self._run_id = None

        main = ttk.Panedwindow(self, orient=tk.HORIZONTAL)
        main.pack(fill="both", expand=True)

        right = ttk.Frame(main)
        main.add(right, weight=1)

        self.voltage_class = tk.StringVar(value="200V")

        ttk.Label(right, text="Running Parameters", font=(None, 10, "bold")).pack(anchor="w")
        ttk.Label(right, text="Set the parameters below and press Start to send a run request to the RPi HIL Service.").pack(anchor="w")

        ctrl = ttk.Frame(right)
        ctrl.pack(fill="x", pady=4)
        ttk.Button(ctrl, text="RUN", command=self._start).pack(side="left", padx=6)
        ttk.Button(ctrl, text="Stop", command=self._stop).pack(side="left", padx=6)

        params = ttk.Frame(right)
        params.pack(fill="x", pady=4)
        ttk.Label(params, text="電圧クラス:").grid(row=0, column=0, sticky="w")
        self.voltage_cb = ttk.Combobox(params, values=["200V", "400V"], textvariable=self.voltage_class, width=6)
        self.voltage_cb.grid(row=0, column=1, sticky="w", padx=4)
        self.voltage_cb.set(self.voltage_class.get())
        ttk.Label(params, text="最高周波数(Hz):").grid(row=0, column=2, sticky="w", padx=(12,0))
        self.highest_freq_var = tk.StringVar(value="60.00")
        ttk.Entry(params, textvariable=self.highest_freq_var, width=8).grid(row=0, column=3, sticky="w")
        ttk.Label(params, text="指令周波数(Hz):").grid(row=0, column=4, sticky="w", padx=(12,0))
        self.command_freq_var = tk.StringVar(value="60.00")
        ttk.Entry(params, textvariable=self.command_freq_var, width=8).grid(row=0, column=5, sticky="w")
        self.direction_var = tk.StringVar(value="FWD")
        dir_frame = ttk.Frame(params)
        dir_frame.grid(row=0, column=6, sticky="w", padx=(8,0))
        ttk.Radiobutton(dir_frame, text="FWD", value="FWD", variable=self.direction_var).pack(side="left")
        ttk.Radiobutton(dir_frame, text="REV", value="REV", variable=self.direction_var).pack(side="left")
        ttk.Label(params, text="可減速時間(s):").grid(row=0, column=7, sticky="w", padx=(12,0))
        self.decel_time_var = tk.StringVar(value="10.00")
        ttk.Entry(params, textvariable=self.decel_time_var, width=8).grid(row=0, column=8, sticky="w")
        ttk.Label(params, text="出力周波数(Hz):").grid(row=0, column=9, sticky="w", padx=(12,0))
        self.output_freq_var = tk.StringVar(value="0.00")
        ttk.Label(params, textvariable=self.output_freq_var).grid(row=0, column=10, sticky="w", padx=4)

        status_frame = ttk.Frame(right)
        status_frame.pack(fill="both", expand=True, pady=4)
        self.status_label = ttk.Label(status_frame, text="idle")
        self.status_label.pack(anchor="w")

        if HAS_MATPLOTLIB:
            self.fig = Figure(figsize=(6, 2))
            self.ax = self.fig.add_subplot(111)
            self.ax.set_title("Waveform")
            self.ax.set_xlabel("time(s)")
            self.ax.set_ylabel("output freq (Hz)")
            self._xdata = []
            self._ydata = []
            self.line, = self.ax.plot(self._xdata, self._ydata)
            self.canvas = FigureCanvasTkAgg(self.fig, master=status_frame)
            self.canvas.get_tk_widget().pack(fill="both", expand=True)
        else:
            ttk.Label(status_frame, text="matplotlib not installed — waveform disabled").pack()

        logf = ttk.Frame(right)
        logf.pack(fill="both", expand=False, pady=4)
        ttk.Label(logf, text="Log").pack(anchor="w")
        self.log_text = tk.Text(logf, height=6)
        self.log_text.pack(fill="both", expand=True)
        csvf = ttk.Frame(logf)
        csvf.pack(fill="x")
        ttk.Button(csvf, text="Export CSV", command=self._export_csv).pack(side="left")

        ttk.Label(right, text="Script Console").pack(anchor="w")
        scf = ttk.Frame(right)
        scf.pack(fill="both", expand=False)
        self.script_text = tk.Text(scf, height=4)
        self.script_text.pack(fill="both", expand=True)
        ttk.Button(scf, text="Run Script", command=self._run_script).pack()

        self._start_time = None

    def _log(self, text: str):
        ts = time.strftime("[%Y-%m-%d %H:%M:%S]")
        self.log_text.insert("end", f"{ts} {text}\n")
        self.log_text.see("end")

    def _start(self):
        if self._running:
            self._log("Already running")
            return
        try:
            hf = float(self.highest_freq_var.get())
            raw_cmd = float(self.command_freq_var.get())
            dt = float(self.decel_time_var.get())
        except Exception:
            messagebox.showerror("Invalid", "Frequency/time parameters must be numbers")
            return
        hf = max(0.0, min(590.0, hf))
        mag = max(0.0, min(590.0, abs(raw_cmd)))
        if getattr(self, 'direction_var', None) and self.direction_var.get() == "REV":
            cmd = -mag
        else:
            cmd = mag
        dt = max(0.0, dt)

        self._running = True
        self._paused = False
        self._start_time = time.time()
        self.status_label.config(text="running")
        self._log(f"Run requested dir={getattr(self, 'direction_var', tk.StringVar(value='FWD')).get()} cmd={cmd}")

        try:
            payload = {
                "voltage_class": self.voltage_class.get(),
                "highest_freq": hf,
                "command_freq": cmd,
                "decel_time": dt,
            }
            try:
                res = self.client._post("/profile/run", payload)
                self._run_id = res.get("run_id")
                self._log(f"Run started: {res}")
            except Exception:
                self._log("/profile/run not supported by server (simulated run)")
        except Exception as e:
            self._log(f"Start failed: {e}")

        if hf <= 0.0:
            t_target = 0.0
        else:
            t_target = dt * (abs(cmd) / hf)
        try:
            start_val = float(self.output_freq_var.get())
        except Exception:
            start_val = 0.0
        self._ramp_start_value = start_val
        self._ramp_target = cmd
        self._ramp_ttarget = t_target
        self._ramp_start = time.time()
        self._xdata = []
        self._ydata = []
        self._simulate_step()

    def _stop(self):
        if not self._running:
            return
        try:
            cur = float(self.output_freq_var.get())
        except Exception:
            cur = (self._ydata[-1] if hasattr(self, "_ydata") and self._ydata else 0.0)
        try:
            hf = float(self.highest_freq_var.get())
        except Exception:
            hf = 60.0
        try:
            dt = float(self.decel_time_var.get())
        except Exception:
            dt = 10.0
        hf = max(0.0, hf)
        dt = max(0.0, dt)
        if hf <= 0.0:
            t_target = 0.0
        else:
            t_target = dt * (abs(cur) / hf)
        self._ramp_start_value = cur
        self._ramp_target = 0.0
        self._ramp_ttarget = t_target
        self._ramp_start = time.time()
        self.status_label.config(text="stopping")
        self._log("Stop requested")
        try:
            if self._run_id:
                self.client._post("/profile/stop", {"run_id": self._run_id})
        except Exception:
            pass

    def _simulate_step(self):
        if not self._running:
            return
        elapsed = time.time() - (self._ramp_start or time.time())
        if self._ramp_ttarget <= 0.0:
            frac = 1.0
        else:
            frac = min(1.0, elapsed / self._ramp_ttarget)
        start = float(getattr(self, "_ramp_start_value", 0.0))
        out = start + (self._ramp_target - start) * frac
        t = elapsed
        self._xdata.append(t)
        self._ydata.append(out)
        if len(self._xdata) > 200:
            self._xdata.pop(0); self._ydata.pop(0)
        if HAS_MATPLOTLIB:
            self.line.set_data(self._xdata, self._ydata)
            self.ax.relim(); self.ax.autoscale_view()
            self.canvas.draw_idle()
        self.output_freq_var.set(f"{out:.2f}")
        self._log(f"output_freq {t:.2f} -> {out:.2f}")
        if frac < 1.0:
            self.after(200, self._simulate_step)
        else:
            if abs(self._ramp_target) < 1e-6:
                self._running = False
                self.status_label.config(text="stopped")
                self._log("Stopped")
                return
            else:
                self.after(500, self._simulate_step)

    def _export_csv(self):
        if not hasattr(self, "_xdata") or not self._xdata:
            messagebox.showinfo("No data", "No samples to export")
            return
        outdir = os.path.join("reports", "system")
        os.makedirs(outdir, exist_ok=True)
        path = os.path.join(outdir, f"profile-{int(time.time())}-data.csv")
        with open(path, "w", encoding="utf-8") as f:
            f.write("time,value\n")
            for x, y in zip(self._xdata, self._ydata):
                f.write(f"{x:.6f},{y:.6f}\n")
        messagebox.showinfo("Saved", f"CSV saved to {path}")
        self._log(f"Exported CSV to {path}")

    def _run_script(self):
        txt = self.script_text.get("1.0", "end").strip()
        if not txt:
            return
        self._log("Script executed (simulated):\n" + txt.replace("\n", "\\n"))


class MFTabApp(tk.Tk):
    def __init__(self, host: str = "127.0.0.1", port: int = 8000):
        super().__init__()
        self.title("MF Tab")
        self.geometry("820x420")

        base_url = f"http://{host}:{port}/api/v1"
        ws_url = f"ws://{host}:{port}/ws/v1"

        nb = ttk.Notebook(self)
        nb.pack(fill="both", expand=True)

        mem_tab = ttk.Frame(nb)
        nb.add(mem_tab, text="MEMOBUS通信")
        mem_frame = MemobusFrame(mem_tab, base_url=base_url)
        mem_frame.pack(fill="both", expand=True)

        run_tab = ttk.Frame(nb)
        nb.add(run_tab, text="Run")
        run_frame = TestFrame(run_tab, base_url=base_url)
        run_frame.pack(fill="both", expand=True)

        mf_tab = ttk.Frame(nb)
        nb.add(mf_tab, text="MF")
        paned = ttk.Panedwindow(mf_tab, orient=tk.HORIZONTAL)
        paned.pack(fill="both", expand=True)
        left = MFDIControls(paned, base_url=base_url)
        right = MFDOIndicators(paned, ws_url=ws_url)
        paned.add(left, weight=1)
        paned.add(right, weight=0)

        self._right = right
        self.protocol("WM_DELETE_WINDOW", self._on_close)

    def _on_close(self):
        try:
            self._right.stop()
        except Exception:
            pass
        self.destroy()


if __name__ == "__main__":
    app = MFTabApp()
    app.mainloop()
