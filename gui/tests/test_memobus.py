import json
import tkinter as tk

import pytest

from gui.pc.mf_tab_tk import MemobusFrame


def make_frame():
    try:
        root = tk.Tk()
    except tk.TclError:
        pytest.skip("Tkinter not available in this environment")
    # avoid showing window
    root.withdraw()
    f = MemobusFrame(root, base_url="http://127.0.0.1:8000/api/v1")
    return root, f


def test_insert_log_pretty_print_and_tags():
    root, f = make_frame()
    try:
        obj = {"a": 1, "b": {"c": 3}}
        f._log('RX', json.dumps(obj))
        # get text content
        txt = f.log_text.get('1.0', 'end')
        # pretty-printed JSON should contain newlines and indent
        assert '"a": 1' in txt
        assert '\n' in txt
        # tags should include RX tag
        tags = f.log_text.tag_names()
        assert 'RX' in tags
    finally:
        root.destroy()


def test_log_capacity_trimming():
    root, f = make_frame()
    try:
        f._log_capacity = 3
        # add 5 entries
        for i in range(5):
            f._log('TX', f'msg{i}')
        # internal _logs should be trimmed to capacity
        assert len(f._logs) == 3
        # oldest entries removed, so remaining are msg2, msg3, msg4
        kinds_texts = f._logs
        assert kinds_texts[0][1] == 'msg2'
    finally:
        root.destroy()
