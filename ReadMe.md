<table border="0">
<tr>
<td width="400">
<img src="https://raw.githubusercontent.com/smartchessguiapp/smartchess/master/web/mainwindow.PNG">
</td>
<td>
Smartchess is a chess graphical user interface that supports a wide range of variants. It comes with a Java executable which you can run provided that the latest version of Java is installed on your computer.<br><br>

For details see the <a href="https://github.com/smartchessguiapp/smartchess/wiki">Wiki</a> !
</td>
</tr>
</table>
</table>
```

## Migration notes (GUI / Raspberry Pi reorganization)

The repository layout was reorganized to separate PC GUI code from Raspberry Pi-specific code and to centralize GUI tests/reports.

- PC GUI modules moved to `gui/pc/` (Tkinter samples and WS client samples). Do not edit old `host_client/pc/` copies — they have been removed.
- Raspberry Pi placeholder package is at `raspberry_pi/pi/` for Pi-specific code.
- GUI-related tests and test reports have been moved to `gui/tests/` and `gui/tests/reports/`.

If you are maintaining CI or deployment scripts, update any references from `host_client.pc.*` or `host_client.ws_tk_sample` to `gui.pc.*`.

For contributors: run tests from the repository root after activating the venv:

```powershell
.\.venv\Scripts\Activate.ps1
;$env:PYTHONPATH='.'
pytest -q gui/tests
```

If your CI runs on a headless system without Tcl/Tk, some GUI tests gracefully skip themselves when Tk is not available.

See `MIGRATION.md` for more details and a change log.