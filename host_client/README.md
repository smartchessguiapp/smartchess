SmartChecker Host Client (minimal)

This folder contains a minimal Python client to talk to the Raspberry Pi HIL Service.

Quick start

1. Create a virtualenv and install dependencies:

```powershell
````markdown
SmartChecker Host Client (minimal)

This folder contains a minimal Python client to talk to the Raspberry Pi HIL Service.

Quick start

1. Create a virtualenv and install dependencies:

```powershell
python -m venv .venv
.\.venv\Scripts\Activate.ps1
$env:PYTHONPATH='.'
pip install -r host_client\requirements.txt
```

2. Run tests:

```powershell
pytest -q
```

Usage example

```python
from host_client.client import SmartCheckerClient

c = SmartCheckerClient(base_url="http://192.168.50.2:8000/api/v1")
print(c.get_dac_status())
```

Notes

- The client is intentionally minimal and synchronous for REST calls. WebSocket handling uses `websocket-client` and runs in a background thread.
- Extend the client to your project's needs and add integration tests for the actual RPi hardware.

OpenAPI / Client generation

- An OpenAPI (v3) definition for the v1 API is available at `docs/openapi.yaml`.
- You can generate a full-featured Python client from the OpenAPI file. Two common ways:
	- openapi-python-client (Python-native)
		- Install: `pip install openapi-python-client`
		- Generate: `openapi-python-client generate --path docs/openapi.yaml --output host_client/openapi_client`
	- openapi-generator-cli (Java-based)
		- Install/Run (example):
			- `brew install openapi-generator` (macOS) or use the official JAR
			- `openapi-generator generate -i docs/openapi.yaml -g python -o host_client/openapi_client`

Notes:
- Generated clients provide typed models and more complete request/response handling than the minimal `SmartCheckerClient` provided here.
- If you add a generated client to the repo, prefer placing it under `host_client/openapi_client/` and update `host_client/requirements.txt` accordingly.
- Consider adding a CI job to validate `docs/openapi.yaml` (lint/validate) if the spec will be maintained in-repo.

Development: start mock server & GUI

The repository includes a local mock HIL server for development and a Tkinter-based GUI sample. Use the steps below to reproduce the dev workflow on Windows (PowerShell).

1. Prepare the virtual environment (if you haven't already):

```powershell
python -m venv .venv
.\.venv\Scripts\Activate.ps1
$env:PYTHONPATH='.'
pip install -r host_client\requirements.txt
```

2. Start the mock server (detached/background):

```powershell
# stop any previous mock_server instances first (safe)
Get-CimInstance Win32_Process | Where-Object { $_.CommandLine -and $_.CommandLine -match 'mock_server.py' } |
  ForEach-Object { Stop-Process -Id $_.ProcessId -Force }
# start mock server in background
Start-Process -FilePath python -ArgumentList 'host_client\mock_server.py' -WindowStyle Hidden
Start-Sleep -Seconds 1
# verify the server is listening on port 8000
netstat -ano | findstr ":8000"
```

3. Run the GUI (visible window):

```powershell
$env:PYTHONPATH='.'; Start-Process -FilePath python -ArgumentList 'host_client\pc\mf_tab_tk.py' -WindowStyle Normal
```

4. Quick end-to-end test (AO set → WS broadcast):

```powershell
python tools\test_set_ao.py
# a short run-log is appended to tools/test_set_ao.log for later inspection
Get-Content tools\test_set_ao.log -Tail 20
```

Troubleshooting

- If the GUI reports a WebSocket connection refused (WinError 10061), ensure the mock server is running and listening on port 8000. Use `netstat -ano | findstr ":8000"` to confirm.
- If multiple `mock_server.py` or `pc\\mf_tab_tk.py` processes are running (you may see duplicate entries in `Get-CimInstance Win32_Process`), stop the extra processes with `Stop-Process -Id <PID> -Force` and start a single instance.
- The GUI's WS client includes a simple exponential-backoff retry and an HTTP health-check; if the server is starting slowly, the GUI should reconnect automatically within a few seconds.

If you'd like, copy these commands into a short PowerShell script in `tools/` to automate startup for development.

````
