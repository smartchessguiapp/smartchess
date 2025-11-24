<#
tools/start-dev.ps1
PowerShell helper to start the development environment for host_client.

What it does (safe-ordered):
  - Activate the local .venv if present
  - Set PYTHONPATH
  - Stop any existing mock_server.py and mf_tab_tk.py processes
  - Start mock_server.py in background (hidden)
  - Optionally run a short end-to-end test (tools/test_set_ao.py)
  - Start the GUI (host_client/mf_tab_tk.py) as a visible window
  - Show netstat and recent test log tail

Usage:
  .\.venv\Scripts\Activate.ps1
  .\tools\start-dev.ps1

You can edit the script to remove the test step if you don't want automatic POSTs.
#>

# ensure running from repo root
Set-Location -Path (Split-Path -Parent $MyInvocation.MyCommand.Definition)
Set-Location -Path ..\

Write-Output "[start-dev] Working directory: $(Get-Location)"

# Activate virtualenv if it exists
if (Test-Path '.venv\Scripts\Activate.ps1') {
    Write-Output '[start-dev] Activating virtualenv .venv'
    . .\.venv\Scripts\Activate.ps1
} else {
    Write-Output '[start-dev] Warning: .venv not found. Create it with: python -m venv .venv' -ForegroundColor Yellow
}

# set PYTHONPATH for local imports
$env:PYTHONPATH = '.'

# Stop existing mock_server and GUI processes (safe)
Write-Output '[start-dev] Stopping existing mock_server.py and mf_tab_tk.py processes (if any)'
Get-CimInstance Win32_Process |
    Where-Object { $_.CommandLine -and ($_.CommandLine -match 'mock_server.py' -or $_.CommandLine -match 'mf_tab_tk.py') } |
    ForEach-Object {
        try {
            Write-Output "Stopping PID $($_.ProcessId): $($_.CommandLine)"
            Stop-Process -Id $_.ProcessId -Force -ErrorAction SilentlyContinue
        } catch {
            Write-Output "Failed to stop PID $($_.ProcessId): $_" -ForegroundColor Yellow
        }
    }

Start-Sleep -Milliseconds 500

# Start mock_server in background
Write-Output '[start-dev] Starting mock_server in background'
Start-Process -FilePath python -ArgumentList 'host_client\\mock_server.py' -WindowStyle Hidden
Start-Sleep -Seconds 1

Write-Output '[start-dev] Verify mock_server listening (port 8000)'
netstat -ano | findstr ":8000"

# Run quick AO test script to exercise POST+WS
if (Test-Path 'tools\test_set_ao.py') {
    Write-Output '[start-dev] Running tools/test_set_ao.py (POST AO + listen for WS; non-blocking)'
    try {
        python tools\test_set_ao.py
    } catch {
        Write-Output "[start-dev] test_set_ao failed: $_" -ForegroundColor Yellow
    }
    if (Test-Path 'tools\test_set_ao.log') {
        Write-Output '[start-dev] Last lines of tools/test_set_ao.log:'
        Get-Content tools\test_set_ao.log -Tail 20
    }
} else {
    Write-Output '[start-dev] No tools/test_set_ao.py found; skipping test'
}

# Start GUI (visible)
Write-Output '[start-dev] Starting GUI (host_client/mf_tab_tk.py)'
Start-Process -FilePath python -ArgumentList 'host_client\\mf_tab_tk.py' -WindowStyle Normal
Start-Sleep -Seconds 2

Write-Output '[start-dev] netstat / process list for verification'
netstat -ano | findstr ":8000"
Get-CimInstance Win32_Process | Where-Object { $_.CommandLine -and ( $_.CommandLine -match 'mock_server.py' -or $_.CommandLine -match 'mf_tab_tk.py' ) } | Select-Object ProcessId, CommandLine | Format-List

Write-Output '[start-dev] Done. If the GUI window is not visible, run: $env:PYTHONPATH="."; python host_client\\mf_tab_tk.py'
