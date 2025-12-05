# PowerShell script to find remaining references to old GUI/test paths
# Usage: .\tools\find_old_refs.ps1

$patterns = @(
    'host_client/pc',
    'host_client\\pc',
    'host_client\.pc',
    'host_client\.ws_tk_sample',
    'tests/test_client.py',
    'tests/test_memobus.py',
    'tests/test_profile_manager.py',
    'tests/test_wsapp.py'
)

Write-Output "Scanning repository for old GUI/test path references (excluding .venv and .pytest_cache)...\n"

$root = Get-Location

Get-ChildItem -Recurse -File -ErrorAction SilentlyContinue | Where-Object {
    $_.FullName -notmatch "\\.venv\\" -and $_.FullName -notmatch "\\.pytest_cache\\" -and $_.FullName -notmatch "\\node_modules\\"
} | ForEach-Object {
    $file = $_.FullName
    $text = Get-Content -Raw -ErrorAction SilentlyContinue $file
    foreach ($p in $patterns) {
        if ($text -match [regex]::Escape($p)) {
            Write-Output "MATCH: $p -> $file"
        }
    }
}

Write-Output "\nScan complete."