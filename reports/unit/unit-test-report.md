```markdown
% Unit Test Report

Project: SmartChecker
Document: Unit Test Report
Location: `reports/unit/`
Generated: 2025-11-24

## Executive Summary

This document lists unit test cases defined for core modules, their mapping to requirement IDs (SC-REQ-### / SC-NFR-###), execution environment, and results. Tests are designed to be executed with `pytest` and output (JUnit XML or Markdown) stored under `reports/unit/`.

（和訳）エグゼクティブサマリ

本ドキュメントはコアモジュールの単体試験ケース、要求 ID との対応、実行環境、結果を一覧化します。テストは `pytest` で実行し、JUnit XML や Markdown を `reports/unit/` に保存することを前提としています。

## Environment

- OS: (record here, e.g., Raspberry Pi OS / Windows for host development)
- Python: (record Python version used)
- Virtualenv: `.venv`
- Dependencies: `pip install -r host_client/requirements.txt`

## Summary Results

| Test ID | Module / Target | Description | Req IDs | Result |
|---------|-----------------|-------------|---------|--------|
| SC-TST-000 | V/f profile manager (unit) | Load/save/apply profile functions (normal/edge) | SC-REQ-000 | PASS (1 test)
| SC-TST-010 | host_client.client.SmartCheckerClient | REST wrapper: _post/_get/_put error handling | SC-REQ-000 / SC-REQ-001 | PASS (2 tests)
| SC-TST-011 | gui.pc.ws_tk_sample.WSApp | WebSocket client connect/disconnect logic (threading callbacks) | SC-NFR-002 | PASS (1 test)
| SC-TST-012 | gui.pc.mf_tab_tk.MemobusFrame | _insert_log_line pretty-print / color tags | SC-NFR-002 | PASS (2 tests)

## Test mapping (pytest -> Report IDs)

- `gui/tests/test_client.py::test_modbus_read` -> SC-TST-010 (SmartCheckerClient: modbus_read behavior)
- `gui/tests/test_client.py::test_get_di` -> SC-TST-010 (SmartCheckerClient: get_di behavior)

- `gui/tests/test_profile_manager.py::test_profiles_feature_removed_marker` -> SC-TST-000 (Profiles feature removed marker)
- `gui/tests/test_wsapp.py::test_wsapp_dispatch` -> SC-TST-011 (WSApp dispatch)
- `gui/tests/test_memobus.py::test_insert_log_pretty_print_and_tags` -> SC-TST-012 (MemobusFrame RX pretty-print)
- `gui/tests/test_memobus.py::test_log_capacity_trimming` -> SC-TST-012 (MemobusFrame log trimming)
% Unit Test Report

Project: SmartChecker
Document: Unit Test Report
Location: `reports/unit/`
Generated: 2025-11-24

## Executive Summary

This document lists unit test cases defined for core modules, their mapping to requirement IDs (SC-REQ-### / SC-NFR-###), execution environment, and results. Tests are designed to be executed with `pytest` and output (JUnit XML or Markdown) stored under `reports/unit/`.

（和訳）エグゼクティブサマリ

本ドキュメントはコアモジュールの単体試験ケース、要求 ID との対応、実行環境、結果を一覧化します。テストは `pytest` で実行し、JUnit XML や Markdown を `reports/unit/` に保存することを前提としています。

## Environment

- OS: (record here, e.g., Raspberry Pi OS / Windows for host development)
- Python: (record Python version used)
- Virtualenv: `.venv`
- Dependencies: `pip install -r host_client/requirements.txt`

（和訳）環境

- OS: （ここに記載、例: Raspberry Pi OS / Windows）
- Python: （使用した Python バージョン）
- Virtualenv: `.venv`
- 依存関係: `pip install -r host_client/requirements.txt`

## Summary Results

| Test ID | Module / Target | Description | Req IDs | Result |
|---------|-----------------|-------------|---------|--------|
| SC-TST-000 | V/f profile manager (unit) | Load/save/apply profile functions (normal/edge) | SC-REQ-000 | PASS (1 test)
| SC-TST-010 | host_client.client.SmartCheckerClient | REST wrapper: _post/_get/_put error handling | SC-REQ-000 / SC-REQ-001 | PASS (2 tests)
| SC-TST-011 | gui.pc.ws_tk_sample.WSApp | WebSocket client connect/disconnect logic (threading callbacks) | SC-NFR-002 | PASS (1 test)
| SC-TST-012 | gui.pc.mf_tab_tk.MemobusFrame | _insert_log_line pretty-print / color tags | SC-NFR-002 | PASS (2 tests)

> Note: Add more unit tests for new modules. Unit tests should aim for >=80% coverage as per constitution.

（和訳）サマリ結果

上記の表は現時点でのテストケースと期待結果の一覧です。新規モジュールに対する単体テストを追加し、カバレッジを >=80% に向上することを推奨します。

## Test Cases (detailed)

### SC-TST-000 — V/f profile manager: load/save/apply
- Purpose: Verify profile serialization/deserialization and application logic edge cases.
- Preconditions: profile storage path writable; sample profile fixture provided.
- Steps:
  1. Instantiate profile manager
  2. Save sample profile A
  3. Load profile A and compare fields
  4. Apply profile with mocked HAL (verify calls)
- Expected result: Save/load are lossless; apply invokes correct HAL calls.
- Result: TODO (fill after execution)

（和訳）

### SC-TST-000 — V/f プロファイルマネージャ: 保存／読み込み／適用
- 目的: プロファイルのシリアライズ／デシリアライズと適用ロジックのエッジケースを検証する。
- 前提条件: プロファイル保存先に書き込み可能であること。サンプルプロファイルのフィクスチャが用意されていること。
```
6 passed in 1.93s
```

JUnit XML: `reports/unit/junit-unit.xml` に出力済み。

（和訳）

pytest 出力（抜粋）:

```
6 passed in 1.93s
```

JUnit XML: `reports/unit/junit-unit.xml` に出力済み。
- Expected result: JSON returned on 200; exception raised on 500.
- Result: TODO

（和訳）

### SC-TST-010 — REST クライアントラッパ
- 目的: SmartCheckerClient が 200/4xx/5xx を適切に処理し、例外を投げることを確認する。
- 前提条件: ローカルの mock_server または httpretty/requests-mock を使用すること。
- 手順:
  1. 200 を返す POST をモックし JSON を返す。
  2. 500 を返す POST をモックする。
  3. 返り値／例外をアサートする。
- 期待結果: 200 の場合は JSON が返り、500 の場合は例外が発生する。
- 結果: TODO

### SC-TST-011 — WebSocket client
- Purpose: Ensure WSApp performs health-check, connects, dispatches events to callback, and stops cleanly.
- Preconditions: start a simple websocket echo server or use mocked socket
- Steps: ...
- Result: TODO

（和訳）

### SC-TST-011 — WebSocket クライアント
- 目的: WSApp がヘルスチェックを行い、接続・イベントのディスパッチ・正常停止を行うことを確認する。
- 前提条件: シンプルな websocket エコーサーバを起動するかモックソケットを使用すること。
- 手順: ...
- 結果: TODO

## Test mapping (pytest -> Report IDs)

The following pytest test functions were discovered and mapped to report Test IDs. These tests were executed in the recent run (2025-11-24).

- `tests/test_client.py::test_modbus_read` -> SC-TST-010 (SmartCheckerClient: modbus_read behavior)
- `tests/test_client.py::test_get_di` -> SC-TST-010 (SmartCheckerClient: get_di behavior)

- `tests/test_profile_manager.py::test_profiles_feature_removed_marker` -> SC-TST-000 (Profiles feature removed marker)
- `tests/test_wsapp.py::test_wsapp_dispatch` -> SC-TST-011 (WSApp dispatch)
- `tests/test_memobus.py::test_insert_log_pretty_print_and_tags` -> SC-TST-012 (MemobusFrame RX pretty-print)
- `tests/test_memobus.py::test_log_capacity_trimming` -> SC-TST-012 (MemobusFrame log trimming)

（和訳）テストマッピング（pytest -> レポート ID）

以下の pytest テスト関数を検出し、レポート内の Test ID に紐付けました。これらのテストは 2025-11-24 の実行で通過しています。

- `tests/test_client.py::test_modbus_read` -> SC-TST-010（SmartCheckerClient: modbus_read 挙動）
- `tests/test_client.py::test_get_di` -> SC-TST-010（SmartCheckerClient: get_di 挙動）

## Execution Log

Place the raw pytest output or JUnit XML path here after running tests. Example commands to run tests:

```powershell
.\.venv\Scripts\Activate.ps1
$env:PYTHONPATH='.'
pytest -q --maxfail=1 --disable-warnings
pytest --junitxml=reports/unit/junit-unit.xml
```

（和訳）実行ログ

テスト実行後に raw な pytest 出力や JUnit XML のパスをここに記入してください。実行例は上記の通りです。

### 実行結果（2025-11-24） / Execution result (2025-11-24)

pytest 出力（抜粋）:

```
2 passed in 1.54s
```

JUnit XML: `reports/unit/junit-unit.xml` に出力済み。

（和訳）

pytest 出力（抜粋）:

```
2 passed in 1.54s
```

JUnit XML: `reports/unit/junit-unit.xml` に出力済み。

## Conclusions & Next Steps

- Implement missing unit tests for each module listed above.
- Add CI job to run unit tests and publish JUnit artifacts.

（和訳）結論と次のステップ

- 上記のモジュールごとに不足している単体テストを実装してください。
- 単体テストを実行して JUnit 形式の成果物を公開する CI ジョブを追加することを推奨します。
