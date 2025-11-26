
% Integration Test Report

Project: SmartChecker
Document: Integration Test Report
Location: `reports/integration/`
Generated: 2025-11-24

## Executive Summary

Integration tests validate the interaction between the Application layer, HAL (or HAL-mock), and I/O endpoints (REST / WebSocket). Tests are marked in range SC-TST-100..149.

（和訳）エグゼクティブサマリ

統合テストはアプリケーション層、HAL（または HAL モック）、および I/O エンドポイント（REST / WebSocket）間の相互作用を検証します。テスト範囲は SC-TST-100..149 です。

## Environment

- Hardware: (Raspberry Pi bench or host emulator)
- Python: (record version)
- Mock server: `host_client/mock_server.py` (used for E2E in development)

（和訳）環境

- ハードウェア: （Raspberry Pi ベンチまたはホストエミュレータ）
- Python: （バージョンを記録）
- モックサーバ: `host_client/mock_server.py`（開発時の E2E 用）

## Summary Results

| Test ID | Scenario | Description | Req IDs | Result |
|---------|----------|-------------|---------|--------|
| SC-TST-100 | HIL V/f E2E | V/f command -> HAL -> simulated output -> sampling -> app receives | SC-REQ-001 / SC-NFR-000 | TODO |
| SC-TST-101 | AO Set REST + WS | POST /api/v1/io/ao and receive WS status.update with ao fields | SC-REQ-000 | PASS (example run)
| SC-TST-102 | WS Reconnect | WS client health-check + exponential backoff behavior | SC-NFR-002 | TODO |
| SC-TST-103 | MFDI reflect/STOP | MFDI reflect writes DI pending and STOP clears bits | SC-REQ-001 | TODO |

（和訳）概要結果

上記は統合テストのサマリです。SC-TST-101 の AO シナリオはモックサーバで検証済みです。

## Recorded Example (SC-TST-101)

This integration scenario has been executed using the provided `host_client/mock_server.py` and `tools/test_set_ao.py`. Example terminal excerpt recorded during development:

```
Using ws client: websocket-client
Connecting to ws://127.0.0.1:8000/ws/v1
WS connected
POST 200 {"ok": true, "data": {"ao1": 4.2}}
WS message: {"type": "status.update", "timestamp": 1018744.4678946, "di_byte": 0, "pending_di_byte": 0, "dac": {...}, "ao1": 4.2, "ao2": 0.0}
>>> Found ao in message
WS closed
```

Result: PASS (mock server returned 200 and WS broadcast contained `ao1`)

（和訳）記録された例（SC-TST-101）

この統合シナリオは `host_client/mock_server.py` と `tools/test_set_ao.py` を使用して実行されました。開発時に記録された端末抜粋は上記の通りです。

結果: PASS（モックサーバが 200 を返し、WS ブロードキャストに `ao1` が含まれていました）

## Test Cases (detailed)

### SC-TST-100 — HIL V/f E2E
- Purpose: Verify end-to-end V/f command flows through Application -> HAL -> DA/PWM -> sample -> Application.
- Preconditions: bench setup with HAL-mock or actual AD/DA board.
- Steps: ...
- Expected: Sampled data consistent with command (within tolerance).
- Result: TODO

（和訳）

### SC-TST-100 — HIL V/f E2E
- 目的: Application -> HAL -> DA/PWM -> サンプリング -> Application のエンドツーエンド V/f コマンドフローを検証する。
- 前提条件: HAL モックまたは実際の AD/DA ボードを備えたベンチ環境。
- 手順: ...
- 期待結果: サンプリングデータがコマンドと一致する（許容範囲内）。
- 結果: TODO

### SC-TST-102 — WS reconnect/backoff
- Purpose: Confirm WSApp health-check then connect with exponential backoff on failure.
- Preconditions: Start GUI before mock_server and observe retry behavior.
- Steps: ...
- Result: TODO

（和訳）

### SC-TST-102 — WS 再接続／バックオフ
- 目的: WSApp がヘルスチェックを行い、接続失敗時に指数バックオフで再接続することを確認する。
- 前提条件: GUI を mock_server より先に起動して再試行挙動を観察する。
- 手順: ...
- 結果: TODO

## Execution artifacts

- Store raw logs, CSV measurement data, and JUnit XML under `reports/integration/`.

（和訳）実行成果物

- 生ログ、CSV 計測データ、JUnit XML を `reports/integration/` に保存してください。

## Conclusions & Actions

- SC-TST-101 validated via mock_server; other integration tests require hardware bench or more extended scripted runs.
- Recommend adding automated runner for integration tests that can toggle between mock and hardware modes.

（和訳）結論と対処

- SC-TST-101 はモックサーバで検証済み。他の統合テストはハードウェアベンチや長時間のスクリプト実行が必要です。
- モック／ハードウェアを切り替えられる統合テスト自動実行ランナーの追加を推奨します。
