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
- Python: （記録する）
- モックサーバ: `host_client/mock_server.py`（開発時の E2E 用）

## Summary Results

| Test ID | Scenario | Description | Req IDs | Result |
|---------|----------|-------------|---------|--------|
| SC-TST-100 | HIL V/f E2E | V/f command -> HAL -> simulated output -> sampling -> app receives | SC-REQ-001 / SC-NFR-000 | TODO |
| SC-TST-101 | AO Set REST + WS | POST /api/v1/io/ao and receive WS status.update with ao fields | SC-REQ-000 | PASS (example run)
| SC-TST-102 | WS Reconnect | WS client health-check + exponential backoff behavior | SC-NFR-002 | TODO |
| SC-TST-103 | MFDI reflect/STOP | MFDI reflect writes DI pending and STOP clears bits | SC-REQ-001 | TODO |

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
