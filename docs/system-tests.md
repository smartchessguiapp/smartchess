
# System Tests

This document lists system-level test items (SC-TST-200..249) and the required metadata fields. Per `docs/constitution.md`, each item must include Test ID, Purpose, Preconditions, Steps, Expected result, Related requirement IDs, Owner, Result, and Notes.

（和訳）システム試験一覧

この文書はシステムレベルの試験項目（SC-TST-200..249）と必要なメタデータ項目を一覧化します。`docs/constitution.md` に従い、各項目は Test ID、目的、事前条件、手順、期待結果、関連要件 ID、担当者、結果、備考を含む必要があります。

## How to use

- Add a new row for each system test triggered by a change/PR. Maintain traceability by referencing the related PR and requirement IDs.
- Save test results and measurement data under `reports/system/` in both Markdown and JUnit XML when applicable.

（和訳）使い方

- 変更や PR により影響するシステム試験項目を新規追加または更新してください。関連する PR と要件 ID を参照してトレーサビリティを確保します。
- 試験結果や計測データは該当の `reports/system/` に Markdown と JUnit XML（該当する場合）で保存してください。

## System Test Items (initial set)

（和訳）システム試験項目（初期セット）

### SC-TST-200 — GUI: Start/Stop Run (Run tab)
- Title: Run start/stop behavior and deceleration to 0Hz
- Purpose: Verify RUN request is accepted and Stop causes controlled deceleration to 0Hz per decel_time.
- Preconditions: Mock server or HIL bench ready; GUI launched; profile parameters set.
- Steps:
  1. On Run tab, set voltage class and parameters (highest_freq, command_freq, decel_time).
  2. Press RUN.
  3. Observe output frequency trace and status; after some time press Stop.
  4. Measure deceleration time and final frequency.
- Expected result: Output frequency ramps according to profile; stop decelerates to 0 within decel_time +/- tolerance.
- Related requirements: SC-REQ-000, SC-NFR-000
- Owner: (assign)
- Result: TODO
- Notes: Record waveform CSV and attach to `reports/system/SC-TST-200-*.csv`.

（和訳）

### SC-TST-200 — GUI: 起動/停止（Run タブ）
- タイトル: Run の開始/停止挙動および 0Hz への減速
- 目的: RUN リクエストが受け入れられ、Stop により decel_time に従った制御減速が行われることを確認する。
- 事前条件: モックサーバまたは HIL ベンチが準備済み、GUI 起動済み、プロファイルパラメータが設定されている。
- 手順:
  1. Run タブで電圧クラスと各パラメータ（highest_freq, command_freq, decel_time）を設定する。
  2. RUN ボタンを押す。
  3. 出力周波数トレースとステータスを観測し、しばらくして Stop を押す。
  4. 減速時間と最終周波数を計測する。
- 期待結果: 周波数はプロファイルに従ってランプし、Stop により decel_time ± 許容範囲で 0Hz に到達する。
- 関連要件: SC-REQ-000, SC-NFR-000
- 担当: （割当て）
- 結果: TODO
- 備考: 波形 CSV を記録し `reports/system/SC-TST-200-*.csv` に添付すること。

### SC-TST-201 — GUI: MFDI Reflect / STOP
- Title: MFDI reflect / STOP semantics
- Purpose: Verify that pressing 反映 posts DI pending and STOP sets pending to 0 and clears local checkboxes.
- Preconditions: GUI connected to mock_server or hardware; DI initial state known.
- Steps:
  1. Toggle CH checkboxes and press 反映.
  2. Verify via mock_server or device API that /di/pending (or equivalent) was updated.
  3. Press STOP and verify pending is 0 and checkboxes clear.
- Expected result: pending bits reflect the assembled mask; STOP clears.
- Related requirements: SC-REQ-001
- Owner: (assign)
- Result: TODO

（和訳）

### SC-TST-201 — GUI: MFDI 反映 / STOP
- タイトル: MFDI の反映／STOP の意味論
- 目的: 反映を押すと DI の pending が送信され、STOP は pending を 0 にしてローカルのチェックボックスをクリアすることを確認する。
- 事前条件: GUI が mock_server または実機に接続されており、DI の初期状態が既知であること。
- 手順:
  1. CH チェックボックスを切り替え、反映を押す。
  2. mock_server やデバイス API を通じて /di/pending（または同等）が更新されたことを確認する。
  3. STOP を押して pending が 0 になり、チェックボックスがクリアされることを確認する。
- 期待結果: pending ビットが組み立てられたマスクを反映し、STOP によりクリアされる。
- 関連要件: SC-REQ-001
- 担当: （割当て）
- 結果: TODO

### SC-TST-202 — GUI: MFAI Set and MFAO display
- Title: MFAI Set → AO output and MFAO rendering
- Purpose: Verify setting AI via UI results in REST POST and that MFAO numeric + progress bars update from WS broadcasts.
- Preconditions: mock_server provides /api/v1/io/ao and broadcasts status.update containing ao1/ao2 values.
- Steps:
  1. In MFAI panel, select mode and value for AI1, click Set.
  2. Observe POST result and subsequent status.update containing ao1.
  3. Verify MFAO progressbars and numeric displays update to expected values.
- Expected result: POST returns 200 and WS message contains ao fields; UI updates within throttle period.
- Related requirements: SC-REQ-000
- Owner: (assign)
- Result: Example PASS recorded during development (see `reports/integration/integration-test-report.md`)

（和訳）

### SC-TST-202 — GUI: MFAI 設定 と MFAO 表示
- タイトル: MFAI 設定 → AO 出力と MFAO のレンダリング
- 目的: UI から AI を設定すると REST POST が行われ、MFAO の数値とプログレスバーが WS ブロードキャストから更新されることを確認する。
- 事前条件: mock_server が `/api/v1/io/ao` を提供し、`status.update` に ao1/ao2 を含めてブロードキャストすること。
- 手順:
  1. MFAI パネルで AI1 のモードと値を選択し、Set をクリックする。
  2. POST 結果と、その後に ao1 を含む `status.update` が来ることを確認する。
  3. MFAO のプログレスバーと数値表示が期待値に更新されることを確認する。
- 期待結果: POST が 200 を返し、WS メッセージに ao フィールドが含まれる。UI はスロットル期間内に更新される。
- 関連要件: SC-REQ-000
- 担当: （割当て）
- 結果: 開発時の例として PASS（`reports/integration/integration-test-report.md` を参照）

### SC-TST-203 — GUI: MEMOBUS Read/Write
- Title: MEMOBUS Read/Write basic flows
- Purpose: Verify Read/Write buttons send expected REST/modbus calls and responses are displayed in Data cell.
- Preconditions: mock_server supports modbus_read / modbus_write or equivalent endpoint.
- Steps:
  1. Add a row with valid address and press Read.
  2. Verify Data field updates with response.
  3. Enter integer in Data and press Write; verify remote acknowledged.
- Expected result: Data updates and RX logs show responses.
- Related requirements: SC-REQ-001
- Owner: (assign)
- Result: TODO

（和訳）

### SC-TST-203 — GUI: MEMOBUS 読み書き基本フロー
- タイトル: MEMOBUS の Read/Write 基本フロー
- 目的: Read/Write ボタンが期待される REST/modbus 呼び出しを送信し、応答が Data セルに表示されることを確認する。
- 事前条件: mock_server が modbus_read / modbus_write 相当のエンドポイントを提供していること。
- 手順:
  1. 有効なアドレスで行を追加し Read を押す。
  2. Data フィールドが応答で更新されることを確認する。
  3. Data に整数を入力して Write を押し、リモートが応答したことを確認する。
- 期待結果: Data が更新され、RX ログに応答が表示される。
- 関連要件: SC-REQ-001
- 担当: （割当て）
- 結果: TODO

### SC-TST-204 — WS robustness under server restart
- Title: WebSocket resilience when server restarts
- Purpose: Validate WS client reconnect/backoff when server becomes temporarily unavailable.
- Preconditions: Start GUI and mock_server; prepare to stop/start mock_server.
- Steps:
  1. Start GUI connected to mock_server.
  2. Kill mock_server process, wait 5s, restart mock_server.
  3. Observe GUI WS reconnect behavior and eventual resubscription.
- Expected result: GUI reconnects without crashing; status returns to normal.
- Related requirements: SC-NFR-002
- Owner: (assign)
- Result: TODO

（和訳）

### SC-TST-204 — サーバ再起動時の WS 応答性
- タイトル: サーバ再起動時の WebSocket の回復性
- 目的: サーバが一時的に利用不可になった場合に WS クライアントが再接続/バックオフすることを確認する。
- 事前条件: GUI と mock_server を起動し、mock_server の停止/再起動準備をする。
- 手順:
  1. GUI を mock_server に接続した状態で起動する。
  2. mock_server プロセスを kill し、5 秒待ってから mock_server を再起動する。
  3. GUI の WS 再接続の挙動と再サブスクライブを観察する。
- 期待結果: GUI はクラッシュせず再接続し、ステータスが正常に戻る。
- 関連要件: SC-NFR-002
- 担当: （割当て）
- 結果: TODO

## Template for additional items

Follow the same item structure. When executing, fill in Result, attach logs, and reference related PR.

（和訳）テンプレート

同じフォーマットに従って項目を追加してください。実行時は Result を記入し、ログを添付し、関連 PR を参照してください。

# System Tests / システム試験一覧

この文書は、変更・追加（設計/仕様/実装）に起因する影響範囲を網羅的に検証するためのシステム試験項目を一覧化するための単一管理ドキュメントです。

- ファイル: `docs/system-tests.md`
- 試験 ID 範囲: `SC-TST-200` 〜 `SC-TST-249`（システム試験用）
- レポート保存先: `reports/system/`

運用ルール（概要）:
- 変更を含む PR を作成する際、影響範囲に応じたシステム試験項目を新規作成または更新すること。
- 各項目は少なくとも下記のカラムを満たすこと。

試験項目フォーマット（例）:

| Test ID | Title | Purpose | Preconditions | Steps | Expected result | Test data | Related change | Related requirements | Owner | Result | Notes |
|---------|-------|---------|---------------|-------|-----------------|-----------|----------------|---------------------|-------|--------|-------|
| SC-TST-200 | Example: V/f sweep E2E | V/f プロファイルの出力から ADC サンプリングまでの E2E 検証 | Raspberry Pi + AD/DA board connected; GA700 in test mode; pinout locked | 1. Load profile 2. Start run 3. Record data | ADC readings within tolerance; no fault | reports/system/sc-tst-200-data.csv | PR #123 | SC-REQ-001, SC-NFR-000 | alice | Pass | measurement attached |

詳細な記載ルール:
- Preconditions は必ず `config/pinout.yaml` の該当エントリを参照すること（SoT）。
- Steps は 1 つずつ番号化し、再現性を確保すること。
- Test data は CSV などで生データを保存し、レポートにリンクを張る。
- Result は Pass/Fail 形式で記録し、失敗時は原因分析（Root cause）を備考に記載する。

更新履歴:
- 2025-11-24: initial template (by tool)
