
# GUI Specification — SmartChecker Host Panel

Location: `docs/specify.md`
Generated: 2025-11-24

## Purpose

This document specifies the GUI (Tkinter) behavior, layout, interactions and API contracts for the SmartChecker host control panel. It is written to be consistent with `docs/constitution.md` and maps controls to Requirement and Test IDs where appropriate.

（和訳）目的

本書は SmartChecker ホストコントロールパネルの GUI（Tkinter）に関する振る舞い、レイアウト、インタラクション、及び API 契約を規定します。`docs/constitution.md` に整合する形で、コントロールと要件／テスト ID の対応も示します。


## Overview

The GUI is a local Tkinter-based control panel for development and HIL testing. Primary goals:
- Provide fast, responsive controls for MEMOBUS, Power Interface (MFDI/MFDO/MFAI/MFAO), and Run operations.
- Display real-time status via WebSocket `status.update` messages.
- Offer small-scale plotting (Matplotlib) for waveforms when present.

（和訳）概要

本 GUI は開発と HIL テストのためのローカル Tkinter ベースのコントロールパネルです。主な目的は次の通りです:
- MEMOBUS、電源インターフェース（MFDI/MFDO/MFAI/MFAO）、および Run 操作に対する高速で応答性の高い操作を提供すること。
- WebSocket の `status.update` メッセージを通じてリアルタイムの状態を表示すること。
- 必要に応じて小規模な波形表示（Matplotlib）を提供すること。


## Top-level layout

Main window: `MFTabApp`
- Dimensions: Default 820x420 (resizable)
- Tabs (ttk.Notebook):
  - MEMOBUS通信 (MemobusFrame)
  - Run (TestFrame)
  - MF (left: MFDIControls, right: MFDOIndicators)

（和訳）トップレベルのレイアウト

メインウィンドウ: `MFTabApp`
- 標準サイズ: 820x420（リサイズ可）
- タブ（ttk.Notebook）:
  - MEMOBUS通信 (MemobusFrame)
  - Run (TestFrame)
  - MF（左: MFDIControls、右: MFDOIndicators）


## MEMOBUS通信 (MemobusFrame)

Controls:
- Baud rate combobox — values: `BAUDS` list
- Parity combobox — `None`, `Even`, `Odd`
- ONLINE / OFFLINE toggle button (local state)

Log area:
- Text widget with colored tags:
  - TS: gray (#888888)
  - TX: blue (#1f77b4)
  - RX: green (#2ca02c)
  - ERR: red (#d62728)
- Auto-scroll: always ON (no checkbox). New lines are appended and scrolled into view.
- In-memory buffer: keeps the most recent 200 entries (FIFO). Older entries are trimmed.
- RX JSON messages are pretty-printed with indent=2.

Read/Write rows:
- Scrollable table implemented as a Canvas with a Frame window.
- Each row: Address entry, Read button, Write button, Data entry, Delete button.
- Read button: sends a modbus_read via SmartCheckerClient in a background thread and _log('TX', ...) before call; on response logs _log('RX', json.dumps(res)) and updates Data cell.
- Write button: sends modbus_write likewise.

UX notes:
- All blocking network calls are done in daemon threads and marshal UI updates via `after(0, ...)`.
- Error dialogs use `messagebox.showerror` invoked on the main thread.

（和訳）MEMOBUS 通信（MemobusFrame）

コントロール:
- ボーレートコンボボックス — 値は `BAUDS` リスト
- パリティコンボボックス — `None`, `Even`, `Odd`
- ONLINE / OFFLINE トグルボタン（ローカル状態）

ログ領域:
- カラータグ付きの Text ウィジェット:
  - TS: グレー (#888888)
  - TX: 青 (#1f77b4)
  - RX: 緑 (#2ca02c)
  - ERR: 赤 (#d62728)
- 自動スクロール: 常時有効（チェックボックスは無し）。新しい行は追加され自動で表示されます。
- メモリ内バッファ: 最新 200 エントリを保持（FIFO）。古いエントリは削除されます。
- 受信（RX）の JSON メッセージは indent=2 で整形表示します。

読み書き行:
- Canvas+Frame で実装されたスクロール可能なテーブルを使用します。
- 各行: アドレス入力、Read ボタン、Write ボタン、Data 入力、削除ボタン。
- Read ボタン: バックグラウンドスレッドで SmartCheckerClient の modbus_read を呼び出し、呼び出し前に _log('TX', ...) を行います。応答時に _log('RX', json.dumps(res)) でログ出力し、Data セルを更新します。
- Write ボタン: 同様に modbus_write を実行します。

UX 注意点:
- ブロッキングするネットワーク呼び出しはすべてデーモンスレッドで行い、UI の更新は `after(0, ...)` でメインスレッドに委譲します。
- エラーダイアログはメインスレッドから `messagebox.showerror` で表示します。

Traceability:
- Memobus read/write: SC-REQ-001、テスト: SC-TST-012（単体）、SC-TST-203（システム）


## Run (TestFrame)

Controls:
- Voltage class combobox (200V/400V)
- RUN and Stop buttons
- Highest frequency, Command frequency, Decel time inputs
- Direction radio FWD/REV

Behavior:
- RUN: validates inputs, constructs payload and attempts POST `/profile/run`. If server rejects, GUI simulates run locally.
- Stop: initiates controlled deceleration to 0 Hz according to decel_time; if a server run_id exists, POST `/profile/stop`.
- Waveform: if Matplotlib present (`HAS_MATPLOTLIB`), display waveform plot; otherwise display text placeholder.

（和訳）Run（TestFrame）

コントロール:
- 電圧クラスコンボボックス（200V / 400V）
- RUN と Stop ボタン
- 最高周波数、指令周波数、減速時間の入力
- 方向ラジオ（FWD / REV）

振る舞い:
- RUN: 入力を検証し、ペイロードを組み立てて `/profile/run` に POST を試みます。サーバが拒否した場合は GUI 内でのシミュレーションにフォールバックします。
- Stop: 指定した decel_time に従って 0 Hz へ制御減速を開始します。サーバ側の run_id が存在すれば `/profile/stop` に POST します。
- 波形: Matplotlib (`HAS_MATPLOTLIB`) が利用可能であれば波形プロットを表示し、無ければテキストプレースホルダを表示します。

Traceability:
- Run start/stop: SC-REQ-000、システムテスト SC-TST-200


## MF Tab (MFDIControls + MFDOIndicators)

Left: MFDIControls
- CH1..CH8 checkboxes
- Buttons: 反映 (Reflect), STOP
- MFAI: AI1/AI2 controls — combobox mode ["0-10V","-10-10V","0-20mA","4-20mA"], value entry, Set button

Behavior:
- Reflect: assemble bits and call `client.put_di_pending(di_byte=v)` in background; on_done shows info dialog.
- STOP: clears checkboxes locally and posts pending=0.
- MFAI Set: validates value per mode, posts to `/api/v1/io/ao` as `{"channel":"ao#","mode":"<mode>","value":<v>}` and shows result dialog; label shows measurement error simulation.

Right: MFDOIndicators
- LED indicators for items: P1-PC, P2-PC, MA-MB-MC, M1-M2, M3-M4
- MFAO numeric displays and progress bars for ao1/ao2
- MFAO client-side throttle: default 500 ms (combobox choices: 200/500/1000 ms)

Behavior:
- WS subscription: `WSApp(ws_url, on_status)` starts background thread and dispatches `status.update` payloads to `on_status`.
- `_apply_status(data)`: updates DI LEDs, status label, MFAO numeric and progress bars; MFAO updates are throttled to `self._mfao_interval_ms`.

REST / WS Contracts (development prototype)
- REST endpoints used by GUI (base `/api/v1`):
  - POST `/io/ao` — set AO: payload `{ "channel": "ao1", "mode": "0-10V", "value": 4.2 }` → returns `{ "ok": true, "data": {...} }`
  - POST `/profile/run` — start run (payload documented in TestFrame)
  - POST `/profile/stop` — stop run
- WebSocket: `ws://<host>:<port>/ws/v1` — broadcasts messages with type `status.update` carrying at least:
  - `di_byte`, `pending_di_byte`, `dac` (cur_freq, target_freq, ...), optional `ao1`, `ao2` fields

Accessibility & Errors
- All network operations show error dialogs on failure. The UI should not block on network ops. Background threads must not raise unhandled exceptions; all exceptions are marshalled to main thread using `after`.

Performance
- GUI responsiveness target: actions should respond within 100 ms per SC-NFR-002. Heavy update rates (high-frequency WS messages) should be throttled in UI (MFAO throttle) to avoid UI freezes.

Test Hooks
- For testing, `WSApp` accepts a configurable `base_http` and `health-check` behavior to allow deterministic unit/integration tests.
- The `SmartCheckerClient` should be mockable (dependency injection) for unit tests.

Changes & Versioning
- Maintain this doc with the same rules as `docs/constitution.md` (English + Japanese where applicable). Update traceability mappings when requirements change.

（和訳）MF タブ（MFDIControls と MFDOIndicators）

左側: MFDIControls
- CH1..CH8 のチェックボックス
- ボタン: 反映（Reflect）、STOP
- MFAI: AI1/AI2 のコントロール — モード選択（"0-10V", "-10-10V", "0-20mA", "4-20mA"）、値入力、Set ボタン

振る舞い:
- Reflect: ビットを組み立てて `client.put_di_pending(di_byte=v)` をバックグラウンドで呼び出します。完了時に情報ダイアログを表示します。
- STOP: ローカルのチェックをクリアし、pending=0 を POST します。
- MFAI Set: モードに応じて値を検証し、`/api/v1/io/ao` に `{ "channel":"ao#","mode":"<mode>","value":<v> }` を POST して結果ダイアログを表示します。ラベルは測定誤差のシミュレーションを示します。

右側: MFDOIndicators
- LED 指標: P1-PC, P2-PC, MA-MB-MC, M1-M2, M3-M4
- MFAO 数値表示および ao1/ao2 のプログレスバー
- MFAO クライアント側のスロットリング: デフォルト 500 ms（コンボボックス: 200/500/1000 ms）

REST / WS 契約（開発プロトタイプ）
- GUI が使用する REST エンドポイント（ベース `/api/v1`）:
  - POST `/io/ao` — AO 設定: 例 `{ "channel": "ao1", "mode": "0-10V", "value": 4.2 }` → `{ "ok": true, "data": {...} }` を返す想定
  - POST `/profile/run` — 実行開始
  - POST `/profile/stop` — 実行停止
- WebSocket: `ws://<host>:<port>/ws/v1` — `status.update` 型のメッセージをブロードキャストし、少なくとも `di_byte`, `pending_di_byte`, `dac`（cur_freq, target_freq 等）、および任意で `ao1`, `ao2` を含む。

アクセシビリティとエラー処理:
- 全てのネットワーク操作は失敗時にエラーダイアログを表示します。UI がブロックしないことが前提です。バックグラウンドスレッドは未処理例外を吐かないようにし、例外は `after` を使ってメインスレッドで処理します。

パフォーマンス:
- GUI の応答性目標: 操作は 100 ms 以内に反応すること（SC-NFR-002）。高頻度の WS メッセージは UI フリーズを避けるためスロットリングする。

テストフック:
- `WSApp` はテスト用に `base_http` やヘルスチェック動作を設定可能にしておくこと。
- `SmartCheckerClient` はユニットテストで差し替え可能（依存性注入）にしておくこと。

変更とバージョン管理:
- 本ドキュメントは `docs/constitution.md` のルールに従って管理します（必要に応じて和英併記）。要件が変わったらトレース情報を更新してください。
