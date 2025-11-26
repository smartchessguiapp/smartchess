```markdown
# SmartChecker Host ↔ Raspberry Pi API 定義書

- バージョン: v1.0.0  
- 対象: SmartChecker Host アプリ（PC, tkinter） ⇔ SmartChecker サーバ（Raspberry Pi 5）  
- 目的: DI 出力、3相 DAC 正弦波出力、トレース取得、インバータ状態取得などを PC から一元制御する。

---

## 0. 物理構成とトランスポート

### 0.1 物理トポロジ

```text
+-----------------------+        ローカル有線LAN         +------------------------+
| Host PC (Windows等)   | <----------------------------> | Raspberry Pi 5         |
| - SmartChecker Host   |   (RJ45 イーサネット, TCP/IP)  | - SmartChecker server  |
+-----------------------+                                +------------------------+
                                                                  ||
                                                                  || RS-485 (MEMOBUS/Modbus RTU) ※任意
                                                                  \/
                                                           +----------------+
                                                           | Inverter (VF) |
                                                           +----------------+
```

* PC と Raspberry Pi 5 は **RJ45 有線LAN** によるローカル TCP/IP で接続される。
* インターネット接続は前提とせず、**スタンドアロンなローカルネットワーク**での動作を想定する。

### 0.2 接続パターン

* パターン A: 小型ルータ／スイッチ経由

  * 例: ルータ/スイッチに PC と RPi5 を接続
  * ルータはインターネット未接続でも良い（DHCP で IP を配るだけでも良い）

* パターン B: PC ↔ RPi5 のイーサネット直結

  * 両者に固定 IP を設定（例）

    | 機器   | IPアドレス       | マスク           |
    | ---- | ------------ | ------------- |
    | PC   | 192.168.50.1 | 255.255.255.0 |
    | RPi5 | 192.168.50.2 | 255.255.255.0 |

* Raspberry Pi 5 の USB Type-C ポートは **電源専用（5V/5A AC アダプタ）** として利用し、
  PC とのデータ通信には使用しない。

### 0.3 アプリケーションプロトコル

* Host ↔ RPi5:

  * プロトコル: **HTTP/1.1 (REST, JSON)** + **WebSocket**
  * デフォルトポート: `8000`
  * ベースURL（例）: `http://192.168.50.2:8000/api/v1`
* RPi5 ↔ インバータ（任意）:

  * 物理: RS-485
  * プロトコル: MEMOBUS/Modbus RTU
  * 詳細は別文書（`docs/inverter_modbus.md`）にて定義

---

## 1. 共通仕様

### 1.1 HTTP 共通事項

* ベースURL: `http://<rpi-ip>:8000/api/v1`
* リクエスト/レスポンスボディ:

  * `Content-Type: application/json; charset=utf-8`
* 文字コード: UTF-8
* JSON フィールド名:

  * `snake_case` を基本とする（例: `cur_freq`, `acc_time`）。

### 1.2 HTTP ステータスコード

* `2xx`: 正常

  * `200 OK` … 取得系
  * `201 Created` … リソース新規作成
  * `204 No Content` … ボディなしの成功
* `4xx`: クライアントエラー

  * `400 Bad Request` … JSON 不正、バリデーションエラー
  * `404 Not Found` … エンドポイント/リソースなし
  * `409 Conflict` … 状態衝突（例: 既に capture 実行中）
* `5xx`: サーバエラー

  * `500 Internal Server Error` … 予期しない例外

### 1.3 共通レスポンスフォーマット

#### 成功レスポンス（例）

```json
{
  "ok": true,
  "data": { ... 任意 ... }
}
```

#### エラーレスポンス

```json
{
  "ok": false,
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "freq_hz must be between 0.0 and 60.0",
    "details": {
      "field": "freq_hz",
      "min": 0.0,
      "max": 60.0
    }
  }
}
```

* `error.code` は英大文字+アンダースコアとする:

  * 例: `VALIDATION_ERROR`, `RESOURCE_BUSY`, `NOT_IMPLEMENTED`, `IO_ERROR`, `INTERNAL_ERROR`

### 1.4 単位と範囲（論理値）

* 周波数: `freq_hz` … [Hz], `float`, 範囲 `0.0 ≤ f ≤ 60.0`（v1 想定）
* 加減速時間: `acc_s` … [s], `float`, 範囲 `0.0 ≤ t ≤ 600.0`
* 電圧:

  * `vmax_v`, `vf_vbase`, `vf_vboost` … [V], `float`, 範囲 `0.0 ≤ V ≤ 200.0`
* DI 出力: `di_byte` … `0..255`, `uint8`

  * bit0 = DI1, bit1 = DI2, … bit7 = DI8

### 1.5 DAC ステータスオブジェクト

```json
{
  "cur_freq": 30.0,          // 現在出力周波数[Hz]
  "target_freq": 60.0,       // ランプ目標周波数[Hz]
  "cmd_freq": 60.0,          // ユーザ指令周波数[Hz]
  "acc_time": 5.0,           // 加減速時間[s]
  "vf_enable": true,         // V/f 制御有効フラグ
  "vf_fbase": 60.0,          // V/f 基準周波数[Hz]
  "vf_vbase": 200.0,         // V/f 基準電圧[V]
  "vf_vboost": 0.0,          // V/f Vboost[V]
  "vmax_applied": 180.0,     // 現在 LUT に適用されている Vmax[V]
  "output_enabled": true     // DAC 出力が有効かどうか
}
```

### 1.6 インバータステータスオブジェクト（任意）

RS-485 で実機インバータと接続している場合のみ有効。

```json
{
  "connected": true,
  "run_state": "RUN",      // "RUN" | "STOP" | "FAULT" | "UNKNOWN"
  "output_freq_hz": 29.5,
  "output_current_a": 3.2,
  "status_word": 1234,
  "alarm_code": 0
}
```

---

## 2. HTTP API 詳細

### 2.1 ヘルスチェック & システム情報

#### 2.1.1 ヘルスチェック

* `GET /api/v1/health`

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "status": "healthy",
    "version": "1.0.0",
    "uptime_s": 1234.5
  }
}
```

#### 2.1.2 システム情報

* `GET /api/v1/system/info`

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "hostname": "smartchecker-rpi5",
    "ip_address": "192.168.50.2",
    "model": "Raspberry Pi 5",
    "fs_hz": 20000,            // キャリア周波数[Hz]（例）
    "capture_hz": 1000,        // トレースサンプリング周波数[Hz]
    "lut_size": 1024           // 正弦波 LUT サイズ
  }
}
```

---

### 2.2 DI（デジタル出力）制御

#### 2.2.1 DI 現在状態取得

* `GET /api/v1/di`

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "di_byte": 13,         // 現在 DO に出力されている値
    "pending_di_byte": 13  // GUI から設定済みだが未反映の pending 値
  }
}
```

#### 2.2.2 DI pending 状態設定（Host GUI のチェックボックス変更時）

* `PUT /api/v1/di/pending`

**リクエスト**

```json
{
  "di_byte": 13
}
```

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "pending_di_byte": 13
  }
}
```

#### 2.2.3 DI START（pending → actual へ反映）

* `POST /api/v1/di/start`

**リクエスト**

```json
{
  "di_byte": 13   // 任意。指定があれば pending を上書きしてから反映
}
```

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "di_byte": 13
  }
}
```

#### 2.2.4 DI STOP（全 OFF）

* `POST /api/v1/di/stop`

**リクエスト**

```json
{}
```

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "di_byte": 0
  }
}
```

---

### 2.3 DAC 制御（Run/Stop, V/f, Vmax）

#### 2.3.1 現在 DAC ステータス取得

* `GET /api/v1/dac/status`

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "dac": {
      "cur_freq": 30.0,
      "target_freq": 60.0,
      "cmd_freq": 60.0,
      "acc_time": 5.0,
      "vf_enable": true,
      "vf_fbase": 60.0,
      "vf_vbase": 200.0,
      "vf_vboost": 0.0,
      "vmax_applied": 180.0,
      "output_enabled": true
    }
  }
}
```

#### 2.3.2 指令周波数・加減速パラメータ更新

* `PUT /api/v1/dac/command`

**リクエスト**

```json
{
  "freq_hz": 50.0,
  "acc_s": 3.0
}
```

* バリデーション:

  * `0.0 ≤ freq_hz ≤ 60.0`
  * `0.0 ≤ acc_s ≤ 600.0`

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "cmd_freq": 50.0,
    "acc_time": 3.0
  }
}
```

#### 2.3.3 DAC RUN（ランプアップ開始 + 出力有効）

* `POST /api/v1/dac/run`

**リクエスト**

```json
{
  "freq_hz": 50.0,
  "acc_s": 3.0
}
```

* `freq_hz`, `acc_s` を省略した場合、直近の設定値が使用される。

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "state": "RUNNING",
    "target_freq": 50.0
  }
}
```

#### 2.3.4 DAC STOP（ランプダウン → 0Hz → 出力 OFF）

* `POST /api/v1/dac/stop`

**リクエスト**

```json
{
  "acc_s": 3.0   // 省略時は前回 acc_s を使用。0.0 の場合は即時停止。
}
```

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "state": "STOPPED"
  }
}
```

#### 2.3.5 V/f パラメータ設定

* `PUT /api/v1/dac/vf`

**リクエスト**

```json
{
  "enable": true,
  "fbase": 60.0,
  "vbase": 200.0,
  "vboost": 0.0
}
```

* バリデーション:

  * `1.0 ≤ fbase ≤ 1000.0`
  * `0.0 ≤ vbase ≤ 200.0`
  * `0.0 ≤ vboost ≤ vbase`

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "vf_enable": true,
    "vf_fbase": 60.0,
    "vf_vbase": 200.0,
    "vf_vboost": 0.0
  }
}
```

#### 2.3.6 固定 Vmax 設定（V/f 無効モード用）

* `PUT /api/v1/dac/vmax-fixed`

**リクエスト**

```json
{
  "vmax_v": 150.0
}
```

* バリデーション:

  * `0.0 ≤ vmax_v ≤ 200.0`

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "vmax_applied": 150.0,
    "vf_enable": false
  }
}
```

* この API 呼び出し時、内部的に `vf_enable = false` として V/f モードを無効化する実装を想定。

---

### 2.4 トレース取得（Capture）

#### 2.4.1 取得開始

* `POST /api/v1/capture/start`

**リクエスト**

```json
{
  "sample_rate_hz": 1000.0,      // 任意。省略時は設定値 CAPTURE_HZ を使用
  "max_duration_s": 10.0         // 任意。省略時は内部デフォルト（例: 10s）
}
```

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "capture_id": "cap-20251124-001",
    "sample_rate_hz": 1000.0
  }
}
```

* 既に capture 実行中の場合:

  * `409 Conflict` + `error.code = "CAPTURE_ALREADY_RUNNING"`

#### 2.4.2 取得停止 + データ取得

* `POST /api/v1/capture/stop`

**リクエスト**

```json
{
  "capture_id": "cap-20251124-001",
  "return_data": true,          // true: JSON でデータ返却
  "save_csv": true              // true: CSV ファイル保存も行う
}
```

**レスポンス 200（JSON データ返却の場合）**

```json
{
  "ok": true,
  "data": {
    "capture_id": "cap-20251124-001",
    "sample_rate_hz": 1000.0,
    "columns": [
      "elapsed_us",
      "di_byte",
      "dacA_code",
      "dacB_code",
      "dacW_code"
    ],
    "rows": [
      [102,  0,   1000, 2000, 1800],
      [1202, 1,   1200, 1900, 1700]
    ]
  }
}
```

**レスポンス 200（CSV 保存のみを行う場合）**

```json
{
  "ok": true,
  "data": {
    "capture_id": "cap-20251124-001",
    "sample_rate_hz": 1000.0,
    "csv_path": "/home/pi/SmartChecker/tracedata/SmartChecker_20251124_123456.csv",
    "download_url": "/downloads/SmartChecker_20251124_123456.csv"
  }
}
```

#### 2.4.3 最新キャプチャ情報の取得（任意）

* `GET /api/v1/capture/last`

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "capture_id": "cap-20251124-001",
    "sample_rate_hz": 1000.0,
    "csv_path": "/home/pi/SmartChecker/tracedata/SmartChecker_20251124_123456.csv",
    "rows": 1024
  }
}
```

---

### 2.5 インバータステータス（任意）

RS-485 でインバータと接続している場合のみ有効。

* `GET /api/v1/inverter/status`

**レスポンス 200**

```json
{
  "ok": true,
  "data": {
    "connected": true,
    "run_state": "RUN",
    "output_freq_hz": 29.5,
    "output_current_a": 3.2,
    "status_word": 1234,
    "alarm_code": 0,
    "last_update_ms_ago": 85
  }
}
```

* 未接続・通信エラー等の場合:

  * `connected: false`
  * 数値項目は `null` または 0 とする実装を想定。

---

## 3. WebSocket API

### 3.1 エンドポイント

* URL: `ws://<rpi-ip>:8000/ws/v1`
* プロトコル:

  * テキストフレームに JSON メッセージ
* 共通フィールド:

  * `type`: イベント種別
  * `timestamp`: ISO8601 UTC 文字列
    例: `"2025-11-24T12:34:56.789Z"`

### 3.2 イベント一覧

| type               | 説明                             |
| ------------------ | ------------------------------ |
| `status.update`    | 100〜500ms ごとの DAC/DI/インバータ状態通知 |
| `capture.complete` | capture 終了時の完了通知               |
| `error`            | サーバ側のエラー通知（任意）                 |

### 3.3 status.update フォーマット

```json
{
  "type": "status.update",
  "timestamp": "2025-11-24T12:34:56.789Z",
  "di_byte": 13,
  "dac": {
    "cur_freq": 30.0,
    "target_freq": 60.0,
    "cmd_freq": 60.0,
    "acc_time": 5.0,
    "vf_enable": true,
    "vf_fbase": 60.0,
    "vf_vbase": 200.0,
    "vf_vboost": 0.0,
    "vmax_applied": 180.0,
    "output_enabled": true
  },
  "inverter": {
    "connected": true,
    "run_state": "RUN",
    "output_freq_hz": 29.5,
    "output_current_a": 3.2,
    "status_word": 1234,
    "alarm_code": 0
  }
}
```

* インバータ未接続の場合:

  * `inverter` フィールドは省略、または `null`。

### 3.4 capture.complete フォーマット

```json
{
  "type": "capture.complete",
  "timestamp": "2025-11-24T12:35:10.000Z",
  "capture_id": "cap-20251124-001",
  "rows": 1024,
  "csv_path": "/home/pi/SmartChecker/tracedata/SmartChecker_20251124_123456.csv",
  "download_url": "/downloads/SmartChecker_20251124_123456.csv"
}
```

### 3.5 error フォーマット（任意）

```json
{
  "type": "error",
  "timestamp": "2025-11-24T12:35:11.000Z",
  "error": {
    "code": "CAPTURE_BUFFER_OVERFLOW",
    "message": "Capture buffer overflowed. Data may be truncated."
  }
}
```

---

## 4. バージョニングと互換性ポリシー

* ベースパス `/api/v1` によりメジャーバージョンを表現する。
* minor 変更（エンドポイント追加 / フィールド追加）は後方互換を維持する:

  * 既存フィールドの意味・型は変更しない。
  * 新しいフィールドは省略可能として追加する。
* 互換性を壊す変更（フィールド削除/名称変更/型変更）は `/api/v2` として別バージョンの API を定義する。

---

## 5. セキュリティと運用

* 全ての通信はローカル有線LAN 上の TCP/IP のみを対象とし、インターネット公開を想定しない。
* v1 では認証・暗号化は行わない（閉じた実験環境での利用を前提）。
  必要に応じて将来バージョンでトークン認証や TLS を追加する。
* OS ファイアウォール（iptables 等）により、外部インターフェースからのポート 8000 へのアクセスを制限することを推奨。
* Host アプリは「接続先 IP アドレス」を設定可能とし、異なるローカル環境でも同一 API で動作できるようにする。

---

以上を `docs/api.md` としてコミットすることで、

* 「PC とラズパイ5は RJ45 有線LANでローカル TCP/IP 通信」
* 「Host ↔ RPi の正式な API スキーマ（JSON ペイロード、HTTP ステータス、WebSocket イベント一覧）」

という条件を満たす **完全版 API 定義** となる。

```
::contentReference[oaicite:0]{index=0}
```
