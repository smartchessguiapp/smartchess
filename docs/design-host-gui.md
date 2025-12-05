# SmartChecker — Host PC (Tkinter) Design

文書種別: Host PC（Tkinter GUI）用ソフトウェア設計書 / Host PC (Tkinter) Software Design

Trace ID 範囲: SC-DES-100 〜 SC-DES-240 (GUI 仕様含む)

注意: 本設計書は Host 側の GUI（Tkinter ベース、以後 "Host GUI"）の仕様を定義する。Host GUI は Raspberry Pi 上で動作する HIL サービス（以後 "RPi HIL Service"）と連携して SmartChecker を操作・可視化する。

前提と仮定
- "Tkinter" 表記は `Tkinter` と解釈する。
- Host GUI と Raspberry Pi 間のリアルタイム制御は RPi HIL Service を介して行う。Host GUI は直接ハードウェア（AD/DA、GPIO、RS-485）を操作せず、RPC/REST/WebSocket を通じて RPi HIL Service にコマンドを送る設計を推奨する。
- ハードウェア詳細（ピン割当等）は `docs/pinout.md` と `config/pinout.yaml` に確定される（SoT）。
- 欠落している箇所は合理的に推定して実装方針を示す（例: MFDO / MFAI / MFAO の表示/制御の詳細は一般的な HIL GUI の慣習に準拠する）。

変更履歴
- 2025-11-24: 初版（要求に基づき Host GUI の設計書を作成）

## 1. 目的 / Purpose (SC-DES-100)
Host GUI はエンジニアやテスト担当者が PC 上で SmartChecker を操作し、設定・可視化・ログ管理を行うためのデスクトップアプリケーションである。主要な要件は以下:
- 直感的な操作性と低遅延の可視化
- RPi HIL Service と堅牢に接続できること
- 実験プロファイルの保存・ロード、CSV ログ出力、波形表示

## 2. 範囲 / Scope (SC-DES-101)
- 対象: Host PC 上で動作する Tkinter アプリケーション本体、UI ロジック、RPi 通信ラッパー、ログ表示/保存機能
- 非対象: RPi 側の HAL 実装（ADC/DAC、SPI IO エクスパンダ、Modbus 実装）は RPi 側の設計書に記載

## 3. 高レベルアーキテクチャ (SC-DES-110)
Host GUI の主要コンポーネント:
- UI 層 (Tkinter ウィジェット群 / Matplotlib 埋め込み)
- Application 層 (Controller / ViewModel 相当: 状態管理 / イベント処理)
- Communication 層 (RPi HIL Service との RPC/REST/WebSocket ラッパー)
- Persistence 層 (設定 / プロファイル / ログの保存: YAML/JSON/CSV)
- Plugin/Extension 層（将来的に追加する場合）

通信設計（推奨）:
- 設定取得/コマンド: REST API (HTTP JSON) を使用（構成、プロファイルの CRUD）
- リアルタイムストリーム: WebSocket を使用し、時間系列データ（サンプル値、ステータス更新）を受信
- コマンド実行の確認は WebSocket イベントまたは REST の同期応答を利用
- 設計上の理由: REST は CRUD に適し、WebSocket は低遅延の双方向ストリームに適する

セキュリティ/認証: 企業内 LAN 前提であるが、認証トークン（API キー）と TLS を推奨

## 3.8 GUI 画面仕様（Host GUI）
（この節は要求に従い詳細に記述する）

Host GUI は 3 つのタブを持つ: タブ1 MEMOBUS 通信、タブ2 Power Interface、タブ3 Test

### 3.8.1 タブ1: MEMOBUS通信 (SC-DES-130)
タブ名: MEMOBUS通信 / MEMOBUS Communication

目的: インバータ等に対する保持レジスタの読み書きを手動で行い、通信動作の検証とトラブルシューティングを行うためのインターフェース。PCからYASKAWAインバータGA700とMEMOBUS通信にてレジスタのRead、Writeを実行する。

UI コンポーネント一覧
- ボーレート（ドロップダウン）: 選択肢 1.2, 2.4, 4.8, 9.6, 19.2, 38.4, 57.6, 76.8, 115.2 kbps
- データ長: 表示固定 "8 bit"
- パリティ: ラジオボタン/ドロップダウン (None / Even / Odd)
- ストップビット: 表示固定 "1"
- ONLINE/OFFLINE ボタン: 接続確立時：青表示。 接続未確立時：赤表示。
- 送受信メッセージログエリア: スクロール可能なテキストビュー、フィルタ（送信/受信）付き、タイムスタンプ付き
- 読み書きテーブル (Address / Read / Write / Data): 行を追加可能、各行に Read / Write ボタン
- 各 Read/Write 操作に対する短いステータスバッジ（成功/失敗）


#### 3.8.1.1MODBUSの仕様
- 通信対象機器：安川インバータ GA700シリーズ
- 通信モード：RTUモード固定
- スレーブアドレス：000H~FFFFH
- 通信レジスタ範囲：
  - 読取：0000H～FFFFH
  - 書込：0000H～FFFFH

##### 表 MEMOBUS/Modbusの仕様

| 項目 | 仕様 |
|:------|:------|
| インタフェース | RS-485 |
| 同期方式 | 非同期（同期同期） |
| 通信パラメータ | ボーレート：1.2、2.4、4.8、9.6、19.2、38.4、57.6、76.8、115.2 kbps, データ長：8ビット（固定）, パリティ：偶数／奇数／なし, ストップビット：1ビット（固定） |
| 通信プロトコル | MEMOBUS/Modbus準拠（RTUモードのみ） |
| 接続可能台数 | 最大31台 |

### 3.8.1.2インバータへの送受信データフォーマット
#### 3.8.1.3複数保持レジスタ読み出し時
- ファンクションコード03(Hex.)を使って，最大16個の保持レジスタの内容を読み出します。
スレーブ2のインバータからステータス信号，異常の内容，データリンクステータス，周波数指令を読み出すときのメッセージ例を下図に示します。本アプリでは常に1つのレジスタをReadします。

##### 表 保持レジスタ読み出し時のメッセージ例
| バイト | PC指令メッセージ | 設定データ (Hex.) | インバータ応答メッセージ（正常時） | 設定データ (Hex.) | インバータ応答メッセージ（異常時） | 設定データ (Hex.) |
|:------:|:---------------|:------------------:|:---------------------------|:------------------:|:---------------------------|:------------------:|
| 0 | スレーブアドレス | 02 | スレーブアドレス | 02 | スレーブアドレス | 02 |
| 1 | ファンクションコード | 03 | ファンクションコード | 03 | ファンクションコード | 83 |
| 2 | 開始番号 上位 | 00 | データ数 | 08 | エラーコード | 03 |
| 3 | 開始番号 下位 | 20 | 最初の保持レジスタ 上位 | 00 | CRC-16 上位 | F1 |
| 4 | データ個数 上位 | 00 | 最初の保持レジスタ 下位 | 65 | CRC-16 下位 | 31 |
| 5 | データ個数 下位 | 04 | 次の保持レジスタ 上位 | 00 | - | - |
| 6 | CRC-16 上位 | 45 | 次の保持レジスタ 下位 | 00 | - | - |
| 7 | CRC-16 下位 | F0 | 次の保持レジスタ 上位 | 00 | - | - |
| 8 | - | - | 次の保持レジスタ 下位 | 01 | - | - |
| 9 | - | - | 次の保持レジスタ 上位 | F4 | - | - |
| 10 | - | - | 次の保持レジスタ 下位 | AF | - | - |
| 11 | - | - | CRC-16 上位 | 82 | - | - |
| 12 | - | - | CRC-16 下位 | - | - | - |


動作詳細
- コネクション管理:
  - ONLINE 押下: Communication 層に `connect_modbus(serial_params)` を要求。成功時は UI に接続状態を表示し、オンライン時刻をログに記録。
  - OFFLINE 押下: `disconnect_modbus()` を呼び出し、未完了の操作はキャンセルして UI を更新する。
- Read ボタン押下:
  - 入力チェック: Address フィールドが有効な 0..65535 の整数であること
  - RPi HIL Service へ転送: `POST /modbus/read` または WebSocket 経由で { type: "modbus_read", address: <addr>, length: 1 }
  - RPi 側にて Modbus RTU Read Holding Register を実施し、応答を受け取る
  - Host GUI は応答を受け取り Data 欄に表示。受信時は送受信ログにバイト列（HEX）とデコード値（例: 16-bit signed/unsigned, float decode の候補）を表示
  - 失敗時: エラーダイアログ/トーストを表示し、詳細はログに保存
- Write ボタン押下:
  - 入力チェック: Data 値が有効な型であること（数値フォーマットの検査）
  - RPi HIL Service へ { type: "modbus_write", address: <addr>, value: <value> }
  - 書き込みの成功可否をログに記録し、Data 欄を更新

ログ表示仕様
- 表示フォーマット: [YYYY-MM-DD HH:MM:SS.mmm] [TX/RX] [HEX BYTES] [Decoded]
- スクロールは自動（新規メッセージで下部へ）、ユーザがスクロールした場合は自動スクロールを一時停止できる
- エクスポート: ログをテキスト/CSV でダウンロード可能

エラー・例外処理
- タイムアウト／CRC エラー等は赤でハイライトし、詳細（例: CRC mismatch, timeout after X ms）をログに残す
- 接続断時は UI をグレーアウトし、再接続案内を表示

アクセシビリティ/ショートカット
- Address フィールドにフォーカスして Enter で Read をトリガ
- Ctrl+W で Write、Ctrl+L でログクリア

関連設計 ID: SC-DES-130 ～ SC-DES-139

### 3.8.2 タブ2: Power Interface (SC-DES-140)
タブ名: Power Interface

目的: インバータとやり取りするデジタル入出力（DI/DO）およびアナログ入出力（AI/AO）を視覚的に操作・監視する

構成: 4 ブロック: MFDI (Digital Input to Inverter), MFDO (Digital Output from Inverter), MFAI (Analog Input), MFAO (Analog Output)

#### 3.8.2.1 MFDI — Digital Input to Inverter (SC-DES-141)
- UI タイプ: トグルボタン（8 個: S1..S8）
- 各トグルのラベル: S1 .. S8（ラベル横に機能説明が表示可能、ホバーで詳細）
- 状態表示色: ON = 緑、OFF = 灰
- 動作: ユーザがトグルを ON にすると、Host GUI は `POST /io/di` で { pin: <name>, state: true } を RPi HIL Service に送信し、RPi 側で IO エクスパンダまたは GPIO を制御して信号を適用する
- UI 上は成功レスポンス受領後に検証チェックマークを表示

#### 3.8.2.2 MFDO — Digital Output from Inverter (SC-DES-142)
- UI タイプ: LED インジケータ（読み取り専用）
- 項目: "P1-PC", "P2-PC", "MA-MB-MC"（表示は列/グループ化可能）
- 動作: RPi HIL Service から定期的（例: 200 ms 毎）に WebSocket で DO ステータスが送信され、Host GUI はインジケータを更新する
- インジケータはツールチップで最終更新時刻とソース（IO エクスパンダ / RPi GPIO）を表示

#### 3.8.2.3 MFAI — Analog Input (SC-DES-143)
- GUIから入力されたデータをもとにラズパイからインバータにアナログ値を入力する機能。
- UI タイプ: インバータへの入力値をユーザが任意に設定可能。単位、入力範囲はパラメータ1に従って変更
- 項目: AI1、AI2
- パラメータ1 : "0-10V","-10-10V","0-20mA","4-20mA"を項目毎にドロップダウンで選択
- パラメータ2 : 項目ごとにパラメータ1に従ったアナログ入力値を入力。電圧入力（"0-10V","-10-10V"）の選択時、小数点1桁まで入力可能。電流入力（"0-20mA","4-20mA"）選択時、小数点0桁まで選択可能
- 精度表示: 値には測定誤差（例: ±0.01%）とサンプリング時刻を併記
- MFDI表示行の下行に表示する

#### 3.8.2.4 MFAO — Analog Output (SC-DES-144)
- インバータからラズパイにアナログ値を入力する機能。GUIにはラズパイから送信されたデータをモニタとして表示する
- UI タイプ: 数値表示 + ミニメーター（ゲージ）
- 項目: MFAO1、MFAO2
- パラメータ1 : "0-10V","-10-10V","4-20mA"を項目毎にドロップダウンで選択
- インバータからの出力情報をバーにて表示

UI 共有仕様（Power Interface）
- 各ブロックはカード化され、タイトルバーに有効化 / 無効化ボタンを持つ
- カードごとに履歴（過去 10 分の主要イベント）を確認可能
- 入力制御時は Change Log に記録（ユーザ名, 時刻, 旧値→新値）
- バックグラウンドのタブに切り替えても実行は継続し、長時間のプロファイル実行やデータ収集は UI の応答性を維持したままバックグラウンドで完了する設計とする。

関連設計 ID: SC-DES-141 ～ SC-DES-149

### 3.8.3 タブ3: Running (SC-DES-150)
タブ名: Running

目的: V/f プロファイル実行、ランプ、周波数スイープ、シーケンス実行、ログ取得、波形表示、スクリプト実行を行う主要テスト画面

UI コンポーネント
- プロファイルエディタ: プロファイル名、周波数 vs 電圧テーブル、ランプ時間、ループ回数、保存/ロードボタン
- プロファイル一覧（左ペイン）: 保存済みプロファイルをツリー表示、選択で読み込み
- 実行コントロール: Start / Pause / Stop / Emergency Stop（赤ボタン）
- 現行ステータス表示: 実行状態、経過時間、現在周波数/電圧、ステップインデックス
- 波形ビューワ: Matplotlib 埋め込み、複数チャンネル同時表示、ズーム/カーソル読み取り
- ログ & CSV 出力: 実行ごとに `reports/system/` または `reports/unit/` にデータを保存
- スクリプトコンソール: 簡易スクリプト（Python ライク DSL）入力欄＋実行ボタン（オプション）
-   | 電圧クラス | 電圧クラス  指令周波数 | 可減速時間 | 出力周波数 |
    |:----:|:----:|:----:|:----:|:----:|
    | 200V | 60.00Hz | 60.00Hz | 10.00sec | モニタ表示 |

    - 電圧クラス：初期値200V。200V,400Vをタブ形式で選択可能。初期値200V。
    - 最高周波数：最大値590.00Hz。最小値0.00Hz。小数点2桁まで任意にユーザが設定可能。初期値60.00Hz。
    - 指令周波数：最大値590.00Hz。最小値0.00Hz。小数点2桁まで任意にユーザが設定可能。初期値60.00Hz。
    - 可減速時間：出力周波数が0Hzから最高周波数まで到達する時間。最高周波数60.00Hz、指令周波数30.00Hzの場合、RUNして5秒で出力周波数が30.00Hzに到達する。小数点2桁まで任意にユーザが設定可能。初期値10.00sec。
    - 出力周波数：ラズパイから受信する。モニタ表示でユーザは設定不可。

動作詳細
- Start:
  - 入力検査: 選択したプロファイルの整合性チェック
  - RPi HIL Service に `POST /profile/run` を送信し、WebSocket 経由で実行イベント（sample, status, fault）が送られてくる
  - Host GUI は受信した時間系列をリアルタイムでプロットし、CSVへ逐次追記する
- Pause/Resume:
  - `POST /profile/pause` / `POST /profile/resume`
- Stop:
  - 正常停止と強制停止の区別。強制停止（Emergency Stop）は即時 AO/DO を安全状態へ戻すコマンドを発行する（RPi 側は即時ハード手順を実行）
- Waveform viewer:
  - リアルタイムに最大 10 秒分の更新を高頻度で表示（描画間隔 50..200 ms 可変）
  - データはメモリにリングバッファ保存し、CSV 書き出しはバッファにある全データを書く

安全・フェールセーフ
- Emergency Stop は UI でもハードウェア実行でも同等の効果を持つ（RPi HIL Service に即時命令）
- 実行中にフォルトが検出された場合、Host GUI はアラートダイアログを表示し、ログを保存、実行を停止する

操作履歴とレポート
- 実行毎にレポートを `reports/system/` に作成（SC-TST-* にリンク）
- レポートにはプロファイル、実行時間、グラフのスナップショット、CSV データへのリンクを含む

関連設計 ID: SC-DES-150 ～ SC-DES-169

## 4. データモデルと永続化 (SC-DES-170)
- 設定: `config/` に YAML ファイルを置く（例: `config/host-config.yaml`、`config/pinout.yaml`）
- プロファイル: `profiles/<name>.yaml`（周波数/電圧テーブル、メタデータ）
- テストログ: `reports/system/sc-tst-###-data.csv` / `reports/system/sc-tst-###-report.md` / `reports/system/sc-tst-###-results.xml`

YAML 例（pinout）:

```yaml
# config/pinout.yaml (例)
spi_adc_board:
  model: "ADS1256-eval"
  cs_pin: 8
io_expander:
  model: "MCP23S17"
  cs_pin: 7
modbus:
  serial_port: "/dev/ttyS0"
  baud: 19200
```

## 5. API 仕様 (Host ↔ RPi HIL Service) (SC-DES-180)
- REST エンドポイント例:
  - POST /modbus/read { address: int, length: int } → 200 { values: [..], raw: ".." }
  - POST /modbus/write { address: int, value: int } → 200 { status: "OK" }
  - POST /io/di { pin: string, state: bool } → 200 { status: "OK" }
  - POST /io/ao { channel: string, value: float } → 200 { status: "OK" }
  - POST /profile/run { profile_id: string } → 202 { run_id: string }
  - POST /profile/pause { run_id } / POST /profile/stop { run_id }

- WebSocket イベント例:
  - { type: "sample", timestamp: <ts>, data: { ai1: <v>, ai2: <v>, ... } }
  - { type: "status", run_id: <id>, state: "running|paused|stopped", message: <str> }
  - { type: "error", code: <code>, message: <str>, raw: <hex> }

※ 具体的なパス/ペイロードは `docs/api.md` と整合させること

## 6. UI/UX 詳細・操作フロー (SC-DES-190)
- 起動時フロー: 設定読み込み → RPi 接続検査（任意）→ メインウィンドウ表示
- 設定変更フロー: 設定変更は即時反映されない（Apply ボタンで反映）。Apply では構成検証を行いエラーを拒否する
- ローカル保存: プロファイルと設定はローカルに保存。ユーザによりエクスポート/インポート可能

## 7. エラーハンドリング & ロギング (SC-DES-200)
- UI 層: すべての非致命エラーはトースト/ログ表示。致命エラーはダイアログで停止手順を案内
- ログ: `logs/host-yyyy-mm-dd.log` にタイムスタンプ付きで保存。デバッグモードで通信のバイナリログも保存

## 8. テスト計画（Host GUI） (SC-DES-210)
- 単体テスト: UI ロジックは可能な限り ViewModel に切り出し、pytest でテスト可能にする
- 結合試験: RPi HIL Service と組み合わせた E2E テストを用意（SC-TST-100 〜）
- システム試験: `docs/system-tests.md` に従いシナリオテストを実施（SC-TST-200 〜）

## 9. アクセシビリティと国際化 (SC-DES-220)
- 日本語 / 英語の二言語表記をサポート（起動時に locale を選択）
- フォントと色のコントラストは視認性基準を満たす

## 10. 非機能要件との整合 (SC-DES-230)
- GUI 応答性: 主要 UI 操作は 100 ms 以内に画面変化を返す（ネットワーク遅延を除く）
- データフロー: 波形描画はデフォルト 200 ms 更新、最大 50 ms 更新を可能とするが CPU 使用率の上限は 60% 未満を標準目標とする

## 11. トレーサビリティ（設計 ID ↔ 要件 / テスト） (SC-DES-240)
- 本設計の主要 ID 範囲:
  - SC-DES-100: Host GUI 全体要件
  - SC-DES-110: アーキテクチャ
  - SC-DES-130: MEMOBUS タブ
  - SC-DES-140: Power Interface タブ
  - SC-DES-150: Test タブ
  - SC-DES-170: データモデル
  - SC-DES-180: Host↔RPi API
  - SC-DES-190: UI/UX
  - SC-DES-200: ロギング/エラーハンドリング
  - SC-DES-210: テスト計画
  - SC-DES-220: i18n/アクセシビリティ
  - SC-DES-230: NFR 整合性
  - SC-DES-240: トレーサビリティ管理

付録
- 例: MODBUS Read Holding Register のペイロード例、RPi 返却例、Host でのパース例を `docs/api.md` に追記すること
- UI モックアップ: 別途 `webui-mockups/host` に PNG や Figma リンクを管理することを推奨

---

以上が Host PC (Tkinter) 用の初版設計書（`/docs/design-host-gui.md`）です。実装段階で詳細（API エンドポイント、JSON スキーマ、具体的なイベント名など）を `docs/api.md` に合わせて確定してください。
