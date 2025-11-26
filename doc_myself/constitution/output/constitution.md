# SmartChecker Project Constitution / プロジェクト憲章

日本語 (以下本書) と English を併記する形式で記載する。

## 目的 / Purpose

本憲章は、Raspberry Pi 5 上で動作する 3 相交流電圧型インバータ向け HIL（Hardware-in-the-Loop）テストベンチ「SmartChecker」を白紙から設計・実装するための開発上位規約である。

This constitution defines the high-level rules and requirements for designing and implementing SmartChecker — a 3-phase voltage-source inverter HIL test bench running on Raspberry Pi 5.

要件（ハイレベル）:
- リアルタイム性を最重視したアーキテクチャ
- Python モジュール群による高凝集・疎結合な構造
- この文書は `design.md`, `api.md`, `test-plan.md`, `release-notes.md` のトップレベル参照として振る舞う

## 前提 / Assumptions

- 実装言語: Python のみ
- 実行環境: Raspberry Pi 5（Linux）
- 既存コードは使用しない（白紙再設計）
- GUI: Raspberry Pi 上で動作するローカル GUI（Tkinter）に加え、PC 上で操作/可視化するための Tkinter ベースのコントロールパネルを想定する。プロトタイプ段階では Matplotlib を用いた波形表示を採用
- 接続対象インバータ: Yaskawa (安川電機) GA700 を想定する。具体的な物理インターフェース（例: Modbus-RTU/RS-485、アナログ入出力、DI/DO）は設計フェーズで確定する。
- AD/DA ボード: 高精度（High-precision）AD/DA ボードを使用することを前提とする。初期目標値の例: アナログ精度 ±0.01% 程度、サンプリング/更新レートは設計段階で確定する。
- GPIO とインターフェース: Raspberry Pi 5 の汎用 GPIO ピン数が多数のアナログチャネルや外部デバイスに対して不足するため、AD/DA や多チャネル周辺機器との接続は SPI（Serial Peripheral Interface）ベースのボードを基本採用する。足りない GPIO は SPI 接続の IO エクスパンダ（例: Microchip MCP23S17 等）で拡張して制御する方針とする。必要に応じて I2C や UART（Modbus-RTU/RS-485）を併用する。
- Raspberry Pi のピン番号の確定: ハードウェア設計・仕様作成フェーズにおいて、GPIO/ピン割当（デジタル入出力、アナログチャネル、SPI チャネル割当、RS-485 用 UART ピン等）の最終ピン番号を確定し、`docs/pinout.md` と `config/pinout.yaml`（リポジトリ内）を単一のソース・オブ・トゥルース（SoT）として管理する。ピン割当は設計ロック後に変更しない運用とする（変更時は PR とトレーサビリティを必須とする）。

## 範囲 (In-scope / Out-of-scope)

### In-scope（対象）
1. V/f 制御に従う 3 相パルス電圧の模擬生成（DAC または PWM + LPF を前提）
2. 簡易モータモデルまたはルックアップに基づく 3 相電流の擬似生成、表示
3. 3相インバータ I/O（DA/PWM 出力、AD サンプリング、DI/DO）
4. THM、DI/DO の手動・スクリプト制御（GUI 及びスクリプト API）
5. V/f プロファイル定義、ランプ・スイープ、CSV ログ、Matplotlib ベースの波形可視化

### Out-of-scope（対象外）
- 高精度なモータ物理シミュレーション
- 実機インバータの完全な保護制御ロジック（本ツールは評価目的）
- Python 単独で達成が困難なハードリアルタイム制御（必要時は別プロセス/専用 MCU を想定）

## 要件（Requirements）

要件は識別子ポリシーに従って管理する（詳細は「トレーサビリティ」節）。

### 機能要件（例）
- SC-REQ-000: V/f プロファイルの作成・保存・適用機能（対応テスト: SC-TST-000）
- SC-REQ-001: 3 相パルス電圧の出力（DA または PWM + LPF）（対応テスト: SC-TST-001）
- SC-REQ-002: 波形のリアルタイム表示（Matplotlib）および CSV ログ出力（対応テスト: SC-TST-002）

### 非機能要件（SC-NFR-###）
- SC-NFR-000 (Real-time: DAC/PWM 更新周期): 100 µs ±20 µs（Raspberry Pi 上のターゲット実装。許容範囲を超える場合は専用ハード/RT プロセスへオフロード）
- SC-NFR-001 (Sampling latency): AD サンプリングからアプリケーション層反映まで 500 µs 以下（±100 µs）
- SC-NFR-002 (GUI responsiveness): ユーザー操作から反応まで 100 ms 以下（典型）
- SC-NFR-003 (Logging resolution): タイムスタンプ精度 1 µs（CSV 記録。ハード側の制約で緩和必要時は明示）
- SC-NFR-004 (Extensibility): 新チャネル追加時のモジュール再設計は最小限（変更ファイル数: ≤ 3 を目安）
- SC-NFR-005 (Reliability): 連続稼働 72 時間以内での致命的クラッシュ率 < 0.1%（実運用評価後に確定）

数値は初期値であり、設計フェーズで検証・調整する。

## 設計（Architecture）

高レベルのレイヤ構成（クリーンアーキテクチャに近い分離）:

- GUI 層 (Tkinter + Matplotlib)
  - 目的: ユーザー操作、プロファイル管理、波形表示
- Application Service 層
  - 目的: V/f プロファイル管理、ランタイム制御、シーケンス実行
- Hardware Abstraction Layer (HAL)
  - 目的: DAC/PWM/ADC/DI/DO/THM への抽象化されたインターフェースを提供
  - API 例: init(), write_ch(ch, value), write3(values), read_adc(ch), set_dio(pin, state)
  - 物理伝送の想定: ADC/DAC などの高精度アナログ入出力は SPI ベースの高精度 AD/DA ボードを使用する想定。インバータや PLC などのシリアル通信は Modbus-RTU over RS-485 を想定するが、設計段階で最終調整を行う。
  - GPIO のピン数制約を考慮し、複数チャネルや高サンプルレートが必要な入出力は SPI 経由のデバイスを優先して使用する（例: SPI ADC/DAC チップ、外付けデータ収集ボード）。
  - IO 拡張: Raspberry Pi の GPIO が不足する箇所については SPI 接続の IO エクスパンダ（例: MCP23S17）で拡張する運用とし、HAL ではエクスパンダを透過的に扱える抽象 API（例: set_gpio_expander(pin, state) / read_gpio_expander(pin)）を用意する。
- Utilities
  - ロガー、設定管理、ユーティリティ関数

設計原則:
- 高凝集・疎結合を徹底する
- 依存方向は外側 → 内側（GUI は Application を参照、Application は HAL インターフェースを参照）
- 各モジュールは "目的・責務・非責務" をヘッダコメントで明記する

## ドキュメント & コーディング規約

- 全関数先頭に英語の Doxygen 形式コメントを必須とする（概要、前提、引数、戻り値、例外、RT 注意点）
- 主要箇所に日本語の行コメント補足を許容
- Markdown 文書は日本語と英語を併記。章構成は「目的 → 前提 → 要件 → 設計 → 検証 → 結論 → 付録」
- H1 は文書タイトルに 1 回のみ。深さは H2～H3 まで。
- 数値は必ず単位と許容誤差を付す。
 - 単体試験（Unit tests）: すべての新規コード/モジュールには単体試験を必須とする。推奨テストフレームワークは `pytest`。単体試験は CI で自動実行されることを前提とし、カバレッジ目標（初期値: 80%）を定めることを推奨する。
 - 試験結果レポート: 単体試験実行後、JUnit XML または Markdown 形式の試験結果レポートを作成し、PR に添付するか `reports/unit/` に保存してトレーサビリティを確保すること。レポートには実行日時、環境（Python バージョン、依存パッケージ）、テスト対象モジュール、合否サマリを必ず含める。

## トレーサビリティ・ID ポリシー

- 機能要件: SC-REQ-###
- 非機能要件: SC-NFR-###
- 設計項目: SC-DES-###
- テストケース: SC-TST-###
- プロセス/運用: SC-PROC-###

採番範囲（本プロジェクトの初期割当）:
- SC-REQ-000 ～ SC-REQ-049
- SC-NFR-000 ～ SC-NFR-049
- SC-PROC-000 ～ SC-PROC-049

各要件には必ず対応テスト ID を付記し、付録に要件⇄テストのトレーサビリティ表雛形を掲載する。

## 変更管理 / Git & PR ルール

- コミット粒度: 章単位・モジュール単位でわかりやすく
- コミットメッセージ: `SC-###: 要点（<=72 文字）`
- PR テンプレ必須項目:
  - Purpose / 目的
  - Changes / 変更点
  - Alternatives considered / 代替案
  - Real-time & memory impact / RT・メモリ影響（定量）
  - Test results / テスト結果
  - Risks & mitigations / リスクと緩和策

## 生成 AI 利用ポリシー

- 各プロンプトに以下を含めること:
  - 対象文書種別（design/api/test-plan/release-notes 等）
  - 目的
  - 出力先ファイル名
  - 必要セクション
  - Trace ID 採番範囲

- プロンプト例:
  `/docs/api.md に HAL-DAC の API 仕様を作成。範囲は SC-API-200〜249。関数は init / write_ch / write3。各関数に 前提 / 引数 / 戻り / エラー / RT 注意 / 使用例 を含める。`

## 検証（Verification）

- 各機能要件に対応するテストケース（SC-TST-###）を用意する。例:
  - SC-TST-000: V/f プロファイル読み込み・保存・適用の正常系/異常系
  - SC-TST-001: PWM→LPF→DA 出力の周波数応答確認
  - SC-TST-002: 波形表示と CSV ログの時間整合性

- ベンチマーク方法: リアルタイム NFR（更新周期・遅延）を測定する専用テストハーネスを用意し、結果を `test-plan.md` に記録する。

- 単体試験の運用: 各モジュールに対して単体試験（SC-TST-###）を用意し、テスト実行ログと結果レポートを `reports/unit/` に保存する。単体試験は PR の一部として実行・添付し、CI（例: GitHub Actions）で成功することをマージ条件とする。

### 結合試験（Integration tests）運用方針

- 目的: HAL・デバイス・アプリケーション層が統合された状態での E2E（end-to-end）検証を行い、HIL（Hardware-in-the-Loop）シナリオやインターフェース相互作用を確認する。
- 結合試験の識別子: SC-TST-100 〜 SC-TST-149 を結合試験用に予約する（例: SC-TST-100 は HIL: V/f 出力から計測までの E2E 検証）。
- テスト実装とフレームワーク: 推奨フレームワークは `pytest` を拡張して `@pytest.mark.integration` マーカーを使用する。結合試験は単体試験より長時間かつハードウェア依存性が高いため、モック版と実機版の両方を用意することを推奨する。
- 実行環境と前提: 結合試験はハードウェアベンチ（Raspberry Pi + AD/DA ボード + GA700 等）上での実行を前提とする。CI 上での自動化はハードウェアアクセスが可能なランナー（オンプレのラボランナー等）を用いる。または、ハードウェアをエミュレートするソフトウェアフェイク環境での実行を許可するが、HIL シナリオは実機での最終確認を義務付ける。
- レポートと保存: 実行後のレポートは JUnit XML（CI 連携）および Markdown 形式で `reports/integration/` に保存する。レポートには以下を含めること:
  - 実行日時、実行者/実行環境（ランナー名、OS、Python バージョン、依存パッケージバージョン）
  - ハードウェア情報（Raspberry Pi シリアル、AD/DA ボード型番、IO エクスパンダ型番、接続ケーブル、電源条件）
  - テスト対象（SC-TST-### ID、テスト名、関連要件 ID）
  - 合否サマリ、ログの格納場所、測定データ（CSV 等）へのリンク
- CI と運用ルール: 結合試験は単体試験とは異なり、常に CI 上で自動実行する必要はない（ハード依存のため）。ただし、ハードウェア対応のラボランナーがある場合は CI で定期実行し、失敗時は直ちにアラートを上げる運用とする。PR でハードウェア関連の変更を加える場合は、対応する結合試験を実行し、`reports/integration/` にレポートを添付することを強く推奨する。

### 結合試験のトレーサビリティ

- 各結合試験ケースには必ず関連する要件 ID（SC-REQ-### / SC-NFR-###）を紐付け、付録のトレーサビリティ表に記録する。付録には結合試験→要件→試験データの参照列を設ける。

### システム試験（System tests）と変更由来の試験項目管理

- 目的: 変更・追加（設計/仕様/実装）に起因する影響範囲を網羅的に検証するため、システムレベルの試験項目を変更時にリストアップし、ドキュメント化・実行する運用を定める。
- 試験 ID 範囲: SC-TST-200 〜 SC-TST-249 をシステム試験用に予約する。
- 必要ドキュメント: すべてのシステム試験項目は `docs/system-tests.md` に一覧で管理する。各項目は以下のカラムを必須とする:
  - Test ID (SC-TST-200...)
  - Title
  - Purpose / 目的（変更・追加のどの内容に由来するかを明記）
  - Preconditions / 前提（必要なハードウェア、ソフトウェア状態、ピン割当など）
  - Steps / 手順（再現性のある手順）
  - Expected result / 期待結果（合否判断基準を明示）
  - Test data / 測定データの格納場所（CSV 等）
  - Related change / 変更 PR または仕様 ID（リンク）
  - Related requirement IDs（SC-REQ-### / SC-NFR-###）
  - Owner / 担当者
  - Pass/Fail / 結果
  - Notes / 備考
- 運用ルール:
  - 変更・追加（新しい PR を作成する際）は、必ず影響範囲に応じたシステム試験項目の作成または既存項目の更新を行い、`docs/system-tests.md` を更新すること。PR には更新した/追加した Test ID を明示する。
  - システム試験の実行結果レポートは `reports/system/` に保存する。フォーマットは Markdown の人間可読版と、CI 連携用に JUnit XML を並記することを推奨する。
  - システム試験は HIL シナリオを含むため、物理ハードウェアでの実行が原則である。CI 実行はハードウェア対応ランナーがある場合に限定する。エミュレーションで実行した場合は、必ず実機での最終確認を行う。
  - 付録のトレーサビリティ表には「変更(PR) → システム試験 ID → 要件 ID → レポート場所」を記録するカラムを設ける。



## 結論 / Conclusion

本憲章は SmartChecker の白紙再設計における最低限のルールセットを定義する。設計段階で数値・境界条件は検証し、必要に応じて本憲章を更新すること。

This constitution establishes the baseline rules for re-designing SmartChecker from scratch. Metrics and thresholds must be verified and the constitution updated accordingly.

## 付録 / Appendices

### A. DFD（Data Flow Diagram）テキスト仕様

Nodes:
- GUI (Tkinter)
- Application core (V/f profile manager, run controller)
- Hardware abstraction (DAC/PWM, ADC, DI/DO, THM)
- Logger (CSV)
- Plotter (Matplotlib)
- External 3-phase inverter / measurement devices

Data flows (簡易表記):
- GUI -> Application core: commands, presets, start/stop
- Application core -> HAL: output commands, sampling requests
- HAL -> Application core: sampled voltages/currents, DI status, faults
- Application core -> Logger/Plotter: time-stamped measurements and states

### B. トレーサビリティ表（雛形）

| 要件 ID | 要約 | 対応テスト ID |
|---------|------|---------------|
| SC-REQ-000 | V/f プロファイル機能 | SC-TST-000 |
| SC-REQ-001 | 3相出力（DA/PWM） | SC-TST-001 |

---

作業の次の推奨ステップ:
1. `/docs/design.md` 用プロンプト作成（HAL の詳細を先に固める）
2. `api.md` に HAL インターフェースの初期仕様を定義
3. `test-plan.md` に NFR 測定手順とベンチマークハーネスを設計


---

（本ファイルは初版。各数値・閾値は設計フェーズでの検証により確定する。）
