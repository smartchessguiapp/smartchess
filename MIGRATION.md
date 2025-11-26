# Migration notes: GUI / Raspberry Pi package reorganization

短い移行ノート（2025-11-26）

変更点（要点）
- PC 用 GUI を `gui/pc/` に移動しました（`mfdi_tk_sample.py`, `mfdo_tk_sample.py`, `mf_tab_tk.py`, `ws_tk_sample.py`）。
- Raspberry Pi 向けプレースホルダを `raspberry_pi/pi/__init__.py` に追加しました。
- GUI 関連のテスト・レポートを `gui/tests/` 配下に移動しました。

残作業 / 注意点
- プロジェクト内にまだ旧パス（`host_client.*`）を参照しているファイルが残っている場合があります。標準的な置換例:
	- `host_client.pc` -> `gui.pc`
	- `host_client.ws_tk_sample` -> `gui.pc.ws_tk_sample`
 置換はドキュメント部分やコメントに対しては安全に行えますが、コード内の import を置換する場合は依存関係（共有モジュール）が壊れないか注意してください。
- テスト実行に必要な依存（`pytest` など）はこの環境にインストールされていません。ローカルで次のコマンドを実行して依存を入れてください（PowerShell の例）:

```powershell
# 依存をインストール
python -m pip install -r host_client/requirements.txt
# または最小で pytest だけ入れる
python -m pip install pytest responses websocket-client

# テスト実行例（GUI テストのみ）
$env:PYTHONPATH='.'; pytest -q gui/tests --maxfail=1 --disable-warnings
```

私が今できること（選択可）
- 旧パスを参照する残ファイルの自動検索と一括置換（`host_client.pc` → `gui.pc` など）。
- 残っている重複ファイルの削除（安全のため、移動済みファイルのバックアップを取るオプションあり）。
- `pytest` が利用できる環境があれば、テスト実行と失敗箇所の修正を行います。

問題や進め方の希望を教えてください。私は旧参照の自動修正と残ファイルのクリーンアップを続けられます（許可をください）。
