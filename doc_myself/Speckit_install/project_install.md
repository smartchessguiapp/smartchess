# GitHub Spec Kit インストール手順
GitHub Spec Kit インストール手順を解説します。

> **⚠️ 注意:** pythonをインストールしていない場合はインストール後に以下の手順実行してください。

## 手順1
- powershellにてpipxをインストール

> ```
> py -m pip install --user pipx
> py -m pipx ensurepath
> ```

## 手順2
- powershellを一度閉じて再度開きインストールされていることを確認
> ```
> pipx --version
> ```

## 手順3
- GitHub Spec Kit を導入

> ```
> pipx install "git+https://github.com/github/spec-kit.git"
> specify --help
> ```

- インストールが完了するとpowershellに以下のような画面が表示される。
![alt text](image.png)

## 手順４
- Specify CLIをインストール(VSCのターミナルで実行)

> ```
> # 一度だけ：恒久インストール
> uv tool install specify-cli --from > git+https://github.com/github/spec-kit.git
>
> # 以後どこでも使える
> cd "C:\Users\81803\OneDrive\デスクトップ\SmartChecker\test"
> specify check                          # 旧「validate」に相当する健全性チェック
> # 必要なら再初期化
> specify init --here --ai copilot --script ps  # Windowsは --script ps 推奨（文字化け回避）
> ```

## 手順5
- プロジェクトを作成

> ```
>uvx --from git+https://github.com/github/spec-kit.git specify init <PROJECT_NAME>
> ```

## 手順6
- 作成済プロジェクトに移動

> ```
> cd "C:\Users\81803\OneDrive\デスクトップ\SmartChecker\test"
> ```

## 手順7
- 仕様のチェック

> ```
> specify check
> ```

## 手順8
- Copilot Chat 上で仕様作成
- VS Code の Copilot Chat で以下を入力

> ```
> /speckit.specify
> ```

![alt text](image-1.png)


**仕様**
- 手順５でプロジェクトを作成すると.githubフォルダ、memoryフォルダ、scriptsフォルダ、templateフォルダが作成される。
- githubフォルダ：選択したコーディングエージェントの既成コマンドを含むプロンプトファイルが用意される。これらを用意することでスラッシュコマンド経由でチャット内で使用できるようなる。
- specify/memory/constitution.md:アプリケーションの基本ルール、基本原則を規定している。ここでコーディング規約などを規定する。
- specify/scripts:powershellスクリプトのプロンプトファイル
- specify/templates：新しい仕様や計画タスクなどを具体化するときに使用する一連のテンプレート、ファイルを含むテンプレート、フォルダがある。

# スラッシュコマンドの概要
## /constitution
- アプリケーション全体の原則を設定。ここでの指示はconstitution.mdに反映される。基本的に一番最初に設定するもので追加仕様を追加する場合などは実施不要。
## /specify
- 実際の機能の仕様を設定する
## /plan
- 機能に関する技術的な設定を記載する。
## /tasks
- 上記の設定から実用的なコーディングタスクの論理的なリストに変換するように指示するコマンド
## /implement
コーディングエージェントにそれらのタスクに基づいて実際にこの新しい機能の作成を開始するように指示する。

## /clarify
- /specifyのあとに実施。specifyの内容をcopilotに確認してもらい指定が不十分な点があればその部分を明確にしてもらいそれらを明確にするための質問をする。

## /anailze
- /tasksのあとに実施。作成された成果物全体を分析し仕様に整合が取れているか矛盾した箇所が存在しないかを確認する。
