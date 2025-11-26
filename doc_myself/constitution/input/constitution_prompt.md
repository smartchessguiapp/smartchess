You are GitHub SpecKit + ChatGPT working as a **project constitution writer** for the SmartChecker project.

The goal is to generate `/docs/constitution.md` for a new system designed **from scratch** (no legacy code), with the following core architecture:

* **Host PC** running a Tkinter-based GUI (“Tkinter GUI”)
* **Raspberry Pi 5** running real-time HIL logic
* A **Yaskawa 3-phase voltage-source inverter** connected to the Raspberry Pi
* **MODBUS/TCP** between PC and Raspberry Pi
* **Analog & digital I/O** between Raspberry Pi and the inverter

Please write a **bilingual (Japanese + English)** project constitution that defines goals, scope, architecture rules, non-functional requirements (especially real-time), documentation style, traceability IDs, test/validation policies, Git/PR rules, and AI usage policies.

---

# 1. Document type / 文書種別

* Type: **Project Constitution（プロジェクト憲章）**
* Role:

  * SmartChecker 開発における「基本原則・禁止事項・非機能要件・運用ルール」を定める最上位の規範文書とする
  * All other documents such as `design.md`, `api.md`, `test-plan.md`, `release-notes.md`, data-flow diagrams, etc., **must comply with this constitution**

---

# 2. Purpose / 目的（日本語＋英語）

### Japanese

* Raspberry Pi 5 をリアルタイム HIL ノード、物理 PC を GUI ノードとし、**3相電圧型インバータ（安川インバータ）の V/f 制御挙動を評価する HIL ツール**をゼロから構築するための憲章を定めます。
* PC と Raspberry Pi の間は **MODBUS/TCP** で接続し、インバータとの間は Raspberry Pi の **アナログ I/O・デジタル I/O** で接続します。
* **高凝集・疎結合・リアルタイム性の確保・長期的な拡張性**を、SmartChecker プロジェクトの基本原則として宣言します。

### English

* Define a project constitution for building, from scratch, a **3-phase voltage-source inverter (Yaskawa) V/f HIL test bench**, using Raspberry Pi 5 as a real-time HIL node and a physical PC as a GUI node.
* The Host PC and Raspberry Pi communicate via **MODBUS/TCP**, while Raspberry Pi and the inverter are connected via **analog and digital I/O**.
* **High cohesion, low coupling, real-time behavior, and long-term extensibility** are declared as core principles of the SmartChecker project.

---

# 3. Target file / 出力先ファイル

* Output file: **`/docs/constitution.md`**

---

# 4. System overview / 対象システム概要（PC–RasPi–Yaskawa, Modbus, I/O）

Describe the overall system, **bilingually**, including:

## 4.1 Hardware nodes / ハードウェア構成

* **Host PC**

  * OS: Windows or Linux
  * Runs Tkinter-based GUI (“Tkinter GUI”)
  * Runs MODBUS/TCP **client**
  * Performs visualization, preset management, offline analysis (CSV, waveform)

* **Raspberry Pi 5 (HIL node)**

  * OS: Linux
  * Runs MODBUS/TCP **server**
  * Executes real-time 3-phase waveform generation (V/f control)
  * Hosts GPIO (DI/DO), PWM/DAC for analog outputs, ADC for analog inputs
  * Interfaces physically with the Yaskawa inverter

* **Yaskawa 3-phase voltage-source inverter**

  * 3-phase U/V/W power terminals (not controlled directly by this software)
  * **Analog inputs (AI)**: e.g., speed command, frequency command, V/f command
  * **Analog outputs (AO)**: e.g., current, voltage, power, load
  * **Digital inputs (DI)**: RUN/STOP, FWD/REV, etc. (driven by RasPi DO)
  * **Digital outputs (DO)**: RUN status, alarm, fault, ready signals (read by RasPi DI)

## 4.2 Role of each node / 各ノードの役割

* Host PC:

  * Operates GUI to set analog input values for the inverter (e.g., frequency command, V/f ratio, THM)
  * Sends these commands to Raspberry Pi via MODBUS
  * Reads back measurement and status data via MODBUS and displays them in the GUI

* Raspberry Pi:

  * Receives commands from PC (via MODBUS holding registers and coils)
  * Generates **analog outputs** (via DAC/PWM+LPF) to the inverter’s analog input terminals
  * Drives **digital outputs** to the inverter’s DI terminals (RUN/STOP, direction, etc.)
  * Samples the inverter’s **analog outputs** (via ADC) and **digital outputs** (via DI inputs)
  * Exposes these sampled results to the PC as MODBUS input registers and discrete inputs
  * Executes real-time loops (e.g., 100 µs – 200 µs) independent of MODBUS latency

* Yaskawa inverter:

  * Receives analog and digital inputs from Raspberry Pi
  * Produces analog and digital outputs reflecting its operating state
  * Acts as the **HIL target device** under test

---

# 5. Functional scope / 機能範囲（In/Out of scope, SC-REQ-000〜049）

Define **In-scope** and **Out-of-scope** functions, with functional requirement IDs `SC-REQ-###`.

## 5.1 In-scope features / 対象範囲（必須）

At a minimum, include the following feature categories:

1. **3-phase V/f pulse voltage emulation / V/f 制御時の 3相パルス電圧模擬**

   * Generating 3-phase pulse/analog waveforms (via DAC or PWM+LPF) according to V/f profiles.

2. **3-phase current emulation / 3相交流電流の疑似生成**

   * Simple motor model or lookup-table-based emulation of 3-phase current waveforms.
   * Optionally, mapping to inverter analog input/output channels.

3. **Inverter I/O handling / インバータ I/O 取扱い**

   * GUI-driven analog input commands (e.g., AI1, AI2, THM, V/f command).
   * Raspberry Pi driving analog outputs → inverter AI.
   * Raspberry Pi reading inverter analog outputs (AO) and digital outputs (DO).
   * Raspberry Pi driving inverter digital inputs (DI).

4. **V/f profiles and frequency ramps / V/f プロファイル・ランプ・周波数スイープ**

   * Definition, execution, and visualization of V/f profiles, ramps, and frequency sweeps.

5. **Measurement, logging, visualization / 計測・ログ・可視化**

   * CSV logging of waveforms and status (timestamp, analog inputs/outputs, DI/DO states).
   * Matplotlib-based waveform display on PC.
   * Basic frequency, RMS, THM visualization.

6. **GUI presets and replay / GUI プリセット・再現**

   * Preset management for inverter input profiles and HIL scenarios.
   * Ability to save and reload configurations.

## 5.2 Out-of-scope / 非対象範囲の例

Clearly state that the following are **not** in scope:

* Full-scale inverter protection firmware design
* High-fidelity motor physics simulation (FEM-level accuracy)
* Hard real-time guarantees beyond what Raspberry Pi + Python can reasonably achieve (if stricter guarantees are required, a separate MCU is assumed and out-of-scope here)

---

# 6. Non-functional requirements / 非機能要件（SC-NFR-000〜049）

Define non-functional requirements with IDs `SC-NFR-###`, **with numeric targets and tolerances**.

### 6.1 Real-time behavior / リアルタイム性

* Real-time loop on Raspberry Pi:

  * 3-phase waveform update period: for example, **100.0 µs ± 20.0 µs (p95)**
* ADC sampling loop:

  * Analog input sampling period: e.g., **200.0 µs ± 50.0 µs (p95)**
* MODBUS communication:

  * Command round-trip (PC → Pi → PC) target: **≤ 50.0 ms ± 10.0 ms (p95)**
* The RT loop **must not block** on MODBUS or file I/O.

### 6.2 Extensibility / 拡張性

* The architecture shall allow adding new:

  * Analog channels (additional DAC/ADC)
  * Digital channels (extra DI/DO)
  * New V/f profiles or operating modes
    with minimal changes to existing code (high cohesion, low coupling).

### 6.3 Readability and coding rules / 可読性・コーディング規約

* Implementation language: **Python only**.
* Every function must have a **Doxygen-style English comment** at its top (`@brief`, `@param`, `@return`).
* Important lines should have **Japanese comments** explaining the intent.
* File headers must briefly describe the responsibility of each module in Japanese.

### 6.4 Logging & observability / ログ・可観測性

* CSV logs must include: timestamp, command values, measured analog values, DI/DO states, errors.
* Timestamp resolution and accuracy must be specified (e.g., `1.0 ms ± 0.1 ms`).
* Logged data should be directly consumable by the PC waveform viewer.

---

# 7. Documentation policy / 文書スタイル・書式ルール

Explain, as a policy, the following rules (these are given by the user and should be treated as **official**):

1. **Languages / 言語**

   * All key documents must be **bilingual (Japanese + English)**.
   * Technical terms must be briefly defined at first appearance.

2. **Section order / 章立て**

   * Use the following order for major documents:

     * 目的 (Purpose) → 前提 (Assumptions) → 要件 (Requirements) → 設計 (Design) → 検証 (Verification) → 結論 (Conclusion) → 付録 (Appendix)

3. **Intra-section structure / 節内構成**

   * Each section should follow:

     1. **Conclusion first** (≤3 lines)
     2. Rationale / 根拠
     3. Figures & tables / 図表
     4. Supplementary notes / 補足

4. **Numbers / 数値表現**

   * Always include **units and permissible error** (e.g., `1.00 ms ±100 µs`).

5. **Glossary / 用語集**

   * `/docs/glossary.md` is the single source of truth.
   * New terms must be added to the glossary.

6. **Formatting / 書式**

   * H1 is used only once as the document title; H2 and deeper headings up to depth 3.
   * Bullet lists are limited to **≤5 items** per section; more than 5 items must be represented as a table.
   * Important terms: **bold**.
   * Inline code: `` `code` `` and fenced code blocks with language tags for long code sections.
   * Figures and tables must be labeled as `Fig.1`, `Table 1`, etc., with captions summarizing the main point.

7. **Writing style / 文体**

   * Japanese text uses polite style（「〜です・ます」調）統一。
   * Each paragraph should be ≤5 lines.
   * Order within a paragraph: **claim → rationale → numeric evidence → impact**.
   * Figures must include legend, units, and assumptions so that they are understandable without reading the text.

---

# 8. Traceability & ID policy / トレーサビリティ・IDポリシー

Define ID types and ranges:

* Functional requirements: `SC-REQ-###`
* Non-functional requirements: `SC-NFR-###`
* Design items: `SC-DES-###`
* Test cases: `SC-TST-###`
* Process/operations: `SC-PROC-###`

In this constitution, use:

* `SC-REQ-000`–`SC-REQ-049` for top-level functional requirements
* `SC-NFR-000`–`SC-NFR-049` for non-functional requirements
* `SC-PROC-000`–`SC-PROC-049` for process and rules

Each requirement must explicitly indicate its **corresponding test IDs** (e.g., `SC-TST-010, SC-TST-011`), and the appendix must include a **traceability matrix** template (requirements ↔ tests).

---

# 9. Data flow specification (DFD text) / データフロー図の文章仕様

Provide a **textual specification for DFD Level 0**, which later can be converted into actual diagrams:

## Nodes / ノード

* Host GUI (Tkinter, “Tkinter GUI”)
* Host MODBUS Client
* Network (TCP/IP)
* Raspberry Pi MODBUS Server
* Raspberry Pi RT Core (V/f profile, waveform generation, ADC/DI handling)
* Raspberry Pi HAL (DAC/PWM/ADC/DI/DO)
* Yaskawa inverter (AI/DI/DO/AO)

## Data flows / データフロー

1. GUI → MODBUS Client:

   * Analog command values (AI1/AI2/THM/3-phase amplitude/frequency/Vf ratio)
   * Digital command values (RUN/STOP, FWD/REV, modes, presets)

2. MODBUS Client → Raspberry Pi MODBUS Server:

   * Writes to **Holding Registers** (analog commands)
   * Writes to **Coils** (digital commands)

3. Raspberry Pi MODBUS Server → RT Core:

   * Updates a shared command snapshot used by the real-time loop (non-blocking).

4. RT Core → HAL:

   * Waveform samples to DAC/PWM for inverter analog inputs
   * DI/DO update commands to drive inverter DI lines

5. Inverter → HAL:

   * Analog outputs (AO) sampled by ADC (e.g., current, voltage, power)
   * Digital outputs (DO) read as DI inputs (RUN status, alarm, fault, ready)

6. HAL → RT Core:

   * Provides sampled values to RT core for logging and derived metrics (e.g., RMS, frequency).

7. Raspberry Pi MODBUS Server → Host PC:

   * Exposes measured values as MODBUS input registers and discrete inputs.

8. Host GUI:

   * Displays waveforms, numeric values, and DO states in real time.

---

# 10. Change management / 変更管理（Git & PR）

Define:

* Commit granularity:

  * One chapter or one module per commit where possible; commits must be review-friendly.
* Commit message style:

  * Use format: `SC-###: Summary (<=72 chars)`
* Pull Request template:

  * Purpose / 目的
  * Changes / 変更点
  * Alternatives considered / 代替案
  * Real-time & memory impact / RT・メモリへの影響
  * Test results / テスト結果
  * Risks & mitigations / リスクと緩和策

---

# 11. AI usage & prompting policy / 生成AI利用ポリシー

Define how developers shall use ChatGPT, GitHub Copilot, and SpecKit:

* Every AI prompt must include:

  * **Document type** (design/api/test-plan/release-notes, etc.)
  * **Purpose**
  * **Target output file path**
  * **Required sections**
  * **Trace ID range** to use

* Provide an example (in Japanese) that can be reused:

> `/docs/api.md に HAL-DAC の API 仕様を作成。範囲は SC-API-200〜249。関数は init / write_ch / write3。各関数に 前提 / 引数 / 戻り / エラー / RT 注意 / 使用例 を含める。`

---

# 12. Output style of constitution.md / constitution.md の出力スタイル

Finally, ensure `/docs/constitution.md` itself:

* Uses **bilingual headings and text** (Japanese + English).
* Follows the global structure:

  * 目的 → 前提 → 要件 → 設計 → 検証 → 結論 → 付録
* Contains clear, self-contained policies so that SmartChecker can be designed and implemented consistently as a **PC–RasPi–Yaskawa inverter HIL system with MODBUS and analog/digital I/O**.

---
