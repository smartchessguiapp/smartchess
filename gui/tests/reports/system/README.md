# reports/system/

This folder stores system test execution reports and raw measurement data.

- Human readable reports: Markdown files named `sc-tst-###-report.md`.
- CI / machine readable reports: JUnit XML named `sc-tst-###-results.xml`.
- Raw measurement data: CSV files named `sc-tst-###-data.csv`.

Each report must include:
- Execution datetime
- Execution environment (runner name, OS, Python version, dependent package versions)
- Hardware information (Raspberry Pi serial, AD/DA board model, IO expander model, power conditions)
- Test ID and name
- Related PR or change ID
- Related requirement IDs
- Pass/Fail summary
- Link to raw data (CSV)

When saving reports from CI, ensure the artifacts are attached to the CI run for traceability.
