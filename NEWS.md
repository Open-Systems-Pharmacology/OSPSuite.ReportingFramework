# ospsuite.reportingframework 2.0.0

This release includes all changes introduced since version 1.0.1.

Key differences vs 1.0.1:

- Logging was migrated to `ospsuite.utils` as backend.
	- `addMessageToLog()`, `captureLog()`, and `writeTableToLog()` now route through `ospsuite.utils::logInfo()` / `ospsuite.utils::logCatch()`.
	- `setLogPath()` now configures file logging through `ospsuite.utils::setLogFolder()` and the `logger` appender.
	- Logging behavior is now controlled via `setShowLogMessages()` and uses a unified package-level setup.

- ReportingFramework is more decoupled from raw `esqlabsR` configuration handling.
	- Added `ProjectConfigurationRF` wrapper that delegates standard `esqlabsR` fields while managing reporting add-ons separately.
	- Added project migration helpers (`upgradeToReportingFramework()`, `initProject()`) with `keepEsqlabsRStructure` support.
	- Added configuration version stamping and stricter handling of read-only/managed configuration properties.
	- Added JSON snapshot/restore support for reporting add-ons (`snapshotProjectConfigurationRF()`, `restoreProjectConfigurationRF()`).

- Workflow/report generation moved from Rmd-centric to Quarto-centric internals.
	- Replaced `RmdPlotManager` with `QmdPlotManager`.
	- Added `mergeQmds()` and updated template/report assembly flows accordingly.
	- Updated workflow and ePackage templates to match the new rendering pipeline.
	- ePackage export scripts now use the updated template structure, reducing manual post-processing when assembling final deliverables.

- Data import and configuration were extended.
	- Added `DATACLASS = numericValues` support for scalar numeric configuration entries.
	- Extended `readObservedDataByDictionary()` to read numeric values.
	- Added `getNumericValue()` for typed scalar retrieval with optional unit checks.

- Sensitivity workflows and plot generation were expanded.
	- Added reference-scenario support in sensitivity workflows.
	- Added representative sensitivity analysis flow and refactored sensitivity plot helpers.
	- Updated PK and demographic plotting utilities/captions for improved consistency.

- Package structure, templates, and extdata were reorganized.
	- Reworked `inst/templates` (including reporting-related workbook layout).
	- Reorganized tutorial/extdata assets under `inst/extdata/Tutorial` and aligned workflow examples.
	- Added template validity automation scripts in `data-raw/`.

- Error/warning/message handling was centralized.
	- Introduced and expanded centralized message definitions in `R/messages.R`.
	- Replaced many inline strings with reusable message helpers across plotting and workflow utilities.

- Documentation and tests were substantially updated.
	- Updated exported API docs in `man/` and namespace exports.
	- Updated README/pkgdown/vignettes and adjusted tests to cover new workflow, plotting, and configuration behavior.

- AI workflow helpers are currently experimental.
	- `exportWorkflowContext()`, `createAIWorkflowPrompt()`, `createAIPromptFromTask()`, and `validateWorkflowForAI()` are provided for early evaluation and may evolve in upcoming releases.

# ospsuite.reportingframework 1.0.1

- Initial release of the package as beta version
