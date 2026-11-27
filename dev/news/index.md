# Changelog

## ospsuite.reportingframework 2.0.0

This release includes all changes introduced since version 1.0.1.

Key differences vs 1.0.1:

- Logging was migrated to `ospsuite.utils` as backend.
  - [`addMessageToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addMessageToLog.md),
    [`captureLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/captureLog.md),
    and
    [`writeTableToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/writeTableToLog.md)
    now route through
    [`ospsuite.utils::logInfo()`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/logInfo.html)
    /
    [`ospsuite.utils::logCatch()`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/logCatch.html).
  - [`setLogPath()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setLogPath.md)
    now configures file logging through
    [`ospsuite.utils::setLogFolder()`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/setLogFolder.html)
    and the `logger` appender.
  - Logging behavior is now controlled via
    [`setShowLogMessages()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setShowLogMessages.md)
    and uses a unified package-level setup.
- ReportingFramework is more decoupled from raw `esqlabsR` configuration
  handling.
  - Added `ProjectConfigurationRF` wrapper that delegates standard
    `esqlabsR` fields while managing reporting add-ons separately.
  - Added project migration helpers
    ([`upgradeToReportingFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/upgradeToReportingFramework.md),
    [`initProject()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md))
    with `keepEsqlabsRStructure` support.
  - Added configuration version stamping and stricter handling of
    read-only/managed configuration properties.
  - Added JSON snapshot/restore support for reporting add-ons
    ([`snapshotProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/snapshotProjectConfigurationRF.md),
    [`restoreProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/restoreProjectConfigurationRF.md)).
- Workflow/report generation moved from Rmd-centric to Quarto-centric
  internals.
  - Replaced `RmdPlotManager` with `QmdPlotManager`.
  - Added
    [`mergeQmds()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mergeQmds.md)
    and updated template/report assembly flows accordingly.
  - Updated workflow and ePackage templates to match the new rendering
    pipeline.
  - ePackage export scripts now use the updated template structure,
    reducing manual post-processing when assembling final deliverables.
- Data import and configuration were extended.
  - Added `DATACLASS = numericValues` support for scalar numeric
    configuration entries.
  - Extended
    [`readObservedDataByDictionary()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/readObservedDataByDictionary.md)
    to read numeric values.
  - Added
    [`getNumericValue()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getNumericValue.md)
    for typed scalar retrieval with optional unit checks.
  - Refactored dictionary mapping in `DataImportMap.xlsx` from
    `sourceColumn`/`filter`/`filterValue` to expression-based
    `valueExpr`/`conditionExpr`.
- Sensitivity workflows and plot generation were expanded.
  - Added reference-scenario support in sensitivity workflows.
  - Added representative sensitivity analysis flow and refactored
    sensitivity plot helpers.
  - Updated PK and demographic plotting utilities/captions for improved
    consistency.
- Package structure, templates, and extdata were reorganized.
  - Reworked `inst/templates` (including reporting-related workbook
    layout).
  - Reorganized tutorial/extdata assets under `inst/extdata/Tutorial`
    and aligned workflow examples.
  - Added template validity automation scripts in `data-raw/`.
- Error/warning/message handling was centralized.
  - Introduced and expanded centralized message definitions in
    `R/messages.R`.
  - Replaced many inline strings with reusable message helpers across
    plotting and workflow utilities.
- Documentation and tests were substantially updated.
  - Updated exported API docs in `man/` and namespace exports.
  - Updated README/pkgdown/vignettes and adjusted tests to cover new
    workflow, plotting, and configuration behavior.
- AI workflow helpers are currently experimental.
  - [`exportWorkflowContext()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportWorkflowContext.md),
    [`createAIWorkflowPrompt()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIWorkflowPrompt.md),
    [`createAIPromptFromTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIPromptFromTask.md),
    and
    [`validateWorkflowForAI()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateWorkflowForAI.md)
    are provided for early evaluation and may evolve in upcoming
    releases.

## ospsuite.reportingframework 1.0.1

- Initial release of the package as beta version
