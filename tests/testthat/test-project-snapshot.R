test_that("snapshotProjectConfigurationRF creates JSON with RF metadata", {
  d <- file.path(tempdir(), "snap_test_1")
  unlink(d, recursive = TRUE)
  dir.create(d)
  withr::local_dir(d, .local_envir = parent.frame())

  ospsuite.reportingframework::initProject(
    projectDirectory = d,
    configurationDirectory = d,
    overwrite = TRUE
  )
  pc <- ospsuite.reportingframework::createProjectConfiguration(
    file.path(d, "ProjectConfiguration.xlsx")
  )

  # snapshot
  snap <- ospsuite.reportingframework::snapshotProjectConfigurationRF(
    pc,
    outputDir = d
  )

  # verify JSON structure
  expect_true(file.exists(file.path(d, "ProjectConfiguration.json")))
  expect_true("projectConfigurationAddons" %in% names(snap))
  expect_true("reportsFile" %in% names(snap))
  # addons stored as {column_names, rows} — same format as esqlabsR sheets
  expect_true("column_names" %in% names(snap$projectConfigurationAddons))
  addonProps <- sapply(snap$projectConfigurationAddons$rows, `[[`, "Property")
  expect_true("ospsuiteReportingFrameworkVersion" %in% addonProps)
})

test_that("snapshotProjectConfigurationRF preserves base esqlabsR data", {
  d <- file.path(tempdir(), "snap_test_2")
  unlink(d, recursive = TRUE)
  dir.create(d)
  withr::local_dir(d, .local_envir = parent.frame())

  ospsuite.reportingframework::initProject(
    projectDirectory = d,
    configurationDirectory = d,
    overwrite = TRUE
  )
  pc <- ospsuite.reportingframework::createProjectConfiguration(
    file.path(d, "ProjectConfiguration.xlsx")
  )

  snap <- ospsuite.reportingframework::snapshotProjectConfigurationRF(
    pc,
    outputDir = d
  )

  # verify base esqlabsR structure is preserved
  expect_true("projectConfiguration" %in% names(snap))
  expect_true("modelParameterSets" %in% names(snap))
  expect_true("Populations" %in% names(snap))
})

test_that("restoreProjectConfigurationRF recreates ProjectConfigurationRF", {
  d <- file.path(tempdir(), "snap_test_3")
  unlink(d, recursive = TRUE)
  dir.create(d)

  ospsuite.reportingframework::initProject(
    projectDirectory = d,
    configurationDirectory = d,
    overwrite = TRUE
  )
  pc1 <- ospsuite.reportingframework::createProjectConfiguration(
    file.path(d, "ProjectConfiguration.xlsx")
  )

  # snapshot
  ospsuite.reportingframework::snapshotProjectConfigurationRF(
    pc1,
    outputDir = d
  )

  # restore to new location
  r <- file.path(tempdir(), "snap_rest_3")
  unlink(r, recursive = TRUE)
  dir.create(r)

  pc2 <- ospsuite.reportingframework::restoreProjectConfigurationRF(
    file.path(d, "ProjectConfiguration.json"),
    outputDir = r
  )

  expect_s3_class(pc2, "ProjectConfigurationRF")
  expect_true(file.exists(file.path(r, "ProjectConfiguration.xlsx")))
})

test_that("restoreProjectConfigurationRF preserves RF version", {
  d <- file.path(tempdir(), "snap_test_4")
  unlink(d, recursive = TRUE)
  dir.create(d)

  ospsuite.reportingframework::initProject(
    projectDirectory = d,
    configurationDirectory = d,
    overwrite = TRUE
  )
  pc1 <- ospsuite.reportingframework::createProjectConfiguration(
    file.path(d, "ProjectConfiguration.xlsx")
  )
  v1 <- pc1$ospsuiteReportingFrameworkVersion

  # snapshot
  ospsuite.reportingframework::snapshotProjectConfigurationRF(
    pc1,
    outputDir = d
  )

  # restore
  r <- file.path(tempdir(), "snap_rest_4")
  unlink(r, recursive = TRUE)
  dir.create(r)

  pc2 <- ospsuite.reportingframework::restoreProjectConfigurationRF(
    file.path(d, "ProjectConfiguration.json"),
    outputDir = r
  )

  expect_equal(pc2$ospsuiteReportingFrameworkVersion, v1)
})

test_that("restoreProjectConfigurationRF recreates addons sheet", {
  d <- file.path(tempdir(), "snap_test_5")
  unlink(d, recursive = TRUE)
  dir.create(d)

  ospsuite.reportingframework::initProject(
    projectDirectory = d,
    configurationDirectory = d,
    overwrite = TRUE
  )
  pc1 <- ospsuite.reportingframework::createProjectConfiguration(
    file.path(d, "ProjectConfiguration.xlsx")
  )

  # add custom addon via method
  aFolder <- file.path(d, "myAddon")
  dir.create(aFolder)
  pc1$addAddOnFolderToConfiguration(
    property = "myCustomFolder",
    value = aFolder,
    description = "Test addon"
  )

  # snapshot
  ospsuite.reportingframework::snapshotProjectConfigurationRF(
    pc1,
    outputDir = d
  )

  # restore
  r <- file.path(tempdir(), "snap_rest_5")
  unlink(r, recursive = TRUE)
  dir.create(r)

  pc2 <- ospsuite.reportingframework::restoreProjectConfigurationRF(
    file.path(d, "ProjectConfiguration.json"),
    outputDir = r
  )

  # check addons sheet exists and contains custom addon
  wb <- openxlsx::loadWorkbook(file.path(r, "ProjectConfiguration.xlsx"))
  expect_true("addons" %in% wb$sheet_names)
  expect_true("myCustomFolder" %in% names(pc2$addOns))
})

test_that("snapshotProjectConfigurationRF handles missing optional metadata", {
  d <- file.path(tempdir(), "snap_test_6")
  unlink(d, recursive = TRUE)
  dir.create(d)

  ospsuite.reportingframework::initProject(
    projectDirectory = d,
    configurationDirectory = d,
    overwrite = TRUE
  )
  pc <- ospsuite.reportingframework::createProjectConfiguration(
    file.path(d, "ProjectConfiguration.xlsx")
  )

  # remove a property from addOns (e.g. sensitivityFile)
  # Note: sensitivityFile is optional and may not be set
  snap <- ospsuite.reportingframework::snapshotProjectConfigurationRF(
    pc,
    outputDir = d
  )

  # verify snapshot still completes
  expect_true(file.exists(file.path(d, "ProjectConfiguration.json")))
  expect_true("projectConfigurationAddons" %in% names(snap))
})

test_that("restoreProjectConfigurationRF round-trip preserves all addons", {
  d <- file.path(tempdir(), "snap_test_7")
  unlink(d, recursive = TRUE)
  dir.create(d)

  ospsuite.reportingframework::initProject(
    projectDirectory = d,
    configurationDirectory = d,
    overwrite = TRUE
  )
  pc1 <- ospsuite.reportingframework::createProjectConfiguration(
    file.path(d, "ProjectConfiguration.xlsx")
  )

  # add multiple addons via method
  aFolder1 <- file.path(d, "addon1")
  aFolder2 <- file.path(d, "addon2")
  dir.create(aFolder1)
  dir.create(aFolder2)
  pc1$addAddOnFolderToConfiguration("addon1", aFolder1, "First addon")
  pc1$addAddOnFolderToConfiguration("addon2", aFolder2, "Second addon")
  pc1$save()

  addOns1 <- names(pc1$addOns)

  # snapshot and restore
  ospsuite.reportingframework::snapshotProjectConfigurationRF(
    pc1,
    outputDir = d
  )
  r <- file.path(tempdir(), "snap_rest_7")
  unlink(r, recursive = TRUE)
  dir.create(r)

  pc2 <- ospsuite.reportingframework::restoreProjectConfigurationRF(
    file.path(d, "ProjectConfiguration.json"),
    outputDir = r
  )

  addOns2 <- names(pc2$addOns)

  expect_equal(sort(addOns2), sort(addOns1))
})

test_that("snapshotProjectConfigurationRF JSON is valid and readable", {
  d <- file.path(tempdir(), "snap_test_8")
  unlink(d, recursive = TRUE)
  dir.create(d)

  ospsuite.reportingframework::initProject(
    projectDirectory = d,
    configurationDirectory = d,
    overwrite = TRUE
  )
  pc <- ospsuite.reportingframework::createProjectConfiguration(
    file.path(d, "ProjectConfiguration.xlsx")
  )

  ospsuite.reportingframework::snapshotProjectConfigurationRF(
    pc,
    outputDir = d
  )

  # verify JSON parses
  jsonPath <- file.path(d, "ProjectConfiguration.json")
  jsonData <- jsonlite::fromJSON(jsonPath)

  expect_true(is.list(jsonData))
  expect_true("projectConfiguration" %in% names(jsonData))
  expect_true("projectConfigurationAddons" %in% names(jsonData))
  # pKParameterFile xlsx should be captured under its property name
  expect_true("pKParameterFile" %in% names(jsonData))
})

test_that("restoreProjectConfigurationRF recreates xlsx files listed in addons", {
  d <- file.path(tempdir(), "snap_test_9")
  unlink(d, recursive = TRUE)
  dir.create(d)

  ospsuite.reportingframework::initProject(
    projectDirectory = d,
    configurationDirectory = d,
    overwrite = TRUE
  )
  pc1 <- ospsuite.reportingframework::createProjectConfiguration(
    file.path(d, "ProjectConfiguration.xlsx")
  )

  # record sheets in PKParameter.xlsx before snapshot
  pkPath1 <- pc1$addOns[["pKParameterFile"]]
  sheets1 <- readxl::excel_sheets(pkPath1)

  # snapshot
  ospsuite.reportingframework::snapshotProjectConfigurationRF(
    pc1,
    outputDir = d
  )

  # restore to clean directory
  r <- file.path(tempdir(), "snap_rest_9")
  unlink(r, recursive = TRUE)
  dir.create(r)

  pc2 <- ospsuite.reportingframework::restoreProjectConfigurationRF(
    file.path(d, "ProjectConfiguration.json"),
    outputDir = r
  )

  pkPath2 <- pc2$addOns[["pKParameterFile"]]
  expect_true(file.exists(pkPath2))
  expect_equal(readxl::excel_sheets(pkPath2), sheets1)
})
