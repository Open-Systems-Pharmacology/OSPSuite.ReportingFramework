# helpers -----------------------------------------------------------------

.makeProjectConfiguration <- function(.env = parent.frame()) {
  configDir <- withr::local_tempdir(.local_envir = .env)
  destPath <- file.path(configDir, "ProjectConfiguration.xlsx")
  file.copy(
    system.file(
      "templates",
      "ProjectConfiguration.xlsx",
      package = "ospsuite.reportingframework"
    ),
    destPath
  )
  createProjectConfiguration(path = destPath, ignoreVersionCheck = TRUE)
}

# ProjectConfigurationRF initialization ----------------------------------

test_that("createProjectConfiguration returns the expected classes", {
  pc <- .makeProjectConfiguration()

  expect_s3_class(pc, "ProjectConfigurationRF")
  expect_s3_class(pc$baseProjectconfiguration, "ProjectConfiguration")
})

test_that("ospsuiteReportingFrameworkVersion is read-only", {
  pc <- .makeProjectConfiguration()

  expect_error(
    pc$ospsuiteReportingFrameworkVersion <- "1.0.0",
    "ospsuiteReportingFrameworkVersion is readonly"
  )
})

# add-on management -------------------------------------------------------

test_that("addAddOnFileToConfiguration adds the file and registers the property", {
  pc <- .makeProjectConfiguration()

  templateFile <- withr::local_tempfile(fileext = ".txt")
  writeLines("template content", templateFile)

  pc$addAddOnFileToConfiguration(
    property = "testAddOnFile",
    value = "testfile.txt",
    description = "test add-on file",
    templatePath = templateFile
  )

  expect_true(file.exists(file.path(pc$configurationsFolder, "testfile.txt")))
  expect_true("testAddOnFile" %in% names(pc$addOns))
})

test_that("addAddOnFolderToConfiguration works with an absolute path", {
  pc <- .makeProjectConfiguration()
  folderPath <- file.path(pc$outputFolder, "testfolder")

  pc$addAddOnFolderToConfiguration(
    property = "testFolder",
    value = folderPath,
    description = "test folder"
  )

  expect_true(dir.exists(folderPath))
  expect_true("testFolder" %in% names(pc$addOns))
})

test_that("addAddOnFolderToConfiguration works with a relative path", {
  pc <- .makeProjectConfiguration()
  folderPath <- file.path(pc$outputFolder, "testfolder2")
  relValue <- fs::path_rel(folderPath, start = pc$configurationsFolder)

  pc$addAddOnFolderToConfiguration(
    property = "testFolder2",
    value = relValue,
    description = "test folder 2"
  )

  expect_true(dir.exists(folderPath))
  expect_true("testFolder2" %in% names(pc$addOns))
})

# two-sheet workbook format -----------------------------------------------

test_that("createProjectConfiguration loads a two-sheet ProjectConfiguration.xlsx", {
  pc <- .makeProjectConfiguration()

  expect_false(is.null(pc$ospsuiteReportingFrameworkVersion))
  expect_true("pKParameterFile" %in% names(pc$addOns))
  expect_true("electronicPackageFolder" %in% names(pc$addOns))
})

test_that("addAddOnFolderToConfiguration writes to addons sheet only", {
  pc <- .makeProjectConfiguration()
  configPath <- pc$projectConfigurationFilePath

  pc$addAddOnFolderToConfiguration(
    "myExtraFolder",
    file.path(tempdir(), "extra"),
    "extra"
  )

  wb <- openxlsx::loadWorkbook(configPath)
  dtAddons <- xlsxReadData(
    wb,
    sheetName = "addons",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_true("myExtraFolder" %in% dtAddons$Property)

  dtMain <- xlsxReadData(
    wb,
    sheetName = "esqlabsR",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_false("myExtraFolder" %in% dtMain$Property)
})

# save / version round-trips ----------------------------------------------

test_that("ospsuiteReportingFrameworkVersion is written to the addons sheet when saving", {
  pc <- .makeProjectConfiguration()
  savedPath <- withr::local_tempfile(fileext = ".xlsx")
  pc$save(path = savedPath)

  saved_pc <- createProjectConfiguration(
    path = savedPath,
    ignoreVersionCheck = TRUE
  )
  expect_equal(
    saved_pc$ospsuiteReportingFrameworkVersion,
    as.character(utils::packageVersion("ospsuite.reportingframework"))
  )
})

test_that("save() round-trips add-ons through addons sheet", {
  pc <- .makeProjectConfiguration()
  pc$addAddOnFolderToConfiguration(
    "savedFolder",
    file.path(tempdir(), "saved"),
    "round-trip"
  )

  savedPath <- withr::local_tempfile(fileext = ".xlsx")
  pc$save(path = savedPath)

  wb <- openxlsx::loadWorkbook(savedPath)
  expect_true("addons" %in% wb$sheet_names)
  dt <- xlsxReadData(
    wb,
    sheetName = "addons",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_true("savedFolder" %in% dt$Property)
})

test_that("an error is raised when the stored RF version does not match", {
  pc <- .makeProjectConfiguration()
  savedPath <- withr::local_tempfile(fileext = ".xlsx")
  pc$save(path = savedPath)

  wb <- openxlsx::loadWorkbook(savedPath)
  dt <- xlsxReadData(
    wb,
    sheetName = "addons",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  dt$Value[dt$Property == REPORTING_FRAMEWORK_VERSION_PROPERTY] <- "0.0.0"
  xlsxWriteData(wb, sheetName = "addons", dt = dt)
  openxlsx::saveWorkbook(wb, savedPath, overwrite = TRUE)

  expect_error(
    createProjectConfiguration(path = savedPath),
    regexp = "ospsuite.reportingframework version mismatch"
  )
})

test_that("an error is raised when no RF version is stored", {
  pc <- .makeProjectConfiguration()
  savedPath <- withr::local_tempfile(fileext = ".xlsx")
  pc$save(path = savedPath)

  wb <- openxlsx::loadWorkbook(savedPath)
  dt <- xlsxReadData(
    wb,
    sheetName = "addons",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  dt <- dt[dt$Property != REPORTING_FRAMEWORK_VERSION_PROPERTY, , drop = FALSE]
  xlsxWriteData(wb, sheetName = "addons", dt = dt)
  openxlsx::saveWorkbook(wb, savedPath, overwrite = TRUE)

  expect_error(
    createProjectConfiguration(path = savedPath),
    regexp = "ospsuite.reportingframework version mismatch"
  )
})

# initProject integration -------------------------------------------------

test_that("initProject stamps the RF version in the addons sheet only", {
  projectDirectory <- withr::local_tempdir()
  initProject(
    projectDirectory = projectDirectory,
    configurationDirectory = projectDirectory,
    overwrite = TRUE
  )
  configPath <- file.path(projectDirectory, "ProjectConfiguration.xlsx")

  expect_true("addons" %in% openxlsx::getSheetNames(configPath))

  dfAddons <- readxl::read_xlsx(
    configPath,
    sheet = "addons",
    col_types = "text"
  )
  expect_true(REPORTING_FRAMEWORK_VERSION_PROPERTY %in% dfAddons$Property)
  expect_equal(
    dfAddons$Value[dfAddons$Property == REPORTING_FRAMEWORK_VERSION_PROPERTY],
    as.character(utils::packageVersion("ospsuite.reportingframework"))
  )

  dfMain <- readxl::read_xlsx(
    configPath,
    sheet = "esqlabsR",
    col_types = "text"
  )
  expect_false(REPORTING_FRAMEWORK_VERSION_PROPERTY %in% dfMain$Property)
})
