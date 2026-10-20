test_that(".sensitivityTableDataFromScenarios supports intersection and union mode", {
  scenarioASim <- .aciclovirSimulation()
  scenarioBSim <- .simpleSimulation()
  scenarioList <- list(
    ScenarioA = list(simulation = scenarioASim),
    ScenarioB = list(simulation = scenarioBSim)
  )

  scenarioAPaths <- ospsuite::potentialVariableParameterPathsFor(
    simulation = scenarioASim
  )
  scenarioBPaths <- ospsuite::potentialVariableParameterPathsFor(scenarioBSim)
  expectedPaths <- unique(intersect(scenarioAPaths, scenarioBPaths))

  expect_error(
    ospsuite.reportingframework:::.sensitivityTableDataFromScenarios(
      scenarioList = scenarioList,
      sheetName = "Sensitivity",
      parameterPathMode = "intersection"
    ),
    "No sensitivity parameter paths available"
  )

  expectedPaths <- unique(union(scenarioAPaths, scenarioBPaths))

  dt <- ospsuite.reportingframework:::.sensitivityTableDataFromScenarios(
    scenarioList = scenarioList,
    sheetName = "Union",
    parameterPathMode = "union"
  )

  expect_setequal(dt$parameterPath, expectedPaths)
  expect_equal(dt$parameter, gsub("\\|", " ", dt$parameterPath))
})

test_that(".sensitivityTableDataFromScenarios validates inputs", {
  expect_error(
    ospsuite.reportingframework:::.sensitivityTableDataFromScenarios(
      scenarioList = list(list(simulation = .simpleSimulation())),
      sheetName = "Sensitivity",
      parameterPathMode = "intersection"
    )
  )

  expect_error(
    ospsuite.reportingframework:::.sensitivityTableDataFromScenarios(
      scenarioList = list(
        ScenarioA = list(simulation = .simpleSimulation())
      ),
      sheetName = "ThisSheetNameIsWayTooLongForExcel",
      parameterPathMode = "intersection"
    )
  )

  expect_error(
    ospsuite.reportingframework:::.sensitivityTableDataFromScenarios(
      scenarioList = list(
        ScenarioA = list(simulation = .simpleSimulation())
      ),
      sheetName = "Sensitivity",
      parameterPathMode = "unsupported"
    )
  )
})


test_that("asSensitivityParameterList groups rows by parameter label", {
  sensitivityParameter <-
    ospsuite.reportingframework:::.asSensitivityParameterList(
      data.table::data.table(
        parameter = c("Clearance", "Clearance", "Volume", ""),
        parameterPath = c("P1|CL", "P2|CL", "P3|V", "P4|Q")
      )
    )

  expect_setequal(
    names(sensitivityParameter),
    c("Clearance", "Volume", "P4|Q")
  )
  expect_equal(sensitivityParameter$Clearance, c("P1|CL", "P2|CL"))
  expect_equal(sensitivityParameter$Volume, "P3|V")
  expect_equal(sensitivityParameter[["P4|Q"]], "P4|Q")
})

test_that("buildSensitivityPopulationFromBaselineRow accepts data.frame input", {
  baselineRow <- data.frame(
    IndividualId = 0,
    check.names = FALSE
  )
  baselineRow[["P1|CL"]] <- 10
  baselineRow[["P2|V"]] <- 25

  result <- ospsuite.reportingframework:::.buildSensitivityPopulationFromBaselineRow(
    baselineRow = baselineRow,
    sensitivityParameter = list(Clearance = c("P1|CL", "P2|V")),
    variationRange = 0.1,
    numberOfSteps = 1L
  )

  expect_s3_class(result, "Population")
  expect_equal(result$allParameterPaths, c("P1|CL", "P2|V"))
  expect_setequal(result$allCovariateNames, c("SensitivityParameter", "factor"))
  expect_equal(
    as.double(result$getCovariateValues(covariateName = 'factor')),
    c(1, 0.9090909, 1.1),
    tolerance = 1e-6
  )
  expect_equal(
    result$getParameterValues("P1|CL"),
    c(10, 9.090909, 11),
    tolerance = 1e-6
  )
})

test_that("getRepresentativeIndividualsForSensitivity selects nearest requested quantiles", {
  tmp <- withr::local_tempdir()
  pkDir <- file.path(tmp, EXPORTDIR$pKAnalysisResults)
  dir.create(pkDir, recursive = TRUE)

  data.table::fwrite(
    data.table::data.table(
      QuantityPath = rep("Organism|Plasma|Drug", 5),
      Parameter = rep("AUC", 100),
      Value = seq(1, 100),
      IndividualId = seq(0, 99)
    ),
    file.path(pkDir, "ScenarioA.csv")
  )

  representatives <-
    ospsuite.reportingframework:::.getRepresentativeIndividualsForSensitivity(
      outputFolder = tmp,
      scenarioName = "ScenarioA",
      outputPaths = "Organism|Plasma|Drug",
      pkParameter = "AUC",
      representativeQuantiles = c(0.05, 0.5, 0.95)
    )

  expect_equal(
    representatives$RepresentativeQuantile,
    c("p5", "p50", "p95")
  )
  expect_equal(
    representatives$RepresentativeIndividualId,
    c(5, 49, 94)
  )
})

test_that(".prepareRepresentativeSensitivityPopulation keeps one individual and perturbs parameters", {
  pop <- loadPopulation(.populationPath())

  representativePopulation <-
    ospsuite.reportingframework:::.prepareRepresentativeSensitivityPopulation(
      population = pop,
      representativeIndividualId = 2,
      sensitivityParameter = list(Hematocrit = "Organism|Hematocrit"),
      variationRange = 0.2,
      numberOfSteps = 2
    )

  expect_setequal(
    unique(representativePopulation$allCovariateNames),
    unique(c(pop$allCovariateNames, c("factor", "SensitivityParameter")))
  )

  dtRepresentativePopulation <- ospsuite::populationToDataFrame(
    representativePopulation
  )

  nonUniqueCols <- names(dtRepresentativePopulation)[
    vapply(
      dtRepresentativePopulation,
      \(x) uniqueN(x) > 1,
      logical(1)
    )
  ]

  expect_equal(
    nonUniqueCols,
    c("IndividualId", "factor", "SensitivityParameter", "Organism|Hematocrit")
  )
  expect_equal(representativePopulation$count, 5)
})

test_that(".getScenarioFilesForSensitivity returns scenario files and supports ratio mode", {
  dtScenarios <- data.table::data.table(
    scenarioName = c("ref", "trt"),
    modelFile = c("ref.pkml", "trt.pkml")
  )

  files <- ospsuite.reportingframework:::.getScenarioFilesForSensitivity(
    dtScenarios = dtScenarios,
    currentScenarioName = "trt",
    modelFolder = "Models",
    referenceScenario = "ref"
  )

  expect_equal(names(files), c("ref", "trt"))
  expect_equal(unname(files), file.path("Models", c("ref.pkml", "trt.pkml")))
})

test_that(".getScenarioFilesForSensitivity validates incompatible or ambiguous inputs", {
  dtScenarios <- data.table::data.table(
    scenarioName = c("trt", "trt"),
    modelFile = c("a.pkml", "b.pkml")
  )

  expect_error(
    ospsuite.reportingframework:::.getScenarioFilesForSensitivity(
      dtScenarios = dtScenarios,
      currentScenarioName = "trt",
      modelFolder = "Models",
      referenceScenario = "trt"
    ),
    "must differ"
  )

  expect_error(
    ospsuite.reportingframework:::.getScenarioFilesForSensitivity(
      dtScenarios = dtScenarios,
      currentScenarioName = "trt",
      modelFolder = "Models"
    ),
    "exactly one modelFile"
  )
})

test_that(".getPKDefinitionsForSensitivity maps PK names to output paths", {
  local_mocked_bindings(
    xlsxReadData = function(...) {
      data.table::data.table(
        name = c("AUC", "Cmax"),
        outputPathIds = c("op1", "op2")
      )
    },
    .package = "ospsuite.reportingframework"
  )

  definitions <- ospsuite.reportingframework:::.getPKDefinitionsForSensitivity(
    projectConfiguration = list(addOns = list(pKParameterFile = "unused.xlsx")),
    pkParameterSheets = "PK",
    dtOutputPaths = data.table::data.table(
      outputPathId = c("op1", "op2"),
      outputPath = c("Organism|Plasma", "Organism|Liver")
    )
  )

  expect_true(all(c("pKParameterName", "outputPath") %in% names(definitions)))
  expect_setequal(definitions$pKParameterName, c("AUC", "Cmax"))
  expect_setequal(
    definitions$outputPath,
    c("Organism|Plasma", "Organism|Liver")
  )
})

test_that(".getPKDefinitionsForSensitivity errors when no valid rows remain", {
  local_mocked_bindings(
    xlsxReadData = function(...) {
      data.table::data.table(
        name = c("AUC", "Cmax"),
        outputPathIds = c("missing1", "missing2")
      )
    },
    .package = "ospsuite.reportingframework"
  )

  expect_error(
    ospsuite.reportingframework:::.getPKDefinitionsForSensitivity(
      projectConfiguration = list(
        addOns = list(pKParameterFile = "unused.xlsx")
      ),
      pkParameterSheets = "PK",
      dtOutputPaths = data.table::data.table(
        outputPathId = "op1",
        outputPath = "Organism|Plasma"
      )
    ),
    "No PK parameter/output path definitions"
  )
})

test_that(".calculateSensitivities computes summary sensitivity with mocked dependencies", {
  tmp <- withr::local_tempdir()
  fileA <- file.path(tmp, "a.pkml")
  fileB <- file.path(tmp, "b.pkml")
  file.create(fileA)
  file.create(fileB)

  local_mocked_bindings(
    prepareSensitivityPopulation = function(...) {
      structure(list(), class = "Population")
    },
    loadSensitivityPKValues = function(...) {
      data.table::data.table(
        IndividualId = c(1L, 2L),
        QuantityPath = c("q1", "q1"),
        PKParameter = c("AUC", "AUC"),
        Value = c(10, 11)
      )
    },
    .package = "ospsuite.reportingframework"
  )

  local_mocked_bindings(
    loadSimulation = function(...) "sim",
    runSimulations = function(...) list(list(dummy = TRUE)),
    calculatePKAnalyses = function(...) "pk",
    exportPKAnalysesToCSV = function(...) invisible(NULL),
    populationToDataFrame = function(...) {
      data.table::data.table(
        IndividualId = c(1L, 2L),
        factor = c(1, 1.1),
        SensitivityParameter = c("", "CL")
      )
    },
    .package = "ospsuite"
  )

  result <- ospsuite.reportingframework:::.calculateSensitivities(
    scenarioFiles = c(base = fileA, trt = fileB),
    outputPaths = "q1",
    pkParameter = "AUC",
    sensitivityParameter = list(CL = "P1|CL"),
    outFolder = tmp,
    writeResult = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$SensitivityParameter, "CL")
  expect_equal(result$QuantityPath, "q1")
  expect_equal(result$PKParameter, "AUC")
  expect_equal(result$sens, 1)
})

test_that(".calculateSensitivities errors when simulation run returns no results", {
  tmp <- withr::local_tempdir()
  fileA <- file.path(tmp, "a.pkml")
  file.create(fileA)

  local_mocked_bindings(
    prepareSensitivityPopulation = function(...) {
      structure(list(), class = "Population")
    },
    .package = "ospsuite.reportingframework"
  )

  local_mocked_bindings(
    loadSimulation = function(...) "sim",
    runSimulations = function(...) list(),
    .package = "ospsuite"
  )

  expect_error(
    ospsuite.reportingframework:::.calculateSensitivities(
      scenarioFiles = c(base = fileA),
      outputPaths = "q1",
      pkParameter = "AUC",
      sensitivityParameter = list(CL = "P1|CL"),
      outFolder = tmp,
      writeResult = FALSE
    ),
    "runSimulations returned no results"
  )
})

test_that(".ensureRepresentativeSensitivityPrerequisites runs scenarios only when needed", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, EXPORTDIR$simulationResult), recursive = TRUE)
  dir.create(file.path(tmp, EXPORTDIR$pKAnalysisResults), recursive = TRUE)

  projectConfiguration <- list(outputFolder = tmp)
  called <- FALSE

  local_mocked_bindings(
    runAndSaveScenarios = function(...) {
      called <<- TRUE
      data.table::fwrite(
        data.table::data.table(a = 1),
        file.path(tmp, EXPORTDIR$pKAnalysisResults, "ScenarioA.csv")
      )
      data.table::fwrite(
        data.table::data.table(a = 1),
        file.path(tmp, EXPORTDIR$simulationResult, "ScenarioA_population.csv")
      )
      invisible(NULL)
    },
    .package = "ospsuite.reportingframework"
  )

  ospsuite.reportingframework:::.ensureRepresentativeSensitivityPrerequisites(
    projectConfiguration = projectConfiguration,
    scenarioList = list(ScenarioA = list(simulation = "unused")),
    scenarioName = "ScenarioA"
  )
  expect_true(called)

  called <- FALSE
  ospsuite.reportingframework:::.ensureRepresentativeSensitivityPrerequisites(
    projectConfiguration = projectConfiguration,
    scenarioList = list(ScenarioA = list(simulation = "unused")),
    scenarioName = "ScenarioA"
  )
  expect_false(called)
})

test_that(".getScenarioPopulationForSensitivity returns in-memory or file population", {
  inMemoryPopulation <- structure(list(id = "in-memory"), class = "Population")
  inMemory <- ospsuite.reportingframework:::.getScenarioPopulationForSensitivity(
    scenarioList = list(ScenarioA = list(population = inMemoryPopulation)),
    projectConfiguration = list(outputFolder = tempdir()),
    scenarioName = "ScenarioA"
  )
  expect_identical(inMemory, inMemoryPopulation)

  tmp <- withr::local_tempdir()
  simDir <- file.path(tmp, EXPORTDIR$simulationResult)
  dir.create(simDir, recursive = TRUE)
  popPath <- file.path(simDir, "ScenarioB_population.csv")
  data.table::fwrite(data.table::data.table(a = 1), popPath)

  local_mocked_bindings(
    loadPopulation = function(filePath) {
      expect_equal(filePath, popPath)
      structure(list(id = "loaded"), class = "Population")
    },
    .package = "ospsuite"
  )

  loaded <- ospsuite.reportingframework:::.getScenarioPopulationForSensitivity(
    scenarioList = list(ScenarioB = list()),
    projectConfiguration = list(outputFolder = tmp),
    scenarioName = "ScenarioB"
  )
  expect_equal(loaded$id, "loaded")
})
