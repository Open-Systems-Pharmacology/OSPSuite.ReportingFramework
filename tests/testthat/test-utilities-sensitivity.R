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

test_that(".sensitivityScenarioType classifies population, individual, and mixed", {
  populationScenario <- list(
    population = structure(list(), class = "Population")
  )
  individualScenario <- list(simulation = "sim")

  expect_equal(
    ospsuite.reportingframework:::.sensitivityScenarioType(
      scenarioList = list(A = populationScenario),
      scenarioNames = "A"
    ),
    "population"
  )

  expect_equal(
    ospsuite.reportingframework:::.sensitivityScenarioType(
      scenarioList = list(A = individualScenario),
      scenarioNames = "A"
    ),
    "individual"
  )

  expect_equal(
    ospsuite.reportingframework:::.sensitivityScenarioType(
      scenarioList = list(A = populationScenario, B = individualScenario),
      scenarioNames = c("A", "B")
    ),
    "mixed"
  )
})

test_that("runSensitivityAnalysisForScenarios errors for population scenarios", {
  local_mocked_bindings(
    getScenarioDefinitions = function(...) {
      data.table::data.table(
        scenarioName = "ScenarioA",
        pKParameter = NA_character_,
        modelFile = "a.pkml"
      )
    },
    getOutputPathIds = function(...) {
      data.table::data.table(
        outputPathId = character(),
        outputPath = character()
      )
    },
    xlsxReadData = function(...) {
      data.table::data.table(
        parameter = "Clearance",
        parameterPath = "P1|CL"
      )
    },
    .package = "ospsuite.reportingframework"
  )

  projectConfiguration <- list(
    addOns = list(
      sensitivityFile = "SensitivityParameter.xlsx",
      reportsFile = "Reports.xlsx"
    ),
    outputFolder = withr::local_tempdir(),
    scenariosFile = "Scenarios.xlsx",
    modelFolder = "Models"
  )

  expect_error(
    ospsuite.reportingframework::runSensitivityAnalysisForScenarios(
      projectConfiguration = projectConfiguration,
      scenarioList = list(
        ScenarioA = list(population = structure(list(), class = "Population"))
      ),
      scenarioNames = "ScenarioA",
      sensitivitysheet = "Sensitivity",
      overwrite = FALSE
    ),
    "runRepresentativeSensitivityAnalysisForScenarios"
  )
})

test_that("runRepresentativeSensitivityAnalysisForScenarios errors for individual scenarios", {
  local_mocked_bindings(
    getScenarioDefinitions = function(...) {
      data.table::data.table(
        scenarioName = "ScenarioA",
        pKParameter = NA_character_,
        modelFile = "a.pkml"
      )
    },
    getOutputPathIds = function(...) {
      data.table::data.table(
        outputPathId = character(),
        outputPath = character()
      )
    },
    xlsxReadData = function(...) {
      data.table::data.table(
        parameter = "Clearance",
        parameterPath = "P1|CL"
      )
    },
    .package = "ospsuite.reportingframework"
  )

  projectConfiguration <- list(
    addOns = list(
      sensitivityFile = "SensitivityParameter.xlsx",
      reportsFile = "Reports.xlsx"
    ),
    outputFolder = withr::local_tempdir(),
    scenariosFile = "Scenarios.xlsx",
    modelFolder = "Models"
  )

  expect_error(
    ospsuite.reportingframework:::runRepresentativeSensitivityAnalysisForScenarios(
      projectConfiguration = projectConfiguration,
      scenarioList = list(ScenarioA = list(simulation = "sim")),
      scenarioNames = "ScenarioA",
      sensitivitysheet = "Sensitivity",
      overwrite = FALSE
    ),
    "runSensitivityAnalysisForScenarios"
  )
})

test_that("runSensitivityAnalysisForScenarios errors for mixed scenarios", {
  local_mocked_bindings(
    getScenarioDefinitions = function(...) {
      data.table::data.table(
        scenarioName = c("ScenarioPop", "ScenarioInd"),
        pKParameter = c(NA_character_, NA_character_),
        modelFile = c("a.pkml", "b.pkml")
      )
    },
    getOutputPathIds = function(...) {
      data.table::data.table(
        outputPathId = character(),
        outputPath = character()
      )
    },
    xlsxReadData = function(...) {
      data.table::data.table(
        parameter = "Clearance",
        parameterPath = "P1|CL"
      )
    },
    .package = "ospsuite.reportingframework"
  )

  projectConfiguration <- list(
    addOns = list(
      sensitivityFile = "SensitivityParameter.xlsx",
      reportsFile = "Reports.xlsx"
    ),
    outputFolder = withr::local_tempdir(),
    scenariosFile = "Scenarios.xlsx",
    modelFolder = "Models"
  )

  expect_error(
    ospsuite.reportingframework::runSensitivityAnalysisForScenarios(
      projectConfiguration = projectConfiguration,
      scenarioList = list(
        ScenarioPop = list(
          population = structure(list(), class = "Population")
        ),
        ScenarioInd = list(simulation = "sim")
      ),
      scenarioNames = c("ScenarioPop", "ScenarioInd"),
      sensitivitysheet = "Sensitivity",
      overwrite = FALSE
    ),
    "requires only individual scenarios"
  )
})

test_that("runRepresentativeSensitivityAnalysisForScenarios errors for mixed scenarios", {
  local_mocked_bindings(
    getScenarioDefinitions = function(...) {
      data.table::data.table(
        scenarioName = c("ScenarioPop", "ScenarioInd"),
        pKParameter = c(NA_character_, NA_character_),
        modelFile = c("a.pkml", "b.pkml")
      )
    },
    getOutputPathIds = function(...) {
      data.table::data.table(
        outputPathId = character(),
        outputPath = character()
      )
    },
    xlsxReadData = function(...) {
      data.table::data.table(
        parameter = "Clearance",
        parameterPath = "P1|CL"
      )
    },
    .package = "ospsuite.reportingframework"
  )

  projectConfiguration <- list(
    addOns = list(
      sensitivityFile = "SensitivityParameter.xlsx",
      reportsFile = "Reports.xlsx"
    ),
    outputFolder = withr::local_tempdir(),
    scenariosFile = "Scenarios.xlsx",
    modelFolder = "Models"
  )

  expect_error(
    ospsuite.reportingframework:::runRepresentativeSensitivityAnalysisForScenarios(
      projectConfiguration = projectConfiguration,
      scenarioList = list(
        ScenarioPop = list(
          population = structure(list(), class = "Population")
        ),
        ScenarioInd = list(simulation = "sim")
      ),
      scenarioNames = c("ScenarioPop", "ScenarioInd"),
      sensitivitysheet = "Sensitivity",
      overwrite = FALSE
    ),
    "requires only population scenarios"
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
  expect_setequal(
    result$allCovariateNames,
    c("SensitivityParameter", "perturbationFactor")
  )
  expect_equal(
    as.double(result$getCovariateValues(covariateName = "perturbationFactor")),
    c(1, 0.9090909, 1.1),
    tolerance = 1e-6
  )
  expect_equal(
    result$getParameterValues("P1|CL"),
    c(10, 9.090909, 11),
    tolerance = 1e-6
  )
})

test_that("buildSensitivityPopulationFromBaselineRow enriches missing paths from simulation", {
  baselineRow <- data.frame(
    IndividualId = 0,
    check.names = FALSE
  )
  baselineRow[["Organism|Hematocrit"]] <- 0.45

  local_mocked_bindings(
    getParameter = function(path, container, stopIfNotFound = FALSE) {
      if (identical(path, "P1|CL")) {
        return(list(value = 10))
      }
      NULL
    },
    .package = "ospsuite"
  )

  result <- ospsuite.reportingframework:::.buildSensitivityPopulationFromBaselineRow(
    baselineRow = baselineRow,
    sensitivityParameter = list(Clearance = "P1|CL"),
    simulation = "dummy-simulation",
    variationRange = 0.1,
    numberOfSteps = 1L
  )

  expect_true("P1|CL" %in% result$allParameterPaths)
  expect_equal(
    result$getParameterValues("P1|CL"),
    c(10, 9.090909, 11),
    tolerance = 1e-6
  )
})

test_that("getRepresentativeIndividualsForSensitivity selects nearest requested quantiles", {
  tmp <- withr::local_tempdir()
  pkDir <- file.path(
    tmp,
    ospsuite.reportingframework:::EXPORTDIR$pKAnalysisResults
  )
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
    c("5th percentile", "median", "95th percentile")
  )
  expect_equal(
    representatives$RepresentativeIndividualId,
    c(5, 49, 94)
  )
})

test_that(".prepareRepresentativeSensitivityPopulation keeps one individual and perturbs parameters", {
  pop <- ospsuite::loadPopulation(.populationPath())

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
    unique(c(
      pop$allCovariateNames,
      c("perturbationFactor", "SensitivityParameter")
    ))
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
    c(
      "IndividualId",
      "perturbationFactor",
      "SensitivityParameter",
      "Organism|Hematocrit"
    )
  )
  expect_equal(representativePopulation$count, 5)
})

test_that(".prepareRepresentativeSensitivityPopulation enriches missing paths from simulation", {
  pop <- ospsuite::loadPopulation(.populationPath())

  local_mocked_bindings(
    getParameter = function(path, container, stopIfNotFound = FALSE) {
      if (identical(path, "Organism|SimulationOnly|CL")) {
        return(list(value = 4))
      }
      NULL
    },
    .package = "ospsuite"
  )

  representativePopulation <-
    ospsuite.reportingframework:::.prepareRepresentativeSensitivityPopulation(
      population = pop,
      representativeIndividualId = 2,
      sensitivityParameter = list(Clearance = "Organism|SimulationOnly|CL"),
      scenarioSimulation = "dummy-simulation",
      variationRange = 0.1,
      numberOfSteps = 1
    )

  expect_true(
    "Organism|SimulationOnly|CL" %in% representativePopulation$allParameterPaths
  )
  expect_equal(
    representativePopulation$getParameterValues("Organism|SimulationOnly|CL"),
    c(4, 3.6363636, 4.4),
    tolerance = 1e-6
  )
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

test_that(".getScenarioFilesForSensitivity serializes configured simulations from scenarioList", {
  dtScenarios <- data.table::data.table(
    scenarioName = c("ref", "trt"),
    modelFile = c("ref.pkml", "trt.pkml")
  )

  savedSimulationIds <- character(0)
  local_mocked_bindings(
    saveSimulation = function(simulation, filePath) {
      savedSimulationIds <<- c(savedSimulationIds, simulation$id)
      file.create(filePath)
      invisible(NULL)
    },
    .package = "ospsuite"
  )

  files <- ospsuite.reportingframework:::.getScenarioFilesForSensitivity(
    dtScenarios = dtScenarios,
    currentScenarioName = "trt",
    modelFolder = "Models",
    scenarioList = list(
      ref = list(simulation = list(id = "configured-ref")),
      trt = list(simulation = list(id = "configured-trt"))
    ),
    referenceScenario = "ref"
  )

  expect_equal(names(files), c("ref", "trt"))
  expect_equal(savedSimulationIds, c("configured-ref", "configured-trt"))
  expect_true(all(file.exists(unname(files))))
  expect_true(all(grepl("sensitivity-scenarios-", dirname(unname(files)))))
  expect_false(any(
    unname(files) %in% file.path("Models", c("ref.pkml", "trt.pkml"))
  ))

  unlink(attr(files, "tempFolder"), recursive = TRUE, force = TRUE)
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

  expect_error(
    ospsuite.reportingframework:::.getScenarioFilesForSensitivity(
      dtScenarios = dtScenarios,
      currentScenarioName = "trt",
      modelFolder = "Models",
      scenarioList = list(),
      referenceScenario = NULL
    ),
    "not available in scenarioList"
  )

  expect_error(
    ospsuite.reportingframework:::.getScenarioFilesForSensitivity(
      dtScenarios = dtScenarios,
      currentScenarioName = "trt",
      modelFolder = "Models",
      scenarioList = list(trt = list()),
      referenceScenario = NULL
    ),
    "does not contain a configured simulation"
  )
})

test_that(".getPKDefinitionsForSensitivity maps PK names to output paths", {
  local_mocked_bindings(
    xlsxReadData = function(...) {
      data.table::data.table(
        name = c("AUC", "Cmax"),
        displayName = c("AUC display", "Cmax display"),
        displayUnit = c("mg*h/L", "mg/L"),
        descriptions = c("", ""),
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
  expect_true(all(
    c("displayNamePKParameter", "displayUnitPKParameter") %in%
      names(definitions)
  ))
  expect_setequal(definitions$pKParameterName, c("AUC", "Cmax"))
  expect_setequal(
    definitions$outputPath,
    c("Organism|Plasma", "Organism|Liver")
  )
  expect_setequal(
    definitions$displayNamePKParameter,
    c("AUC display", "Cmax display")
  )
  expect_setequal(
    definitions$displayUnitPKParameter,
    c("mg*h/L", "mg/L")
  )
})

test_that(".getPKDefinitionsForSensitivity errors when no valid rows remain", {
  local_mocked_bindings(
    xlsxReadData = function(...) {
      data.table::data.table(
        name = c("AUC", "Cmax"),
        displayName = c("AUC display", "Cmax display"),
        displayUnit = c("mg*h/L", "mg/L"),
        descriptions = c("", ""),
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

test_that(".getPKDefinitionsForSensitivity expands empty outputPathIds to all outputs", {
  local_mocked_bindings(
    xlsxReadData = function(...) {
      data.table::data.table(
        name = c("AUC", "Cmax", "Tmax", "Cmin"),
        displayName = c(
          "AUC display",
          "Cmax display",
          "Tmax display",
          "Cmin display"
        ),
        displayUnit = c("mg*h/L", "mg/L", "h", "mg/L"),
        descriptions = c("", "", "", ""),
        outputPathIds = c(NA_character_, "op2", "", "   ")
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
  expect_equal(nrow(definitions), 7)

  aucPaths <- definitions[pKParameterName == "AUC", unique(outputPath)]
  tmaxPaths <- definitions[pKParameterName == "Tmax", unique(outputPath)]
  cminPaths <- definitions[pKParameterName == "Cmin", unique(outputPath)]
  cmaxPaths <- definitions[pKParameterName == "Cmax", unique(outputPath)]

  expect_setequal(aucPaths, c("Organism|Plasma", "Organism|Liver"))
  expect_setequal(tmaxPaths, c("Organism|Plasma", "Organism|Liver"))
  expect_setequal(cminPaths, c("Organism|Plasma", "Organism|Liver"))
  expect_setequal(cmaxPaths, "Organism|Liver")
})

test_that(".readPKDefinitionsFromSheets filters invalid sheet names and separates outputPathIds", {
  capturedSheets <- character(0)

  local_mocked_bindings(
    xlsxReadData = function(wb, sheetName, skipDescriptionRow = TRUE) {
      capturedSheets <<- c(capturedSheets, sheetName)
      data.table::data.table(
        name = paste0("PK_", sheetName),
        outputPathIds = "op1, op2"
      )
    },
    .package = "ospsuite.reportingframework"
  )

  definitions <- ospsuite.reportingframework:::.readPKDefinitionsFromSheets(
    projectConfiguration = list(addOns = list(pKParameterFile = "unused.xlsx")),
    pkParameterSheets = c("PK_A", NA_character_, "", "PK_B", "PK_A")
  )

  expect_setequal(capturedSheets, c("PK_A", "PK_B"))
  expect_true(all(c("name", "outputPathId") %in% names(definitions)))
  expect_setequal(definitions$outputPathId, c("op1", "op2"))
})

test_that(".readPKDefinitionsFromSheets errors when no valid sheet names remain", {
  expect_error(
    ospsuite.reportingframework:::.readPKDefinitionsFromSheets(
      projectConfiguration = list(
        addOns = list(pKParameterFile = "unused.xlsx")
      ),
      pkParameterSheets = c(NA_character_, "")
    ),
    "length >= 1"
  )
})

test_that(".normalizedPKDefinitionsForSensitivity renames expected columns", {
  pkDefinitions <- data.table::data.table(
    name = c("AUC", "Cmax"),
    displayName = c("AUC display", "Cmax display"),
    displayUnit = c("mg*h/L", "mg/L"),
    descriptions = c("desc A", "desc B"),
    outputPathIds = c("op1", "op2")
  )

  normalized <- ospsuite.reportingframework:::.normalizedPKDefinitionsForSensitivity(
    pkDefinitions
  )

  expect_true(all(
    c(
      "pKParameter",
      "displayNamePKParameter",
      "displayUnitPKParameter",
      "outputPathIds"
    ) %in%
      names(normalized)
  ))
  expect_false("descriptions" %in% names(normalized))
  expect_setequal(normalized$pKParameter, c("AUC", "Cmax"))
  expect_setequal(
    normalized$displayNamePKParameter,
    c("AUC display", "Cmax display")
  )
  expect_setequal(
    normalized$displayUnitPKParameter,
    c("mg*h/L", "mg/L")
  )
})

test_that(".calculateSensitivities computes summary sensitivity with mocked dependencies", {
  tmp <- withr::local_tempdir()
  fileA <- file.path(tmp, "a.pkml")
  fileB <- file.path(tmp, "b.pkml")
  file.create(fileA)
  file.create(fileB)

  local_mocked_bindings(
    .prepareSensitivityPopulation = function(...) {
      structure(list(), class = "Population")
    },
    .loadSensitivityPKValues = function(...) {
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
        perturbationFactor = c(1, 1.1),
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
    referenceScenario = "base",
    writeResult = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$SensitivityParameter, "CL")
  expect_equal(result$QuantityPath, "q1")
  expect_equal(result$PKParameter, "AUC")
  expect_equal(result$sens, 1)
  expect_equal(result$referenceScenario, "base")
})

test_that(".calculateSensitivities errors when simulation run returns no results", {
  tmp <- withr::local_tempdir()
  fileA <- file.path(tmp, "a.pkml")
  file.create(fileA)

  local_mocked_bindings(
    .prepareSensitivityPopulation = function(...) {
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
  dir.create(
    file.path(tmp, ospsuite.reportingframework:::EXPORTDIR$simulationResult),
    recursive = TRUE
  )
  dir.create(
    file.path(tmp, ospsuite.reportingframework:::EXPORTDIR$pKAnalysisResults),
    recursive = TRUE
  )

  projectConfiguration <- list(outputFolder = tmp)
  called <- FALSE

  local_mocked_bindings(
    runAndSaveScenarios = function(...) {
      called <<- TRUE
      data.table::fwrite(
        data.table::data.table(a = 1),
        file.path(
          tmp,
          ospsuite.reportingframework:::EXPORTDIR$pKAnalysisResults,
          "ScenarioA.csv"
        )
      )
      data.table::fwrite(
        data.table::data.table(a = 1),
        file.path(
          tmp,
          ospsuite.reportingframework:::EXPORTDIR$simulationResult,
          "ScenarioA_population.csv"
        )
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

test_that(".assertSensitivityBaselineCompatibility handles zero and non-zero baselines", {
  expect_silent(
    ospsuite.reportingframework:::.assertSensitivityBaselineCompatibility(
      parPath = "P1|CL",
      valueScenario1 = 0,
      valueScenario2 = 0
    )
  )

  expect_error(
    ospsuite.reportingframework:::.assertSensitivityBaselineCompatibility(
      parPath = "P1|CL",
      valueScenario1 = 0,
      valueScenario2 = 1
    ),
    "Baseline mismatch"
  )

  expect_silent(
    ospsuite.reportingframework:::.assertSensitivityBaselineCompatibility(
      parPath = "P1|CL",
      valueScenario1 = 1,
      valueScenario2 = 1
    )
  )
})
