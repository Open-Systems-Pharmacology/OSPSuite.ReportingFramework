#' initialize logging. and create test directory
#'
#' @return temporary projectPath
iniLogFileForTest <- function() {
  projectPath <- tempdir()
  if (!dir.exists(projectPath)) dir.create(projectPath)
  initLogfunction(projectPath = projectPath, verbose = FALSE)

  return(projectPath)
}


#
#' Cleanup of project path and ospsuit options
#'
#' @param projectPath temporary projectPath
cleanupLogFileForTest <- function(projectPath) {
  unlink(projectPath, recursive = TRUE)

  optionsToNull <- grep("ospsuite",
    names(options()),
    ignore.case = TRUE,
    value = TRUE
  )
  for (opt in optionsToNull) {
    eval(parse(text = paste0("options(", opt, " = NULL)")))
  }
}
#'  generates random observed data for tests
#'
#' @return data.table in format as created by readObservedDataByDictionary
randomObservedData <- function() {
  dataDT <- data.table()
  for (subject in seq(1001, 1005)) {
    weight <- rnorm(1, 70, 5)
    height <- rnorm(1, 170, 10)
    gender <- sample(c("MALE", "FEMALE"), size = 1)
    age <- sample(seq(20, 60), size = 1)
    for (stud in c(1023, 1024)) {
      dataDT <- rbind(
        dataDT,
        data.table(
          StudyId = as.character(stud), SubjectId = as.character(subject),
          IndividualId = paste0(stud, subject),
          group = as.character(stud), OutputPathId = "output",
          xValues = seq(0, 24, 4), yValues = rnorm(7, 40, 2), yUnit = "ng/L", xUnit = "h",
          lloq = c(rep(10, 6), NA),
          age = age, weight = weight, height = height, gender = gender, population = "population"
        )
      )
    }
  }

  for (dc in c("StudyId", "SubjectId", "IndividualId", "group", "OutputPathId")) {
    data.table::setattr(dataDT[[dc]], "columnType", "identifier")
  }
  for (dc in c("xValues", "yValues", "yUnit", "xUnit", "lloq")) {
    data.table::setattr(dataDT[[dc]], "columnType", "timeprofile")
  }
  for (dc in c("age", "weight", "height", "gender", "population")) {
    data.table::setattr(dataDT[[dc]], "columnType", "covariate")
  }



  return(dataDT)
}


#' setup an empty testproject
#'
#' @param projectPath temporyr projectpath
#'
#' @return projectConfiguration
setUpTestProject <- function(projectPath) {
  projectPath <- initProject(
    projectPath = file.path(projectPath, "testProject"),
    sourceFolder = templateDirectory(),
    overwrite = FALSE
  )

  projectConfiguration <-
    createDefaultProjectConfiguration.wrapped(
      path = file.path(projectPath, "ProjectConfiguration.xlsx")
    )

  file.copy(
    from = system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    to = file.path(projectConfiguration$modelFolder, "Aciclovir.pkml")
  )



  return(projectConfiguration)
}

#' adjust datadictionary ready to use
#'
#' @template projectConfig
setDataDictionary <- function(projectConfiguration) {
  targetColumn <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)


  dtDataFiles <- xlsxReadData(wb = wb, sheetName = "DataFiles")
  dtDataFiles <- dtDataFiles[c(1)]

  dtDataFiles <- rbind(
    dtDataFiles,
    data.table(
      DataFile = file.path("Data", "data.csv"),
      Dictionary = "tpDictionary",
      DataFilter = ""
    )
  )

  xlsxWriteData(wb = wb, sheetName = "DataFiles", dt = dtDataFiles)

  # - tpDictionary

  tpDictionary <- xlsxReadData(wb = wb, sheetName = "tpDictionary")

  tpDictionary <- tpDictionary[targetColumn != "population"]

  tpDictionary[targetColumn == "SubjectId"]$sourceColumn <- "SID"
  tpDictionary[targetColumn == "IndividualId"]$filterValue <- "paste(STUD,SID,sep = '_')"
  tpDictionary[targetColumn == "group"]$sourceColumn <- "STUD"
  tpDictionary[targetColumn == "OutputPathId"]$sourceColumn <- "MOLECULE"
  tpDictionary[targetColumn == "xValues"]$sourceColumn <- "Time"
  tpDictionary[targetColumn == "yValues"]$sourceColumn <- "DV"
  tpDictionary[targetColumn == "yUnit"]$sourceColumn <- "DVUNIT"
  tpDictionary[targetColumn == "lloq"]$sourceColumn <- ""
  tpDictionary[targetColumn == "lloq"]$filter <- "TRUE"
  tpDictionary[targetColumn == "lloq"]$filterValue <- NA
  tpDictionary[targetColumn == "age"]$sourceColumn <- "Age"
  tpDictionary[targetColumn == "weight"]$sourceColumn <- "Weight"
  tpDictionary[targetColumn == "height"]$sourceColumn <- "Height"
  tpDictionary[targetColumn == "gender"]$sourceColumn <- "Gender"

  tpDictionary <- rbind(
    tpDictionary,
    tpDictionary[targetColumn == "IndividualId"] %>%
      dplyr::mutate(targetColumn = "population") %>%
      dplyr::mutate(type = "biometrics") %>%
      dplyr::mutate(filterValue = '"European_ICRP_2002"')
  )

  tpDictionary <- rbind(
    tpDictionary,
    data.table::copy(tpDictionary)[1, ] %>%
      .[, names(tpDictionary) := NA]
  )

  xlsxWriteData(wb = wb, sheetName = "tpDictionary", dt = tpDictionary)

  openxlsx::saveWorkbook(wb, projectConfiguration$dataImporterConfigurationFile, overwrite = TRUE)

  return(invisible())
}

#' generates random observed data to test import
#'
#' @template projectConfig
addRandomSourceData <- function(projectConfiguration) {
  dt <- data.table()
  for (sid in seq(1001, 1005)) {
    weight <- rnorm(1, 70, 5)
    height <- rnorm(1, 170, 10)
    gender <- sample(c(1, 2), size = 1)
    age <- sample(seq(20, 60), size = 1)
    for (stud in c(1023, 1024)) {
      for (molecule in c("PARENT", "METABOLITE")) {
        dt <- rbind(
          dt,
          data.table(
            SID = sid, STUD = stud,
            Time = seq(0, 24, 4), "Time unit" = "h", DV = rnorm(7, 40, 2), "DVUNIT" = "ng/L",
            Gender = gender, Age = age, Weight = weight, Height = height,
            MOLECULE = molecule, COUNTRY = "Germany"
          )
        )
      }
    }
  }

  data.table::fwrite(dt, file = file.path(projectConfiguration$dataFolder, "data.csv"))

  return(invisible())
}
