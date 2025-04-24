# testProject was set up by setup.R  which involved most of the esqlabsR_bracket scripts

test_that("initProject copies files from sourceFolder to destination", {

  configurationDirectory <- projectConfiguration$configurationsFolder

  # Check if the files were copied to the destination folder
  fileList <- list.files(configurationDirectory)
  expect_gte(length(fileList ),expected = 9)
  expect_true("Plots.xlsx" %in% fileList )

})


test_that("TestProject has corect format", {

  expect_s3_class(projectConfiguration, "ProjectConfiguration")

  # Perform assertions
  expect_true(length(scenarioList) > 0)

  expect_true(length(list.files(file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult))) > 0)
})

