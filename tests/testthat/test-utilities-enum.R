# Tests for utilities-enum.R

test_that("TIMERANGE enum has all expected values", {
  expect_equal(TIMERANGE$total, "total")
  expect_equal(TIMERANGE$firstApplication, "firstApplication")
  expect_equal(TIMERANGE$lastApplication, "lastApplication")
})

test_that("DATACLASS enum has all expected values", {
  expect_equal(DATACLASS$tpIndividual, "tp Individual")
  expect_equal(DATACLASS$tpAggregated, "tp Aggregated")
  expect_equal(DATACLASS$pkIndividual, "pk Individual")
  expect_equal(DATACLASS$pkAggregated, "pk Aggregated")
  expect_equal(DATACLASS$tpTwinPop, "tp Virtual Twin Population")
})

test_that("EXPORTDIR enum has all expected values", {
  expect_equal(EXPORTDIR$simulationResult, "SimulationResults")
  expect_equal(EXPORTDIR$sensitivityResults, "SensitivityResults")
  expect_equal(EXPORTDIR$pKAnalysisResults, "PKAnalysisResults")
})

test_that("BIOMETRICUNITS enum has all expected values", {
  expect_equal(BIOMETRICUNITS$age, "year(s)")
  expect_equal(BIOMETRICUNITS$weight, "kg")
  expect_equal(BIOMETRICUNITS$height, "cm")
})
