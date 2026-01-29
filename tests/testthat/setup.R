# prepare test directory
l <- buildTestData(rootDirectory = NULL, writeTestData = FALSE)
list2env(l, envir = .GlobalEnv)
rm(l)

oldOspSuitePlotDefaults <- ospsuite.plots::setDefaults() # Set default plotting parameters
theme_update(legend.position = "top") # Update theme for legend position
options(OSPSuite.RF.skipFailingPlots = FALSE)
options(ospsuite.plots.watermark_enabled = TRUE)

withr::defer(
  {
    ospsuite.plots::resetDefaults(oldOspSuitePlotDefaults)
    options(OSPSuite.RF.skipFailingPlots = TRUE)
    options(ospsuite.plots.watermark_enabled = NULL)
  },
  teardown_env()
)
