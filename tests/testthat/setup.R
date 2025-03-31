# prepare test directory
l <- setupTestDirectoryForTests()
list2env(l, envir = .GlobalEnv)
rm(l)

oldOspSuitePlotDefaults <- ospsuite.plots::setDefaults()  # Set default plotting parameters
theme_update(legend.position = 'top')  # Update theme for legend position


withr::defer(ospsuite.plots::resetDefaults(oldOspSuitePlotDefaults), teardown_env())
