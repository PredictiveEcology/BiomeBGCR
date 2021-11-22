test_that("Missoula example validation test", {
  argv <- "-a"

  iniFileName <- system.file("inputs/ini/enf_test1.ini", package = "BiomeBGCR")
  message("iniFileName: ", iniFileName)

  res <- bgcExecuteSpinup(argv, c(iniFileName))

  res <- bgcExecute(argv, c(iniFileName))

  ini <- res[[2]][[1]]
  resultFile <- paste0(iniGet(ini, "OUTPUT_CONTROL", 1), "_ann.txt")
  referenceFile <- system.file("outputs/reference/oth_ann.txt", package = "BiomeBGCR")
  # compare the reference output to the current output
  expect_true(compareASCIIFiles(resultFile, referenceFile))
})
