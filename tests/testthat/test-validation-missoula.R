test_that("Missoula example validation test", {

  argv <- "-a"

  iniFileName <- system.file("inputs/ini/enf_test1.ini", package="BiomeBGCR")
  print(paste("iniFileName :", iniFileName, sep=""))

  res <- bgcExecuteSpinup(argv, c(iniFileName))
  if (res[[1]] != 0) {
    stop(paste("bgcExecute failed with error ", res[[1]]))
  }

  res <- bgcExecute(argv, c(iniFileName))
  if (res[[1]] != 0) {
  stop(paste("bgcExecute failed with error ", res[[1]]))
  }

  ini <- res[[2]]
  resultFile <- paste(iniGet(ini, "OUTPUT_CONTROL", 1), "_ann.txt", sep = "")
  referenceFile <- system.file("outputs/reference/oth_ann.txt", package="BiomeBGCR")
  # compare the reference output to the current output
  expect_true(compareASCIIFiles(resultFile, referenceFile))
})
