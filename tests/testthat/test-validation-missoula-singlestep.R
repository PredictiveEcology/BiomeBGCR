test_that("Missoula example validation test using single step mode", {
  argv <- "-a"

  iniFileName <- system.file("inputs/ini/enf_test1.ini", package = "BiomeBGCR")
  message("iniFileName: ", iniFileName)

  res <- bgcExecuteSpinup(argv, c(iniFileName))
  if (res[[1]] != 0) {
    stop(paste("bgcExecute failed with error ", res[[1]]))
  }

  ini <- res[[2]][[1]]

  nbYears <- strtoi(iniGet(ini, "TIME_DEFINE", 2))

  # execute one-year steps and compare with reference annual results every step
  for (i in 1:nbYears) {
    res <- bgcExecute(argv, c(iniFileName), 1, i == 1)
    if (res[[1]] != 0) {
      stop(paste("bgcExecute failed with error ", res[[1]]))
    }

    ini <- res[[2]][[1]]

    resultFile <- paste0(iniGet(ini, "OUTPUT_CONTROL", 1), "_ann.txt")
    referenceFile <- system.file("outputs/reference/oth_ann.txt", package = "BiomeBGCR")

    # compare the reference output to the current output
    expect_true(compareASCIILines(resultFile, referenceFile, 11, 10 + i))
  }
})
