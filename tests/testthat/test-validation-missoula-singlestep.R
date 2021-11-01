test_that("Missoula example validation test using single step mode", {

  argv <- "-a"

  iniFileName <- system.file("inputs/ini/enf_test1.ini", package="BiomeBGCR")
  print(paste("iniFileName :", iniFileName, sep=""))

  res <- bgcExecuteSpinup(argv, c(iniFileName))
  if (res[[1]] != 0) {
    stop(paste("bgcExecute failed with error ", res[[1]]))
  }

  ini <- res[[2]]

  nbYears <- strtoi(iniGet(ini, "TIME_DEFINE", 2))

  # execute one-year steps and compare with reference annual results every step
  for (i in 1:nbYears) {
    res <- bgcExecute(argv, c(iniFileName), 1, i == 1)
    if (res[[1]] != 0) {
     stop(paste("bgcExecute failed with error ", res[[1]]))
    }

    ini <- res[[2]]

    resultFile <- paste(iniGet(ini, "OUTPUT_CONTROL", 1), "_ann.txt", sep = "")
    referenceFile <- system.file("outputs/reference/oth_ann.txt", package="BiomeBGCR")

    # compare the reference output to the current output
    expect_true(compareASCIILines(resultFile, referenceFile, 11, 10 + i))

    fileRestart <- iniGet(ini, "RESTART", 5)
    fileRestartOut <- iniGet(ini, "RESTART", 6)

    file.copy(fileRestartOut, fileRestart, overwrite = TRUE)
  }
})
