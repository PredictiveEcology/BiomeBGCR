test_that("Missoula example validation test using single step mode", {
  argv <- "-a"

  tmpd <- tempdir()
  createIOdirs(tmpd)

  sampleInputsDir <- system.file("inputs", package = "BiomeBGCR")
  sampleInputFiles <- list.files(sampleInputsDir, recursive = TRUE)

  expect_true({
    all(file.copy(file.path(sampleInputsDir, sampleInputFiles),
                  file.path(tmpd, "inputs", sampleInputFiles)))
  })

  iniFileName <- file.path(tmpd, "inputs", "ini", "enf_test1.ini")

  res <- bgcExecuteSpinup(argv, c(iniFileName), tmpd)

  ini <- res[[2]][[1]]

  nbYears <- strtoi(iniGet(ini, "TIME_DEFINE", 2))

  # execute one-year steps and compare with reference annual results every step
  for (i in 1:nbYears) {
    res <- bgcExecute(argv, c(iniFileName), tmpd, 1, i == 1)

    ini <- res[[2]][[1]]

    resultFile <- paste0(iniGet(ini, "OUTPUT_CONTROL", 1), "_ann.txt")
    referenceFile <- system.file("outputs/reference/oth_ann.txt", package = "BiomeBGCR")

    # compare the reference output to the current output
    expect_true(compareASCIILines(resultFile, referenceFile, 11, 10 + i))
  }

  unlink(file.path(tmpd, "inputs"), recursive = TRUE)
  unlink(file.path(tmpd, "outputs"), recursive = TRUE)
})
