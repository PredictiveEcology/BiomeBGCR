test_that("Missoula example validation test", {
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
  message("iniFileName: ", iniFileName)

  res <- bgcExecuteSpinup(argv, c(iniFileName), tmpd)
  res <- bgcExecute(argv, c(iniFileName), tmpd)

  ini <- res[[2]][[1]]
  resultFile <- paste0(iniGet(ini, "OUTPUT_CONTROL", 1), "_ann.txt")
  referenceFile <- system.file("outputs/reference/oth_ann.txt", package = "BiomeBGCR")
  # compare the reference output to the current output
  expect_true(compareASCIIFiles(resultFile, referenceFile))

  unlink(file.path(tmpd, "inputs"), recursive = TRUE)
  unlink(file.path(tmpd, "outputs"), recursive = TRUE)
})
