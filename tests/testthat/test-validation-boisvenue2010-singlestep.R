test_that("Boisvenue2010 example validation test using single step mode", {
  argv <- "-a -v1"

  tmpd <- tempdir()
  createIOdirs(tmpd)

  sampleInputsDir <- system.file("inputs", package = "BiomeBGCR")
  sampleInputFiles <- list.files(sampleInputsDir, recursive = TRUE)

  expect_true({
    all(file.copy(file.path(sampleInputsDir, sampleInputFiles),
                  file.path(tmpd, "inputs", sampleInputFiles)))
  })

  spinupFileNames <- file.path(tmpd, "inputs", "ini", c(
    "spinup_b.ini",
    "spinup_bc.ini",
    "spinup_g.ini",
    "spinup_m.ini",
    "spinup_pr.ini",
    "spinup_y.ini"
  ))
  expect_true(all(file.exists(spinupFileNames)))

  message("spinupFileNames:\n", paste(spinupFileNames, collapse = "\n"))

  res <- bgcExecuteSpinup(argv, spinupFileNames, tmpd)

  goFileNames <- file.path(tmpd, "inputs", "ini", c(
    "cccmat63_b.ini",
    "cccmat63_bc.ini",
    "cccmat63_g.ini",
    "cccmat63_m.ini",
    "cccmat63_pr.ini",
    "cccmat63_y.ini"
  ))
  expect_true(all(file.exists(goFileNames)))

  # use the first ini file to get the number of simulation years because it is the same for all sites/scenarios
  ini <- iniRead(goFileNames[[1]])
  nbYears <- strtoi(iniGet(ini, "TIME_DEFINE", 2))

  # execute one-year steps and compare with reference annual results every step
  for (i in 1:nbYears) {
    message("Calling bgcExecute for year ", i)

    res <- bgcExecute(argv, goFileNames, tmpd, 1, i == 1)

    for (ini in res[[2]]) {
      resultFile <- paste0(iniGet(ini, "OUTPUT_CONTROL", 1), "_ann.txt")
      referenceFileRelative <- file.path("outputs", "reference", basename(resultFile))
      referenceFile <- system.file(referenceFileRelative, package = "BiomeBGCR")

      # compare the reference output to the current output
      resini <- compareASCIILines(resultFile, referenceFile, 11, 10 + i)
      expect_true(resini)
    }
  }

  unlink(file.path(tmpd, "inputs"), recursive = TRUE)
  unlink(file.path(tmpd, "outputs"), recursive = TRUE)
})
