test_that("Boisvenue2010 example validation test using single step mode", {
  argv <- "-a -v1"

  spinupFileNames <- c(
    system.file("inputs/ini/spinup_b.ini", package = "BiomeBGCR"),
    system.file("inputs/ini/spinup_bc.ini", package = "BiomeBGCR"),
    system.file("inputs/ini/spinup_g.ini", package = "BiomeBGCR"),
    system.file("inputs/ini/spinup_m.ini", package = "BiomeBGCR"),
    system.file("inputs/ini/spinup_pr.ini", package = "BiomeBGCR"),
    system.file("inputs/ini/spinup_y.ini", package = "BiomeBGCR")
  )

  print(paste("spinupFileNames :", spinupFileNames, sep = ""))

  res <- bgcExecuteSpinup(argv, spinupFileNames)
  if (res[[1]] != 0) {
    stop(paste("bgcExecute failed with error ", res[[1]]))
  }

  goFileNames <- c(
    system.file("inputs/ini/cccmat63_b.ini", package = "BiomeBGCR"),
    system.file("inputs/ini/cccmat63_bc.ini", package = "BiomeBGCR"),
    system.file("inputs/ini/cccmat63_g.ini", package = "BiomeBGCR"),
    system.file("inputs/ini/cccmat63_m.ini", package = "BiomeBGCR"),
    system.file("inputs/ini/cccmat63_pr.ini", package = "BiomeBGCR"),
    system.file("inputs/ini/cccmat63_y.ini", package = "BiomeBGCR")
  )

  iterations <- 1
  for (i in 1:iterations) {
    message("Executing test ", i)
    res <- bgcExecute(argv, goFileNames)

    ini <- res[[2]][[1]]
    resultFile <- paste0(iniGet(ini, "OUTPUT_CONTROL", 1), "_ann.txt")
    lineSplit <- strsplit(resultFile, "/")[[1]]
    referenceFileRelative <- file.path("outputs", "reference", lineSplit[length(lineSplit)])
    referenceFile <- system.file(referenceFileRelative, package = "BiomeBGCR")
    # compare the reference output to the current output
    res <- compareASCIIFiles(resultFile, referenceFile)
    expect_true(res)
  }
})
