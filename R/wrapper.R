#' Execute a spinup simulation
#'
#' Calls the the underlying BiomeBGC C library.
#'
#' @param argv (Character Vector) : Arguments for the BiomeBGC library (same as 'bgc' commandline application)
#' @param iniFiles (String vector) : The list of ini files to simulate, one per site.
#'                 The files can be either spinup ini files or "go" ini files.
#'                 A "go" file will be automatically modified for spinup phase.
#' @param path (Character String) : Path to base directory to use for simulations.
#'
#' @return a list containing the resulting error code (0 means no error) and the ini file structure
#'
#' @export
bgcExecuteSpinup <- function(argv = "-a", iniFiles, path) {
  createIOdirs(path)

  iniList <- vector(mode = "list", length = length(iniFiles))

  for (i in 1:length(iniFiles)) {
    file <- iniFiles[i]
    ini <- iniRead(file)

    # check if we have to modify the ini files for spinup phase
    if (iniGet(ini, "TIME_DEFINE", 4) == "0") {
      ini <- iniMakeSpinup(ini)
    }

    ini <- iniFixPaths(path, ini)

    iniFiles[i] <- paste0(file, ".spinup.fixed")

    iniWrite(ini, iniFiles[i])

    iniList[[i]] <- ini
  }

  res <- bgcExecuteInternal(argv, iniFiles, -1)
  if (res != 0) {
    stop("bgcExecuteInternal failed with error ", res)
  }

  return(list(res, iniList))
}

#' bgcExecute
#'
#' Executes a simulation by calling the underlying BiomeBGC C library
#' @param argv (Character Vector) : Arguments for the BiomeBGC library (same as 'bgc' commandline application)
#' @param iniFiles (String vector) : The list of ini files to simulate, one per site.
#'                 The files are expected to be specified for "go mode".
#' @param path (Character String) : Path to base directory to use for simulations.
#' @param simYearsOverride The number of years to simulate (will override simyears from the ini file if specified).
#' @param firstRun this parameter will make the necessary changes in the ini file for the first run.
#' set to FALSE only when calling subsequent simulation steps in single step mode
#'
#' @return a list containing the resulting error code (0 means no error) and the ini file structure (one per ini file input)
#'
#' @export
bgcExecute <- function(argv = "-a", iniFiles, path, simYearsOverride = -1, firstRun = TRUE) {
  createIOdirs(path)

  if (simYearsOverride <= 0) {
    firstRun <- TRUE # should always be TRUE when not in single step mode
  }

  iniList <- vector(mode = "list", length = length(iniFiles))

  for (i in 1:length(iniFiles)) {
    file <- iniFiles[i]
    ini <- iniRead(file)
    ini <- iniFixPaths(path, ini)
    ini <- iniMakeSingleStep(ini, firstRun)

    iniFiles[i] <- paste0(file, ".go.fixed")

    iniWrite(ini, iniFiles[i])

    iniList[[i]] <- ini
  }

  res <- bgcExecuteInternal(argv, iniFiles, simYearsOverride)
  if (res != 0) {
    stop("bgcExecuteInternal failed with error ", res)
  }

  if (simYearsOverride > 0) {
    for (i in 1:length(iniList)) {
      ini <- iniList[[i]]
      fileRestart <- iniGet(ini, "RESTART", 5)
      fileRestartOut <- iniGet(ini, "RESTART", 6)

      # print(paste("copying ", fileRestartOut, " over ", fileRestart, sep = ""))

      file.copy(fileRestartOut, fileRestart, overwrite = TRUE)
    }
  }

  return(list(res, iniList))
}
