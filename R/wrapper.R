
#' bgcExecuteSpinup
#'
#' Executes a spinup simulation by calling the underlying BiomeBGC C library
#' @param argv (Character Vector) : Arguments for the BiomeBGC library (same as 'bgc' commandline application)
#' @param iniFiles (String vector) : The list of ini files to simulate, one per site.
#' The files are expected to be specified for "go mode" and will be modified accordingly to simulate in spinup mode
#'
#' @return a list containing the resulting error code (0 means no error) and the ini file structure
#'
#' @export
bgcExecuteSpinup <- function(argv = "-a", iniFiles) {

    for (i in 1:length(iniFiles)) {
      file <- iniFiles[i]

      ini <- iniRead(file)

      # we have to modify the ini files for spinup phase
      ini <- iniMakeSpinup(ini)

      ini <- iniFixPaths(ini)

      iniFiles[i] <- paste(file, ".spinup.fixed", sep = "")

      iniWrite(ini, iniFiles[i])
    }

  res <- bgcExecuteInternal(argv, iniFiles, -1)

  return(list(res, ini))
}

#' bgcExecute
#'
#' Executes a simulation by calling the underlying BiomeBGC C library
#' @param argv (Character Vector) : Arguments for the BiomeBGC library (same as 'bgc' commandline application)
#' @param iniFiles (String vector) : The list of ini files to simulate, one per site.
#' The files are expected to be specified for "go mode"
#' @param simYearsOverride The number of years to simulate (will override simyears from the ini file if specified).
#' @param firstRun this parameter will make the necessary changes in the ini file for the first run.
#' set to FALSE only when calling subsequent simulation steps in single step mode
#'
#' @return a list containing the resulting error code (0 means no error) and the ini file structure
#'
#' @export
bgcExecute <- function(argv = "-a", iniFiles, simYearsOverride = -1, firstRun = TRUE) {

  if (simYearsOverride <= 0) {
    firstRun <- TRUE # should always be TRUE when not in single step mode
  }

  for (i in 1:length(iniFiles)) {
    file <- iniFiles[i]

    ini <- iniRead(file)

    ini <- iniFixPaths(ini)

    ini <- iniMakeSingleStep(ini, firstRun)

    iniFiles[i] <- paste(file, ".go.fixed", sep = "")

    iniWrite(ini, iniFiles[i])
  }

  res <- bgcExecuteInternal(argv, iniFiles, simYearsOverride)

  return(list(res, ini))
}
