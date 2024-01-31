#' iniRead
#'
#' Reads the BGC-Biome ini file
#' @param fileName the name of the file to open
#'
#' @return a list of data frames containing one dataframe per section
#'
#' @export
iniRead <- function(fileName) {
  con <- file(fileName, open = "r")

  sections <- vector(mode = "list", length = 15)
  names(sections) <- c(
    "MET_INPUT", "RESTART", "TIME_DEFINE", "CLIM_CHANGE", "CO2_CONTROL", "SITE", "RAMP_NDEP", "EPC_FILE", "W_STATE",
    "C_STATE", "N_STATE", "OUTPUT_CONTROL", "DAILY_OUTPUT", "ANNUAL_OUTPUT", "END_INIT"
  )

  # read the title and description and set as attributes
  line <- readLines(con, n = 1)
  lineSplit <- strsplit(line, ":")
  attr(sections, "title") <- lineSplit[[1]][1]
  attr(sections, "decription") <- lineSplit[[1]][2]

  sections[["MET_INPUT"]] <- iniReadMetInput(con)
  sections[["RESTART"]] <- iniReadRestart(con)
  sections[["TIME_DEFINE"]] <- iniReadTimeDefine(con)
  sections[["CLIM_CHANGE"]] <- iniReadClimChange(con)
  sections[["CO2_CONTROL"]] <- iniReadCO2Control(con)
  sections[["SITE"]] <- iniReadSite(con)
  sections[["RAMP_NDEP"]] <- iniReadRampNdep(con)
  sections[["EPC_FILE"]] <- iniReadEPCFile(con)
  sections[["W_STATE"]] <- iniReadWState(con)
  sections[["C_STATE"]] <- iniReadCState(con)
  sections[["N_STATE"]] <- iniReadNState(con)
  sections[["OUTPUT_CONTROL"]] <- iniReadOutputControl(con)
  sections[["DAILY_OUTPUT"]] <- iniReadOutput(con, "DAILY_OUTPUT")
  sections[["ANNUAL_OUTPUT"]] <- iniReadOutput(con, "ANNUAL_OUTPUT")
  sections[["END_INIT"]] <- iniReadEndInit(con)

  close(con)

  return(sections)
}

#' Get accessor for the BiomeBGC ini data structure
#'
#' @param ini the ini data structure returned by iniRead()
#' @param section the section tag (MET_INPUT, RESTART, etc..)
#' @param index the index (or indices) of the parameters.
#'
#' @return the value of the field (character vector)
#' @seealso iniRead
#'
#' @export
iniGet <- function(ini, section, index) {
  return(ini[[section]][["value"]][index + 1])
}

#' Set accessor for the BiomeBGC ini data structure
#'
#' @param ini the ini data structure returned by iniRead()
#' @param section the section tag (MET_INPUT, RESTART, etc..)
#' @param index the index (or indices) of the parameters.
#' @param value the value (character vector) to set the value to
#'
#' @return the updated ini data
#' @seealso iniRead
#'
#' @export
iniSet <- function(ini, section, index, value) {
  ini[[section]][["value"]][index + 1] <- value
  return(ini)
}

#' Writes the ini data to an INI file
#'
#' @param ini the ini data structure returned by iniRead()
#' @param fileName (character vector) the name of the file to write to
#' @param title (character vector) the title of the file (will be written in the first line)
#' @param description (character vector) the description of the file (will be written in the first line)
#' @seealso iniRead
#'
#' @export
iniWrite <- function(ini, fileName, title = "", description = "") {
  con <- file(fileName, open = "wt")

  if (nchar(title) == 0) {
    title <- attr(ini, "title")
  }

  if (nchar(description) == 0) {
    title <- attr(ini, "description")
  }

  writeLines(paste(title, description, sep = " : "), con)

  for (section in ini) {
    writeLines("", con)
    for (i in 1:nrow(section)) {
      str <- sprintf("%-15s %-10s %s", section[i, "value"], section[i, "unit"], section[i, "comment"])
      writeLines(str, con)
    }
  }

  close(con)
}

#' Fix all the relative paths inside the ini file
#'
#' C++ lib requires absolute paths.
#'
#' @param ini the ini data structure returned by `iniRead()`
#' @param path (Character String) : Path to base directory to use for simulations.
#'
#' @export
iniFixPaths <- function(path, ini) {
  # meteorology input filename
  section <- "MET_INPUT"
  index <- 1
  filename <- iniGet(ini, section, index)
  newFilename <- file.path(path, filename)
  ini <- iniSet(ini, section, index, newFilename)

  # input restart filename
  section <- "RESTART"
  index <- 5
  filename <- iniGet(ini, section, index)
  newFilename <- file.path(path, filename)
  ini <- iniSet(ini, section, index, newFilename)

  # output restart filename
  section <- "RESTART"
  index <- 6
  filename <- iniGet(ini, section, index)
  newFilename <- file.path(path, filename)
  ini <- iniSet(ini, section, index, newFilename)

  # annual variable CO2 filename
  section <- "CO2_CONTROL"
  index <- 3
  if (iniGet(ini, section, 1) == "1") { # filename must be corrected only if in file variation mode
    filename <- iniGet(ini, section, index)
    newFilename <- file.path(path, filename)
    ini <- iniSet(ini, section, index, newFilename)
  }

  # evergreen needleleaf forest ecophysiological constants
  section <- "EPC_FILE"
  index <- 1
  filename <- iniGet(ini, section, index)
  newFilename <- file.path(path, filename)
  ini <- iniSet(ini, section, index, newFilename)

  # prefix for output files
  section <- "OUTPUT_CONTROL"
  index <- 1
  filename <- iniGet(ini, section, index)
  newFilename <- file.path(path, filename)
  ini <- iniSet(ini, section, index, newFilename)

  return(ini)
}

#' iniMakeSpinup
#'
#' Changes the ini file to that it will execute as a spinup simulation
#'
#' @param ini the ini data structure returned by iniRead()
#'
#' @export
iniMakeSpinup <- function(ini) {
  ini <- iniSet(ini, "RESTART", 2, "1") # write restart file
  ini <- iniSet(ini, "TIME_DEFINE", 4, "1") # spinup simulation

  ini <- iniSet(ini, "OUTPUT_CONTROL", 2, "0") # no daily output
  ini <- iniSet(ini, "OUTPUT_CONTROL", 3, "0") # no monthly avg
  ini <- iniSet(ini, "OUTPUT_CONTROL", 4, "0") # no annual avg
  ini <- iniSet(ini, "OUTPUT_CONTROL", 5, "0") # no annual output

  ini[["DAILY_OUTPUT"]] <- ini[["DAILY_OUTPUT"]][c(1:2), ]
  ini <- iniSet(ini, "DAILY_OUTPUT", 1, "0") # no daily output

  ini[["ANNUAL_OUTPUT"]] <- ini[["ANNUAL_OUTPUT"]][c(1:2), ]
  ini <- iniSet(ini, "ANNUAL_OUTPUT", 1, "0") # no annual output

  return(ini)
}

#' iniMakeSingleStep
#'
#' Changes the ini file to enable single step mode
#'
#' @param ini the ini data structure returned by iniRead()
#' @param firstRun set to TRUE if this ini file is the first one to be executed after spinup, FALSE otherwise
#'
#' @export
iniMakeSingleStep <- function(ini, firstRun) {
  readRestartFile <- "1"
  value <- "1"
  if (firstRun) {
    value <- "0"
    readRestartFile <- iniGet(ini, "RESTART", 1)
  }

  ini <- iniSet(ini, "RESTART", 1, readRestartFile) # read restart file
  ini <- iniSet(ini, "RESTART", 2, "1") # write restart file
  ini <- iniSet(ini, "RESTART", 3, value) # use restart metyear
  ini <- iniSet(ini, "RESTART", 4, value) # use restart simyear

  return(ini)
}

#' iniParseSectionLine
#'
#' This function parses a section line (string) and returns a data frame for this line
#' @param line the line to parse into
#' @param lookForSecondField specifies if this line should contain 3 (TRUE) or 2 (FALSE) columns
#'
#' @return a data frame containing three string fields (value, unit, comment)
#'
iniParseSectionLine <- function(line, lookForSecondField = TRUE) {
  temp <- unlist(strsplit(line, " "))
  len <- length(temp)

  value <- ""
  index <- 1
  while (index <= len) {
    if (nchar(temp[index]) > 0) {
      value <- temp[index]
      break
    }
    index <- index + 1
  }

  unit <- ""
  if (lookForSecondField) {
    index <- index + 1
    while (index <= len) {
      if (nchar(temp[index]) > 0) {
        unit <- temp[index]
        break
      }
      index <- index + 1
    }
  }

  index <- index + 1
  comment <- ""
  while (index <= len) {
    if (nchar(temp[index]) > 0) {
      comment <- paste(temp[index:len], sep = " ", collapse = " ")
      break
    }
    index <- index + 1
  }

  return(data.frame("value" = value, "unit" = unit, "comment" = comment))
}

#' iniFindSection
#'
#' This function scans lines until it finds the specified section tag
#' @param con the connection to the file to scan from
#' @param tag The name of the section to look for into the ini file
#'
#' @return the line of the ini file starting with the specified tag
#'
iniFindSection <- function(con, tag) {
  # Read section line
  line <- readLines(con, n = 1)

  while (!startsWith(line, tag)) {
    line <- readLines(con, n = 1)
    if (length(line) == 0) {
      return(list())
    }
  }

  return(line)
}

#' iniReadMetInput
#'
#' This function reads the MET_INPUT ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadMetInput <- function(con) {
  line <- iniFindSection(con, "MET_INPUT")

  section <- iniParseSectionLine(line)

  # meteorology input filename
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1), lookForSecondField = FALSE))

  # header lines in met file
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  return(section)
}

#' iniReadRestart
#'
#' This function reads the RESTART ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadRestart <- function(con) {
  line <- iniFindSection(con, "RESTART")

  section <- iniParseSectionLine(line)

  # read restart file
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # write restart file
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # use restart metyear
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # use restart simyear
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # input restart filename
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1), lookForSecondField = FALSE))

  # output restart filename
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1), lookForSecondField = FALSE))

  return(section)
}

#' iniReadTimeDefine
#'
#' This function reads the TIME_DEFINE ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadTimeDefine <- function(con) {
  line <- iniFindSection(con, "TIME_DEFINE")

  section <- iniParseSectionLine(line)

  # number of meteorological data years
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # number of simulation years
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # first simulation year
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # 1 = spinup simulation    0 = normal simulation
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # maximum number of spinup years (if spinup simulation)
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  return(section)
}

#' iniReadClimChange
#'
#' This function reads the CLIM_CHANGE ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadClimChange <- function(con) {
  line <- iniFindSection(con, "CLIM_CHANGE")

  section <- iniParseSectionLine(line)

  # offset for Tmax
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # offset for Tmin
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # multiplier for Prcp
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # multiplier for VPD
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # multiplier for shortwave radiation
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  return(section)
}

#' iniReadCO2Control
#'
#' This function reads the CO2_CONTROL ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadCO2Control <- function(con) {
  line <- iniFindSection(con, "CO2_CONTROL")

  section <- iniParseSectionLine(line)

  # 0=constant 1=vary with file 2=constant, file for Ndep
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # constant atmospheric CO2 concentration
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # annual variable CO2 filename
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  return(section)
}

#' iniReadSite
#'
#' This function reads the SITE ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadSite <- function(con) {
  line <- iniFindSection(con, "SITE")

  section <- iniParseSectionLine(line)

  # effective soil depth (corrected for rock fraction)
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # sand percentage by volume in rock-free soil
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # silt percentage by volume in rock-free soil
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # clay percentage by volume in rock-free soil
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # site elevation
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1), TRUE))

  # site latitude (- for S.Hem.)
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1), TRUE))

  # site shortwave albedo
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1), TRUE))

  # wet+dry atmospheric deposition of N
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1), TRUE))

  # symbiotic+asymbiotic fixation of N
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1), TRUE))

  return(section)
}

#' iniReadRampNdep
#'
#' This function reads the RAMP_NDEP ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadRampNdep <- function(con) {
  line <- iniFindSection(con, "RAMP_NDEP")

  section <- iniParseSectionLine(line)

  # do a ramped N-deposition run? 0=no, 1=yes
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # reference year for industrial N deposition
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # industrial N deposition value
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  return(section)
}

#' iniReadEPCFile
#'
#' This function reads the EPC_FILE ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadEPCFile <- function(con) {
  line <- iniFindSection(con, "EPC_FILE")

  section <- iniParseSectionLine(line)

  # evergreen needleleaf forest ecophysiological constants
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  return(section)
}

#' iniReadWState
#'
#' This function reads the W_STATE ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadWState <- function(con) {
  line <- iniFindSection(con, "W_STATE")

  section <- iniParseSectionLine(line)

  # water stored in snowpack
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # initial soil water as a proportion of saturation
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  return(section)
}

#' iniReadCState
#'
#' This function reads the C_STATE ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadCState <- function(con) {
  line <- iniFindSection(con, "C_STATE")

  section <- iniParseSectionLine(line)

  # first-year maximum leaf carbon
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # first-year maximum stem carbon
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # coarse woody debris carbon
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # litter carbon, labile pool
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # litter carbon, unshielded cellulose pool
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # litter carbon, shielded cellulose pool
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # litter carbon, lignin pool
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # soil carbon, fast microbial recycling pool
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # soil carbon, medium microbial recycling pool
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # soil carbon, slow microbial recycling pool
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # soil carbon, recalcitrant SOM (slowest)
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  return(section)
}

#' iniReadNState
#'
#' This function reads the N_STATE ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadNState <- function(con) {
  line <- iniFindSection(con, "N_STATE")

  section <- iniParseSectionLine(line)

  # litter nitrogen, labile pool
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # soil nitrogen, mineral pool
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  return(section)
}

#' iniReadOutputControl
#'
#' This function reads the OUTPUT_CONTROL ini section
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadOutputControl <- function(con) {
  line <- iniFindSection(con, "OUTPUT_CONTROL")

  section <- iniParseSectionLine(line)

  # prefix for output files
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # 1 = write daily output   0 = no daily output
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # 1 = monthly avg of daily variables  0 = no monthly avg
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # 1 = annual avg of daily variables   0 = no annual avg
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # 1 = write annual output  0 = no annual output
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  # for on-screen progress indicator
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  return(section)
}

#' iniReadOutput
#'
#' This function can be used to read either DAILY_OUTPUT or ANNUAL_OUTPUT sections
#' @param con the connection to the file to scan from
#' @param section the tag of the section to read (DAILY_OUTPUT or ANNUAL_OUTPUT)
#'
#' @return the data frame containing the section data
#'
iniReadOutput <- function(con, section) {
  line <- iniFindSection(con, section)

  section <- iniParseSectionLine(line)

  # number of variables to output
  section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))

  nbOutputs <- strtoi(section[2, "value"])
  if (nbOutputs > 0) {
    for (i in 1:nbOutputs) {
      section <- rbind(section, iniParseSectionLine(readLines(con, n = 1)))
    }
  }

  return(section)
}

#' iniReadEndInit
#'
#' This function reads the END_INIT section (EOF)
#' @param con the connection to the file to scan from
#'
#' @return the data frame containing the section data
#'
iniReadEndInit <- function(con) {
  line <- iniFindSection(con, "END_INIT")

  section <- iniParseSectionLine(line)

  return(section)
}
