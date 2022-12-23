#' Compares two ASCII files
#'
#' @param fileNameA the name of file A
#' @param fileNameB the name of file B
#'
#' @return TRUE if files are identical, false otherwise
#' @export
#'
compareASCIIFiles <- function(fileNameA, fileNameB) {
  conA <- file(fileNameA, open = "rt")
  conB <- file(fileNameB, open = "rt")

  lineA <- readLines(conA, n = 1)
  lineB <- readLines(conB, n = 1)
  if (length(lineA) != length(lineB)) {
    close(conA)
    close(conB)
    return(FALSE)
  }

  i <- 1
  while (length(lineA) > 0 && length(lineB) > 0) {
    if (lineA != lineB) {
      close(conA)
      close(conB)
      print(paste("Found differences at line ", i, sep = ""))
      return(FALSE)
    }

    lineA <- readLines(conA, n = 1)
    lineB <- readLines(conB, n = 1)

    i <- i + 1
  }

  close(conA)
  close(conB)
  return(TRUE)
}

#' Compares a specific line in two ASCII files
#'
#' @param fileNameA the name of file A
#' @param fileNameB the name of file B
#' @param lineIndexA the line number to compare in file A
#' @param lineIndexB the line number to compare in file B
#'
#' @return TRUE if lines are identical, false otherwise
#'
compareASCIILines <- function(fileNameA, fileNameB, lineIndexA, lineIndexB) {
  conA <- file(fileNameA, open = "rt")
  conB <- file(fileNameB, open = "rt")

  readLines(conA, n = lineIndexA - 1)
  readLines(conB, n = lineIndexB - 1)

  lineA <- readLines(conA, n = 1)
  lineB <- readLines(conB, n = 1)

  close(conA)
  close(conB)

  if (lineA != lineB) {
    message("compareASCIILines found differences:\n", lineA, "\n", lineB)
  }

  return(lineA == lineB)
}

createIOdirs <- function(path) {
  sampleInputsDir <- system.file("inputs", package = "BiomeBGCR")
  sampleInputFiles <- list.files(sampleInputsDir, recursive = TRUE)

  vapply(unique(dirname(sampleInputFiles)), function(d) {
    dir.create(file.path(path, "inputs", d), recursive = TRUE, showWarnings = FALSE)
  }, logical(1))
  dir.create(file.path(path, "outputs"), recursive = TRUE, showWarnings = FALSE)
}

#' Converts single floating point binary data into ASCII text data
#'
#' @param inDir character string giving path to input data directory; `NA` means use current directory.
#' @param outDir character string giving path to output data directory; `NA` means use current directory.
#' @param uniqueSuf character string giving unique suffix.
#' @param varNo integer. the number of variables (columns); `NA` means all columns.
#' @param years integer. the number of years of the data (rows); `NA` means all rows.
#' @param startYear integer. the starting year of the data; `NA` means the first row.
#'
#' @return invoked for side effect of converting data files
#'
#' @export
#' @importFrom utils write.table
binToText <- function(inDir, outDir, uniqueSuf, varNo = NA, years = NA, startYear = NA) {
  if (!is.na(inDir)) {
    setwd(inDir)
  }
  if (is.na(uniqueSuf)) {
    binFile <- dir()
  } else {
    binFiles <- grep(uniqueSuf, dir(), value = TRUE)
  }
  if (!is.na(outDir)) {
    setwd(outDir)
  }
  if (is.na(startYear)) {
    startYear <- 1
  }
  entries <- years * varNo

  for (i in 1:length(binFiles)) {
    dataMat <- matrix(nrow = years, ncol = varNo + 1)
    bfile <- file(binFiles[i], "rb")
    outFile <- file(paste(binFiles[i], "txt", sep = "."), "w")
    dt <- readBin(bfile, double(), entries, size = 4)
    for (j in 1:years) {
      vec <- c(startYear - 1 + j, dt[(j * varNo - (varNo - 1)):(j * varNo)])
      dataMat[j, ] <- vec
    }
    write.table(dataMat, outFile, sep = "\t", row.names = FALSE, col.names = FALSE)
    close(outFile)
    close(bfile)
    print(paste("Finished: ", binFiles[i], ".  FileNo ", i, " of ", length(binFiles), sep = ""))
  }
}
