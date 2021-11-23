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
