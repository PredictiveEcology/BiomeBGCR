#' Hello
#'
#' Print the message `"hello"` and return as a string.
#'
#' @return character string
#' @export
#'
#' @examples
#' hello()
hello <- function() {
  str <- "hello"
  message(str)

  return(str)
}
