#' Is Lex Data
#'
#' @param x The object to test
#'
#' @return A flag indicating whether x is lex data.
#' @export
is.lex_data <- function(x) {
  inherits(x, "lex_data")
}

#' Is Detect Data
#'
#' @param x The object to test
#'
#' @return A flag indicating whether x is detect data.
#' @export
is.detect_data <- function(x) {
  inherits(x, "detect_data")
}

#' Is Analysis Data
#'
#' @param x The object to test
#'
#' @return A flag indicating whether x is analysis data.
#' @export
is.analysis_data <- function(x) {
  inherits(x, "analysis_data")
}


