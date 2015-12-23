check_stop <- function(...) stop(..., call. = FALSE)

#' Check Lake Exploitation Data
#'
#' Checks lake exploitation data and returns a TRUE if passes all the tests.
#' Otherwise stops with an informative error.
#'
#' @inheritParams load_lex_data
#' @return A flag indicating whether the package data passes the checks.
#' @export
check_lex_data <- function(package) {
  load_lex_data(package)

  if (!dplyr::is.tbl(section)) check_stop("section is not a tbl data frame")
  if (!is.matrix(section_distance))
    check_stop("section_distance is not a matrix")

  TRUE
}
