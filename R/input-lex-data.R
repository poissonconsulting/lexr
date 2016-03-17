#' Inputs Lake Exploitation Data
#'
#' Inputs lake exploitation
#' data from an R data package structured like \code{qlexdatr}.
#'
#' @param package A string of the data package.
#' @export
input_lex_data <- function(package = "qlexdatr") {
  check_string(package)

  section <- station <- deployment <- capture <- recapture <- detection <- NULL

  data("section", "station", "deployment", "capture",
       "recapture", "detection",
       package = package, verbose = FALSE, envir = environment())

  data <- list(section = section,
       station = station,
       deployment = deployment,
       capture = capture,
       recapture = recapture,
       detection = detection)

  class(data) <- c("lex_data")
  data
}
