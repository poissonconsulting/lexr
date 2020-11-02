#' Inputs Lake Exploitation Data
#'
#' Inputs lake exploitation
#' data from an R data package structured like \code{klexdatr}.
#'
#' @param package A string of the data package.
#' @export
input_lex_data <- function(package = "klexdatr") {
  chk_string(package)

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
