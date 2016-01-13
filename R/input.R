#' Inputs Lake Exploitation Data
#'
#' Inputs lake exploitation
#' data from an R data package structured like \code{qlexdatr}.
#'
#' @param package A string of the data package.
#' @export
input_lex_data <- function(package = "qlexdatr") {
  if (!assertthat::is.string(package)) error("package must be a string")

  section <- station <- receiver <- deployment <- recapture <- detection <- depth <- capture <- NULL

  data("section", "station", "receiver", "deployment",
       "recapture", "detection", "depth", "capture",
       package = package, verbose = FALSE, envir = environment())

  data <- list(section = section,
       station = station,
       receiver = receiver,
       deployment = deployment,
       recapture = recapture,
       detection = detection,
       depth = depth,
       capture = capture)

  class(data) <- c("lex_data")
  check_data(data)
  data
}
