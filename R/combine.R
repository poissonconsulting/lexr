#' List Lake Exploitation Data
#'
#' @param package A string of the data package.
list_lex_data <- function(package) {
  if (!assertthat::is.string(package)) check_stop("package must be a string")

  section <- NULL
  station <- NULL
  receiver <- NULL
  deployment <- NULL
  recapture <- NULL
  detection <- NULL
  depth <- NULL
  capture <- NULL

  data("section", "station", "receiver", "deployment",
       "recapture", "detection", "depth", "capture",
       package = package, verbose = FALSE, envir = environment())

  list(section = list(name = "section", data = section),
       station = list(name = "station", data = station),
       receiver = list(name = "receiver", data = receiver),
       deployment = list(name = "deployment", data = deployment),
       recapture = list(name = "recapture", data = recapture),
       detection = list(name = "detection", data = detection),
       depth = list(name = "depth", data = depth),
       capture = list(name = "capture", data = capture))
}
