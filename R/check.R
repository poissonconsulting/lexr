plural <- function(x, n = 1, end = "") paste0(x, ifelse(n > 1, "s", ""), end)

punctuate_strings <- function(x, qualifier = "or") {
  if (length(x) == 1)
    return(x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

check_stop <- function(...) stop(..., call. = FALSE)

check_section <- function(section) {

  if (!is.data.frame(section)) check_stop("section is not a data frame")
  if (!nrow(section)) check_stop("section must contain at least one row of data")

  columns <- list("Section" = "integer", "SectionArea" = "double",
               "SectionX" = "double", "SectionY" = "double")

  if (!all(names(columns) %in% colnames(section)))
    check_stop("section must include ", plural("column", length(columns)),
               " ", punctuate_strings(names(columns), "and"))

  if (!identical(sapply(section[,names(columns)], storage.mode), unlist(columns)))
    check_stop("section ", plural("column", length(columns)),
               " ", punctuate_strings(names(columns), "and"),
               " must have ", plural("mode", length(columns)),
               " ", punctuate_strings(unlist(columns), "and"),
               ", respectively")

  if (!noNA(section[,names(columns)]))
    check_stop("section cannot include missing values in ",
               punctuate_strings(names(columns), "or"))

  if (any(section$SectionArea <= 0))
    check_stop("section areas must be positive numbers")

  TRUE
}

check_section_polygons <- function(section_polygons) {

  if (class(section_polygons) != "SpatialPolygonsDataFrame")
    check_stop("section_polygons is not a spatial polygons data frame")

  TRUE
}

check_capture <- function(capture) {

  if (!is.data.frame(capture)) check_stop("capture is not a data frame")

  TRUE
}

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

  check_section(section)
  check_section_polygons(section_polygons)
  if (!is.matrix(section_distance)) check_stop("section_distance is not a matrix")
  if (!is.data.frame(station)) check_stop("station is not a data frame")
  if (!is.data.frame(receiver)) check_stop("receiver is not a data frame")
  if (!is.data.frame(station_deployment))
    check_stop("station_deployment is not a data frame")
  if (!is.data.frame(recapture)) check_stop("recapture is not a data frame")
  if (!is.data.frame(detection)) check_stop("detection is not a data frame")
  if (!is.data.frame(depth)) check_stop("depth is not a data frame")
  check_capture(capture)

  TRUE
}
