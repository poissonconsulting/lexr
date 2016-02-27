#' Get Difftime
#'
#' @param object The object to get the difftime for.
#' @return A difftime object
#' @export
get_difftime <- function(object) {
  UseMethod("get_difftime", object)
}

get_difftime.POSIXct <- function(object) {
  datetimes <- unique(object)
  datetimes %<>% sort()
  n <- length(datetimes)
  difftimes <- difftime(datetimes[2:n], datetimes[1:(n-1)])
  min(difftimes, na.rm = TRUE)
}

#' @export
get_difftime.lex_data <- function(object) {
  datetimes <- unique(object$detection$DateTimeDetection)
  datetimes %<>% sort()
  n <- length(datetimes)
  difftimes <- difftime(datetimes[2:n], datetimes[1:(n-1)])
  min(difftimes, na.rm = TRUE)
}

#' @export
get_difftime.detect_data <- function(object) {
  difftime(object$interval$DateTime[2], object$interval$DateTime[1])
}

#' @export
get_difftime.analysis_data <- function(object) {
  difftime(object$period$DateTime[2], object$period$DateTime[1])
}

