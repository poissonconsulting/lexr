#' No Spawning
#'
#' A function that simply returns FALSE for every time period.
#'
#' To identify spawning events when making analysis data pass a custom
#' function in place of spawning_no. The function has to take the same
#' arguments and return a logical vector even if there are no detections
#' for an individual. It should do this by returning FALSE for all
#' periods outside the spawning window and NA for all periods inside
#' the spawning window if no information is available.
#'
#' To see the columns and types in detection and period view
#' the function definition for \code{spawning_no}.
#'
#' @param detection A data.frame of the detection data for the capture.
#' @param period A data.frame of the periods.
#' @export
spawning_no <- function(detection, period) {

  check_data3(detection, values = list(
    Capture = factor(1),
    Species = factor(1),
    Date = lubridate::today(),
    Section = factor(1),
    Period = factor(1)), min_row = 0)

  check_data3(period, list(Period = factor(1),
                           Date = lubridate::today(),
                           Days = c(1, 366)),
              key = c("Period"))

  stopifnot(length(unique(detection$Capture)) < 2)

  return(rep(FALSE, nrow(period)))
}
