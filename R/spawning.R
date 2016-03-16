#' No Spawning
#'
#' Function that simply returns FALSE for every time period.
#'
#' detection has the columns Section, DateTime and Interval while
#' period has the columns Period and Interval for all period.
#' Any replacement function has to use this information to return a logical
#' vector indicating whether the individual was spawing in each period.
#'
#' @param detection A data.frame of the detection data for the capture.
#' @param years A data.frame of the period data.
#' @export
spawning_no <- function(detection, capture, period) {
  check_detect_detection(detection)
  check_detect_capture(capture)
  check_analysis_period(period)

  return(rep(FALSE, nrow(period)))
}
