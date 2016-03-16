#' No Spawning
#'
#' Function that simply returns FALSE for every time period.
#'
#' detection has the columns Capture, Species, Section, DateTime and Period.
#' Any replacement function has to use this information to return a logical
#' vector indicating whether the individual was spawing in each period.
#'
#' @param detection A data.frame of the detection data for the capture.
#' @export
spawning_no <- function(detection) {
  check_data3(detection, values = list(
    Capture = factor(1),
    Species = factor(1),
    DateTime = lubridate::now(),
    Section = factor(1),
    Period = factor(1)
  ))
  return(rep(FALSE, nlevels(detection$Period)))
}
