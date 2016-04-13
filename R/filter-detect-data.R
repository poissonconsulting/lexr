filter_detect_captures <- function(data, capture) {
  detection <- data$detection
  recapture <- data$recapture

  capture$Capture %<>% droplevels()
  capture$Species %<>% droplevels()

  recapture %<>% dplyr::filter_(~Capture %in% capture$Capture)
  detection %<>% dplyr::filter_(~Capture %in% capture$Capture)

  recapture$Capture %<>% factor(levels = levels(capture$Capture))
  detection$Capture %<>% factor(levels = levels(capture$Capture))

  data$capture <- capture
  data$recapture <- recapture
  data$detection <- detection
  data
}

#' Filter Detect Data
#'
#' @inheritParams make_detect_data
#' @return A lex_data object.
#' @export
filter_detect_data <-  function(data, capture = data$capture) {
  capture %<>% check_detect_capture()
  data %<>% check_detect_data()

  data %<>% filter_detect_captures(capture)

  data <- data[detect_data_names()]
  class(data) <- "detect_data"
  check_detect_data(data)
}
