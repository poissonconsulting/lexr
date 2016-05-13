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

filter_detect_alive_only_capture <- function (x) {
  x %<>% dplyr::arrange_(~IntervalDetection)
  x %<>% dplyr::mutate_(.dots = list(Move = ~as.integer(Section),
                                     Move = ~c(diff(Move), 0),
                                     Move = ~Move != 0))
  last_move <- which(x$Move)
  x$Move <- NULL
  if (!length(last_move))
    return(dplyr::slice_(x, ~0))
  dplyr::slice_(x, ~1:last_move[length(last_move)])
}

filter_detect_alive_only <- function (data) {
  detection <- data$detection

  detection %<>% plyr::ddply("Capture", filter_detect_alive_only_capture)

  data$detection <- detection
  data
}

#' Filter Detect Data
#'
#' @param data The detect_data object.
#' @param capture A data frame of the capture data to use.
#' @param section A data frame of the section data to use.
#' @param alive_only A flag indicating wether to discard detections after last movement.
#' @return A lex_data object.
#' @export
filter_detect_data <-  function(data, capture = data$capture, section = data$section, alive_only = FALSE) {
  check_flag(alive_only)

  capture %<>% check_detect_capture()
  data %<>% check_detect_data()

  data %<>% filter_detect_captures(capture)
  if(alive_only)
    data %<>% filter_detect_alive_only()
  data %<>% filter_detect_captures_section(capture = data$capture, section)

  data <- data[detect_data_names()]
  class(data) <- "detect_data"
  check_detect_data(data)
}
