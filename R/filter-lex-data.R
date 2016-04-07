drop_recaptures_after_harvest <- function(recapture) {
  if (all(recapture$Released))
    return(recapture)
  recapture %<>% dplyr::arrange_(~DateTimeRecapture)
  recapture %<>% dplyr::slice(1:min(which(!recapture$Released)))
  recapture$TagsRemoved[nrow(recapture)] <- TRUE
  recapture
}

filter_lex_captures <- function(data, capture) {
  detection <- data$detection
  recapture <- data$recapture

  capture$Capture %<>% droplevels()

  recapture %<>% dplyr::filter_(~Capture %in% capture$Capture)
  detection %<>% dplyr::filter_(~Capture %in% capture$Capture)

  recapture$Capture %<>% factor(levels = levels(capture$Capture))
  detection$Capture %<>% factor(levels = levels(capture$Capture))

  data$capture <- capture
  data$recapture <- recapture
  data$detection <- detection
  data
}

filter_lex_captures_recaptures <- function(data, capture, recapture) {
  detection <- data$detection

  capture$Capture %<>% droplevels()

  recapture %<>% dplyr::filter_(~Capture %in% capture$Capture)
  detection %<>% dplyr::filter_(~Capture %in% capture$Capture)

  recapture$Capture %<>% factor(levels = levels(capture$Capture))
  detection$Capture %<>% factor(levels = levels(capture$Capture))

  recapture %<>% plyr::ddply("Capture", drop_recaptures_after_harvest)

  harvest <- dplyr::filter_(recapture, ~!Released)
  if (nrow(harvest)) { # drop detections after harvest
    detection %<>% dplyr::left_join(dplyr::select_(harvest, ~Capture, ~DateTimeRecapture), by = "Capture")
    detection %<>% dplyr::filter_(~is.na(DateTimeRecapture) | DateTimeDetection < DateTimeRecapture)
    detection$DateTimeRecapture <- NULL
  }

  data$capture <- capture
  data$recapture <- recapture
  data$detection <- detection
  data
}

filter_lex_sections <- function(data, section) {

  assert_that(is.list(section) && !is.null(names(section)))

#  levels(data$capture$SectionCapture) <- section
#  levels(data$capture$SectionCapture) <- section

  station <- data$station
  deployment <- data$deployment
  detection <- data$detection
  capture <- data$capture
  recapture <- data$recapture


#  data$section <- section
  data$recapture <- recapture
  data$capture <- capture
  data$detection <- detection
  data$deployment <- deployment
  data$station <- station
}

#' Filter Lex Data
#'
#' Filters lex data by capture and recapture.
# #'  and combines and drops section.
#' Drops all recaptures and detections after first coded
#' harvest and ensures coded as tags removed.
#'
#' @inheritParams make_detect_data
# #' @param section A named list of sections (musth be adjoining) to combine.
#' @return A lex_data object.
#' @export
filter_lex_data <-  function(
  data, capture = data$capture, recapture = data$recapture,
#  section = stats::setNames(as.list(levels(data$section@data$Section)), levels(data$section@data$Section)),
    start_date = min(lubridate::date(capture$DateTimeCapture)),
  end_date = max(lubridate::date(capture$DateTimeTagExpire))) {

#  assert_that(is.list(section) && !is.null(names(section)))

  check_date(start_date)
  check_date(end_date)

  if (end_date <= start_date) error("start_date must be before end_date")

  capture %<>% check_lex_capture()
  data %<>% check_lex_data()

  capture %<>% dplyr::filter_(~lubridate::date(DateTimeCapture) >= start_date,
                              ~lubridate::date(DateTimeCapture) <= end_date)

  if (!nrow(capture)) error("no captures fall within the specified dates")

  recapture %<>% dplyr::filter_(~lubridate::date(DateTimeRecapture) >= start_date,
                              ~lubridate::date(DateTimeRecapture) <= end_date)

  data %<>% filter_lex_captures_recaptures(capture, recapture)
#  data %<>% filter_lex_sections(section)

  data$recapture %<>% check_lex_recapture()

  data <- data[lex_data_names()]
  class(data) <- "lex_data"
  check_lex_data(data)
}
