expand_deployment_row <- function(x) {
  stopifnot(nrow(x) == 1)
  data <- dplyr::data_frame(
    IntervalDeployment = x$IntervalReceiverIn:x$IntervalReceiverOut)
  x %<>% merge(data)
  x$IntervalReceiverIn <- NULL
  x$IntervalReceiverOut <- NULL
  x
}

expand_deployment <- function(deployment) {
  if (any(deployment$IntervalDateTimeIn < deployment$IntervalDateTimeOut))
    error("receiver deployed before retrieved")
  deployment %<>% plyr::adply(1, expand_deployment_row)
  stopifnot(!anyDuplicated(deployment))
  deployment
}

add_coverage_code <- function(x) {
  if (anyDuplicated(x$Station))
    error("multiple receivers at the same station")
  x %<>% dplyr::arrange_(~Station)
  x$CoverageCode <- paste0(x$Station)
  x
}

calc_coverage_code_interval <- function(y, section) {
  stopifnot(nrow(section@data) == 1)
  cov <- (nrow(y) * pi * 0.5 ^ 2) / section@data$Area[1]
  cov %<>% ifelse(. > 1, 1, .)
  cov
}

calc_coverage_code <- function(x, section) {
  x %<>% dplyr::arrange_(~IntervalDeployment)
  y <- dplyr::filter_(x, ~IntervalDeployment == x$IntervalDeployment[1]) %>% dplyr::as.tbl()
  x$Coverage <- calc_coverage_code_interval(y, section)
  x %<>% dplyr::group_by_(~IntervalDeployment, ~Section) %>%
    dplyr::summarise_(.dots = list(Stations = "n()", Coverage = "first(Coverage)"))
  x
}

calc_coverage <- function(data, section) {
  section <- section[section@data$Section == data$Section[1],]
  data$Station %<>% droplevels() %>% as.integer()
  stopifnot(max(data$Station) < 10)
  data %<>% plyr::ddply("IntervalDeployment", add_coverage_code)
  data %<>% plyr::ddply("CoverageCode", calc_coverage_code, section)
  data$CoverageCode <- NULL
  data
}

make_coverage <- function(data) {
  message("making coverage...")
  deployment <- data$deployment

  deployment %<>% expand_deployment()
  data$section@data$Area <- rgeos::gArea(data$section, byid = TRUE) / 10 ^ 6
  coverage <- dplyr::inner_join(data$station, deployment, by = "Station")
  coverage %<>% dplyr::select_(~IntervalDeployment, ~Section, ~Station, ~EastingStation, ~NorthingStation)

  coverage %<>% plyr::ddply(c("Section"), calc_coverage, section = data$section)
  data$coverage <- dplyr::as.tbl(coverage)
  data
}

filter_captures <- function(data, capture) {
  capture %<>% check_lex_capture()
  capture$Capture %<>% droplevels()
  levels(data$recapture$Capture) <- levels(capture$Capture)
  levels(data$detection$Capture) <- levels(capture$Capture)
  levels(data$depth$Capture) <- levels(capture$Capture)

  data$recapture %<>% dplyr::filter_(~!is.na(Capture))
  data$detection %<>% dplyr::filter_(~!is.na(Capture))
  data$depth %<>% dplyr::filter_(~!is.na(Capture))
  data$capture <- capture
  data
}

set_interval <- function(col, interval) {
  lubridate::minute(col) <- 0
  lubridate::second(col) <- 0

  hourly_interval <- interval$Hour[2] - interval$Hour[1]
  lubridate::hour(col) <- lubridate::hour(col) %/% hourly_interval * hourly_interval
  data <- dplyr::data_frame(DateTime = col)
  data %<>% dplyr::inner_join(interval, by = "DateTime")
  data$Interval
}

set_intervals <- function(data, interval) {
  colnames <- colnames(data)
  colnames <- colnames[grepl("^DateTime.+", colnames)]
  if (length(colnames)) {
    data[colnames] %<>% lapply(set_interval, interval = interval)
    colnames(data) %<>% sub("^(DateTime)(.+)$", "Interval\\2" , .)
  }
  data
}

make_interval <- function(data, start_date, end_date, hourly_interval) {
  message("making interval...")
  check_date(start_date)
  check_date(end_date)
  check_scalar(hourly_interval, c(1L,2L,3L,4L,6L,12L,24L))

  tz <- lubridate::tz(data$capture$DateTimeCapture[1])

  start_date %<>% paste("00:00:00") %>% as.POSIXct(tz = tz)
  end_date %<>% paste("23:59:59") %>% as.POSIXct(tz = tz)

  interval <- dplyr::data_frame(DateTime = seq(start_date, end_date, by = "6 hours"))
  interval %<>% dplyr::mutate_(.dots = list(Interval = ~1:nrow(.),
                                            Date = ~as.Date(DateTime),
                                            Hour = ~lubridate::hour(DateTime)))
  interval %<>% dplyr::select_(~Interval, ~Date, ~Hour, ~DateTime)
  data$interval <- interval
  data %<>% lapply(set_intervals, interval = .$interval)
  data
}

make_capture <- function(data) {
  message("making capture...")
   capture <- data$capture
   capture$Reward1 %<>% factor(levels = c(0, 10, 100))
   capture$Reward2 %<>% factor(levels = c(0, 10, 100))
   capture %<>% dplyr::select_(~Capture, ~IntervalCapture, ~SectionCapture, ~Length,
                               ~Reward1, ~Reward2, ~Species)
   data$capture <- capture
   data
}

make_distance <- function(data) {
  message("making distance...")
  neighbours <- spdep::poly2nb(data$section, row.names = data$section@data$Section)
  adjacency <- matrix(FALSE, nrow = nrow(data$section@data), ncol = nrow(data$section@data))
  for (i in 1:nrow(data$section@data))
    adjacency[i,neighbours[[i]]] <- TRUE

  diag(adjacency) <- TRUE
  stopifnot(isSymmetric(adjacency))
  distance <- spa::uDist(adjacency, nrow(adjacency))
  dim <- dim(distance)
  distance <- as.integer(distance)
  dim(distance) <- dim
  section_names <- levels(data$section@data$Section)
  colnames(distance) <- section_names
  distance %<>% as.data.frame()
  distance$SectionFrom <- section_names
  distance %<>% tidyr::gather_("SectionTo", "Distance", section_names)
  data$distance <- dplyr::as.tbl(distance)
  data
}

sum_detections <- function(data) {
  Detections <- sum(data$Detections)
  Receivers <- length(unique(data$Receiver))
  dplyr::data_frame(Receivers = Receivers, Detections = Detections)
}

make_detection <- function(data) {
  message("making detection...")
  detection <- data$detection
  detection %<>% dplyr::inner_join(data$deployment, by = "Receiver")
  detection %<>% dplyr::filter_(~IntervalDetection >= IntervalReceiverIn,
                                ~IntervalDetection <= IntervalReceiverOut)
  detection %<>% dplyr::inner_join(data$station, by = "Station")
  detection %<>% plyr::ddply(c("IntervalDetection", "Section", "Capture"), sum_detections)
  data$detection <- dplyr::as.tbl(detection)
  data
}

make_section <- function(data) {
  message("making section...")
  data$section <- data$section@data
  data
}
#

#' Make Detect Data
#'
#' Makes detect_data object from a lex_data object.
#' @inheritParams check_lex_data
#' @param capture A data frame of the capture data to use.
#' @param hourly_interval A count indicating the hourly interval.
#'
#' @return A detect_data object.
#' @export
make_detect_data <-  function(
  data, capture = data$capture,
  start_date = min(as.Date(capture$DateTimeCapture)),
  end_date = min(Sys.Date(), max(as.Date(capture$DateTimeTagExpire))),
  hourly_interval = 6L) {

  data %<>% check_lex_data()
  data %<>% filter_captures(capture)
  data %<>% make_interval(start_date = start_date, end_date = end_date,
                          hourly_interval = hourly_interval)
  data %<>% make_detection()
  data %<>% make_coverage()
  data %<>% make_capture()
  data %<>% make_distance()
  data %<>% make_section()
  data <- data[detect_data_names()]
  class(data) <- "detect_data"
  return(data)
}
