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
  x %<>% dplyr::arrange_(~Station)
  x$CoverageCode <- paste0(x$Station, collapse = "")
  x
}

calc_coverage_code_interval <- function(y, section) {
  . <- NULL
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

check_consecutive_dups <- function(duplicates) {
  intervals <- duplicates$IntervalDeployment
  intervals %<>% sort() %>% diff()
  if (any(intervals == 1))
    error("multiple receivers at the same station")
  NULL
}

calc_coverage <- function(data, section) {
  section <- section[section@data$Section == data$Section[1],]
  data$Station %<>% droplevels() %>% as.integer()
  stopifnot(max(data$Station) < 10)

  if (anyDuplicated(data[c("IntervalDeployment", "Station")])) {
    duplicates <- data[c("IntervalDeployment", "Station")]
    data <- data[!duplicated(duplicates),,drop = FALSE]
    duplicates <- duplicates[duplicated(duplicates),,drop = FALSE]
    plyr::ddply(duplicates, "Station", check_consecutive_dups)
  }

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
  coverage %<>% dplyr::select_(~IntervalDeployment, ~Section, ~Station,
                               ~EastingStation, ~NorthingStation)

  coverage %<>% plyr::ddply(c("Section"), calc_coverage, section = data$section)
  coverage %<>% dplyr::rename_(.dots = list(Interval = "IntervalDeployment"))
  data$coverage <- dplyr::as.tbl(coverage)
  data
}

filter_captures <- function(data, capture) {
  capture %<>% check_lex_capture()
  capture$Capture %<>% droplevels()

  data$recapture$Capture %<>% as.character()
  data$detection$Capture %<>% as.character()
  data$depth$Capture %<>% as.character()

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
  if(!length(col))
    return(integer(0))
  lubridate::minute(col) <- 0
  lubridate::second(col) <- 0

  hourly_interval <- difftime(interval$DateTime[2], interval$DateTime[1], units = "hours") %>%
    as.integer()
  lubridate::hour(col) <- lubridate::hour(col) %/% hourly_interval * hourly_interval
  data <- dplyr::data_frame(DateTime = col)
  data %<>% dplyr::left_join(interval, by = "DateTime")
  data$Interval
}

adjust_datetime_section <- function(section, start_date, end_date) {
  section
}

adjust_datetime_station <- function(station, start_date, end_date) {
  station
}

adjust_datetime_deployment <- function(deployment, start_date, end_date) {
  deployment %<>% dplyr::filter_(~DateTimeReceiverIn <= end_date)
  deployment %<>% dplyr::filter_(~DateTimeReceiverOut >= start_date)
  deployment$DateTimeReceiverIn[deployment$DateTimeReceiverIn < start_date] <- start_date
  deployment$DateTimeReceiverOut[deployment$DateTimeReceiverOut > end_date] <- end_date
  deployment
}

adjust_datetime_capture <- function (capture, start_date, end_date) {
  capture %<>% dplyr::filter_(~DateTimeCapture >= start_date)
  capture %<>% dplyr::filter_(~DateTimeCapture <= end_date)
  capture$DateTimeTagExpire[capture$DateTimeTagExpire > end_date] <- end_date
  capture
}

adjust_datetime_recapture <- function (recapture, start_date, end_date) {
  recapture %<>% dplyr::filter_(~DateTimeRecapture >= start_date)
  recapture %<>% dplyr::filter_(~DateTimeRecapture <= end_date)
  recapture
}

adjust_datetime_detection <- function(detection, start_date, end_date) {
  detection %<>% dplyr::filter_(~DateTimeDetection >= start_date)
  detection %<>% dplyr::filter_(~DateTimeDetection <= end_date)
  detection
}

adjust_datetime_depth <- function(depth, start_date, end_date) {
  depth %<>% dplyr::filter_(~DateTimeDepth >= start_date)
  depth %<>% dplyr::filter_(~DateTimeDepth <= end_date)
  depth
}

set_intervals <- function(data, interval) {
  . <- NULL
  colnames <- colnames(data)
  colnames <- colnames[grepl("^DateTime.+", colnames)]
  if (length(colnames)) {
    data[colnames] %<>% lapply(set_interval, interval = interval)
    data[colnames] %<>% na.omit() # delete rows with missing intervals
    colnames(data) %<>% sub("^(DateTime)(.+)$", "Interval\\2" , .)
  }
  data
}

make_interval <- function(data, start_date, end_date, hourly_interval) {
  message("making interval...")
  . <- NULL

  tz <- lubridate::tz(data$capture$DateTimeCapture)

  start_date %<>% paste("00:00:00") %>% as.POSIXct(tz = tz)
  end_date %<>% paste("23:59:59") %>% as.POSIXct(tz = tz)

  data %<>% purrr::lmap(fun_data_name, fun = "adjust_datetime", start_date, end_date)

  interval <- dplyr::data_frame(DateTime = seq(start_date, end_date, by = paste(hourly_interval,"hours")))
  interval %<>% dplyr::mutate_(.dots = list(Interval = ~1:nrow(.),
                                            Date = ~date(DateTime),
                                            DayteTime = ~DateTime,
                                            Year = ~as.integer(lubridate::year(DateTime)),
                                            Month = ~as.integer(lubridate::month(DateTime)),
                                            Hour = ~lubridate::hour(DateTime)))
  lubridate::year(interval$DayteTime) <- 2000
  interval %<>% dplyr::select_(~Interval, ~Date, ~Year, ~Month, ~Hour, ~DayteTime, ~DateTime)
  data$interval <- interval
  data %<>% lapply(set_intervals, interval = .$interval)
  data
}

make_capture <- function(data) {
  message("making capture...")
  capture <- data$capture
  capture$Reward1 %<>% factor(levels = c(0, 10, 100))
  capture$Reward2 %<>% factor(levels = c(0, 10, 100))
  levels(capture$Reward1) <- list("Low" = c("0", "10"), "High" = "100")
  levels(capture$Reward2) <- list("Low" = c("0", "10"), "High" = "100")
  capture %<>% dplyr::select_(~Capture, ~IntervalCapture, ~SectionCapture,
                              ~IntervalTagExpire, ~Length,
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
  section_names <- as.character(data$section@data$Section)
  colnames(distance) <- section_names
  distance %<>% as.data.frame()
  distance$SectionFrom <- factor(section_names, levels = levels(data$section@data$Section))
  distance %<>% tidyr::gather_("SectionTo", "Distance", section_names)
  distance$Distance %<>% as.integer()
  distance$SectionTo %<>% factor(levels = levels(data$section@data$Section))
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

#' Make Detect Data
#'
#' Makes detect_data object from a lex_data object.
#' @inheritParams check_lex_data
#' @param capture A data frame of the capture data to use.
#' @param start_date A date of the start.
#' @param end_date A date of the end.
#' @param hourly_interval A count indicating the hourly interval.
#'
#' @return A detect_data object.
#' @export
make_detect_data <-  function(
  data, capture = data$capture,
  start_date = min(lexr::date(capture$DateTimeCapture)),
  end_date = min(as.Date("2015-12-31"), max(lexr::date(capture$DateTimeTagExpire))),
  hourly_interval = 6L) {

  check_data2(capture)
  check_date(start_date)
  check_date(end_date)
  check_scalar(hourly_interval, c(1L,2L,3L,4L,6L,12L,24L))

  if (end_date <= start_date) error("start_date must be before end_date")

  data %<>% check_lex_data()
  capture %<>% dplyr::filter_(~date(capture$DateTimeCapture) >= start_date,
                              ~date(capture$DateTimeCapture) <= end_date)

  if (!nrow(capture)) error("no captures fall within the specified dates")

  data %<>% filter_captures(capture)
  data %<>% make_interval(start_date = start_date, end_date = end_date,
                          hourly_interval = hourly_interval)
  data %<>% make_coverage()
  data %<>% make_capture()
  data %<>% make_detection()
  data %<>% make_distance()
  data %<>% make_section()
  data <- data[detect_data_names()]
  class(data) <- "detect_data"
  return(data)
}
