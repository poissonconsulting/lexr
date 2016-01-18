



#   deployment %<>% dplyr::arrange_(~ReceiverDateTimeIn)
#   deployment %<>% dplyr::mutate_(.dots = list(
#     "Duration" = ~as.integer(difftime(ReceiverDateTimeOut, ReceiverDateTimeIn, units = "secs"))))
#   if (any(deployment$Duration <= 0)) {
#     deployment %<>% dplyr::filter_(~Duration <= 0)
#     error("receiver retrieved before deployed\n", capture_output(deployment))
#   }
#   deployment_diff <- function (x) {
#     x %<>% dplyr::arrange_(~ReceiverDateTimeIn)
#     x$Difference <- c(diff(as.integer(x$ReceiverDateTimeIn)), NA)
#     x
#   }
#   deployment %<>% plyr::ddply("Station", deployment_diff)
#   overlap <- !is.na(deployment$Difference) & deployment$Difference < deployment$Duration
#   if (FALSE) { #(any(overlap)) {
#     overlap <- which(overlap)
#     overlap <- sort(unique(c(overlap, overlap + 1)))
#     deployment %<>% dplyr::slice_(~overlap)
#     error("multiple receivers at the same station\n", capture_output(deployment))
#   }


make_coverage <- function(section, section_polygons, station, station_deployment,
                               hourly_interval = 6) {

  station_deployment %<>% dplyr::full_join(station, by = "Station")
  if (any(is.na(station_deployment$Receiver)))
    check_stop("st")


  coverage <- dplyr::inner_join(section, receiver, by = "Section")
  coverage %<>% dplyr::inner_join(deployment, by = "Receiver")
  coverage %<>% dplyr::group_by(Section, DeploymentDate) %>% dplyr::summarise(Receivers = n())
  coverage %<>% dplyr::inner_join(section, by = "Section")
  coverage %<>% dplyr::mutate(Coverage = Receivers * pi * 0.5^2 / Area)
  coverage$Coverage[coverage$Coverage > 1] <- 1
  coverage %<>% dplyr::select(Section, DeploymentDate, Coverage)

  all <- expand.grid(DeploymentDate = seq(from = min(coverage$DeploymentDate), to = max(coverage$DeploymentDate), by = "day"),
                     Section = section$Section)

  coverage %<>% dplyr::right_join(all, by = c("Section", "DeploymentDate"))
  coverage$Coverage[is.na(coverage$Coverage)] <- 0

  as.data.frame(coverage)
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
  data$deployment <- NULL
  data$station <- NULL
  data
}

make_section <- function(data) {
  message("making section...")
  data$polygon <- dplyr::as.tbl(broom::tidy(data$section))
  data$polygon %<>% dplyr::rename_(.dots = list(Easting = "lat",
                                                Northing = "long",
                                                Hole = "hole",
                                                Group = "id"))
  data$polygon %<>% dplyr::select_(~Easting, ~Northing, ~Hole, ~Group)
  data$section@data$Area <- rgeos::gArea(data$section, byid = TRUE) / 10 ^ 6
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
  data$depth <- NULL
  data %<>% make_interval(start_date = start_date, end_date = end_date,
                          hourly_interval = hourly_interval)
  data %<>% make_detection()
  data %<>% make_distance()
  data %<>% make_section()
  class(data) <- "detect_data"
  return(data)
}
