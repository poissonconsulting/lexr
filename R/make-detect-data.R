



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


# calculate_coverage <- function(section, section_polygons, station, station_deployment,
#                                hourly_interval = 6) {
#
#   station_deployment %<>% dplyr::full_join(station, by = "Station")
#   if (any(is.na(station_deployment$Receiver)))
#     check_stop("st")
#
#
#   coverage <- dplyr::inner_join(section, receiver, by = "Section")
#   coverage %<>% dplyr::inner_join(deployment, by = "Receiver")
#   coverage %<>% dplyr::group_by(Section, DeploymentDate) %>% dplyr::summarise(Receivers = n())
#   coverage %<>% dplyr::inner_join(section, by = "Section")
#   coverage %<>% dplyr::mutate(Coverage = Receivers * pi * 0.5^2 / Area)
#   coverage$Coverage[coverage$Coverage > 1] <- 1
#   coverage %<>% dplyr::select(Section, DeploymentDate, Coverage)
#
#   all <- expand.grid(DeploymentDate = seq(from = min(coverage$DeploymentDate), to = max(coverage$DeploymentDate), by = "day"),
#                      Section = section$Section)
#
#   coverage %<>% dplyr::right_join(all, by = c("Section", "DeploymentDate"))
#   coverage$Coverage[is.na(coverage$Coverage)] <- 0
#
#   as.data.frame(coverage)
# }

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

make_interval <- function(data, start_date, end_date, hourly_interval) {
  check_date(start_date)
  check_date(end_date)
  check_scalar(hourly_interval, c(1L,2L,3L,4L,6L,12L,24L))

#   start_date %<>% as.POSIXct(tz = "UTC")
#   end_date %<>% as.POSIXct(tz = "UTC")
#
#   lubridate::hour(start_date) <-
#   lubridate::minute(start_date) <-
#   lubridate::second(start_date) <-
#
#   interval <- dplyr::data_frame(DateTime = seq(
#     start_date, end_date, by = "6 hours"))
#   interval$Interval <- 1:nrow(interval)
#   interval %<>% dplyr::mutate_(.dots = list(Date = as.Date(DateTime),
#                                             Hour = lubridate::hour(DateTime)))
#   data$interval <- interval
  data
}

make_distance <- function(data) {
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
  rownames(distance) <- levels(data$section@data$Section)
  colnames(distance) <- levels(data$section@data$Section)
  data$distance <- distance
  data
}

make_section <- function(data) {
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
  start_date = min(as.Date(capture$CaptureDateTime)),
  end_date = min(Sys.Date(), max(as.Date(capture$TagExpireDateTime))),
  hourly_interval = 6L) {

  data %<>% check_lex_data()
  data %<>% filter_captures(capture)
  data$depth <- NULL
  data %<>% make_interval(start_date = start_date, end_date = end_date,
                          hourly_interval = hourly_interval)
  data %<>% make_distance()
  data %<>% make_section()
  class(data) <- "detect_data"
  return(data)
}
