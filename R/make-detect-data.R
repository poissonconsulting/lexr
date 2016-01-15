# # section_polygons@data$SectionArea <- gArea(section_polygons, byid = TRUE) / 10^6
#
#
# neighbours <- poly2nb(section_polygons, row.names = section_polygons@data$Section)
# plot(section_polygons)
# plot(neighbours, coordinates(section_polygons), col = "red", add = TRUE)
# plot(neighbours, coordinates(section_polygons), col = "red")
#
# section_adjacency <- matrix(FALSE, nrow = nrow(section_polygons@data), ncol = nrow(section_polygons@data))
# for (i in 1:nrow(section_polygons@data)) {
#   section_adjacency[i,neighbours[[i]]] <- TRUE
# }
# diag(section_adjacency) <- TRUE
# stopifnot(isSymmetric(section_adjacency))
# section_distance <- spa::uDist(section_adjacency, nrow(section_adjacency))
# dim <- dim(section_distance)
# section_distance <- as.integer(section_distance)
# dim(section_distance) <- dim
# use_data(section_distance, overwrite = TRUE)



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
  data %<>% check_lex_data()
  data
}

#' Make Detect Data
#'
#' Makes detect_data object from a lex_data object.
#' @inheritParams check_lex_data
#' @param capture A data frame of the capture data to use.
#' @param hourly_interval A count indicating the hourly interval.
#'
#' @return A detect_data object.
#' @export
make_detect_data <-  function(data, capture = data$capture, hourly_interval = 6) {
  data %<>% check_lex_data()
  data %<>% filter_captures(capture)
  return(data)
}
