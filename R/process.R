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

#' Process Data
#'
#' @inheritParams load_lex_data
#' @param hourly_interval A count indicating the hourly interval.
#'
#' @return A list of the data frames and transition matrix.
#' @export
#' @examples
#' \dontrun{
#' process_lex_data("qlexdatr")
#' }
process_lex_data <-  function(package, hourly_interval = 6) {
  check_lex_data(package)
  load_lex_data(package)
  section_transition <- section_distance < 2
  list(section = section, section_transition = section_transition)
}
