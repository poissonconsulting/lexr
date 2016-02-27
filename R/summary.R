percent_true <- function(x) {
  round(sum(x) / length(x) * 100)
}

summarise_movement <- function (data) {
  dplyr::data_frame(
    Simultaneous = percent_true(data$Sections > 1),
    Jumps = percent_true(data$Jump > 0),
    MaxJump = max(data$Jump),
    Values = nrow(data)
  )
}

#' @export
summary.lex_data <- function(object, ...) {
  data <- object
  summary <- list()
  summary$number_sections <- nlevels(data$section@data$Section)
  summary$number_captures <- nlevels(data$capture$Capture)
  summary$number_species <- nlevels(data$capture$Species)
  summary$number_recaptures <- nrow(data$recapture)
  summary$number_intervals <- nrow(data$interval)
  summary$time_difference <- get_difftime(data)
  summary$first_detection <- data$detection$DateTimeDetected[1]
  summary$last_detection <- data$detection$DateTimeDetected[nrow(data$detection)]
  summary$total_detections <- sum(data$detection$Detections)

  summary
}

#' @export
summary.detect_data <- function(object, ...) {
  data <- object
  summary <- list()
  summary$number_sections <- nlevels(data$section@data$Section)
  summary$number_captures <- nlevels(data$capture$Capture)
  summary$number_recaptures <- nrow(data$recapture)
  summary$number_intervals <- nrow(data$interval)
  summary$time_difference <- get_difftime(data)
  summary$start_datetime <- data$interval$DateTime[1]
  summary$end_datetime <- data$interval$DateTime[nrow(data$interval)]
  summary$total_detections <- sum(data$detection$Detections)
  summary$simultaneous <- percent_true(data$detection$Sections > 1)
  summary$jumps <- percent_true(data$detection$Jump > 0)
  summary$max_jump <- max(data$detection$Jump)

  summary$section <- dplyr::inner_join(data$section@data, data$detection, by  = "Section")
  summary$section %<>% plyr::ddply("Section", summarise_movement) %>% dplyr::arrange_(~Section)

  summary$capture <- dplyr::inner_join(data$capture, data$detection, by  = "Capture")
  summary$capture %<>% plyr::ddply("Capture", summarise_movement) %>% dplyr::arrange_(~Capture)

  summary
}
