#' Merge Detect Data
#'
#' Merges Detect data into a single data frame which is useful for
#' calculating habitat use.
#'
#' @param data The detect_data object to merge.
#' @return A tbl data frame.
#' @export
merge_detect_data <-  function(data) {
  data %<>% check_detect_data()
  lex <- data
  data <- merge(lex$section@data, lex$interval)
  data %<>% dplyr::left_join(lex$coverage, by = c("Section", "Interval"))
  data$Coverage[is.na(data$Coverage)] <- 0
  data %<>% dplyr::left_join(lex$detection, by = c(Interval = "IntervalDetection", Section = "Section"))
  data$Detections[is.na(data$Detections)] <- 0
  data %<>% dplyr::mutate_(.dots = list(Detected = ~Detections > 0))
  data %<>% dplyr::group_by_(~Section, ~Habitat, ~Area, ~Bounded, ~EastingSection,
                             ~NorthingSection, ~ColorCode, ~DateTime, ~Coverage) %>%
    dplyr::summarise_(.dots = list(DetectedFish = ~sum(Detected))) %>%
    dplyr::ungroup()
  data %<>% dplyr::arrange_(~Section, ~DateTime)

  check_key(data, key = c("Section", "DateTime"))
  data
}
