#' @method as.data.frame analysis_data
#' @export
as.data.frame.analysis_data <- function(x, ...) {
  analysis <- x
  stopifnot(inherits(analysis, "analysis_data"))

  data <- merge(analysis$capture, analysis$period, by = NULL)

  length <- reshape2::melt(analysis$length, as.is = TRUE, value.name = "Length")
  length$Capture %<>% factor(levels = levels(data$Capture))
  length$Period %<>% factor(levels = levels(data$Period))
  data %<>% dplyr::inner_join(length, by = c("Capture", "Period"))

  detected <- reshape2::melt(analysis$detected, as.is = TRUE, value.name = "Detected")
  detected$Capture %<>% factor(levels = levels(data$Capture))
  detected$Period %<>% factor(levels = levels(data$Period))
  data %<>% dplyr::inner_join(detected, by = c("Capture", "Period"))

  monitored <- reshape2::melt(analysis$detected, as.is = TRUE, value.name = "Monitored")
  monitored$Capture %<>% factor(levels = levels(data$Capture))
  monitored$Period %<>% factor(levels = levels(data$Period))
  data %<>% dplyr::inner_join(monitored, by = c("Capture", "Period"))

  moved <- reshape2::melt(analysis$moved, as.is = TRUE, value.name = "Moved")
  moved$Capture %<>% factor(levels = levels(data$Capture))
  moved$Period %<>% factor(levels = levels(data$Period))
  data %<>% dplyr::inner_join(moved, by = c("Capture", "Period"))

  reported <- reshape2::melt(analysis$reported, as.is = TRUE, value.name = "Reported")
  reported$Capture %<>% factor(levels = levels(data$Capture))
  reported$Period %<>% factor(levels = levels(data$Period))
  data %<>% dplyr::inner_join(reported, by = c("Capture", "Period"))

  public <- reshape2::melt(analysis$public, as.is = TRUE, value.name = "Public")
  public$Capture %<>% factor(levels = levels(data$Capture))
  public$Period %<>% factor(levels = levels(data$Period))
  data %<>% dplyr::inner_join(public, by = c("Capture", "Period"))

  removed <- reshape2::melt(analysis$removed, as.is = TRUE, value.name = "Removed")
  removed$Capture %<>% factor(levels = levels(data$Capture))
  removed$Period %<>% factor(levels = levels(data$Period))
  data %<>% dplyr::inner_join(removed, by = c("Capture", "Period"))

  released <- reshape2::melt(analysis$released, as.is = TRUE, value.name = "Released")
  released$Capture %<>% factor(levels = levels(data$Capture))
  released$Period %<>% factor(levels = levels(data$Period))
  data %<>% dplyr::inner_join(released, by = c("Capture", "Period"))

  spawned <- reshape2::melt(analysis$spawned, as.is = TRUE, value.name = "Spawned")
  spawned$Capture %<>% factor(levels = levels(data$Capture))
  spawned$Period %<>% factor(levels = levels(data$Period))
  data %<>% dplyr::inner_join(spawned, by = c("Capture", "Period"))

  reward <- as.data.frame(analysis$reward)
  colnames(reward) <- c("Reward1", "Reward2")
  reward$Capture <- factor(levels(data$Capture), levels = levels(data$Capture))
  data %<>% dplyr::inner_join(reward, by = c("Capture"))

  tags <- reshape2::melt(analysis$tags, as.is = TRUE, value.name = "Tagged")
  tags$Capture %<>% factor(levels = levels(data$Capture))
  tags$Period %<>% factor(levels = levels(data$Period))
  tags$Tag %<>% factor()
  tags %<>% tidyr::spread_("Tag", "Tagged")
  data %<>% dplyr::inner_join(tags, by = c("Capture", "Period"))

  data %<>% dplyr::arrange_(~Capture, ~Period)

  data %<>% dplyr::mutate_(.dots = list(Capture = ~droplevels(Capture)))
  data %<>% dplyr::mutate_(.dots = list(Species = ~droplevels(Species)))
  data %<>% check_analysis_data_frame()
  data %<>% dplyr::filter_(~as.integer(Period) >= as.integer(PeriodCapture))
  data %<>% dplyr::as.tbl()
  data
}
