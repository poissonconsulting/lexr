check_analysis_data_frame <- function(data) {
  check_data3(
    data, list(
      Species = factor(1),
      Capture = factor(1),
      Period = factor(1),
      Days = 1,
      PeriodCapture = factor(1),
      SectionCapture = factor(1),
      Date = lubridate::today(),
      Year = 1L,
      Month = c(1L, 12L),
      Length = c(0L, 1000L),
      Weight = c(0.5, 10, NA),
      Reward1 = c(0L, 200L),
      Reward2 = c(0L, 200L, NA),
      TBarTag1 = c(TRUE, NA),
      TBarTag2 = c(TRUE, NA),
      Monitored = TRUE,
      Detected = TRUE,
      Moved = TRUE,
      Reported = TRUE,
      Public = c(TRUE, NA),
      Removed = c(TRUE, NA),
      Released = c(TRUE, NA),
      SpawningPeriod = TRUE,
      Spawned = c(TRUE, NA)),
    key = c("Capture", "Period"), select = TRUE)
}

