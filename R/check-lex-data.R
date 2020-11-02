check_lex_section <- function(section) {
  chk_is(section, "sf")

  check_data(section, values = list(
    Section = factor(1),
    Habitat = factor(1),
    Bounded = TRUE), key = "Section", nrow = TRUE)
}

check_lex_station <- function(station) {
  chk_is(station, "sf")

  check_data(station, values = list(
    Station = factor(1),
    Section = factor(1)), key = "Station", nrow = TRUE)
}

check_lex_deployment <- function(deployment) {

  check_data(deployment, values = list(
    Station = factor(1),
    Receiver = factor(1),
    DateTimeReceiverIn = Sys.time(),
    DateTimeReceiverOut = Sys.time()),
    key = c("Station", "Receiver", "DateTimeReceiverIn"), nrow = TRUE)

  if (any(deployment$DateTimeReceiverIn >= deployment$DateTimeReceiverOut))
    error("All deployment DateTimeReceiveIn must be before DateTimeReceiverOut.")

  invisible(NULL)
}

check_lex_capture <- function(capture) {

  check_data(capture, values = list(
    Capture = factor(1),
    DateTimeCapture = Sys.time(),
    SectionCapture = factor(1),
    Species = factor(1),
    Length = c(200L, 1000L),
    Weight = c(0.5, 15, NA),
    Reward1 = c(0L, 500L),
    Reward2 = c(0L, 500L, NA),
    DateTimeTagExpire = Sys.time()),
    key = "Capture", nrow = TRUE)
}

check_lex_recapture <- function(recapture) {

  check_data(recapture, values = list(
    DateTimeRecapture = Sys.time(),
         Capture = factor(1),
         SectionRecapture = factor(c(1, NA)),
         TBarTag1 = TRUE,
         TBarTag2 = TRUE,
         TagsRemoved = TRUE,
         Released = TRUE,
         Public = TRUE))

  if (any(!recapture$TBarTag1 & !recapture$TBarTag2))
    error("Each recapture must have at least one T-bar tag.")
  if (any(!recapture$Public & recapture$TagsRemoved))
    error("Crew should not have removed tags on recapture.")
  if (any(!recapture$Public & !recapture$Released))
    error("Crew should not have harvested recapture.")
  if(anyDuplicated(recapture$Capture[!recapture$Released]))
    error("A recapture can't be harvested twice!")
  if (any(!recapture$Released & !recapture$TagsRemoved))
    error("All non-released recaptures must have had their tags removed!")

  invisible(NULL)
}

check_lex_detection <- function(detection) {

  check_data(detection, values = list(
    DateTimeDetection = Sys.time(),
    Capture = factor(1),
    Receiver = factor(1),
    Detections = c(3L, 100000L)),
    key = c("DateTimeDetection", "Capture", "Receiver"))

  if(any(dttr2::dtt_second(detection$DateTimeDetection) != 0) ||
     any(dttr2::dtt_minute(detection$DateTimeDetection) != 0)) {
    error("All detections must be be hourly.")
  }
  invisible(NULL)
}

check_lex_joins <- function(data) {
  chk_join(data$station, tibble::as_tibble(data$section), "Section")
  chk_join(data$deployment,  tibble::as_tibble(data$station), "Station")
  chk_join(data$capture, tibble::as_tibble(data$section), c(SectionCapture = "Section"))
  chk_join(data$recapture,  data$capture, "Capture")
  chk_join(data$recapture[!is.na(data$recapture$SectionRecapture),],
           tibble::as_tibble(data$section),
           c(SectionRecapture = "Section"))
  chk_join(data$detection, data$capture, "Capture")

  chk_subset(data$detection$Receiver, data$deployment$Receiver)
  invisible(NULL)
}

check_lex_deployment_detection <- function(deployment, detection) {

  deployment$DeploymentID <- 1:nrow(deployment)
  detection <- dplyr::inner_join(detection, deployment, by = "Receiver")
  ndetection <- nrow(detection)
  detection <- detection[detection$DateTimeDetection >= detection$DateTimeReceiverIn,]
  detection <- detection[detection$DateTimeDetection <= detection$DateTimeReceiverOut,]
  if(ndetection != nrow(detection)){
    error("There are detections without deployments.")
  }

  deployment <- deployment[!deployment$DeploymentID %in% unique(detection$DeploymentID),]
  if(nrow(deployment)) {
    warning("There are ", nrow(deployment), " deployments with no detections.")
  }
  invisible(NULL)
}

#' Check Lake Exploitation Data
#'
#' Checks loaded lake exploitation data and returns an invisible NULL if passes
#' all the tests.
#' Otherwise stops with an informative error.
#'
#' @param data The lex_data object to check.
#' @param all A flag indicating whether to run deployment/detection checks.
#' @return An invisible NULL if passes checks otherwise throws an informative error.
#' @seealso input_lex_data
#' @export
check_lex_data <- function(data, all = FALSE) {
  chk_is(data, "lex_data")
  chk_flag(all)
  chk_identical(names(data), lex_data_names())

  purrr::lmap(data, fun_data_name, fun = "check_lex")
  check_lex_joins(data)
  if (all) {
    check_lex_deployment_detection(data$deployment, data$detection)
  }
  invisible(NULL)
}
