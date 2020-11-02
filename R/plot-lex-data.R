tidy_section <- function(section) {
  if (!is.null(section)) {
    suppressMessages(section %<>% broom::tidy())
    section <- list(
      ggplot2::geom_polygon(data = dplyr::filter_(section, ~!hole),
                            ggplot2::aes_(x = ~long / 1000, y = ~lat / 1000, group = ~group),
                            alpha = 1/5, color = "grey50"),
      ggplot2::geom_polygon(data = dplyr::filter_(section, ~hole),
                            ggplot2::aes_(x = ~long / 1000, y = ~lat / 1000, group = ~group),
                            color = "white"))
  }
  section
}

circles_polygons <- function (x, section) {
  x %<>% dplyr::arrange_(~Station)
  radius <- 500
  circles <- sampSurf::spCircle(
    radius = radius, spUnits = sp::CRS(sp::proj4string(section)),
    centerPoint = c(x = x$EastingStation[1], y = x$NorthingStation[1]),
    spID = x$Station[1])$spCircle

  if (nrow(x) > 1) {
    for (i in 2:nrow(x)) {
      circle <- sampSurf::spCircle(
        radius = radius, spUnits = sp::CRS(sp::proj4string(section)),
        centerPoint = c(x = x$EastingStation[i], y = x$NorthingStation[i]),
        spID = x$Station[i])$spCircle
      circles <- raster::bind(circles, circle, keepnames = TRUE)
    }
  }
  sp::proj4string(circles) <- sp::proj4string(section)
  section %<>% rgeos::gUnionCascaded()
  circles <- rgeos::gIntersection(circles, section, byid = TRUE, id = row.names(circles))
  suppressMessages(circles %<>% broom::tidy())

  x %<>% dplyr::mutate_(.dots = list(id = ~as.character(Station)))
  circles %<>% dplyr::inner_join(x, by = "id")
  circles
}

tidy_station_coverage <- function(station, section) {
  circles <- circles_polygons(station, section)

  list(ggplot2::geom_polygon(data = dplyr::filter_(circles, ~!hole),
                             ggplot2::aes_(x = ~long / 1000, y = ~lat / 1000, group = ~group,
                                           alpha = ~I(Coverage)), color = "red", fill = "red"),
       ggplot2::geom_polygon(data = dplyr::filter_(circles, ~hole),
                             ggplot2::aes_(x = ~long / 1000, y = ~lat / 1000, group = ~group),
                             color = "white"))
}

plot_lex_section <- function(section) {

  ggplot2::ggplot(data = section@data, ggplot2::aes_(x = ~EastingSection / 1000,
                                                     y = ~NorthingSection / 1000)) +
    tidy_section(section) +
    ggplot2::geom_point(alpha = 1/3) +
    ggrepel::geom_text_repel(ggplot2::aes_(x = ~EastingSection / 1000, label = ~Section),
                             size = 4) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_lex_station <- function(station, section = NULL) {
  ggplot2::ggplot(data = station, ggplot2::aes_(x = ~EastingStation / 1000,
                                                y = ~NorthingStation / 1000)) +
    tidy_section(section) +
    ggplot2::geom_point(alpha = 1/3) +
    ggrepel::geom_text_repel(ggplot2::aes_(label = ~Station), size = 4) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}

plot_lex_deployment <- function(deployment) {
  tz <- lubridate::tz(deployment$DateTimeReceiverIn)
  ggplot2::ggplot(data = deployment, ggplot2::aes_(y = ~Station)) +
    ggplot2::geom_segment(ggplot2::aes_(
      x = ~DateTimeReceiverIn, xend = ~DateTimeReceiverOut, yend = ~Station),
      alpha = 1/2, size = 3) +
    ggplot2::geom_point(ggplot2::aes_(
      x = ~DateTimeReceiverIn), color = "blue") +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz)) +
    ggplot2::scale_y_discrete()
}

plot_lex_capture <- function(capture) {
  capture %<>% dplyr::mutate_(.dots = list("Year" = ~lubridate::year(DateTimeCapture)))

  ggplot2::ggplot(data = capture, ggplot2::aes_(x = ~Length)) +
    ggplot2::facet_grid(Species~Year) +
    ggplot2::geom_histogram(binwidth = 50, color = "white") +
    ggplot2::scale_x_continuous(name = "Fork Length (mm)") +
    ggplot2::scale_y_continuous(name = "Captures")
}

plot_lex_recapture <- function(recapture) {
  tz <- lubridate::tz(recapture$DateTimeRecapture)
  recapture %<>% dplyr::mutate_(.dots = list("SectionRecapture" = ~factor(SectionRecapture)))

  ggplot2::ggplot(data = recapture, ggplot2::aes_(x = ~DateTimeRecapture, y = ~SectionRecapture)) +
    ggplot2::geom_point(ggplot2::aes_(shape = ~Released), alpha = 1/2, size = 4) +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz))
}

plot_lex_detection <- function(detection) {
  detection %<>% dplyr::mutate_(.dots = list("Date" = ~lubridate::date(DateTimeDetection)))

  detection %<>% dplyr::group_by_(~Date) %>%
    dplyr::summarise_(.dots = list("Detections" = ~sum(Detections))) %>%
    dplyr::ungroup()

  ggplot2::ggplot(data = detection, ggplot2::aes_(x = ~Date, y = ~Detections)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_date(name = "Date", labels = scales::date_format("%b %Y")) +
    ggplot2::scale_y_continuous(name = "Total Daily Detections", labels = scales::comma)
}

plot_lex_deployment_detection <- function (deployment, detection, station) {

  deployment %<>% dplyr::inner_join(station, by = "Station")
  detection %<>% dplyr::inner_join(deployment, by = "Receiver")
  detection %<>% dplyr::filter_(~DateTimeDetection >= DateTimeReceiverIn)
  detection %<>% dplyr::filter_(~DateTimeDetection <= DateTimeReceiverOut)

  tz <- lubridate::tz(deployment$DateTimeReceiverIn)
  ggplot2::ggplot(data = deployment, ggplot2::aes_(y = ~Station)) +
    ggplot2::facet_grid(Section~., scales = "free_y", space = "free_y") +
    ggplot2::geom_segment(ggplot2::aes_(
      x = ~DateTimeReceiverIn, xend = ~DateTimeReceiverOut, yend = ~Station),
      alpha = 1/3, size = 3, color = "red") +
    ggplot2::geom_point(data = detection, ggplot2::aes_(x = ~DateTimeDetection), alpha = 1/4) +
    ggplot2::geom_point(ggplot2::aes_(x = ~DateTimeReceiverIn), color = "blue") +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz)) +
    ggplot2::scale_y_discrete()
}

plot_lex_receiver_detection <- function (deployment, detection) {

  detection %<>% dplyr::inner_join(deployment, by = "Receiver")
  detection %<>% dplyr::filter_(~DateTimeDetection >= DateTimeReceiverIn)
  detection %<>% dplyr::filter_(~DateTimeDetection <= DateTimeReceiverOut)

  tz <- lubridate::tz(deployment$DateTimeReceiverIn)
  ggplot2::ggplot(data = deployment, ggplot2::aes_(y = ~Receiver)) +
    ggplot2::geom_segment(ggplot2::aes_(
      x = ~DateTimeReceiverIn, xend = ~DateTimeReceiverOut, yend = ~Receiver),
      alpha = 1/3, size = 3, color = "red") +
    ggplot2::geom_point(data = detection, ggplot2::aes_(x = ~DateTimeDetection), alpha = 1/4) +
    ggplot2::geom_point(ggplot2::aes_(x = ~DateTimeReceiverIn), color = "blue") +
    ggplot2::scale_x_datetime(name = "Date", labels = scales::date_format("%b %Y", tz)) +
    ggplot2::scale_y_discrete()
}

#' Plot Coverage
#'
#' Plots the sections with receivers and coverage areas with alpha equivalent to
#' deployment during period of interest.
#'
#' @param data The \code{lex_data} object to plot.
#' @param start_date A date of the start.
#' @param end_date A date of the end.
#' @export
plot_lex_coverage <- function(data, start_date = min(lubridate::date(data$capture$DateTimeCapture)),
                              end_date = max(lubridate::date(data$capture$DateTimeTagExpire))
) {

  check_date(start_date)
  check_date(end_date)

  if (end_date <= start_date) error("start_date must be before end_date")

  station <- data$station
  deployment <- data$deployment
  section <- data$section

  deployment %<>% dplyr::mutate_(.dots = list(
    DateReceiverIn = ~lubridate::date(DateTimeReceiverIn),
    DateReceiverOut = ~lubridate::date(DateTimeReceiverOut)
  ))
  deployment$DateTimeReceiverIn <- NULL
  deployment$DateTimeReceiverOut <- NULL

  deployment %<>% dplyr::filter_(~lubridate::date(DateReceiverIn) < end_date,
                                 ~lubridate::date(DateReceiverOut) > start_date)

  if(!nrow(deployment))
    error("no remaining deployments")

  deployment$DateReceiverIn[deployment$DateReceiverIn < start_date] <- start_date
  deployment$DateReceiverOut[deployment$DateReceiverOut > end_date] <- end_date

  deployment %<>% dplyr::mutate_(.dots = list(Days = ~as.integer(DateReceiverOut) - as.integer(DateReceiverIn)))

  stopifnot(all(deployment$Days >= 1))

  deployment %<>% dplyr::group_by_(~Station) %>% dplyr::summarise_(.dots = list(
    Coverage = ~sum(Days))) %>% dplyr::ungroup()

  deployment %<>% dplyr::mutate_(.dots = list(Coverage = ~Coverage / (as.integer(end_date) - as.integer(start_date))))

  if(any(deployment$Coverage > 1))
    message("overlapping deployments")

  station %<>% dplyr::inner_join(deployment, by = "Station")

  ggplot2::ggplot(data = station, ggplot2::aes_(x = ~EastingStation / 1000,
                                                y = ~NorthingStation / 1000)) +
    tidy_section(section) +
    tidy_station_coverage(station, section) +
    ggplot2::geom_point() +
    ggrepel::geom_text_repel(ggplot2::aes_(label = ~Station), size = 2) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)", labels = scales::comma) +
    ggplot2::scale_y_continuous(name = "Northing (km)", labels = scales::comma)
}


#' Plot Lex Data
#'
#' @param x The lex_data object to plot.
#'
#' @param all A flag indicating whether to produce all plots.
#' @param ... unused.
#' @method plot lex_data
#' @export
plot.lex_data <- function(x, all = FALSE, ...) {
  chk_flag(all)
  y <- purrr::lmap(x, fun_data_name, fun = "plot_lex")
  lapply(y, print)
  if (all) {
    print(plot_lex_station(x$station, x$section))
    print(plot_lex_deployment_detection(x$deployment, x$detection, x$station))
    print(plot_lex_receiver_detection(x$deployment, x$detection))
  }
  invisible(NULL)
}
