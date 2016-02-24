#' Plot Data to Directory
#'
#' @param data The data object to plot
#' @param all A flag indicating whether to plot all the data.
#' @export
plot_data_dir <- function (data, all = FALSE) {
  name <- deparse(substitute(data))
  dir.create(name, showWarnings = FALSE)
  png(file.path(name, paste0(name, "%03d.png")), width = 6, height = 6, units = "in", res = 300)
  on.exit(dev.off(), add = TRUE)
  plot(data, all = all)
  invisible()
}
