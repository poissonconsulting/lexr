#' Print Spatial Polygons Data Frame
#'
#' @param x The SpatialPolygonsDataFrame object to plot.
#'
#' @param ... unused.
#' @method print SpatialPolygonsDataFrame
#' @export
print.SpatialPolygonsDataFrame <- function(x, ...) {
  x <- dplyr::as.tbl(x@data)
  print(x)
}
