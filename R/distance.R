# section_polygons@data$SectionArea <- gArea(section_polygons, byid = TRUE) / 10^6
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
