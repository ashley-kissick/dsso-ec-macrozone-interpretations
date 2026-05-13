#Plotting macrozones and clusters of zones, by feature


#' get_macrozone_plots
#' @name get_macrozone_plots
#' @author Ashley L. Kissick
#' @description Function to get the plots, saved as pdf's for macrozones and macrozone
#' feature clusters.
#' @param i iteration, character string, the feature name
#' @param macrozones SpatVect, from terra package, the macrozones
#' @param macrozone_trends List, return of get_macrozone_trends
#' @param trend_directory A character string indicating the directory to store the plots
#' @return pdf plots of macrozones and feature clusters
#' @export

get_macrozone_plots <- function(i, macrozones, macrozone_trends, trend_directory) {

  feature_name <- i

  regions <- macrozone_trends$feature_regions
  feature_shp <- regions[[which(names(regions) == feature_name)]]

  #Prepare macrozones and feature shapefile for plotting so colors are consistent
  macrozone_info <- prepare_shps(shps = macrozones, identifier = "label")
  feature_info <- prepare_shps(shps = feature_shp, identifier = "cluster")

  macrozones_formatted <- macrozone_info$zones
  col_zones <- macrozone_info$colors

  features_formatted <- feature_info$zones
  col_features <- feature_info$colors


  #Plot the new zones, along with the old ones, by feature

  if (!dir.exists(paste0(trend_directory, "feature_cluster_shps/"))) {
    dir.create(paste0(trend_directory, "feature_cluster_shps/"))
  }

  pdf(file = paste0(trend_directory, "feature_cluster_shps/", feature_name, "_zones_clusters.pdf"), onefile = TRUE)
  par(mfrow = c(1,2))

  #Macrozones, with feature clusters added
  terra::plot(macrozones_formatted, "label", col = adjustcolor(col_zones, alpha = 0.5),
              border = NA, main = "Macrozones with feature clusters",
              cex.main = 0.5, sort = FALSE, plg=list(ncol = 2))
  terra::plot(feature_shp, lwd = 1, add = TRUE, border = "royalblue4")

  #Feature clusters, with cluster # added
  terra::plot(features_formatted, "cluster", col = col_features, , border = "royalblue4",
              lwd = 0.5, main = paste0(feature_name, " clusters by similar average annual trends"),
              cex.main = 0.5, sort = FALSE)
  #text(feature_shp, "cluster", col = "white", cex = 1.5, font = 2)
  dev.off()


}


