#' get_cluster_map_trend_plots
#' @name get_feature_trend_plots
#' @author Ashley L. Kissick
#' @description Get plots of 1) macrozone plus the feature cluster,
#' 2) The macrozones in the feature cluster,
#' 3) The monthly trends of each macrozone in the cluster, with average
#' @param i iteration, character string, the feature name
#' @param macrozone_trends List, the output of 'get_macrozone_trends'
#' @param macrozones SpatVect, the macrozones
#' @param trend_directory A character string indicating the directory to store the plots
#' @return pdf's saved of each feature
#' @export

get_cluster_map_trend_plots <- function(i, macrozone_trends, macrozones, trend_directory) {

  feature_name <- i

  #The feature cluster shapefile
  regions <- macrozone_trends$feature_regions
  feature_shp <- regions[[which(names(regions) == feature_name)]]

  #The summaries
  summaries <- macrozone_trends$feature_summaries
  feature_summary <- summaries[[which(names(summaries) == feature_name)]]

  #The cluster assignments
  clusters <- macrozone_trends$feature_clusters
  feature_cluster <- clusters[[which(names(clusters) == feature_name)]]


  #Actions


  #SpatVectors
  #Prepare macrozones and feature shapefile for plotting so colors are consistent
  macrozone_info <- prepare_shps(shps = macrozones, identifier = "label")
  feature_info <- prepare_shps(shps = feature_shp, identifier = "cluster")

  mac_df <- macrozone_info$zone_df
  fea_df <- feature_info$zone_df

  macrozones_prepared <- macrozone_info$zones
  features_prepared <- feature_info$zones

  col_zone <- macrozone_info$colors
  col_features <- feature_info$colors

  #Trend plots

  if (!dir.exists(paste0(trend_directory, "feature_cluster_trends/"))) {
    dir.create(paste0(trend_directory, "feature_cluster_trends/"))
  }

  pdf(file = paste0(trend_directory, "feature_cluster_trends/",
                    feature_name, "_feature_cluster_trend.pdf"), onefile = TRUE)
  par(mfrow = c(1,1))

  mat <- matrix(c(1, 2,  # Row 1
                  3, 3), # Row 2
                nrow = 2, byrow = TRUE)

  # 2. Apply the layout
  layout(mat)

  for (j in 1:length(feature_cluster)) {

    # First panel:  Macozones with the clustering
    #Macrozones, with feature clusters added
    terra::plot(macrozones_prepared, "label", col = adjustcolor(col_zone, alpha = 0.5),
                border = NA, main = "Macrozones with feature cluster",
                cex.main = 0.5, sort = FALSE, plg=list(ncol = 2))
    terra::plot(feature_shp[which(feature_shp$cluster == j), ], lwd = 1,
                add = TRUE, border = "royalblue4")


    # Second panel:  The macrozones within a specific cluster
    #Get the macrozones
    macrozones_cropped <- macrozones_prepared[feature_cluster[[j]],]
    col_zone_app <- col_zone[feature_cluster[[j]]]

    #The macrozones in that cluster
    terra::plot(macrozones_cropped, "label", border = "royalblue4", lwd = 0.5,
                main = paste0("Macrozones in cluster ", j),
                col = adjustcolor(col_zone_app, alpha = 0.5), cex.main = 0.5,
                sort = FALSE)


    #Third panel:  The monthly trends for each macrozone in the cluster
    #The trends for this cluster
    get_cluster_trend_plot(feature_cluster = feature_cluster,
                           feature_summary = feature_summary,
                           feature_name = feature_name,
                           j = j)

  }
  dev.off()

}

