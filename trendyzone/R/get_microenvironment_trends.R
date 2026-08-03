#' get_microenvironment_trends
#' @name get_microenvironment_trends
#' @author Ashley L. Kissick
#' @description A function for getting summaries for features for each microenvironment.
#' @param features A character vector of feature names.
#' @param num_microenvironments An integer; the number of microenvironments in the
#' microenvironments model (i.e., 40)
#' @param centroids A dataframe containing the original centroids for each feature
#' @param trend_directory A character string indicating the directory where to
#' save scree plots and dendrograms
#' @param save Logical; if TRUE, the resulting shapefile showing the area the
#' feature trend covers will be saved to the path indicated by 'trend_directory'.
#' @return A list.  The first element contains the average trend lines for each feature in 'features'.
#' The second element contains the cluster assignments of the trend lines for each feature in 'features'.
#' The third element contains the resulting trend shapefiles.
#' Resulting scree plots and dendrograms of the clustering process are saved.
#' Also, the maps of the resulting trend shapefiles are saved.
#' @export

get_microenvironment_trends <- function(features, microenvironments, centroids,
                                        trend_directory, save) {

  #microenvironment_labels <- sort(as.numeric(microenvironments$label), decreasing = FALSE)
  #num_microenvironments <- microenvironments_labels[length(microenvironments_labels)]

  feature_summary_list <- list()
  feature_clusters_list <- list()
  feature_region_list <- list()

  for (i in 1:length(features)) {

    feature_df <- centroids[,grep(features[i], colnames(centroids))]
    feature_name <- features[i]

    #1) Get cluster assignments for microenvironments for the weather feature
    feature_clusters <- get_macrozone_clusters(feature_summary = feature_df,
                                               feature_name = NULL,
                                               groups = groups,
                                               trend_directory = trend_directory)
    feature_clusters_list[[i]] <- feature_clusters


    #3) Plot and save the maps of the resulting clusters
    feature_region_zone <- get_feature_region_zones(feature_clusters = feature_clusters,
                                                    trend_directory = trend_directory,
                                                    save = FALSE,
                                                    feature_name = NULL,
                                                    macrozones = microenvironments)

    feature_region_list [[i]] <- feature_region_zone

  }

  names(feature_summary_list) <- features
  names(feature_clusters_list) <- features
  names(feature_region_list) <- features

  all_list <- list()
  all_list$feature_summaries <- feature_summary_list
  all_list$feature_clusters <- feature_clusters_list
  all_list$feature_regions <- feature_region_list

  return(all_list)

}




# Functions in get_macrozone_trends:
# I. get_macrozone_trends
#    A. get_macrozone_clusters
#       1.  get_screeplot (function in 'get_clusters.R')
#    C. get_feature_region_plots











#' get_feature_region_zones
#' @name get_feature_region_zones
#' @author Ashley L. Kissick
#' @description A function for obtaining the weather clusters for interpretation
#' @param feature_clusters A list containing the clusters for a feature
#' @param trend_directory A character string indicating the directory to store the plots
#' @param save Logical; if TRUE, the resulting shapefile showing the area the
#' feature trend covers will be saved to the path indicated by 'trend_directory'.
#' @param feature_name A character string; the feature name of interest
#' @param macrozones Terra vect, the clusters of the geographic area
#' @return Saved trend cluster shapefiles
#' @export

get_feature_region_zones <- function(feature_clusters, trend_directory, save,
                                     feature_name, macrozones) {

  #Get a dataframe with cluster assignments to macrozones
  cluster_list <- list()
  for (j in 1:length(feature_clusters)) {

    cluster <- feature_clusters[[j]]

    cluster_df <- data.frame(matrix(ncol = 2, nrow = length(cluster)))
    colnames(cluster_df) <- c("label", "cluster")
    cluster_df$label <- cluster
    cluster_df$cluster <- rep(j, length = length(cluster))
    cluster_list[[j]] <- cluster_df

  }
  cluster_macs <- do.call(rbind, cluster_list)

  #Now merge, unify the regions, and plot
  cluster_names <- as.character(1:length(unique(cluster_macs$cluster)))
  weather_macs <- sp::merge(macrozones, cluster_macs, by = "label")
  weather_macs$cluster <- factor(weather_macs$cluster, levels = cluster_names)
  clusters_id <- weather_macs$cluster

  feature_spdf <- terra::aggregate(weather_macs, by="cluster")


  if(save == TRUE) {

    if (!dir.exists(paste0(trend_directory, "feature_cluster_shps/"))) {
      dir.create(paste0(trend_directory, "feature_cluster_shps"))
    }

    terra::writeVector(feature_spdf, paste0(trend_directory, "feature_cluster_shps/",
                                            feature_name, ".shp"), overwrite = TRUE)
  }

  return(feature_spdf)

}




#' get_screeplot
#' @name get_screeplot
#' @author Ashley L. Kissick
#' @description Plots a screeplot with x groups
#' @param dendrogram A hclust object
#' @param groups An ingeger; the number of possible groups to include along the x axis
#' @return A plotted scree plot
#' @return

get_screeplot <- function(dendrogram, groups) {

  d_height <- sort(dendrogram$height, decreasing = TRUE)
  d_height <- d_height[1:20]    #Limit the observations to 20
  d_height <- data.frame(d_height, c(1:length(d_height)))
  colnames(d_height) <- c("height", "group")

  plot(d_height$group, d_height$height, type = "b", xlab = "Group", ylab = "Height")

}



