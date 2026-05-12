#' get_macrozone_trends
#' @name get_macrozone_trends
#' @author Ashley L. Kissick
#' @description A function for getting trend lines for features at a monthly temporal
#' resolution for each macrozone.  For features where multiple years data are provided
#' (i.e., weather), the average trend is obtained.  This function then clusters the
#' trends across macrozones to investigate underlining conditions describing macrozones.
#' @param features A character vector of feature names.
#' @param years An integer vector contianing the years of interest
#' @param months An integer vector containing the months of interest
#' @param month_names A character vector naming the months of interest
#' @param num_macrozones An integer; the number of macrozones in the macrozone model (i.e., 40)
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

get_macrozone_trends <- function(features, years, macrozones, centroids,
                                 trend_directory, save) {

  macrozone_labels <- sort(as.numeric(macrozones$label), decreasing = FALSE)
  num_macrozones <- macrozone_labels[length(macrozone_labels)]

  feature_summary_list <- list()
  feature_clusters_list <- list()
  feature_region_list <- list()

  for (i in 1:length(features)) {

    feature_df <- centroids[,grep(features[i], colnames(centroids))]
    feature_name <- features[i]

    #1) Format the trend (if applicable, get average trend across years for macrozones)
    if(feature_name == "daylength") {

      #Make sure columns are in the correct order
      col_order <- paste0("daylength", "_", months)
      daylength_dt <- data.table::data.table(feature_df)
      data.table::setcolorder(daylength_dt, col_order)
      feature_summary_list[[i]] <- data.frame(daylength_dt)

    } else{
      feature_summary_list[[i]] <- get_feature_summary(feature_df = feature_df,
                                                       feature_name = feature_name,
                                                       years = years,
                                                       months = months,
                                                       num_macrozones = num_macrozones)
    }


    #2) Get cluster assignments for macrozones for the weather feature
    feature_clusters <- get_macrozone_clusters(feature_summary = feature_summary_list[[i]],
                                               feature_name = feature_name,
                                               trend_directory = trend_directory)
    feature_clusters_list[[i]] <- feature_clusters


    #3) Plot and save the maps of the resulting clusters
    feature_region_zone <- get_feature_region_zones(feature_clusters = feature_clusters,
                                                    trend_directory = trend_directory,
                                                    save = save,
                                                    feature_name = feature_name,
                                                    macrozones = macrozones)

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
#    A. get_feature_summary
#    B. get_macrozone_clusters
#       1.  get_screeplot (function in 'get_clusters.R')
#    C. get_feature_region_plots




#' get_feature_summary
#' @name get_feature_summary
#' @author Ashley L. Kissick
#' @description A function for formatting a weather feature so trends can be examined through cluster analysis and plotting
#' @param feature_df A data frame containing the feature of interest across months and years
#' @param feature_name A character string naming the weather feature
#' @param years An integer vector for the year of interest
#' @param months An integer vector of months of interest
#' @param num_macrozones An integer, the number of macrozones (i.e., 40, 70)
#' @return A data frame of weather formatted to contain only the year of interest
#' @export

get_feature_summary <- function(feature_df, feature_name, years, months, num_macrozones) {

  feature_list <- list()
  for (i in 1:length(years)) {

    year_of_interest <- years[i]
    dates <- paste0(month_names, "-", year_of_interest)

    #Make sure columns are in the correct order
    col_order <- paste0(feature_name, "_", year_of_interest, "_", months)
    year <- data.table::data.table(feature_df[,grep(year_of_interest, colnames(feature_df))])
    data.table::setcolorder(year, col_order)
    year <- data.frame(year)

    colnames(year) <- paste0(feature_name, "_", months)
    year$macrozone <- 1:num_macrozones

    feature_list[[i]] <- year

  }

  feature <- do.call(rbind, feature_list)

  #Get a summary for each macrozone
  feature_summary_list <- list()
  for (i in 1:num_macrozones) {
    macrozone <- feature[which(feature$macrozone == i),]
    feature_summary_list[[i]] <- colMeans(macrozone)
  }

  feature_summary <- data.frame(do.call(rbind, feature_summary_list))
  rownames(feature_summary) <- paste0("macrozone_", 1:num_macrozones)
  feature_summary$macrozone <- NULL

  return(feature_summary)

}




#' get_macrozone_clusters
#' @name get_macrozone_clusters
#' @author Ashley L. Kissick
#' @description A function for getting clusters of a weather feature across macrozones
#' @param feature_summary A data frame with weather averaged for each month across years of interest.
#' Each row is a macrozone, each column is the average weather for a given month (Jan - Dec)
#' @param feature_name A character string naming the weather feature
#' @param trend_directory A character string indicating the directory where to
#' save scree plots and dendrograms
#' @return A list of cluster assignments for macrozones
#' @export

get_macrozone_clusters <- function(feature_summary, feature_name, trend_directory) {

  #Let's look at trends across average weather patterns (2009-2018) for the macrozones
  #Perform cluster analysis on the weather feature for the given year

  dist_mat <- dist(feature_summary, method = "euclidean")
  dendrogram <- hclust(dist_mat, method = "ward.D2")
  screeplot <- get_screeplot(dendrogram = dendrogram, groups = 20)

  number_clusters_prompt <- readline(prompt = "How many clusters? ")
  num_clusters <- as.integer(number_clusters_prompt)

  #Get the groups with clusters and information they contain:
  par(mfrow = c(1,1))
  dendrogram <- hclust(dist_mat, method = "ward.D2")
  plot(dendrogram, main = feature_name)
  clusters <- rect.hclust(dendrogram, num_clusters)


  #Save scree plot
  png(file = paste0(trend_directory, feature_name, "_scree_plot.png"),
      height = 2400, width = 2400, res = 300)
  screeplot <- get_screeplot(dendrogram = dendrogram, groups = 20)
  dev.off()


  #Save dendrogram
  png(file = paste0(trend_directory, feature_name, "_dendrogram.png"),
      height = 1800, width = 2400, res = 300)

  plot(dendrogram, main = feature_name)
  clusters <- rect.hclust(dendrogram, num_clusters)

  dev.off()

  return(clusters)

}





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
    terra::writeVector(feature_spdf, paste0(trend_directory, feature_name, ".shp"),
                       overwrite = TRUE)
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



