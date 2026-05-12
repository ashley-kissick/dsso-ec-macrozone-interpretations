# HELPER FUNCTIONS FOR INTERPRETING MACROZONES

# Note:  Need to come back to this to:
#        1) refactor considering changes in prior ouput format and
#        2) complete unit tests

#' get_macrozone_summary
#' @name get_macrozone_summary
#' @author Ashley L. Kissick
#' @description A function for getting the summaries of feature cluster trends
#' for each macrozone
#' @param weather_macrozone_clusters A list, the return of 'get_macrozone_clusters' for weather features
#' @param daylength_clusters A list, the return of 'get_macrozone_clusters' for daylength
#' @return Plots of each macrozone trend with the cluster average trend
#' @export

get_macrozone_summary <- function(weather_macrozone_clusters, daylength_clusters, centroids) {

  #Get a dataframe for weather features and which macrozones have which trends

  #WEATHER
  weather_df_list <- list()
  weather_summary_list <- list()
  weather_mean_list <- list()
  for (i in 1:length(weather_macrozone_clusters)) {

    #1) Get a data frame of trend assignments
    weather_list <- weather_macrozone_clusters[[i]]
    feature_name <- names(weather_macrozone_clusters)[i]
    weather_df <- get_trends(feature_list = weather_list, feature_name = feature_name)
    weather_df_list[[i]] <- weather_df

    #2) Get a dataframe containing the trend for each macrozone
    weather_feature <- centroids_40[,grep(weather_features[i], colnames(centroids_40))]
    weather_summary <- get_feature_summary(feature_df = weather_feature,
                                           feature_name = feature_name,
                                           years = years,
                                           months = months,
                                           num_macrozones = num_macrozones)

    #3) Assign a cluster number to each row
    weather_summary$macrozone <- c(1:40)
    new_weather_summary <- merge(weather_summary, weather_df[,1:2], by = "macrozone")
    weather_summary_list[[i]] <- new_weather_summary

    #4) Get the average weather trend per cluster
    weather_mean_list[[i]] <- get_summary(feature_summary = new_weather_summary)

  }
  weather_df <- do.call(rbind, weather_df_list)
  names(weather_summary_list) <- names(weather_macrozone_clusters)
  names(weather_mean_list) <- names(weather_macrozone_clusters)

  #DAY LENGTH
  #1) Get a data frame of trend assignments
  daylength_df <- get_trends(feature_list = daylength_clusters, feature_name = "daylength")

  #2) Get a dataframe containing the trend for each macrozone
  daylength <- centroids_40[,grep("daylength", colnames(centroids_40))]
  col_order <- paste0("daylength", "_", months)
  daylength_dt <- data.table::data.table(daylength)
  data.table::setcolorder(daylength_dt, col_order)
  daylength <- data.frame(daylength_dt)
  daylength$macrozone <- c(1:40)
  daylength_summary <- merge(daylength, daylength_df[,1:2], by = "macrozone")


  #3) Get the average weather trend per cluster
  daylength_mean <- get_summary(feature_summary = daylength_summary)


  #AGGREGATIONS
  #get trends for each macrozone
  macrozone_weather_trends_list <- list()
    for (j in 1:length(weather_summary_list)) {
      weather_mac <- weather_summary_list[[j]]
      weather_summary_mac <- weather_mean_list[[j]]

      macrozone_weather_trends_list[[j]] <- get_macrozone_trend(centroids = centroids_40,
                                                                sum_df = weather_mac,
                                                                mean_df = weather_summary_mac)
    }

  macrozone_weather_trends_list[[length(weather_summary_list) + 1]] <- get_macrozone_trend(centroids = centroids_40,
                                                                                           sum_df = daylength_summary,
                                                                                           mean_df = daylength_mean)
  names(macrozone_weather_trends_list) <- c(names(weather_macrozone_clusters), "daylength")

  return(macrozone_weather_trends_list)

}



#Functions in 'get_macrozone_summary'
# I. get_macrozone_summary
#    A.  get_trends
#    B.  get_summary
#    C.  get_macrozone_trend




#' get_trends
#' @name get_trends
#' @author Ashley L. Kissick
#' @description A function for getting a dataframe of cluster assignments for a feature
#' @param feature_list A list of cluster assignments
#' @param feature_name The name of the feature
#' @return A data frame of cluster assignments for a feature
#' @export

get_trends <- function(feature_list, feature_name) {

  group_list <- list()
  for (j in 1:length(feature_list)) {

    group_df <- data.frame(feature_list[[j]], rep(j, length = length(feature_list[[j]])),
                           rep(feature_name, length = length(feature_list[[j]])))
    colnames(group_df) <- c("macrozone", "group", "feature")
    group_list[[j]] <- group_df


  }
  groups <- do.call(rbind, group_list)
  return(groups)

}



#' get_summary
#' @name get_summary
#' @author Ashley L. Kissick
#' @description A function for getting a summary of trends within a group
#' @param feature_summary A data frame containing trend lines for macrozones
#' @return A dataframe of an average trendline within clusters of macrozones
#' @export

get_summary <- function(feature_summary) {

  summary_list <- list()
  groups <- sort(unique(feature_summary$group), decreasing = FALSE)
  for (j in 1:length(groups)) {
    group_x <- feature_summary[which(feature_summary$group == groups[j]),]
    sum_group <- colMeans(group_x[,2:(ncol(group_x) - 1)])
    summary_list[[j]] <- sum_group
  }
  summaries <- data.frame(do.call(rbind, summary_list))
  summaries$group <- 1:length(groups)

  return(summaries)

}



#' get_macrozone_trend
#' @name get_macrozone_trend
#' @author Ashley L. Kissick
#' @description A function for getting the macrozone trend and the cluster average trend
#' @param centroids A data frame with original centroid values
#' @param sum_df A data frame containing the macrozone trends with cluster assignment
#' @param mean_df A data frame containing the macrozone cluster trends
#' @return A list with an element per macrozone containing the macrozone trend and the cluster trend
#' @export

get_macrozone_trend <- function(centroids, sum_df, mean_df) {

  plot_value_list <- list()
  for (k in 1:nrow(centroids)) {

    mac_trend <- sum_df[k,2:(ncol(sum_df) - 1)]
    mac_group <- sum_df[k, ncol(sum_df)]
    group_mean <- mean_df[mac_group, 1:(ncol(mean_df) - 1)]

    to_plot <- rbind(mac_trend, group_mean)
    rownames(to_plot) <- c("mac_trend", "group_mean")
    plot_value_list[[k]] <- to_plot

  }

  return(plot_value_list)

}


#' get_plot
#' @name get_plot
#' @author Ashley L. Kissick
#' @description Plot a feature for a macrozone and average cluster
#' @param feature_df A data frame with the trend for the macrozone and the trend for the cluster
#' @param feature_name A character string, the name of the feature
#' @param y_max An integer, the maximum for the y axis
#' @return A plot
#' @export

get_plot <- function(feature_df, feature_name, y_max) {

  subset_mac <- feature_df[1,]
  subset_group <- feature_df[2,]

  plot(1:12, subset_mac, ylim = c(0, y_max), type = "l", col = "black", main = feature_name,
       ylab = feature_name, xlab = "Month")
  lines(1:12, subset_group, col = "red", lwd = 2)


}




#' prepare_centroid
#' @name prepare_centroid
#' @author Ashley L. Kissick
#' @description Function to convert long to wide, given the centroid file
#' @param i iteration in an apply function, the feature name, a character string
#' @param df Dataframe of the features, in long format
#' @return Dataframe of features converted to wide format, and column names
#' are consistent with what is expected in downstream functions.
#' @export

prepare_centroid <- function(i, df) {

  cent <- df[which(df$feature == i),]
  cent$feature <- paste0(cent$feature, "_", cent$year, "_", cent$month)
  cent <- cent[,c("zone", "feature", "value")]

  #convert to wide format, with column names "feature_year_month
  wide_df <- reshape(cent,
                     idvar = "zone",
                     timevar = "feature",
                     direction = "wide")

  names(wide_df) <- gsub("value.", "", names(wide_df))

  return(wide_df)

}





#' prepare_shps
#' @name prepare_shps
#' @author Ashley L. Kissick
#' @description Function to get the zones and the respective colors
#' @param shps SpatVect of the zones
#' @param identifier Character string (either 'label' for macrozones,
#' or 'cluster' for feature clusters)
#' @return List of the zones, formatted, and their assigned colors
#' @export

prepare_shps <- function(shps, identifier) {

  #Get the max number of zones
  #Convert to an ordered factor

  if(identifier == "label") {
    labels <- sort(as.numeric(shps$label), decreasing = FALSE)
    shps$label <- ordered(shps$label, levels = shps$label)
  }

  if(identifier == "cluster") {
    labels <- sort(as.numeric(shps$cluster), decreasing = FALSE)
    shps$cluster <- ordered(shps$cluster, levels = shps$cluster)
  }

  num_zones <- labels[length(labels)]


  #Match colors
  if(identifier == "label") {

    #Matching macrozone colors
    zone_df <- data.frame(label = 1:num_zones, col_zone = colors_macrozones[1:num_zones])

  }


  if(identifier == "cluster") {

    #Matching cluster colors

    set.seed(2000)
    color_sample <- sample(seq_along(custom_colors), size = num_clusters)
    color_sample <- sort(color_sample)
    cluster_colors <- custom_colors[color_sample]

    zone_df <- data.frame(label = 1:num_zones, col_zone = cluster_colors)

  }

  zone_df$col_zone <- as.character(zone_df$col_zone)
  zone_df$label <- as.factor(zone_df$label)

  #Add in the clusters
  if(identifier == "label") {
    col_zones <- as.vector(zone_df$col_zone[match(levels(shps$label), zone_df$label)])
  }

  if(identifier == "cluster") {
    col_zones <- as.vector(zone_df$col_zone[match(levels(shps$cluster), zone_df$label)])
  }


  return_list <- list()
  return_list$zones <- shps
  return_list$colors <- col_zones
  return_list$zone_df <- zone_df

  return(return_list)

}

