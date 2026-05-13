#' get_feature_trends
#' @name get_feature_trends
#' @author Ashley L. Kissick
#' @description A function for formatting trends of features per cluster assignment
#' and getting an average trend line per cluster.
#' @param feature_cluster An element of the list in the return of 'get_macrozone_clusters'.
#' An list containing clusters and macrozone assignments
#' @param feature_summary An element of the list in the return of 'get_macrozone_clusters'.
#' A data frame with weather averaged for each month across years of interest.
#' @return A list; the first element contains the original trends per cluster assignemnt.  The
#' second element contains the average trend per cluster assignment.
#' @return

get_feature_trends <- function(feature_cluster, feature_summary) {

  #Get the trends per group and their average, then plot
  original_trends <- list()
  feature_average <- list()
  for (j in 1:length(feature_cluster)) {
    original_trends[[j]] <- feature_summary[feature_cluster[[j]],]
    feature_average[[j]] <- colMeans(original_trends[[j]])
  }

  feature_list <- list()
  feature_list$original_trends <- original_trends
  feature_list$feature_average <- feature_average

  return(feature_list)

}



#' get_mac_plot_params
#' @name get_mac_plot_params
#' @author Ashley L. Kissick
#' @description A function for setting the parameters for plotting feature trends across macrozones
#' @param feature_summary A data frame of all values for a feature across macrozones
#' @param feature_cluster A list; the assignments of
#' @param feature_name A character string; the name of the feature
#'
get_mac_plot_params <- function(feature_summary, feature_cluster, feature_name) {

  cluster_list <- list()
  for (i in 1:length(feature_cluster)) {

    param_list <- list()

    #Set the min and max values for plots
    param_list$y_min <- min(feature_summary)

    if(feature_name == "daylength") {
      param_list$y_max <- max(feature_summary) + 0.5

    } else {
      param_list$y_max <- max(feature_summary) + 2
    }

    #Colors
    zones <- as.numeric(feature_cluster[[i]])
    param_list$cluster_colors <- colors_macrozones[zones]

    cluster_list[[i]] <- param_list

  }

  return(cluster_list)

}



#' get_plot_trends
#' @name get_plot_trends
#' @author Ashley L. Kissick
#' @description A function for plotting the trends and average trend lines of a feature for a group
#' @param original_trend A data frame containing trend lines for a feature within a group
#' @param feature_average A named numeric vector with the average trend line for the group
#' @param feature_name A character string of the feature name
#' @param feature_group An integer, the group number as part of an iteration
#' @param group_plot_param A list, each element containing plotting parameters
#' @param feature_group An iteration to be passed as part of a loop
#' @return Plots of feature trends and average trend
#' @export

get_plot_trends <- function(original_trend, feature_average, feature_name,
                            group_plot_param, feature_group) {

  #Create trend data frame
  trend_df <- data.frame(t(rbind(months, original_trend)))
  colnames(trend_df) <- c("Month", colnames(trend_df)[2:length(colnames(trend_df))])
  rownames(trend_df) <- NULL

  #Create a data frame for the average trend
  average_df <- data.frame(t(rbind(months, feature_average)))
  colnames(average_df) <- c("Month", feature_name)
  rownames(average_df) <- NULL

  #Plot first trend to establish the plot
  plot(trend_df[,2] ~ trend_df[,1],  xlab = "Month", ylab = feature_name,
       ylim = c(group_plot_param$y_min, group_plot_param$y_max), type = "l",
       col = group_plot_param$cluster_colors[1], lwd = 2, main = paste0(feature_name, " Group ", feature_group))

  #Plot the other macrozone weather trends, if they exist
  if(dim(trend_df)[2] >= 3) {
    for (j in 3:ncol(trend_df)) {
      lines(trend_df[,j] ~ trend_df[,1], type = "l",
            col = group_plot_param$cluster_colors[j-1], lwd = 2)
    }
  }

  lines(average_df[,2] ~ average_df[,1],  xlab = "Month", ylab = feature_name,
        ylim = c(group_plot_param$y_min, group_plot_param$y_max), type = "l",
        col = "black", lwd = 1)




}

