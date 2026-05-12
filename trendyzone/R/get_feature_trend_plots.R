#Plotting trend lines of features

#' get_feature_trend_plots
#' @name get_feature_mac_plots
#' @author Ashley L. Kissick
#' @description A function for plotting variation in weather trends within mac clusters
#' and the overall weather trend and saves the plots
#' @param macrozone_trends The return of 'get_macrozone_clusters'.  A list containing
#' clusters and macrozone assignments
#' @param feature_summary A data frame with weather averaged for each month across years of interest.
#' @param trend_directory A character string indicating the directory to store the plots
#' @param panel Logical; if TRUE, plots will be plotted together on the same page using
#' mac_row_col_params to set the column and rows of the plots on the page.
#' @return Plots saved by weather feature
#' @export

get_feature_trend_plots <- function(macrozone_trends, mac_row_col_params,
                                    trend_directory, panel) {

  # Get the original trends for each macrozone
  # Get the average trend within a cluster

  feature_clusters = macrozone_trends$feature_clusters
  feature_summaries = macrozone_trends$feature_summaries

  for (i in 1:length(feature_clusters)) {

    feature_name <- names(feature_clusters)[[i]]
    feature_cluster <- feature_clusters[[i]]      #Cluster assignments for macrozones for a feature
    feature_summary <- feature_summaries[[i]]     #Feature trends across all macrozones

    #Get the feature trends and average trend per cluster assignment
    feature_trends <- get_feature_trends(feature_cluster = feature_cluster,
                                         feature_summary = feature_summary)

    #Get parameters for plots
    plot_params <- get_mac_plot_params(feature_summary = feature_summary,
                                       feature_cluster = feature_cluster,
                                       feature_name = feature_name)

    mac_row_col_param <- mac_row_col_params[[i]]

    if(panel == TRUE) {

      #png(file = paste0(trend_directory, feature_name, "_panel.png"),
      #    height = 2400, width = 2400, res = 300)
      #par(mfrow = c(mac_row_col_param$param_1, mac_row_col_param$param_2))

      pdf(file = paste0(trend_directory, feature_name, "_panel.pdf"), onefile = TRUE)#,
          #height = 2400, width = 2400, res = 300)
      par(mfrow = c(3, 2))

      for (j in 1:length(feature_trends$original_trends)) {

        group_plot_param <- plot_params[[j]]
        original_trend <- feature_trends$original_trends[[j]]
        feature_average <- feature_trends$feature_average[[j]]
        plot_trends <- get_plot_trends(original_trend = original_trend,
                                       feature_average = feature_average,
                                       feature_name = feature_name,
                                       group_plot_param = group_plot_param,
                                       feature_group = j)

      }

      dev.off()

    }

    if(panel == FALSE) {

      for (j in 1:length(feature_trends$original_trends)) {
        #png(file = paste0(trend_directory, feature_name, "_Group_", j, ".png"),
        #    height = 2400, width = 2400, res = 300)
        #par(mfrow = c(1, 1))
        plot_trends <- get_plot_trends(original_trend = original_trend,
                                       feature_average = feature_average,
                                       feature_name = feature_name,
                                       group_plot_param = group_plot_param,
                                       feature_group = j)
        #dev.off()
      }

    }

  }

}






# Functions in get_feature_mac_plots:
# I. get_feature_trend_plots
#     A. get_feature_trends
#     B. get_mac_plot_params
#     C. plot_trends


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
       col = group_plot_param$cluster_colors[1], lwd = 2, main = paste0("Group ", feature_group))

  #Plot the other macrozone weather trends, if they exist
  if(dim(trend_df)[2] >= 3) {
    for (j in 3:ncol(trend_df)) {
    lines(trend_df[,j] ~ trend_df[,1], type = "l",
          col = group_plot_param$cluster_colors[j], lwd = 2)
    }
  }

  lines(average_df[,2] ~ average_df[,1],  xlab = "Month", ylab = feature_name,
        ylim = c(group_plot_param$y_min, group_plot_param$y_max), type = "l",
        col = "black", lwd = 1)




}

