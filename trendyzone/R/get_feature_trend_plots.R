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

      if (!dir.exists(paste0(trend_directory, "feature_cluster_trends/"))) {
        dir.create(paste0(trend_directory, "feature_cluster_trends/"))
      }

      #png(file = paste0(trend_directory, feature_name, "_panel.png"),
      #    height = 2400, width = 2400, res = 300)
      #par(mfrow = c(mac_row_col_param$param_1, mac_row_col_param$param_2))

      pdf(file = paste0(trend_directory, "feature_cluster_trends/",
                        feature_name, "_panel.pdf"), onefile = TRUE)#,
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


# I. get_feature_trend_plots
#     A. get_feature_trends
#     B. get_mac_plot_params
#     C. plot_trends
