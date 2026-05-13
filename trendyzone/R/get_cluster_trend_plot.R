#' get_cluster_trend_plot
#' @name get_cluster_trend_plot
#' @author Ashley L. Kissick
#' @description Get the plot of similar trends by feature.
#' @param feature_cluster List, length of clusters for each feature, with macrozone
#' assignments
#' @param feature_summary Dataframe, the monthly temporal averages for the feature,
#' each line is macrozone
#' @param feature_name Character string, the name of the feature
#' @param j iteration
#' @return The plot of the trends for each feature cluster, along with the
#' average monthly trend
#' @export

get_cluster_trend_plot <- function(feature_cluster, feature_summary, feature_name, j) {

  #Get the feature trends and average trend per cluster assignment
  feature_trends <- get_feature_trends(feature_cluster = feature_cluster,
                                       feature_summary = feature_summary)

  #Get parameters for plots
  plot_params <- get_mac_plot_params(feature_summary = feature_summary,
                                     feature_cluster = feature_cluster,
                                     feature_name = feature_name)

  group_plot_param <- plot_params[[j]]
  original_trend <- feature_trends$original_trends[[j]]
  feature_average <- feature_trends$feature_average[[j]]

  get_plot_trends(original_trend = original_trend,
                  feature_average = feature_average,
                  feature_name = feature_name,
                  group_plot_param = group_plot_param,
                  feature_group = j)


}
