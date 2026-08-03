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

get_macrozone_clusters <- function(feature_summary, feature_name, trend_directory, groups) {

  #Let's look at trends across average weather patterns (2009-2018) for the macrozones
  #Perform cluster analysis on the weather feature for the given year

  dist_mat <- dist(feature_summary, method = "euclidean")
  dendrogram <- hclust(dist_mat, method = "ward.D2")
  screeplot <- get_screeplot(dendrogram = dendrogram, groups = groups)

  number_clusters_prompt <- readline(prompt = "How many clusters? ")
  num_clusters <- as.integer(number_clusters_prompt)

  #Get the groups with clusters and information they contain and save the dendrogram:
  png(file = paste0(trend_directory, "screeplot_dendrogram/", feature_name, "_dendrogram.png"),
      height = 1800, width = 2400, res = 300)

  par(mfrow = c(1,1))
  dendrogram <- hclust(dist_mat, method = "ward.D2")
  plot(dendrogram, main = feature_name)
  clusters <- rect.hclust(dendrogram, num_clusters)
  dev.off()

  #Get the cluster list object to return in the function
  dendrogram <- hclust(dist_mat, method = "ward.D2")

  if(!is.null(feature_name)) { plot(dendrogram, main = feature_name) }
  if(is.null(feature_name)) { plot(dendrogram, main = "microenvironments") }

  clusters <- rect.hclust(dendrogram, num_clusters)

  #Reformat to make sure the right zone label is assigned
  for (i in 1:length(clusters)) {
    c_c <- as.integer(sub(".*?_", "", names(clusters[[i]])))
    names(c_c) <- names(clusters[[i]])
    clusters[[i]] <- c_c
  }

  if (!dir.exists(paste0(trend_directory, "screeplot_dendrogram/"))) {
    dir.create(paste0(trend_directory, "screeplot_dendrogram/"))
  }

  #Save scree plot
  png(file = paste0(trend_directory, "screeplot_dendrogram/", feature_name, "_scree_plot.png"),
      height = 2400, width = 2400, res = 300)
  screeplot <- get_screeplot(dendrogram = dendrogram, groups = 20)
  dev.off()

  return(clusters)

}
