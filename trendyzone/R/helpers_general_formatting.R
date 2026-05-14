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
    color_sample <- sample(seq_along(custom_colors), size = num_zones)
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



#' list_to_dflong
#' @name list_to_dflong
#' @author Ashley L. Kissick
#' @description Function to convert list of macrozone-feature cluster assignments
#' from macrozone_trends into a data frame in long format.
#' @param i iteration, character string, the feature name
#' @param macrozone_trends List, return of 'get_macrozone_trends'
#' @return data frame,

list_to_dflong <- function(i, macrozone_trends) {

  feature_name <- i

  #Subset by the feature
  feature_index <- which(names(macrozone_trends$feature_clusters) == feature_name)
  list_subset <- macrozone_trends$feature_clusters[[feature_index]]

  #Format so it can be converted to df long format
  list_subset <- lapply(list_subset, unname)
  names(list_subset) <- paste0(feature_name, "_", 1:length(list_subset))

  #df long format, with formatting
  df_long <- stack(list_subset)
  df_long$ind <- sub(".*_", "", df_long$ind)
  names(df_long) <- c("macrozone", feature_name)
  df_long <- df_long[order(df_long$macrozone), ]

  return(df_long)

}

