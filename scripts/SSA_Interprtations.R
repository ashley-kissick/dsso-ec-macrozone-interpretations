library(devtools)
devtools::load_all()


#Format data to right format.
#Run the analysis



#data:
data_dir <- "../../../Precision_Agriculture/Environmental_Modeling/SSA_Interpretations/data/"

#ssa macrozones
macrozones_ssa <- terra::vect(paste0(data_dir, "emea_ssa_240802_M025.geojson"))
macrozones_ssa$label <- as.integer(macrozones_ssa$label)
macrozones_ssa$label <- as.character(macrozones_ssa$label + 1)

#centroids (average of each feature, acrosss zone boundaries)
ssa_centroids <- read.csv(paste0(data_dir, "/emea-ssa-25macrozones-zone-centroids.csv"),
                          header = TRUE)


#extract elevation
elevation <- ssa_centroids[which(ssa_centroids$feature == "elevation"),]
ssa_centroids <- ssa_centroids[-which(ssa_centroids$feature == "elevation"),]


features <- c("max_temperature", "temp_range", "total_precipitation", "daylength")
feature_list <- lapply(features, prepare_centroid, df = ssa_centroids)

ssa_long <- do.call(cbind, feature_list)
ssa_long <- ssa_long[, !duplicated(colnames(ssa_long))]
centroids_25 <- ssa_long
ssa_long <- NULL
ssa_centroids <- NULL

#change NA for year for daylength to an arbitrary year
names(centroids_25) <- gsub("_NA_", "_", names(centroids_25))





#---------------------------------------------------------

#Weather and daylength features (monthly, 2014-2023:
#  1) max_temperature
#  2) temp_range
#  3) total_precipitation
#  4) daylength



#Create a monthly time series of weather features for each macrozone

#Where to save plots
trend_directory <- "../data/SSA/plots/"

features <- features
years <- c(2014:2023)
save = TRUE

#There are so many trend lines it is difficult to distinguish trends.
#Let's work on that by getting an average trend across years.

#1) Get monthly trends (across years for weather data)
#2) Cluster them
#3) Get resulting shapefiles of underlining trends

macrozone_trends <- get_macrozone_trends(features = features,
                                         years = years,
                                         macrozones = macrozones_ssa,
                                         centroids = centroids_25,
                                         trend_directory = trend_directory,
                                         save = save)


#max temperature:  12 (could use a few more)
#temp range:  11 (looks good)
#total precipitation: 10 (probably)
#daylength: 8 (appropriate)


#4) Get the plots of the clusters, with the macrozones
macrozone_plots <- lapply(features, get_macrozone_plots,
                          macrozones = macrozones_ssa,
                          macrozone_trends = macrozone_trends,
                          trend_directory = trend_directory)


#5) Plot the macrozones, and which macrozones belong to which feature cluster

cluster_plots <- lapply(features, get_cluster_map_trend_plots,
                        macrozone_trends = macrozone_trends,
                        macrozones = macrozones_ssa,
                        trend_directory = trend_directory)




#5) Plot the trends of each feature

#a.  Plot trends all in one panel for the macrozone report
feature_plots <- get_feature_trend_plots(macrozone_trends = macrozone_trends,
                                         mac_row_col_params = mac_row_col_params,
                                         trend_directory = trend_directory,
                                         panel = TRUE)




#Maybe rethink the visualizations...
#What about having a small map with the macrozones, then beside it, the given macrozone
# w/text showing the average elevation per zone.

#Then, below, panels with the 4 features, and average trend line, showing the zones with the
# similar trends...



#The feature cluster shapefile
regions <- macrozone_trends$feature_regions
#feature_shp <- regions[[which(names(regions) == feature_name)]]

#The summaries
summaries <- macrozone_trends$feature_summaries
#feature_summary <- summaries[[which(names(summaries) == feature_name)]]

#The clusters
clusters <- macrozone_trends$feature_clusters



#Get cluster assignments for each macrozone, across features
feature_list <- lapply(features, list_to_dflong,
                       macrozone_trends = macrozone_trends)
cluster_assignments <- Reduce(function(x, y) merge(x, y, by = "macrozone", all = TRUE),
                              feature_list)


for (i in 1:dim(macrozones)[1]) {
  
  macrozone <- macrozones[i,]
  macrozone_color <- colors_macrozones[i]
  
  #Get the cluster assignments for the macrozone
  max_temperature <- cluster_assignments$max_temperature[i]
  temp_range <- cluster_assignments$temp_range[i]
  total_precipitation <- cluster_assignments$total_precipitation[i]
  daylength <- cluster_assignments$daylength[i]
  
  #Get the macrozones in each cluster
  m_temp_macs <- macrozones[which(cluster_assignments$max_temperature == max_temperature),] 
  temp_r_macs <- macrozones[which(cluster_assignments$temp_range == temp_range),]
  t_prec_macs <- macrozones[which(cluster_assignments$total_precipitation == total_precipitation),]
  daylen_macs <- macrozones[which(cluster_assignments$daylength == daylength),]  
  
  #Get the colors
  m_temp_cols <- colors_macrozones[which(cluster_assignments$max_temperature == max_temperature)]
  temp_r_cols <- colors_macrozones[which(cluster_assignments$temp_range == temp_range)]
  t_prec_cols <- colors_macrozones[which(cluster_assignments$total_precipitation == total_precipitation)]
  daylen_cols <- colors_macrozones[which(cluster_assignments$daylength == daylength)]
  
  
  
  #Plot
  
  pdf(file = paste0(trend_directory, "messing_around/",
                    "Macrozone_", i, ".pdf"), onefile = TRUE)
  
  #par(mfrow = c(1,1))
  #mat <- matrix(c(1, 1,  # Row 1
  #                3, 2), # Row 2
  #              nrow = 2, byrow = TRUE)
  #layout(mat)
  
  par(mfrow = c(2,2))
  
  #Macrozones, with macrozone boundary added
  terra::plot(macrozones, "label", col = adjustcolor(colors_macrozones, alpha = 0.5),
              border = NA, main = "Macrozones with feature cluster",
              cex.main = 0.5, sort = FALSE, plg=list(ncol = 2))
  terra::plot(macrozones[which(macrozones$label == i)], lwd = 1,
              add = TRUE, border = "royalblue4")
  
  
  plot.new()
  plot.new()
  
  
  par(mfrow = c(2,2))
  
  #Get trends with map of macrozones
  
  #Max temperature
  index <- as.integer(max_temperature)
  trend_plot_mac <- get_trend_plots_macrozone(feature_name = "max_temperature", 
                                              summaries = summaries,
                                              clusters = clusters, 
                                              index = index)
  
  terra::plot(macrozones, "label", col = adjustcolor(colors_macrozones, alpha = 0.5),
              border = NA, main = "Macrozones with similar maximum temperature trends",
              cex.main = 0.5, legend = NULL)
  
  
  terra::plot(m_temp_macs, "label", col = adjustcolor(m_temp_cols, alpha = 0.5),
              border = "royalblue4", sort = FALSE, add = TRUE)
  
  
  
  
  
  #Temperature range
  index <- as.integer(temp_range)
  trend_plot_mac <- get_trend_plots_macrozone(feature_name = "temp_range", 
                                              summaries = summaries,
                                              clusters = clusters, 
                                              index = index)
  
  terra::plot(macrozones, "label", col = adjustcolor(colors_macrozones, alpha = 0.5),
              border = NA, main = "Macrozones with similar temperature range trends",
              cex.main = 0.5, legend = NULL)
  
  terra::plot(temp_r_macs, "label", col = adjustcolor(m_temp_cols, alpha = 0.5),
              border = "royalblue4", sort = FALSE, add = TRUE)
  
  
  
  
  #Total precipitation
  index <- as.integer(total_precipitation)
  trend_plot_mac <- get_trend_plots_macrozone(feature_name = "total_precipitation", 
                                              summaries = summaries,
                                              clusters = clusters, 
                                              index = index)
  
  terra::plot(macrozones, "label", col = adjustcolor(colors_macrozones, alpha = 0.5),
              border = NA, main = "Macrozones with similar total precipitation trends",
              cex.main = 0.5, legend = NULL)
  
  terra::plot(t_prec_macs, "label", col = adjustcolor(t_prec_cols, alpha = 0.5),
              border = "royalblue4", sort = FALSE, add = TRUE)
  
  
  
  
  #Daylength
  index <- as.integer(daylength)
  trend_plot_mac <- get_trend_plots_macrozone(feature_name = "daylength", 
                                              summaries = summaries,
                                              clusters = clusters, 
                                              index = index)
  
  terra::plot(macrozones, "label", col = adjustcolor(colors_macrozones, alpha = 0.5),
              border = NA, main = "Macrozones with similar daylength trends",
              cex.main = 0.5, legend = NULL)
  
  terra::plot(daylen_macs, "label", col = adjustcolor(daylen_cols, alpha = 0.5),
              border = "royalblue4", sort = FALSE, add = TRUE)
  
  
  
  
  
  dev.off()

}













pdf(file = paste0(trend_directory, "cluster_trend_panels/",
                  macrozone_name, "_cluster_trend_panels.pdf"), onefile = TRUE)


#Layout
par(mfrow = c(1,1))
mat <- matrix(c(1, 2,  # Row 1
                3, 3), # Row 2
              nrow = 2, byrow = TRUE)
layout(mat)











#---------------------------------------------------------
#PLOTTING AND VISUALIZATIONS OF MACROZONES

#Plot the macrozones
pdf(file = paste0(trend_directory, "Macrozones_25.pdf"), onefile = TRUE)
par(mfrow = c(1,1))
#Macrozones
terra::plot(macrozones, "label", col = adjustcolor(colors_macrozones, alpha = 0.5),
            border = NA, main = "Macrozones for SSA, 25 class model",
            cex.main = 0.5, sort = FALSE)
dev.off()



#---------------------------------------------------------









# EXTRA/DEPRECATED


#Plot the macrozones and save for visualizations
#png(file = paste0(trend_directory, "Macrozones_25.png"),
#    height = 2400, width = 2400, res = 300)
#terra::plot(macrozones_ssa, "label", col = col_regions, lwd = 0.5)
#dev.off()


#Plot each macrozone seperately
#for (i in 1:length(macrozone_number))
#
#  png(file = paste0(trend_directory, "Macrozone_plot_", i, ".png"),
#      height = 2400, width = 2400, res = 300)
#  plot(macrozones_ssa, lwd = 0.5)
#  plot(macrozones_ssa[which(macrozones_ssa$label == i),], add = TRUE, col = col_regions[i])
#  dev.off()
#
#}



# Add a legend for the zones
#macrozone_names <- paste0("Macrozone ", as.character(feature_cluster[[i]]))

#plot.new()
#legend("center",
#       legend = macrozone_names,
#       col = col_zone[feature_cluster[[i]]],
#       pch = 16,
#       title = "Legend Only")

#The feature cluster of interest
#plot(features_prepared, border = "royalblue4", lwd = 0.5,
#     main = paste0(feature_name, " clusters"), cex.main = 0.5)
#plot(feature_shp[which(feature_shp$cluster == i),], add = TRUE,
#     col = col_features[i], border = "royalblue4", lwd = 1.5)

#The zone assignments in this cluster, coded















