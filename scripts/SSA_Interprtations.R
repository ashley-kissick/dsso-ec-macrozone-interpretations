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
#PLOTTING AND VISUALIZATIONS OF MACROZONES

#Plot the macrozones and save for visualizations
png(file = paste0(trend_directory, "Macrozones_25.png"),
    height = 2400, width = 2400, res = 300)
terra::plot(macrozones_ssa, "label", col = col_regions, lwd = 0.5)
dev.off()


#Plot each macrozone seperately
for (i in 1:length(macrozone_number)) {

  png(file = paste0(trend_directory, "Macrozone_plot_", i, ".png"),
      height = 2400, width = 2400, res = 300)
  plot(macrozones_ssa, lwd = 0.5)
  plot(macrozones_ssa[which(macrozones_ssa$label == i),], add = TRUE, col = col_regions[i])
  dev.off()

}

#---------------------------------------------------------




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

#b.  Plot trends all in one panel for the macrozone report




#Plot the macrozones
pdf(file = paste0(trend_directory, "Macrozones.pdf"), onefile = TRUE)
par(mfrow = c(1,1))
#Macrozones
terra::plot(macrozones_formatted, "label", col = adjustcolor(col_zones, alpha = 0.5),
            border = NA, main = "Macrozones",
            cex.main = 0.5, sort = FALSE)
dev.off()





# EXTRA/DEPRECATED
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















