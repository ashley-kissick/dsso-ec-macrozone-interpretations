library(devtools)
devtools::load_all()


#Format data to right format.
#Run the analysis



#data:
data_dir <- "../data/SSA/"

#ssa macrozones
macrozones_ssa <- terra::vect(paste0(data_dir, "macrozones/emea_ssa_240802_M025.geojson"))

#centroids (average of each feature, acrosss zone boundaries)
ssa_centroids <- read.csv(paste0(data_dir, "macrozones/emea-ssa-25macrozones-zone-centroids.csv"),
                          header = TRUE)
ssa_centroids$zone <- ssa_centroids$zone - 1 


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
                                         panel = TRUE, macrozones = macrozones_ssa)




#6) Create bar charts of each feature, across zones

macrozone_info <- prepare_shps(shps = macrozones_ssa, identifier = "label")
macrozones_prepared <- macrozone_info$zones


#1.  Elevation

#elevation already extracted
elevation_bar <- elevation[,c("zone", "value")]
elevation_bar$color <- macrozones_prepared$color

#Sort the elevation from lowest to highest
#elevation_bar <- elevation_bar[order(elevation_bar$value), ]

pdf(file = paste0(trend_directory, "summaries/barplots/elevation.pdf"), onefile = TRUE,
    width = 9, height = 5)
barplot(height = elevation_bar$value, 
        names.arg = elevation_bar$zone, 
        col = adjustcolor(elevation_bar$color, alpha = 0.5),
        main = "Average Elevation", 
        xlab = "Macrozone", 
        ylab = "Elevation, m",
        ylim = c(0, (max(elevation_bar$value + 200))),
        cex.names = 0.75)
dev.off()

names(elevation_bar)[which(names(elevation_bar) == "value")] <- "elevation"
elevation_bar <- elevation_bar[,c("zone", "elevation")]


#2.  Max Temperature

m_temp <- ssa_centroids[grep("max_temperature", ssa_centroids$feature),]
m_temp <- m_temp[,c("zone", "value")]
m_temp <- m_temp %>%
  group_by(zone) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))
m_temp <- data.frame(m_temp)
m_temp$color <- macrozones_prepared$color

#Sort the elevation from lowest to highest
#m_temp <- m_temp[order(m_temp$mean_value), ]

pdf(file = paste0(trend_directory, "summaries/barplots/max_temperature.pdf"), onefile = TRUE,
    width = 9, height = 5)
barplot(height = m_temp$mean_value, 
        names.arg = m_temp$zone, 
        col = adjustcolor(m_temp$color, alpha = 0.5),
        main = "Annual Average Maximum Temperature", 
        xlab = "Macrozone", 
        ylab = "Temperature (\u00B0C)",
        ylim = c(20, (max(m_temp$mean_value + 3))), xpd = FALSE,
        cex.names = 0.75)
dev.off()

names(m_temp)[which(names(m_temp) == "mean_value")] <- "max_temperature"
m_temp <- m_temp[,c("zone", "max_temperature")]



#3.  Temperature Range

temp_r <- ssa_centroids[grep("temp_range", ssa_centroids$feature),]
temp_r <- temp_r[,c("zone", "value")]
temp_r <- temp_r %>%
  group_by(zone) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))
temp_r <- data.frame(temp_r)
temp_r$color <- macrozones_prepared$color

#Sort the elevation from lowest to highest
#temp_r <- temp_r[order(temp_r$mean_value), ]


pdf(file = paste0(trend_directory, "summaries/barplots/temperature_range.pdf"), onefile = TRUE,
    width = 9, height = 5)
barplot(height = temp_r$mean_value, 
        names.arg = temp_r$zone, 
        col = adjustcolor(temp_r$color, alpha = 0.5),
        main = "Annual Average Temperature Range", 
        xlab = "Macrozone", 
        ylab = "Temperature (\u00B0C)",
        ylim = c(4, (max(temp_r$mean_value + 1))), xpd = FALSE,
        cex.names = 0.75)
dev.off()

names(temp_r)[which(names(temp_r) == "mean_value")] <- "temp_range"
temp_r <- temp_r[,c("zone", "temp_range")]






#4.  Total Precipitation

precip <- ssa_centroids[grep("total_precipitation", ssa_centroids$feature),]
precip <- precip[,c("zone", "value")]
precip <- precip %>%
  group_by(zone) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))
precip <- data.frame(precip)
precip$color <- macrozones_prepared$color

#Sort the elevation from lowest to highest
#precip <- precip[order(precip$mean_value), ]


pdf(file = paste0(trend_directory, "summaries/barplots/total_precipitation.pdf"), onefile = TRUE,
    width = 9, height = 5)
barplot(height = precip$mean_value, 
        names.arg = precip$zone, 
        col = adjustcolor(precip$color, alpha = 0.5),
        main = "Annual Average Total Precipitation", 
        xlab = "Macrozone", 
        ylab = "Precipitation, mm",
        ylim = c(0, (max(precip$mean_value + 50))),
        cex.names = 0.75)
dev.off()

names(precip)[which(names(precip) == "mean_value")] <- "total_precipitation"
precip <- precip[,c("zone", "total_precipitation")]




#5. Daylength

daylen <- ssa_centroids[grep("daylength", ssa_centroids$feature),]
daylen <- daylen[,c("zone", "value")]
daylen <- daylen %>%
  group_by(zone) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))
daylen <- data.frame(daylen)
daylen$color <- macrozones_prepared$color

#Sort the elevation from lowest to highest
daylen <- daylen[order(daylen$mean_value), ]


barplot(height = daylen$mean_value, 
        names.arg = daylen$zone, 
        col = daylen$color, 
        main = "Maximum Temperature", 
        xlab = "Macrozone", 
        ylab = "Temperature (\u00B0C)",
        ylim = c(0, (max(daylen$mean_value))),
        cex.names = 0.75)

names(daylen)[which(names(daylen) == "mean_value")] <- "daylength"
daylen <- daylen[,c("zone", "daylength")]





#Get the average values for each feature/zone
all_averages <- merge(elevation_bar, m_temp, by = "zone")
all_averages <- merge(all_averages, temp_r, by = "zone")
all_averages <- merge(all_averages, precip, by = "zone")
all_averages <- merge(all_averages, daylen, by = "zone")
all_averages_round <- data.frame(lapply(all_averages[,2:ncol(all_averages)], round))
all_averages_final <- cbind(all_averages$zone, all_averages_round)
names(all_averages_final) <- c("Macrozone", "Elevation, m", "Max Temperature, (\u00B0C)", 
                         "Temperature Range, (\u00B0C)", "Total Precipitation, mm",
                         "Daylength")
all_averages_final <- all_averages_final[,c(1:5)]

pdf(file = paste0(trend_directory, "summaries/spreadsheets/averages.pdf"), 
    height = 10, width = 11)
gridExtra::grid.table(all_averages_final)
dev.off()

#write.csv(all_averages_final, file = paste0(trend_directory, "summaries/spreadsheets/averages.csv"),
#          row.names = TRUE)



#7) 
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

#SpatVectors
#Prepare macrozones and feature shapefile for plotting so colors are consistent
macrozone_info <- prepare_shps(shps = macrozones, identifier = "label")
mac_df <- macrozone_info$zone_df
macrozones_prepared <- macrozone_info$zones



#Get cluster assignments for each macrozone, across features
feature_list <- lapply(features, list_to_dflong,
                       macrozone_trends = macrozone_trends)
cluster_assignments <- Reduce(function(x, y) merge(x, y, by = "macrozone", all = TRUE),
                              feature_list)
rownames(cluster_assignments) <- paste0("macrozone_", cluster_assignments$macrozone)


for (i in 1:dim(macrozones_prepared)[1]) {
  
  macrozone <- macrozones_prepared[i,]
  macrozone_color <- macrozone$color
  
  #Get the cluster assignments for the macrozone
  max_temperature <- cluster_assignments$max_temperature[i]
  temp_range <- cluster_assignments$temp_range[i]
  total_precipitation <- cluster_assignments$total_precipitation[i]
  daylength <- cluster_assignments$daylength[i]
  
  #Get the macrozones in each cluster
  m_temp_macs <- macrozones_prepared[which(cluster_assignments$max_temperature == max_temperature),] 
  temp_r_macs <- macrozones_prepared[which(cluster_assignments$temp_range == temp_range),]
  t_prec_macs <- macrozones_prepared[which(cluster_assignments$total_precipitation == total_precipitation),]
  daylen_macs <- macrozones_prepared[which(cluster_assignments$daylength == daylength),]  
  
  #Get the colors
  m_temp_cols <- macrozones_prepared$color[which(cluster_assignments$max_temperature == max_temperature)]
  temp_r_cols <- macrozones_prepared$color[which(cluster_assignments$temp_range == temp_range)]
  t_prec_cols <- macrozones_prepared$color[which(cluster_assignments$total_precipitation == total_precipitation)]
  daylen_cols <- macrozones_prepared$color[which(cluster_assignments$daylength == daylength)]
  
  
  
  #Plot
  
  pdf(file = paste0(trend_directory, "summaries/",
                    "Macrozone_", (i-1), ".pdf"), onefile = TRUE)
  
  #par(mfrow = c(1,1))
  #mat <- matrix(c(1, 1,  # Row 1
  #                3, 2), # Row 2
  #              nrow = 2, byrow = TRUE)
  #layout(mat)
  
  par(mfrow = c(3,2))
  
  #Macrozones, with macrozone boundary added
  terra::plot(macrozones_prepared, "label", col = adjustcolor(macrozones_prepared$color, alpha = 0.25),
              border = NA, cex.main = 0.5, sort = FALSE, plg=list(ncol = 2))
  terra::plot(macrozones_prepared[i,], lwd = 1, col = macrozones_prepared$color[i],
              add = TRUE, border = "royalblue4")
  terra::plot(all_ssa, border = "black", add = TRUE, lwd = 0.5)
  
  
  #Average elevation
  plot(1:3, 1:3, axes = FALSE, ann = FALSE, type = "n")
  text(x = 2, y = 2.5, labels = paste0("Zone ", macrozones_prepared$label[i]), font.main = 2, cex = 3)
  text(x = 2, y = 1.5, labels = paste0("Average elevation = ", 
                                       round(elevation$value[i], digits = 0), "m"), 
       pos = 3, col = "black")
  
  
  #4x4 plots of trends
  
  #Maximum Temperature
  trend_df <- summaries[[which(names(summaries) == "max_temperature")]]
  names(trend_df) <- as.character(months)
  
  plot(months, trend_df[i,], xlab = "Month", ylab = "Maximum Temperature, \u00B0C",
       type = "l", col = macrozones_prepared$color[i], lwd = 2, 
       main = "Average Zone Maximum Temperature", cex.main = 0.75)
  
  
  #Temperature Range
  trend_df <- summaries[[which(names(summaries) == "temp_range")]]
  names(trend_df) <- as.character(months)
  
  plot(months, trend_df[i,], xlab = "Month", ylab = "Temperature Range, \u00B0C",
       type = "l", col = macrozones_prepared$color[i], lwd = 2, 
       main = "Average Zone Temperature Range", cex.main = 0.75)
  
  
  #Total Precipitation
  trend_df <- summaries[[which(names(summaries) == "total_precipitation")]]
  names(trend_df) <- as.character(months)
  
  plot(months, trend_df[i,], xlab = "Month", ylab = "Total Precipitation, mm",
       type = "l", col = macrozones_prepared$color[i], lwd = 2, 
       main = "Average Zone Total Precipitation", cex.main = 0.75)
  
  
  #Daylength
  trend_df <- summaries[[which(names(summaries) == "daylength")]]
  names(trend_df) <- as.character(months)
  
  plot(months, trend_df[i,], xlab = "Month", ylab = "Daylength",
       type = "l", col = macrozones_prepared$color[i], lwd = 2, 
       main = "Average Zone Daylength", cex.main = 0.75)
  
  
  par(mfrow = c(2,2))
  
  #Get trends with map of macrozones
  
  #Max temperature
  index <- as.integer(max_temperature)
  trend_plot_mac <- get_trend_plots_macrozone(feature_name = "max_temperature", 
                                              summaries = summaries,
                                              clusters = clusters, 
                                              index = index, 
                                              macrozones_prepared = macrozones_prepared)
  
  terra::plot(macrozones_prepared, "label", col = adjustcolor(macrozones_prepared$color, alpha = 0.25),
              border = NA, main = "Macrozones with similar maximum temperature trends",
              cex.main = 0.5, legend = NULL, sort = FALSE, plg = list(ncol = 2))
  
  terra::plot(m_temp_macs, "label", col = m_temp_macs$color,
              border = "royalblue4", sort = FALSE, add = TRUE)
  
  terra::plot(all_ssa, border = "black", add = TRUE, lwd = 0.5)
  
  
  
  
  
  
  #Temperature range
  index <- as.integer(temp_range)
  trend_plot_mac <- get_trend_plots_macrozone(feature_name = "temp_range", 
                                              summaries = summaries,
                                              clusters = clusters, 
                                              index = index,
                                              macrozones_prepared = macrozones_prepared)
  
  terra::plot(macrozones_prepared, "label", col = adjustcolor(macrozones_prepared$color, alpha = 0.25),
              border = NA, main = "Macrozones with similar temperature range trends",
              cex.main = 0.5, legend = NULL, sort = FALSE, plg = list(ncol = 2))
  
  terra::plot(temp_r_macs, "label", col = temp_r_macs$color,
              border = "royalblue4", sort = FALSE, add = TRUE)
  
  terra::plot(all_ssa, border = "black", add = TRUE, lwd = 0.5)
  
  
  
  
  #Total precipitation
  index <- as.integer(total_precipitation)
  trend_plot_mac <- get_trend_plots_macrozone(feature_name = "total_precipitation", 
                                              summaries = summaries,
                                              clusters = clusters, 
                                              index = index,
                                              macrozones_prepared = macrozones_prepared)
  
  terra::plot(macrozones_prepared, "label", col = adjustcolor(macrozones_prepared$color, alpha = 0.25),
              border = NA, main = "Macrozones with similar total precipitation trends",
              cex.main = 0.5, legend = NULL, sort = FALSE, plg = list(ncol = 2))
  
  terra::plot(t_prec_macs, "label", col = t_prec_macs$color,
              border = "royalblue4", sort = FALSE, add = TRUE)
  
  terra::plot(all_ssa, border = "black", add = TRUE, lwd = 0.5)
  
  
  
  
  #Daylength
  index <- as.integer(daylength)
  trend_plot_mac <- get_trend_plots_macrozone(feature_name = "daylength", 
                                              summaries = summaries,
                                              clusters = clusters, 
                                              index = index,
                                              macrozones_prepared = macrozones_prepared)
  
  terra::plot(macrozones_prepared, "label", col = adjustcolor(macrozones_prepared$color, alpha = 0.25),
              border = NA, main = "Macrozones with similar daylength trends",
              cex.main = 0.5, legend = NULL, sort = FALSE, plg = list(ncol = 2))
  
  terra::plot(daylen_macs, "label", col = daylen_macs$color,
              border = "royalblue4", sort = FALSE, add = TRUE)
  
  terra::plot(all_ssa, border = "black", add = TRUE, lwd = 0.5)
  
  
  
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
            border = "royalblue4", main = "Macrozones for SSA, 25 class model",
            cex.main = 0.75, sort = FALSE)
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















