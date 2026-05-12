#Macrozone interpretations

#Centroids with standardized values were provided by Mike Martinez (Smart Operations).
#The function for returning them to their unstandardized value is found in "./R/spatial_data_formatting_functions.R".
#The result of the function is saved as "centroids.rda".

#Here, we generate summary statistics of the features in each group to get an idea of what they represent.
#This will aid the interpretation of BR product segments.

#While in the directory of 'segmentosbrasilieros' package:
library(devtools)
devtools::load_all()
library(zoo)

#trend_directory <- paste0(documentation_directory, "/Macrozone_interpretation/plots/")

#Plot macrozones with their colors
macrozone_number <- 1:40
table_df <- data.frame(layer = macrozone_number, col = colors_macrozones)
table_df$layer <- as.factor(table_df$layer)
table_df$col <- as.character(table_df$col)

#Make layers in macrozones_poly a factor (1-40)
levels_layer <- as.character(macrozone_number)
macrozones_poly@data$layer <- as.character(macrozones_poly@data$layer)
macrozones_poly@data$layer <- factor(macrozones_poly@data$layer, levels = levels_layer)

col_regions <- as.vector(table_df$col[match(levels(macrozones_poly@data$layer), table_df$layer)])


# Get the macrozone centroids
centroids_40 <- centroids[[1]]  #40 macrozones, the result of 373 features


#---------------------------------------------------------
#PLOTTING AND VISUALIZATIONS OF MACROZONES

#Plot the macrozones and save for visualizations
png(file = paste0(trend_directory, "Macrozones_40.png"),
    height = 2400, width = 2400, res = 300)
  spplot(macrozones_poly, zcol = "layer", col.regions = col_regions, lwd = 0.5)
dev.off()


#Plot each macrozone seperately
for (i in 1:length(macrozone_number)) {

  png(file = paste0(trend_directory, "Macrozone_plot_", i, ".png"),
      height = 2400, width = 2400, res = 300)
  plot(macrozones_poly, lwd = 0.5)
  plot(macrozones_poly[which(macrozones_poly@data$layer == i),], add = TRUE, col = "green")
  dev.off()

}

#---------------------------------------------------------



#---------------------------------------------------------
# ELEVATION

#Get average elevation of each macrozone
elevation_df <- data.frame(centroids_40$elevation)
rownames(elevation_df) <- paste0("Macrozone_", 1:40)
colnames(elevation_df) <- "elevation"

#Summarize elevation and plot
elevation <- sort(centroids_40[,grep("elevation", colnames(centroids_40))],
                  decreasing = TRUE)

png(file = paste(trend_directory, "Elevation_summary.png"),
    height = 2400, width = 4800, res = 300)
par(mfrow = c(1, 1))#, mar = c(1, 1, 1, 1))
barplot(elevation, col = colors_macrozones, main = "Average elevation within macrozones",
        xlab = "Macrozones", ylab = "Elevation",
        ylim = c(0, max(elevation) + 100), names.arg = c(1:40)
)
dev.off()
#---------------------------------------------------------





#---------------------------------------------------------

#Weather features (monthly, 2009-2018:
#  1) avg_max_temp
#  2) avg_temp_range
#  3) total_precip

# daylength (by month, no years)

#Create a monthly time series of weather features for each macrozone

features <- c("avg_max_temp", "avg_temp_range", "total_precip", "daylength")
years <- c(2009:2018)
months <- c(1:12)
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                 "Aug", "Sep", "Oct", "Nov", "Dec")
num_macrozones <- 40
save = TRUE
trend_directory <- "../data/messing_around/"

#There are so many trend lines it is difficult to distinguish trends.
#Let's work on that by getting an average trend across years.

#1) Get monthly trends (across years for weather data)
#2) Cluster them
#3) Get resulting shapefiles of underlining trends

macrozone_trends <- get_macrozone_trends(features = features, years = years,
                                         months = months, month_names = month_names,
                                         num_macrozones = num_macrozones,
                                         centroids = centroids_40,
                                         trend_directory = trend_directory,
                                         save = save)

#4) Plot the trends of each feature
# 'mac_row_col_params' defined by user based on the number of clusters selected in 'get_macrozone_trends'
# File to make modifications is 'mac_row_col_params.R'.

#a.  Plot trends all in one panel for the macrozone report
feature_plots <- get_feature_mac_plots(macrozone_trends = macrozone_trends,
                                       mac_row_col_params = mac_row_col_params,
                                       trend_directory = trend_directory,
                                       panel = TRUE, months = months)

#b.  Plot trends all in one panel for the macrozone report
feature_plots <- get_feature_mac_plots(macrozone_trends = macrozone_trends,
                                       mac_row_col_params = mac_row_col_params,
                                       trend_directory = trend_directory,
                                       panel = FALSE, months = months)












#Summarize each macrozone with average condition with the centoids of each zone

elevation <- centroids_40$elevation #Already average condition


#Growing season months: 1, 2, 3, 10, 11, 12
season <- c("_1a", "_2a", "_3a", "_10a","_11a", "_12a")
to_obtain <- centroids_40
colnames(to_obtain) <- paste0(colnames(to_obtain), "a")

weather <- to_obtain[,grep(paste(season, collapse = "|"),
                              colnames(to_obtain), value = TRUE)]
colnames(weather) <- substr(colnames(weather), 1, nchar(colnames(weather)) - 1)


daylength <- weather[,grep("daylength", names(weather))]
precip <- weather[,grep("precip", names(weather))]
temp_r <- weather[,grep("temp_range", names(weather))]
temp_m <- weather[,grep("max_temp", names(weather))]

avg_daylength <- rowMeans(daylength)
avg_precip <- rowMeans(precip)
avg_temp_r <- rowMeans(temp_r)
avg_temp_m <- rowMeans(temp_m)



barplot(avg_daylength)
barplot(avg_precip)
barplot(avg_temp_r)
barplot(avg_temp_m)



for (i in 1:40) {




}














#Now, lets' summarize each macrozone:
macrozone_list <- get_macrozone_summary(weather_macrozone_clusters = weather_macrozone_clusters,
                                        daylength_clusters = daylength_clusters,
                                        centroids = centroids_40)


#make the plot

max_temp <- macrozone_list[[1]]
temp_range <- macrozone_list[[2]]
precip <- macrozone_list[[3]]
daylength <- macrozone_list[[4]]

for (i in 1:nrow(centroids_40)) {

  png(file = paste0(trend_directory, "Macrozone_", i, "_summary.png"),
      height = 2400, width = 2400, res = 300)
  par(mfrow = c(2, 2))

  #Max temperature
  max_temp_df <- max_temp[[i]]
  y_max_max_temp <- max(do.call(rbind, max_temp)) + 2
  max_temp_plot <- get_plot(feature_df <- max_temp_df,
                            feature_name <- "avg_max_temp",
                            y_max = y_max_max_temp)

  #Avg Temperature Range
  avg_temp_df <- temp_range[[i]]
  y_max_avg_temp <- max(do.call(rbind, max_temp)) + 2
  avg_temp_plot <- get_plot(feature_df <- avg_temp_df,
                            feature_name <- "avg_temp_range",
                            y_max = y_max_avg_temp)

  #Total Precipitation
  precip_df <- precip[[i]]
  y_max_precip <- max(do.call(rbind, precip)) + 2
  precip_plot <- get_plot(feature_df <- precip_df,
                          feature_name <- "total_precip",
                          y_max = y_max_precip)

  #Day Length
  daylength_df <- daylength[[i]]
  y_max_daylength <- max(do.call(rbind, daylength)) + 2
  daylength_plot <- get_plot(feature_df <- daylength_df,
                             feature_name <- "daylength",
                             y_max = y_max_daylength)

  dev.off()

}



















#Plot each feature as a monthly time series by year
for (i in 1:length(weather_features)) {

  weather_feature <- centroids_40[,grep(weather_features[i], colnames(centroids_40))]

  par(mfrow = c(5,2))
  for (j in 1:length(years)) {

    dates <- paste0(month_names, "-", years[j])

    #Make sure columns are in the correct order
    col_order <- paste0(weather_features[i], "_", years[j], "_", months)
    year <- data.table(weather_feature[,grep(years[j], colnames(weather_feature))])
    setcolorder(year, col_order)
    year <- data.frame(year)

    y_min <- min(year)
    y_max <- max(year) + 2

    #Create a dataframe to plot for each macrozone
    to_plot <- data.frame(t(rbind(months, year[1,])))
    colnames(to_plot) <- c("Month", weather_features[i])
    to_plot$Month <- as.yearmon(c(dates), "%b-%Y")
    rownames(to_plot) <- NULL



    plot(to_plot[,2] ~ as.yearmon(c(to_plot$Month), "%b-%Y"),  xlab = "Month", ylab = weather_features[i],
         ylim = c(y_min, y_max), type = "l", col = colors_macrozones[1], lwd = 1, main = years[j])

    #Add lines for the remaining macrozones
    for(k in 2:nrow(year)) {

      to_plot <- data.frame(t(rbind(months, year[k,])))
      colnames(to_plot) <- c("Month", weather_features[j])
      to_plot$Month <- as.yearmon(c(dates), "%b-%Y")
      rownames(to_plot) <- NULL

      lines(to_plot[,2] ~ as.yearmon(c(to_plot$Month), "%b-%Y"),  xlab = "Month", ylab = weather_features[i],
            ylim = c(y_min, y_max), type = "l", col = colors_macrozones[k], lwd = 1)

    }

  }

}


















#Deprecated code:
#Summarize daylength (deprecated...)


#Plot trends of daylength
daylength_plots <- get_feature_mac_plots(macrozone_clusters = daylength_clusters,
                                         feature_summary = daylength,
                                         feature_name = "daylength",
                                         param_1 = param_1,
                                         param_2 = param_2,
                                         trend_directory = trend_directory)






year_cluster <- rbind(year_cluster, summary_cluster)

#Add large red line for the average trend line
for(k in 2:nrow(year)) {

  to_plot <- data.frame(t(rbind(months, year[k,])))
  colnames(to_plot) <- c("Month", weather_features[i])
  rownames(to_plot) <- NULL

  lines(to_plot[,2] ~ as.yearmon(c(to_plot$Month), "%b-%Y"),  xlab = "Month", ylab = weather_features[i],
        ylim = c(y_min, y_max), type = "l", col = colors_macrozones[k], lwd = 1)

}












y_min <- min(weather_summary)
y_max <- max(weathter_summary) + 2

#Create a dataframe to plot for each macrozone
to_plot <- data.frame(t(rbind(months, weather_summary[1,])))
colnames(to_plot) <- c("Month", weather_features[i])
rownames(to_plot) <- NULL
#to_plot$Month <- as.yearmon(c(dates), "%b")

par(mar=c(4,4,4,6), xpd = FALSE)
plot(to_plot[,2] ~ to_plot[,1],  xlab = "Month", ylab = weather_features[i],
     ylim = c(y_min, y_max), type = "l", col = colors_macrozones[1], lwd = 1, main = years[j])

#Add lines for the remaining macrozones
for(k in 2:nrow(weather_summary)) {

  to_plot <- data.frame(t(rbind(months, weather_summary[k,])))
  colnames(to_plot) <- c("Month", weather_features[j])
  rownames(to_plot) <- NULL

  lines(to_plot[,2] ~ to_plot[,1],  xlab = "Month", ylab = weather_features[i],
        ylim = c(y_min, y_max), type = "l", col = colors_macrozones[k], lwd = 1)



}
add_legend("right", c(paste0("macrozone ", 1:40)), lwd = rep(1, length = 40), col = colors_macrozones, bty = "n")






add_legend("right", "Mean", lwd = 5, lty = 3, col = "red", bty = "n")










#Cluster weather trends

for (i in 1:length(weather_features)) {

  weather_feature <- centroids_40[,grep(weather_features[i], colnames(centroids_40))]

  for (j in 1:length(years)) {

    dates <- paste0(month_names, "-", years[j])

    #Make sure columns are in the correct order
    col_order <- paste0(weather_features[i], "_", years[j], "_", months)
    year <- data.table(weather_feature[,grep(years[j], colnames(weather_feature))])
    setcolorder(year, col_order)
    year <- data.frame(year)

    #Perform cluster analysis on the weather feature for the given year
    dist_mat <- dist(weather_summary, method = "euclidean")
    dendrogram <- hclust(dist_mat, method = "ward.D2")

    ggplot(dendrogram$height %>%
             tibble::enframe() %>%
           add_column(groups = length(dendrogram$height):1) %>%
           rename(height=value),
           aes(x=groups, y=height)) +
      geom_point() +
      geom_line()

    number_clusters_prompt <- readline(prompt = "How many clusters? ")
    num_clusters <- as.integer(number_clusters_prompt)

    #Cluster information
    cluster_information <- list()

    #Get the groups with clusters and information they contain:
    clusters <- rect.hclust(dendrogram, num_clusters)

    #Get the average weather for each month for each group
    cluster_list <- list()

    for (k in 1:length(clusters)) {

      cluster <- clusters[[k]]
      year_cluster <- year[cluster,]

      summary_cluster <- colMeans((year_cluster))
      cluster_list[[k]] <- summary_cluster

    }

    cluster_list_b <- do.call(rbind, cluster_list)

      year_cluster <- rbind(year_cluster, summary_cluster)



      y_min <- min(year_cluster)
      y_max <- max(year_cluster) + 2

      #Create a dataframe to plot for each macrozone
      to_plot <- data.frame(t(rbind(months, year_cluster[1,])))
      colnames(to_plot) <- c("Month", weather_features[i])
      to_plot$Month <- as.yearmon(c(dates), "%b-%Y")
      rownames(to_plot) <- NULL

      plot(to_plot[,2] ~ as.yearmon(c(to_plot$Month), "%b-%Y"),  xlab = "Month", ylab = weather_features[i],
           ylim = c(y_min, y_max), type = "l", col = colors_macrozones[1], lwd = 1, main = years[j])

      #Add lines for the remaining macrozones
      for(m in 2:nrow(year_cluster) - 1) {

        to_plot <- data.frame(t(rbind(months, year[m,])))
        colnames(to_plot) <- c("Month", weather_features[j])
        to_plot$Month <- as.yearmon(c(dates), "%b-%Y")
        rownames(to_plot) <- NULL

        lines(to_plot[,2] ~ as.yearmon(c(to_plot$Month), "%b-%Y"),  xlab = "Month", ylab = weather_features[i],
              ylim = c(y_min, y_max), type = "l", col = colors_macrozones[m], lwd = 1)

      }

      to_plot <- data.frame(t(rbind(months, year_cluster[nrow(year_cluster),])))
      colnames(to_plot) <- c("Month", weather_features[i])
      to_plot$Month <- as.yearmon(c(dates), "%b-%Y")
      rownames(to_plot) <- NULL
      lines(to_plot[,2] ~ as.yearmon(c(to_plot$Month), "%b-%Y"),  xlab = "Month", ylab = weather_features[i],
            ylim = c(y_min, y_max), type = "l", col = "red", lwd = 3)








    }

  }







#Plot each feature each month across years
for (i in 1:length(weather_features)) {

  weather_feature <- centroids_40[,grep(weather_features[i], colnames(centroids_40))]

  par(mfrow = c(3,4))
  for (j in 1:length(months)) {

    year_columns <- grep(paste0("_", months[j]), colnames(weather_feature))
    month <- weather_feature[,year_columns]

    #remove "10", "11" and "12" if selected when months 1 and/or two are selected
    if(j == 1 | j == 2) {

      eleven <- grep("_11", colnames(month))
      twelve <- grep("_12", colnames(month))

      if(length(eleven) > 0 & length(twelve) > 0) {
        to_remove <- c(eleven, twelve)
        month <- month[,-to_remove]
      }

      if(length(eleven) > 0 & length(twelve) == 0) {
        to_remove <- eleven
        month <- month[,-to_remove]
      }
      if(length(eleven) == 0 & length(twelve) > 0) {
        to_remove <- twelve
        month <- month[,-to_remove]
      }

      #remove "10" if selected
      ten <- grep("_10", colnames(month))
      if(length(ten) > 0) {
        month <- month[,-ten]
      }

    }

    #Make sure columns are in the correct order
    col_order <- paste0(weather_features[i], "_", years, "_", months[j])
    month <- data.table(month)
    setcolorder(month, col_order)
    month <- data.frame(month)

    y_min <- min(month)
    y_max <- max(month) + 2

    #Create a dataframe to plot for each macrozone
    to_plot <- data.frame(t(rbind(years, month[1,])))
    colnames(to_plot) <- c("Year", weather_features[i])
    rownames(to_plot) <- NULL

    plot(to_plot[,2] ~ to_plot$Year,  xlab = "Year", ylab = weather_features[i],
         ylim = c(y_min, y_max), type = "l", col = colors_macrozones[1], lwd = 1, main = month_names[j])

    #Add lines for the remaining macrozones
    for(k in 2:nrow(month)) {

      to_plot <- data.frame(t(rbind(years, month[k,])))
      colnames(to_plot) <- c("Year", weather_features[i])
      rownames(to_plot) <- NULL

      lines(to_plot[,2] ~ to_plot$Year,  xlab = "Month", ylab = weather_features[i],
            ylim = c(y_min, y_max), type = "l", col = colors_macrozones[k], lwd = 1)

    }

  }




  }





  axis(1, to_plot$Month, format(to_plot$Month, "%b-%Y"), cex.axis = 0.7)

barplot(centroids_40[,1])
