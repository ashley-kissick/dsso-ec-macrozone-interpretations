#Macrozone visualizations

#colors_macrozones, color_list
#summer_vis_8 (segment_map, segment_municipality_map)
#macrozones_poly

macrozone_directory <- paste0(documentation_directory, "Macrozone_interpretation/")
macrozone_map_directory <- paste0(visualization_directory, "macrozone_segments/")

#For the 3 summer groups:
macrozone_map_summer_3_directory <- paste0(visualization_directory, "summer_3/macrozone_segments/")

#Plot a product segment with its macrozones

#Plot a product segment with weather or daylength spatial trends
feature_list <- list()
feature_list$avg_max_temp <- readOGR(layer = "avg_max_temp", dsn = macrozone_directory)
feature_list$avg_temp_range <- readOGR(layer = "avg_temp_range", dsn = macrozone_directory)
feature_list$total_precip <- readOGR(layer = "total_precip", dsn = macrozone_directory)
feature_list$daylength <- readOGR(layer = "daylength", dsn = macrozone_directory)

features <- c("avg_max_temp", "avg_temp_range", "total_precip", "daylength")


#----------------------------------------------------------------------------------
#SUMMER (6 segments)

# (individual product segments with macrozones and trends)
season <- "summer"
segment_map <- summer_vis$segment_map
mac_maps <- get_macrozone_percent_maps(macrozone_spdf = macrozones_poly,
                                       segment_map = segment_map,
                                       season = season,
                                       region = NULL,
                                       macrozones = TRUE,
                                       feature_name = NULL,
                                       map_directory = macrozone_map_directory)

for (i in 1:length(features)) {

  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_macrozone_percent_maps(macrozone_spdf = feature_spdf,
                                             segment_map = segment_map,
                                             season = season,
                                             region = NULL,
                                             macrozones = FALSE,
                                             feature_name = feature_name,
                                             map_directory = macrozone_map_directory)

}

#----------------------------------
#  subtropical 
#  (maps of macrozones and trends)
region <- "subtropical"
subtropical_segment_map <- segment_map[1:3,]
subtropical_segment_map@data$region <- "region"
segment_map_union <- get_union_poly(original_spdf = subtropical_segment_map, id_column = "region")

mac_maps <- get_macrozone_percent_maps(macrozone_spdf = macrozones_poly,
                                       segment_map = segment_map_union,
                                       season = season,
                                       region <- region,
                                       macrozones = TRUE,
                                       feature_name = NULL,
                                       map_directory = macrozone_map_directory)

for (i in 1:length(features)) {
  
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_macrozone_percent_maps(macrozone_spdf = feature_spdf,
                                             segment_map = segment_map_union,
                                             season = season,
                                             region = region,
                                             macrozones = FALSE,
                                             feature_name = feature_name,
                                             map_directory = macrozone_map_directory)
}


#Plot map of macrozones and trends in subtropical region with product segments
sum_sub <- get_env_plot(macrozone_spdf = macrozones_poly, 
                        segment_map = segment_map,
                        segment_from = 1, 
                        segment_to = 3,
                        season = season, 
                        region = region, 
                        feature_name = "macrozones", 
                        map_directory = macrozone_map_directory,
                        macrozones = TRUE)

for (i in 1:length(features)) {
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_env_plot(macrozone_spdf = feature_spdf,
                               segment_map = segment_map,
                               segment_from = 1, 
                               segment_to = 3,
                               season = season,
                               region = region,
                               macrozones = FALSE,
                               feature_name = feature_name,
                               map_directory = macrozone_map_directory)
}





#----------------------------------
#  tropical 
#  (maps of macrozones and trends)
region <- "tropical"
tropical_segment_map <- segment_map[4:6,]
tropical_segment_map@data$region <- "region"
segment_map_union <- get_union_poly(original_spdf = tropical_segment_map, id_column = "region")

mac_maps <- get_macrozone_percent_maps(macrozone_spdf = macrozones_poly,
                                       segment_map = segment_map_union,
                                       season = season,
                                       region <- region,
                                       macrozones = TRUE,
                                       feature_name = NULL,
                                       map_directory = macrozone_map_directory)

for (i in 1:length(features)) {
  
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_macrozone_percent_maps(macrozone_spdf = feature_spdf,
                                             segment_map = segment_map_union,
                                             season = season,
                                             region = region,
                                             macrozones = FALSE,
                                             feature_name = feature_name,
                                             map_directory = macrozone_map_directory)
}


#Plot map of macrozones and trends in subtropical region with product segments
sum_sub <- get_env_plot(macrozone_spdf = macrozones_poly, 
                        segment_map = segment_map,
                        segment_from = 4, 
                        segment_to = 6,
                        season = season, 
                        region = region, 
                        feature_name = "macrozones", 
                        macrozones = TRUE,
                        map_directory = macrozone_map_directory)

for (i in 1:length(features)) {
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_env_plot(macrozone_spdf = feature_spdf,
                               segment_map = segment_map,
                               segment_from = 4, 
                               segment_to = 6,
                               season = season,
                               region = region,
                               macrozones = FALSE,
                               feature_name = feature_name,
                               map_directory = macrozone_map_directory)
}













#----------------------------------------------------------------------------------
#SUMMER (3 segments)

# (individual product segments with macrozones and trends)
season <- "summer"
segment_map <- summer_vis$segment_map
mac_maps <- get_macrozone_percent_maps(macrozone_spdf = macrozones_poly,
                                       segment_map = segment_map,
                                       season = season,
                                       region = NULL,
                                       macrozones = TRUE,
                                       feature_name = NULL,
                                       map_directory = macrozone_map_summer_3_directory)

for (i in 1:length(features)) {
  
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_macrozone_percent_maps(macrozone_spdf = feature_spdf,
                                             segment_map = segment_map,
                                             season = season,
                                             region = NULL,
                                             macrozones = FALSE,
                                             feature_name = feature_name,
                                             map_directory = macrozone_map_summer_3_directory)
  
}

#----------------------------------
#  subtropical 
#  (maps of macrozones and trends)
region <- "subtropical"
subtropical_segment_map <- segment_map[1,]
subtropical_segment_map@data$region <- "region"
segment_map_union <- get_union_poly(original_spdf = subtropical_segment_map, id_column = "region")

mac_maps <- get_macrozone_percent_maps(macrozone_spdf = macrozones_poly,
                                       segment_map = segment_map_union,
                                       season = season,
                                       region <- region,
                                       macrozones = TRUE,
                                       feature_name = NULL,
                                       map_directory = macrozone_map_summer_3_directory)

for (i in 1:length(features)) {
  
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_macrozone_percent_maps(macrozone_spdf = feature_spdf,
                                             segment_map = segment_map_union,
                                             season = season,
                                             region = region,
                                             macrozones = FALSE,
                                             feature_name = feature_name,
                                             map_directory = macrozone_map_summer_3_directory)
}


#Plot map of macrozones and trends in subtropical region with product segments
sum_sub <- get_env_plot(macrozone_spdf = macrozones_poly, 
                        segment_map = segment_map,
                        segment_from = 1, 
                        segment_to = 1,
                        season = season, 
                        region = region, 
                        feature_name = "macrozones", 
                        map_directory = macrozone_map_summer_3_directory,
                        macrozones = TRUE)

for (i in 1:length(features)) {
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_env_plot(macrozone_spdf = feature_spdf,
                               segment_map = segment_map,
                               segment_from = 1, 
                               segment_to = 1,
                               season = season,
                               region = region,
                               macrozones = FALSE,
                               feature_name = feature_name,
                               map_directory = macrozone_map_summer_3_directory)
}





#----------------------------------
#  tropical 
#  (maps of macrozones and trends)
region <- "tropical"
tropical_segment_map <- segment_map[2:3,]
tropical_segment_map@data$region <- "region"
segment_map_union <- get_union_poly(original_spdf = tropical_segment_map, id_column = "region")

mac_maps <- get_macrozone_percent_maps(macrozone_spdf = macrozones_poly,
                                       segment_map = segment_map_union,
                                       season = season,
                                       region <- region,
                                       macrozones = TRUE,
                                       feature_name = NULL,
                                       map_directory = macrozone_map_summer_3_directory)

for (i in 1:length(features)) {
  
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_macrozone_percent_maps(macrozone_spdf = feature_spdf,
                                             segment_map = segment_map_union,
                                             season = season,
                                             region = region,
                                             macrozones = FALSE,
                                             feature_name = feature_name,
                                             map_directory = macrozone_map_summer_3_directory)
}


#Plot map of macrozones and trends in subtropical region with product segments
sum_sub <- get_env_plot(macrozone_spdf = macrozones_poly, 
                        segment_map = segment_map,
                        segment_from = 2, 
                        segment_to = 3,
                        season = season, 
                        region = region, 
                        feature_name = "macrozones", 
                        macrozones = TRUE,
                        map_directory = macrozone_map_summer_3_directory)

for (i in 1:length(features)) {
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_env_plot(macrozone_spdf = feature_spdf,
                               segment_map = segment_map,
                               segment_from = 2, 
                               segment_to = 3,
                               season = season,
                               region = region,
                               macrozones = FALSE,
                               feature_name = feature_name,
                               map_directory = macrozone_map_summer_3_directory)
}

























#----------------------------------------------------------------------------------
#SAFRINHA 

# (individual product segments with macrozones and trends)
season <- "safrinha"
segment_map <- safrinha_vis_6$segment_map
mac_maps <- get_macrozone_percent_maps(macrozone_spdf = macrozones_poly,
                                       segment_map = segment_map,
                                       season = season,
                                       region = NULL,
                                       macrozones = TRUE,
                                       feature_name = NULL,
                                       map_directory = macrozone_map_directory)

for (i in 1:length(features)) {
  
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_macrozone_percent_maps(macrozone_spdf = feature_spdf,
                                             segment_map = segment_map,
                                             season = season,
                                             region = NULL,
                                             macrozones = FALSE,
                                             feature_name = feature_name,
                                             map_directory = macrozone_map_directory)
  
}

#----------------------------------
#  subtropical 
#  (maps of macrozones and trends)
region <- "subtropical"
subtropical_segment_map <- segment_map[5:6,]
subtropical_segment_map@data$region <- "region"
segment_map_union <- get_union_poly(original_spdf = subtropical_segment_map, id_column = "region")

mac_maps <- get_macrozone_percent_maps(macrozone_spdf = macrozones_poly,
                                       segment_map = segment_map_union,
                                       season = season,
                                       region <- region,
                                       macrozones = TRUE,
                                       feature_name = NULL,
                                       map_directory = macrozone_map_directory)

for (i in 1:length(features)) {
  
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_macrozone_percent_maps(macrozone_spdf = feature_spdf,
                                             segment_map = segment_map_union,
                                             season = season,
                                             region = region,
                                             macrozones = FALSE,
                                             feature_name = feature_name,
                                             map_directory = macrozone_map_directory)
}


#Plot map of macrozones and trends in subtropical region with product segments
saf_sub <- get_env_plot(macrozone_spdf = macrozones_poly, 
                        segment_map = segment_map,
                        segment_from = 5, 
                        segment_to = 6,
                        season = season, 
                        region = region, 
                        feature_name = "macrozones",
                        macrozones = TRUE,
                        map_directory = macrozone_map_directory)

for (i in 1:length(features)) {
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_env_plot(macrozone_spdf = feature_spdf,
                               segment_map = segment_map,
                               segment_from = 5, 
                               segment_to = 6,
                               season = season,
                               region = region,
                               macrozones = FALSE,
                               feature_name = feature_name,
                               map_directory = macrozone_map_directory)
}





#----------------------------------
#  tropical 
#  (maps of macrozones and trends)
region <- "tropical"
tropical_segment_map <- segment_map[1:4,]
tropical_segment_map@data$region <- "region"
segment_map_union <- get_union_poly(original_spdf = tropical_segment_map, id_column = "region")

mac_maps <- get_macrozone_percent_maps(macrozone_spdf = macrozones_poly,
                                       segment_map = segment_map_union,
                                       season = season,
                                       region <- region,
                                       macrozones = TRUE,
                                       feature_name = NULL,
                                       map_directory = macrozone_map_directory)

for (i in 1:length(features)) {
  
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_macrozone_percent_maps(macrozone_spdf = feature_spdf,
                                             segment_map = segment_map_union,
                                             season = season,
                                             region = region,
                                             macrozones = FALSE,
                                             feature_name = feature_name,
                                             map_directory = macrozone_map_directory)
}


#Plot map of macrozones and trends in subtropical region with product segments
saf_trop <- get_env_plot(macrozone_spdf = macrozones_poly, 
                         segment_map = segment_map,
                         segment_from = 1, 
                         segment_to = 4,
                         season = season, 
                         region = region, 
                         feature_name = "macrozones", 
                         macrozones = TRUE,
                         map_directory = macrozone_map_directory)

for (i in 1:length(features)) {
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_env_plot(macrozone_spdf = feature_spdf,
                               segment_map = segment_map,
                               segment_from = 1, 
                               segment_to = 4,
                               season = season,
                               region = region,
                               macrozones = FALSE,
                               feature_name = feature_name,
                               map_directory = macrozone_map_directory)
}







  




#sub_trop_macs <- raster::crop(macrozones_poly, segment_map_union)
#plot(sub_trop_macs, lwd = 0.5)
#for (i in 1:dim(sub_trop_macs)[1]) {
#  mac_col <- colors_macrozones[sub_trop_macs@data[i,]]
#  plot(sub_trop_macs[i,], col = mac_col, add = TRUE, border = FALSE)
#}
#plot(segment_map[1:3,], add = TRUE, lwd = 1.5)











#----------------------------------------------------------------------------------


#SAFRINHA

# (individual product segments with macrozones and trends)
season <- "safrinha"
segment_map <- safrinha_vis_10$segment_map
mac_maps <- get_macrozone_percent_maps(macrozone_spdf = macrozones_poly,
                                       segment_map = segment_map,
                                       season = season,
                                       region = NULL,
                                       macrozones = TRUE,
                                       feature_name = NULL,
                                       map_directory = macrozone_map_directory)

features <- c("avg_max_temp", "avg_temp_range", "total_precip", "daylength")
for (i in 1:length(features)) {
  
  feature_name <- features[i]
  feature_spdf <- feature_list[[i]]
  feature_maps <- get_macrozone_percent_maps(macrozone_spdf = feature_spdf,
                                             segment_map = segment_map,
                                             season = season,
                                             region = NULL,
                                             macrozones = FALSE,
                                             feature_name = feature_name,
                                             map_directory = macrozone_map_directory)
  
}

