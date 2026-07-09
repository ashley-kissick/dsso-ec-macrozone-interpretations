library(devtools)
devtools::load_all()


library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyr)
if(!require(patchwork)) install.packages("patchwork")
library(patchwork)

#Format data to right format.
#Run the analysis



#data:
data_dir <- "../data/microenvironments/"

#microenvironments
#microenvironments <- terra::vect(paste0(data_dir, "macrozones/emea_ssa_240802_M025.geojson"))

#centroids (average of each feature, acrosss zone boundaries)
centroids <- read.csv(paste0(data_dir, "hac_0250_ec_feature_centroids.csv"),
                      header = TRUE)
centroids <- centroids[,c("label", names(centroids)[1:7])]
#rownames(centroids) <- centroids$label

#Get the microenvironments of interest in the fields
fields <- readxl::read_excel(paste0(data_dir, "shortstature_hybrids_fields_26_master_file.xlsx"))
fields <- data.frame(fields)
classes <- fields$ec_class_percentages

field_list <- list()
for (i in 1:nrow(fields)) {
  
  field <- fields[i,]
  
  if(is.na(field$ec_class_percentages)) {next}
  
  class_row <- field$ec_class_percentages
  check <- strsplit(class_row, split = "%")
  check <- strsplit(check[[1]], split = ":")
  check <- lapply(check, function(x) {gsub(", ", "", x)})
  check <- lapply(check, function(x) {gsub(" ", "", x)})
  check <- lapply(check, function(x) {gsub("us-", "", x)})
  
  df <- as.data.frame(check)
  df <- t(df)
  colnames(df) <- c("label", "percent")
  rownames(df) <- NULL
  df <- as.data.frame(df)
  df$field <- field$fieldName
  
  field_list[[i]] <- df
}

all_fields <- do.call(rbind, field_list)
zones <- as.integer(all_fields$label)



#Get the centroids that correspond to those zones
field_centroids <- centroids[which(centroids$label %in% zones == TRUE),]
rownames(field_centroids) <- as.character(field_centroids$label)
field_centroids <- field_centroids[,2:ncol(field_centroids)]

microenvironment_labels <- zones
num_microenvironments <- length(zones)
features <- c("elevation", "slope", "hli", "aws0_20", "soc0_20", "rootznemc", "rootznaws")
groups <- 20 #cutoff for scree plot
trend_directory <- "../data/microenvironments/plots/"

feature_clusters <- get_macrozone_clusters(feature_summary = field_centroids, 
                                           feature_name = NULL,
                                           groups = groups,
                                           trend_directory = trend_directory)

names(feature_clusters) <- paste0("cluster_", 1:length(feature_clusters))

#For each cluster, subset the data frame and conduct the summaries for each feature.  
summary_list <- list()
for (i in 1:length(feature_clusters)) {
  
  cluster <- as.character(unique(feature_clusters[[i]]))
  df <- field_centroids[which(rownames(field_centroids) %in% cluster == TRUE),]
  df$Data_Type = "Group Observation"
  df$Group_ID <- names(feature_clusters)[i]
  df$Subject_ID <- paste0("Ind_", 1:nrow(df))
  df$label <- rownames(df)
  
  two_char <- nchar(df$label)
  idx <- which(two_char == 2)
  
  #If the label has only two letters, add back the 0
  df$label[idx] <- paste0("0", df$label[idx]) 

  summary_list[[i]] <- df
  
}

names(summary_list) <- paste0("cluster_", 1:14)

summaries <- do.call(rbind, summary_list)
#summaries contains the features, the cluster assignment, and 
#its rownames indicates the microzone label.


#Get the microenviornments that potentially belong to the same cluster:
dims <- lapply(summary_list, function(x) {dim(x)[1]} )
dims <- do.call(c, dims)

dup_clst <- which(dims > 1)
dup_clsts <- summary_list[c(dup_clst)]

#Get the fields with these zones
#dup_flds <- lapply(dup_clsts, check_fields, to_do = to_do)


#Merge cluster assignments
df_merge <- merge(all_fields, summaries, by = "label", all = FALSE)
all_fields <- df_merge
all_fields$percent <- as.numeric(all_fields$percent)


#Add in the folder
names(fields)[which(names(fields) == "fieldName")] <- "field"
all_fields <- merge(all_fields, fields[,c("field", "feature_id")], by = "field", all = FALSE)
names(all_fields)[which(names(all_fields) == "feature_id")] <- "folder"
all_fields$folder <- gsub("}", "", all_fields$folder, fixed = TRUE)
all_fields$folder <- gsub("{", "", all_fields$folder, fixed = TRUE)



#Flag rows where there are fields with redundant zones in a cluster
flagged_df <- all_fields %>%
  group_by(field, Group_ID) %>%
  mutate(is_duplicate_in_field = n() > 1) 
flagged_df <- data.frame(flagged_df)
head(flagged_df)


#Flag the number of zones by field that are less than 2% of the field
flagged_df <- flagged_df %>% group_by(field, Group_ID) %>%
  mutate(small_area = percent < 2) %>%
  ungroup()
flagged_df <- data.frame(flagged_df)
head(flagged_df)

small_area <- which(flagged_df$small_area == TRUE)
length(small_area) #Looks like 127 rows

#Let's go ahead and remove them, dropping to 152 rows
high_percent <- flagged_df[-small_area,]


# Find repeated values within each group
repeated_values <- high_percent %>%
  group_by(field) %>%
  filter(duplicated(Group_ID) | duplicated(Group_ID, fromLast = TRUE)) %>%
  ungroup()
repeated_values <- data.frame(repeated_values)

to_check <- repeated_values[,c("field", "Group_ID", "label", "percent")]

check_repeats <- to_check %>%
  ungroup() %>%
  group_by(Group_ID) %>%
  arrange(field, .by_group = TRUE)
check_repeats <- data.frame(check_repeats)
check_repeats <- check_repeats[,c("Group_ID", "field", "label", "percent")]
check_repeats


c_12 <- check_repeats[which(check_repeats$Group_ID == "cluster_12"), 1:3]
c_12

c_14 <- check_repeats[which(check_repeats$Group_ID == "cluster_14"), 1:3]
c_14

c_9 <- check_repeats[which(check_repeats$Group_ID == "cluster_9"), 1:3]
c_9

#Looks like we can examine values between: 
#cluster 14 (label 146 and 148), (140, 148)
#cluster 9 (label 141, 142), label (136, 141, 142)
#cluster 12 (label 096, 090, 095), (090, 096)



#Let's try the multivariate analysis to see how similar these zones generally
#are in the microenviornment multivariate space. 

#Get all the zones per cluster to do the plots, going back to all_fields.  
zones <- all_fields
zones$Subject_ID <- zones$label
zones <- zones[,4:13]
names(zones)[1:7] <- paste0("Feature_", names(zones)[1:7])
zones <- unique(zones)

#Overall feature space
global_background <- field_centroids
names(global_background) <- paste0("Feature_", names(global_background))
global_background$Data_Type <- "Global Background Context"
global_background$Group_ID  = "None"
global_background$Subject_ID <- "None"

similar_clusters <- repeated_values
similar_clusters$Subject_ID <- similar_clusters$label
similar_clusters <- similar_clusters[,4:13]
names(similar_clusters)[1:7] <- paste0("Feature_", names(similar_clusters)[1:7])
similar_clusters <- unique(similar_clusters)

all_observations <- rbind(global_background, zones)


#Multivariate plots are in 'microenvironment_AI_code_modified.R


#Let's consider merging these zones together:

#142 and 136 (cluster 9)
#090, 095, 096 (cluster 12)
#146, 148 (cluster 14)




#Let's now plot the fields with the redundant EC's.

ec_folder_dir <- "../data/microenvironments/fields/"

#c_9 <- with_folders[which(with_folders$Group_ID == "cluster_9"),]
#c_12 <- with_folders[which(with_folders$Group_ID == "cluster_12"),]
#c_14 <- with_folders[which(with_folders$Group_ID == "cluster_14"),]



#folders <- unique(c_12$feature_id)
save_dir <- "../data/microenvironments/EC_shp/"
folders <- list.files(ec_folder_dir)

#field_names <- unique(c_12$field)



for (i in 1:length(folders)) {
  
  folder <- folders[i]
  file <- paste0(ec_folder_dir, folder,"/EC_Cropped.geojson")
  check_exists <- file.exists(file)
  
  if(check_exists == FALSE) { next }
  
  ec <- terra::vect(paste0(ec_folder_dir, folder,"/EC_Cropped.geojson"))
  ec$hac2_0250_label <- as.character(ec$hac2_0250_label)
  ec$hac2_0250_label <- as.factor(ec$hac2_0250_label)
  
  # Set up a blank template raster (defines grid layout)
  template_raster <- terra::rast(extent = ext(ec), resolution = 0.0001, crs = crs(ec))
  
  # Rasterize the points
  # The 'field' argument chooses your data column; 'fun' determines how overlapping points aggregate
  r <- terra::rasterize(ec, template_raster, field = "hac2_0250_label", background = NA)
  #plot(r)
  
  #Make the raster categories a shapefile
  polygons <- terra::as.polygons(r, aggregate = TRUE)
  
  #Remove any NA's:  
  nas <-which(is.na(polygons$hac2_0250_label) == TRUE)
  if(length(nas) > 0) {
    polygons <- polygons[-nas,]
  }
  
  #Write the original one
  terra::writeVector(polygons, file = paste0(save_dir, folder, "_original.geojson"),
                     overwrite = TRUE)
  
  

  #Coallesce any repetitive zones
  #142 and 136 (cluster 9)
  #090, 095, 096 (cluster 12)
  #146, 148 (cluster 14)
  
  c_142 <- which(polygons$hac2_0250_label == "us-142")
  c_136 <- which(polygons$hac2_0250_label == "us-136")

  if(length(c_142) > 0 && length(c_136) > 0) { 
    polygons$hac2_0250_label[c_142] <- "us-136_142" 
    polygons$hac2_0250_label[c_136] <- "us-136_142" 
    polygons <- aggregate(polygons, by = "hac2_0250_label")
  } 
  
  c_090 <- which(polygons$hac2_0250_label == "us-090")
  c_095 <- which(polygons$hac2_0250_label == "us-095")
  c_096 <- which(polygons$hac2_0250_label == "us-096")
  
  if(length(c_090) > 0 && length(c_095) > 0 && length(c_096) > 0) { 
    polygons$hac2_0250_label[c_090] <- "new_c"
    polygons$hac2_0250_label[c_095] <- "new_c"
    polygons$hac2_0250_label[c_096] <- "new_c"
  }
  
  if(length(c_090) > 0 && length(c_095) > 0) { 
    polygons$hac2_0250_label[c_090] <- "new_c"
    polygons$hac2_0250_label[c_095] <- "new_c"
  }
  
  if(length(c_090) > 0 && length(c_096) > 0) { 
    polygons$hac2_0250_label[c_090] <- "new_c"
    polygons$hac2_0250_label[c_096] <- "new_c"
  }
  
  if(length(c_095) > 0 && length(c_096) > 0) { 
    polygons$hac2_0250_label[c_095] <- "new_c"
    polygons$hac2_0250_label[c_096] <- "new_c"
  }
 
  check_cover <- which(polygons$hac2_0250_label == "new_c")
  if(length(check_cover) > 1) {
    polygons$hac2_0250_label[check_cover] <- "us-090_095_096"
    polygons <- aggregate(polygons, by = "hac2_0250_label")
  }
  
  c_146 <- which(polygons$hac2_0250_label == "us-146")
  c_148 <- which(polygons$hac2_0250_label == "us-148")
  
  if(length(c_146) > 0 && length(c_148) > 0) { 
    polygons$hac2_0250_label[c_146] <- "us-146_148" 
    polygons$hac2_0250_label[c_148] <- "us-146_148" 
    polygons <- aggregate(polygons, by = "hac2_0250_label")
  }
  
  
  #Remove the classes in the field that were flagged to be small
  
  #terra::plot(polygons, "hac2_0250_label")
  flagged <- which(flagged_df$folder == folder)
  if(length(flagged) > 0) {
    
    fl <- flagged_df[flagged,]
    to_remove <- which(fl$small_area == TRUE)
    if(length(to_remove) > 0) {
      
      small_labels <- paste0("us-", fl$label[to_remove])
      remove_poly <- which(polygons$hac2_0250_label %in% small_labels == TRUE)
      if(length(remove_poly) > 0) {
        polygons$hac2_0250_label[remove_poly] <- "small_area"
      }
      
      #terra::plot(polygons, "hac2_0250_label")
      
    }                   
    
  }
  
  
  #cat_count <- length(unique(polygons$hac2_0250_label))
  #col_palette <- viridis::viridis(cat_count)
  terra::writeVector(polygons, file = paste0(save_dir, folder, "_simp.geojson"),
                     overwrite = TRUE)
  
}




#Let's plot original and simplified side by side:   

file_dir <- "../data/microenvironments/EC_shp/"
save_dir <- "../data/microenvironments/field_visuals/"
for(i in 1:length(folders)) {
  
  folder <- folders[i]
  o = paste0(file_dir, folder, "_original.geojson")
  s <- paste0(file_dir, folder, "_simp.geojson")
  
  o_file <- file.exists(o)
  s_file <- file.exists(s)

  if(o_file == FALSE || s_file == FALSE) { next }
    
  original <- terra::vect(o)
  simplify <- terra::vect(s)
    
  pdf(file = paste0(save_dir, folder, ".pdf"), width = 8, height = 8)
  par(mfrow = c(1,2))
  
  cat_count <- length(unique(original$hac2_0250_label))
  col_palette <- viridis::viridis(cat_count)
  terra::plot(original, "hac2_0250_label", col = col_palette)
  
  cat_count <- length(unique(simplify$hac2_0250_label))
  col_palette <- viridis::viridis(cat_count)
  terra::plot(simplify, "hac2_0250_label", col = col_palette)
  dev.off()  
  
}












#Take a look at individual distributions
#Take a look at each feature's distribution
ggplot(field_centroids, aes(x = Feature_elevation)) +
  geom_density(fill = "royalblue", alpha = 0.5) +
  labs(title = "Density Plot of elevation", x = "m", y = "Density") +
  theme_minimal()

#Take a look at each feature's distribution
ggplot(field_centroids, aes(x = Feature_slope)) +
  geom_density(fill = "royalblue", alpha = 0.5) +
  labs(title = "Density Plot of slope", x = "unknown", y = "Density") +
  theme_minimal()

#Take a look at each feature's distribution
ggplot(field_centroids, aes(x = Feature_hli)) +
  geom_density(fill = "royalblue", alpha = 0.5) +
  labs(title = "Density Plot of hli", x = "unknown", y = "Density") +
  theme_minimal()

#Take a look at each feature's distribution
ggplot(field_centroids, aes(x = Feature_aws0_20)) +
  geom_density(fill = "royalblue", alpha = 0.5) +
  labs(title = "Density Plot of aws", x = "unknown", y = "Density") +
  theme_minimal()

#Take a look at each feature's distribution
ggplot(field_centroids, aes(x = Feature_soc0_20)) +
  geom_density(fill = "royalblue", alpha = 0.5) +
  labs(title = "Density Plot of soc", x = "unknown", y = "Density") +
  theme_minimal()

#Take a look at each feature's distribution
ggplot(field_centroids, aes(x = Feature_rootznemc)) +
  geom_density(fill = "royalblue", alpha = 0.5) +
  labs(title = "Density Plot of rootznemc", x = "unknown", y = "Density") +
  theme_minimal()

#Take a look at each feature's distribution
ggplot(field_centroids, aes(x = Feature_rootznaws)) +
  geom_density(fill = "royalblue", alpha = 0.5) +
  labs(title = "Density Plot of rootzaws", x = "unknown", y = "Density") +
  theme_minimal()





#Cluster 9
#Similar enough aws0_20, soc0_20, rootznemc, rootznaws
#That leaves elevation, slope, hli

clust_9 <- clsts[[which(names(clsts) == "cluster_9")]]
#cluster 9 (label 141, 142), label (136, 141, 142)
to_check <- clust_9[which(rownames(clust_9) %in% as.character(c(136, 141, 142)) == TRUE), 1:7]



# Look at elevation (EC 142 diverges a little)
ec_136 <- to_check$Feature_elevation[1]
ec_141 <- to_check$Feature_elevation[2]
ec_142 <- to_check$Feature_elevation[3]

ggplot(field_centroids, aes(x = Feature_elevation)) +
  geom_density(fill = "royalblue", alpha = 0.3) +
  # Map 'color' INSIDE aes() to create a legend entry
  geom_vline(aes(xintercept = ec_136, color = "ec_136"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_141, color = "ec_141"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_142, color = "ec_142"), linetype = "dashed", linewidth = 1) +
  # Customize the legend colors and title
  scale_color_manual(name = "Metrics", 
                     values = c("ec_136" = "red", "ec_141" = "orange", "ec_142" = "forestgreen")) +
  theme_minimal()


# Look at slope (EC 136 diverges more)
ec_136 <- to_check$Feature_slope[1]
ec_141 <- to_check$Feature_slope[2]
ec_142 <- to_check$Feature_slope[3]

ggplot(field_centroids, aes(x = Feature_slope)) +
  geom_density(fill = "royalblue", alpha = 0.3) +
  # Map 'color' INSIDE aes() to create a legend entry
  geom_vline(aes(xintercept = ec_136, color = "ec_136"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_141, color = "ec_141"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_142, color = "ec_142"), linetype = "dashed", linewidth = 1) +
  # Customize the legend colors and title
  scale_color_manual(name = "Metrics", 
                     values = c("ec_136" = "red", "ec_141" = "orange", "ec_142" = "forestgreen")) +
  theme_minimal()




# Look at hli (EC 141 diverges more)
ec_136 <- to_check$Feature_hli[1]
ec_141 <- to_check$Feature_hli[2]
ec_142 <- to_check$Feature_hli[3]

ggplot(field_centroids, aes(x = Feature_hli)) +
  geom_density(fill = "royalblue", alpha = 0.3) +
  # Map 'color' INSIDE aes() to create a legend entry
  geom_vline(aes(xintercept = ec_136, color = "ec_136"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_141, color = "ec_141"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_142, color = "ec_142"), linetype = "dashed", linewidth = 1) +
  # Customize the legend colors and title
  scale_color_manual(name = "Metrics", 
                     values = c("ec_136" = "red", "ec_141" = "orange", "ec_142" = "forestgreen")) +
  theme_minimal()


#Overall:  
#cluster 9 (label 141, 142), label (136, 141, 142)
# Elevation (EC 142 diverges a little) 136 and 142 are most similar.
# Slope (EC 136 diverges more) 141 and 142 are most similar.
# HLI (EC 141 diverges more).  141 and 142 are most different.

##########


#Cluster 12
#Similar enough aws0_20, soc0_20, rootznemc, rootznaws
#That leaves elevation, slope, hli

clust_12 <- clsts[[which(names(clsts) == "cluster_12")]]
#cluster 9 (label 096, 090, 095), (090, 096)
to_check <- clust_12[which(rownames(clust_12) %in% as.character(c(090, 096, 095)) == TRUE), 1:7]

#Overall, I would be tempted to keep these separate.  



#########

#Cluster 14
#Similar enough aws0_20, soc0_20, rootznemc, rootznaws
#That leaves elevation, slope, hli

clust_14 <- clsts[[which(names(clsts) == "cluster_14")]]
#cluster 14(label 146 and 148), (140, 148)
to_check <- clust_14[which(rownames(clust_14) %in% as.character(c(140, 146, 148)) == TRUE), 1:7]



# Look at elevation (EC 142 diverges a little)
ec_140 <- to_check$Feature_elevation[1]
ec_146 <- to_check$Feature_elevation[2]
ec_148 <- to_check$Feature_elevation[3]

ggplot(field_centroids, aes(x = Feature_elevation)) +
  geom_density(fill = "royalblue", alpha = 0.3) +
  # Map 'color' INSIDE aes() to create a legend entry
  geom_vline(aes(xintercept = ec_136, color = "ec_136"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_141, color = "ec_141"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_142, color = "ec_142"), linetype = "dashed", linewidth = 1) +
  # Customize the legend colors and title
  scale_color_manual(name = "Metrics", 
                     values = c("ec_136" = "red", "ec_141" = "orange", "ec_142" = "forestgreen")) +
  theme_minimal()


# Look at slope (EC 136 diverges more)
ec_136 <- to_check$Feature_slope[1]
ec_141 <- to_check$Feature_slope[2]
ec_142 <- to_check$Feature_slope[3]

ggplot(field_centroids, aes(x = Feature_slope)) +
  geom_density(fill = "royalblue", alpha = 0.3) +
  # Map 'color' INSIDE aes() to create a legend entry
  geom_vline(aes(xintercept = ec_136, color = "ec_136"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_141, color = "ec_141"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_142, color = "ec_142"), linetype = "dashed", linewidth = 1) +
  # Customize the legend colors and title
  scale_color_manual(name = "Metrics", 
                     values = c("ec_136" = "red", "ec_141" = "orange", "ec_142" = "forestgreen")) +
  theme_minimal()




# Look at hli (EC 141 diverges more)
ec_136 <- to_check$Feature_hli[1]
ec_141 <- to_check$Feature_hli[2]
ec_142 <- to_check$Feature_hli[3]

ggplot(field_centroids, aes(x = Feature_hli)) +
  geom_density(fill = "royalblue", alpha = 0.3) +
  # Map 'color' INSIDE aes() to create a legend entry
  geom_vline(aes(xintercept = ec_136, color = "ec_136"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_141, color = "ec_141"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = ec_142, color = "ec_142"), linetype = "dashed", linewidth = 1) +
  # Customize the legend colors and title
  scale_color_manual(name = "Metrics", 
                     values = c("ec_136" = "red", "ec_141" = "orange", "ec_142" = "forestgreen")) +
  theme_minimal()


#Overall:  
#cluster 9 (label 141, 142), label (136, 141, 142)
# Elevation (EC 142 diverges a little) 136 and 142 are most similar.
# Slope (EC 136 diverges more) 141 and 142 are most similar.
# HLI (EC 141 diverges more).  141 and 142 are most different.

##########










#Master list of fields to review:  
flds_v <- unique(do.call(c, flds))

folder_v <- vector()
for (i in 1:length(flds_v)) {
  folder_v[i] <- fields$feature_id[which(fields$fieldName %in% flds_v[i] == TRUE)]
}

fields_list <- list()
#for (i in 1:length(folder_v)) {
for (i in 1:length(flds)) {  
  #folder <- gsub("\\{", "", folder_v[i])
  #folder <- gsub("\\}", "", folder)
  #ecs <- terra::vect(paste0("/Users/gkrbd/Downloads/fields/", folder, "/EC_Cropped.geojson"))
  #terra::plot(ecs, "hac2_0250_label", col = rainbow(50))
  
  #mics <- to_do[which(names(to_do) == flds_v[[i]])]
  
  #Get the clusters with microenvironments in this field
  clusters_v <- vector()
  for (j in 1:length(flds)) {
    fld <- flds[[j]]
    dl <- which(fld == flds_v[[1]])
    if(length(dl) > 0) { clusters_v[[j]] <- names(flds)[j] }
  }
  clusters_v <- clusters_v[!is.na(clusters_v)]
  
  fields_list[[i]] <- clusters_v

}
names(fields_list) <- flds_v









check_fields <- function(x, to_do) {
  
  labels <- x$label

  check_list <- list()
  for (i in 1:length(to_do)) {
    
    mic_in_fields <- to_do[[i]]
    
    if(length(labels) <= length(mic_in_fields)) {
      exam <- which(mic_in_fields %in% labels == TRUE)
    }
    
    if(length(labels) > length(mic_in_fields)) {
      exam <- which(labels %in% mic_in_fields == TRUE)
    }
    
    if(length(exam) < 2) { check_list[[i]] <- FALSE }
    if(length(exam) >= 2) { check_list[[i]] <- TRUE }
    
  }
  
  names(check_list) <- names(to_do)
  
  all_check <- do.call(c, check_list)
  fields_check <- names(all_check)[which(all_check == TRUE)]
  
  return(fields_check)
  
}



















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















