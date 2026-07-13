
library(terra)
library(dplyr)
library(readxl)
library(sf)
library(spatstat)

#function
get_utm_zone <- function(lon, lat) { zone_num <- floor((lon + 180) / 6) + 1 }

#data directories:
data_dir <- "../data/microenvironments/"
file_dir <- paste0(data_dir, "/EC_shp/")
vis_dir <- paste0(data_dir, "/sample_visuals/")

#Let's get the fields and inbreds planted within them
#Get the microenvironments of interest in the fields
fieldsx <- readxl::read_excel(paste0(data_dir, "shortstature_hybrids_fields_26_master_file.xlsx"))
fieldsx <- data.frame(fieldsx)


field_inbred <- fieldsx[,c("feature_id", "fieldName", "female")]
names(field_inbred)[which(names(field_inbred) == "fieldName")] <- "field"
names(field_inbred)[which(names(field_inbred) == "feature_id")] <- "folder"
field_inbred$folder <- gsub("}", "", field_inbred$folder, fixed = TRUE)
field_inbred$folder <- gsub("{", "", field_inbred$folder, fixed = TRUE)

length(which(field_inbred$female == "F5130XYEJZ"))

#Get the field-inbred combinations

inbreds <- field_inbred %>%
  group_by(female) %>%
  mutate(field_number = n())
inbreds <- data.frame(inbreds)


female <- "R7550RQPEZ"
field <- inbreds$field[which(inbreds$female == female)]
folder <- inbreds$folder[which(inbreds$female == female)]


#Create inbred sampling logic
inbred_samples <- unique(inbreds[,c("female", "field_number")])
inbred_samples$n_sample <- NULL
inbred_samples$n_sample[which(inbred_samples$field_number == 1)] <- 20
inbred_samples$n_sample[which(inbred_samples$field_number == 2)] <- 10
inbred_samples$n_sample[which(inbred_samples$field_number == 3)] <- 8
inbred_samples$n_sample[which(inbred_samples$field_number == 7)] <- 5
inbred_samples$n_sample[which(inbred_samples$field_number == 12)] <- 5
inbred_samples$n_sample[which(inbred_samples$field_number == 13)] <- 5
inbred_samples <- inbred_samples[order(inbred_samples$field_number), ]






# Let's break each field apart by microenvironment and apply the rules:
#1. For each EC, calculate the area of the polygons composing it
#2. Remove any polygon areas < 2%
#3. Rank polygons by size and EC assignment.
#4. Calculate the centroid of the largest polygon (main sampling point)
#5. Get the number of unique sampling locations considering this rule.

folders <- list.files(file_dir)
folders <- gsub("_simp.geojson", "", folders)
folders <- gsub("_original.geojson", "", folders)
folders <- unique(folders)

full_names_simp <- list.files(file_dir, pattern = "_simp.geojson", full.names = TRUE)
full_names_orig <- list.files(file_dir, pattern = "_original.geojson", full.names = TRUE)

points_list <- list()
for (i in 1:length(full_names)) {

  folder <- folders[i]
  field_name <- inbreds$field[which(inbreds$folder == folder)]
  inbred <- inbreds$female[which(inbreds$folder == folder)]

  #Inbred logic
  nsamples <- inbred_samples$n_sample[which(inbred_samples$female == inbred)]


  #Load the field
  field_original <- terra::vect(full_names_orig[i])

  field <- terra::vect(full_names_simp[i])
  original_proj <- crs(field)

  #Aggregate for a single boundary
  field$field <- 1
  field_agg <- terra::aggregate(field, by = "field")

  #Get the field centroid and extract the X and Y coordinates
  field_centroid <- terra::centroids(field_agg, inside = TRUE)
  centroid_coords <- crds(field_centroid)

  #Get the UTM zone
  utm <- get_utm_zone(lon = centroid_coords[1], lat = centroid_coords[2])

  #Convert to UTM
  utm_field <- terra::project(field, paste0("+proj=utm +zone=", utm, " +north +datum=WGS84"))
  terra::plot(utm_field, "hac2_0250_label", main = field_name)

  #Split the polygons:
  # 1. Break multipart polygons into individual single-part geometries
  v_single <- terra::disagg(utm_field)

  # 2. Create a unique ID column for each individual polygon
  v_single$Individual_ID <- 1:nrow(v_single)

  # 3. Split the SpatVector into a list of individual SpatVector objects
  v_list <- terra::split(v_single, "Individual_ID")

  #Fill the holes
  #v_list <- lapply(v_list, terra::fillHoles)

  #Calculate area of each polygon
  areas <- lapply(v_list, terra::expanse)

  #Put areas in a data frame and calculate percentage
  v_df <- data.frame(v_single)
  v_df$areas <- do.call(c, areas)

  #Calculate the area of each polyon
  v_df$percentage <- (v_df$areas / sum(v_df$areas)*100)


  #Retain polygons > 2% area
  large <- which(round(v_df$percentage, digits = 2) > 2)
  if(length(large) > 0) {
    v_df <- v_df[large,]
    v_list <- v_list[c(large)]
  }

  #New_field
  new_field <- terra::vect(terra::svc(v_list))
  new_field$areas <- v_df$areas
  new_field$percentage <- v_df$percentage
  new_field$nsamples <- v_df$nsamples

  npoly <- dim(new_field)[1]
  nlabels <- length(new_field$hac2_0250_label)

  v_df$nsamples <- round((v_df$areas / sum(v_df$areas)) * nsamples)
  v_df$nsamples <- pmax(v_df$nsamples, 1)

  #Generate the points in each polygon, within a negative buffer

  # Note: Use a negative value to buffer inward
  shrunken_pols <- buffer(new_field, width = -20)


  # method="random" can be replaced with "regular" depending on your needs
  all_points <- NULL

  for (j in 1:npoly) {

    sub_poly <- shrunken_pols[j,]
    n_pts <- v_df$nsamples[j]

    # Skip if the polygon was completely erased by a buffer too large for its size
    if (nrow(sub_poly) == 0 || is.empty(sub_poly)) {
      warning(paste("Polygon ID", pols$id[i], "is too small for the buffer distance."))
      next
    }

    # Sample the individual polygon
    #pts <- terra::spatSample(sub_poly, size = n_pts, method = "random")

    poly_sf <- sf::st_as_sf(sub_poly)

    #Make sure there are no empty geometries
    check_empty <- sf::st_is_empty(poly_sf)
    if(check_empty == TRUE) {next}

    poly_win <- spatstat.geom::as.owin(poly_sf)

    set.seed(42)
    pts_spatstat <- spatstat.random::rSSI(r = 50, n = n_pts, win = poly_win)
    coords_df <- data.frame(x = pts_spatstat$x, y = pts_spatstat$y)
    pts <- terra::vect(coords_df, geom = c("x", "y"), crs = crs(sub_poly))

    # Append points
    if (is.null(all_points)){
      all_points <- pts
    } else {
      all_points <- rbind(all_points, pts)
    }
  }

  #Remove points for each polygon class until we get our original sample number
  #Start with the polygon with the least area.
  #If another polygon has the same EC class and is larger, remove the smaller one.

  #if(dim(all_points)[1]  > nsamples) {

  #  point_df <- terra::extract(new_field, all_points)

  #  point_df <- point_df %>%
  #    group_by(hac2_0250_label) %>%
  #    mutate(
  #      flag = if_else(n() > 1 & areas == max(areas), FALSE, TRUE))

  #  point_df <- data.frame(point_df)
  #  new_df <- point_df[!point_df$flag | (1:nrow(point_df) == nsamples), ]

  #  all_points <- all_points[new_df$id.y,]

  #}

  #If we are still over, remove redundant EC polygons by smallest area
  #if(dim(all_points)[1]  > nsamples) {

  #  freq <- table(all_points$hac2_0250_label)
  #  threshold <- sum(freq) - nsamples

  #  to_remove_vector <- vector()
  #  new_points <- all_points

  #  for (j in 1:threshold) {

  #    freq2 <- table(new_points$hac2_0250_label)
  #    maj_class <- names(which.max(freq2))

      #Remove the point representing the smaller area polygon
  #    maj_class_df <- new_points[which(all_points$hac2_0250_label == maj_class),]
  #    low_area <- which.min(maj_class_df$areas)

  #    to_remove_df <- maj_class_df[low_area,]
  #    row_remove <- which(new_points$percentage %in% to_remove_df$percentage == TRUE)
  #    if(length(row_remove) > 1) { row_remove <- row_remove[1] }
  #    new_points <- new_points[-row_remove]

  #  }

  #  all_points <- new_points

  #}

  #save the visualizations:

  pdf(file = paste0(vis_dir, folder, ".pdf"), width = 12, height = 8)
  par(mfrow = c(1,2), mar = c(2, 2, 8, 2), oma = c(2, 2, 4, 2))

  terra::plot(field_original, "hac2_0250_label", main = field_name)
  terra::plot(new_field, "hac2_0250_label", main = field_name)
  terra::plot(all_points, add = TRUE, col = "yellow")

  dev.off()

  #Convert the points back to original lat lon
  all_points <- terra::project(all_points, original_proj)
  all_points$field_id <- folders[i]
  all_points$field_name <- field_name
  all_points$female <- inbred
  all_points$recommended_nsample <- nsamples
  all_points$final_sample <- dim(all_points)[1]

  points_list[[i]] <- all_points

}


all_points <- do.call(rbind, points_list)
all_points <- as.data.frame(all_points, geom = "XY")

write.csv(all_points, file = paste0("../data/microenvironments/sampling_design.csv"))

