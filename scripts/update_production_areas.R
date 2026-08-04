library(terra)
library(dplyr)
library(readxl)
library(sf)
library(spatstat)

#functions
get_utm_zone <- function(lon, lat) { zone_num <- floor((lon + 180) / 6) + 1 }

new_funct <- function(x) {
  
  if(dim(x)[1] == 1) {return(x)}
  
  if(dim(x)[2] > 1) {
    x_new <- x[order(-x$areas), ]
    x_sub <- x_new[1:2,]
    return(x_sub)
  }
  
}


data_dir <- "../data/microenvironments/"

file_dir <- paste0(data_dir, "/EC_shp/")
vis_dir <- paste0(data_dir, "/updated_sample_visuals3/")

fields <- readxl::read_excel(paste0(data_dir, 
                                    "shortstature_hybrids_fields_26_fertilewkt_updated.xlsx"))

feature_ids <- gsub("\\{", "", fields$feature_id)
feature_ids <- gsub("\\}", "", feature_ids)

fields_list <- lapply(fields$fertile_wkt, vect, crs = "EPSG:4326")
fields_vect <- do.call(rbind, fields_list)
fields_vect$feature_id <- feature_ids
fields_vect$group_id <- fields$group_id
fields_vect$fieldNumber <- fields$fieldNumber


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



folders <- list.files(file_dir)
folders <- gsub("_simp.geojson", "", folders)
folders <- gsub("_original.geojson", "", folders)
folders <- unique(folders)

full_names_simp <- list.files(file_dir, pattern = "_simp.geojson", full.names = TRUE)
full_names_orig <- list.files(file_dir, pattern = "_original.geojson", full.names = TRUE)

points_list <- list()
for (i in 3:length(full_names_simp)) {

  folder <- folders[i]
  field_name <- inbreds$field[which(inbreds$folder == folder)]
  inbred <- inbreds$female[which(inbreds$folder == folder)]
  
  #new field
  updated_field <- fields_vect[which(fields_vect$feature_id == folder),]

  #Inbred logic
  nsamples <- inbred_samples$n_sample[which(inbred_samples$female == inbred)]


  #Load the field
  field_original <- terra::vect(full_names_orig[i])

  field_simplified <- terra::vect(full_names_simp[i])
  original_proj <- crs(field_simplified)
  
  #Crop the simplified field to the new field boundary
  field <- terra::crop(field_simplified, updated_field)
  
  
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
  areas <- lapply(v_list, terra::expanse, unit = "ha")

  #Put areas in a data frame and calculate percentage
  v_df <- data.frame(v_single)
  v_df$areas <- do.call(c, areas)
  
  v_df$areas <- v_df$areas/2.47105

  #Calculate the area of each polyon
  v_df$percentage <- (v_df$areas / sum(v_df$areas)*100)


  #Retain polygons > 2% area
  #large <- which(round(v_df$percentage, digits = 2) > 2)
  
  large <- which(round(v_df$areas, digits = 2) > 0.1)
  if(length(large) > 0) {
    v_df <- v_df[large,]
    v_list <- v_list[c(large)]
  }
  

  #New_field, considering the larger areas
  new_field <- terra::vect(terra::svc(v_list))
  new_field$areas <- v_df$areas
  new_field$percentage <- v_df$percentage
  new_field$nsamples <- v_df$nsamples
  new_field$point_number <- paste0("sample_", 1:dim(new_field)[1])

  #If the number of samples are larger than required, let's eliminate some redundant EC's by area
  if(dim(new_field)[1] <= nsamples) { nf <- new_field }
  
  if(dim(new_field)[1] > nsamples) {
    nf <- lapply(split(new_field, new_field$hac2_0250_label), 
                 function(sub) sub[which.max(sub$areas), ])
    nf <- terra::vect(nf)
    
    #If it is reduced below what is recommended for the number of samples, let's keep the top two polygons by area
    if(dim(nf)[1] < nsamples) {
      nf <- lapply(split(new_field, new_field$hac2_0250_label), new_funct)
      nf <- terra::vect(nf)
    }
  
  }
  
  
  
  npoly <- dim(nf)[1]
  nlabels <- length(nf$hac2_0250_label)

  nf$nsamples <- round((nf$areas / sum(nf$areas)) * nsamples)
  nf$nsamples <- pmax(nf$nsamples, 1)

  #Generate the points in each polygon, within a negative buffer

  # Note: Use a negative value to buffer inward
  shrunken_pols <- buffer(nf, width = -30)
  
  #if(dim(shrunken_pols)[1] == 0) { field_sample <- nf }
  #if(dim(shrunken_pols)[1] > 0) { field_sample <- shrunken_pols }

  # method="random" can be replaced with "regular" depending on your needs
  all_points <- NULL

  for (j in 1:npoly) {

    sub_poly <- shrunken_pols[j,]
    n_pts <- nf$nsamples[j]

    # Skip if the polygon was completely erased by a buffer too large for its size
    if (nrow(sub_poly) == 0 || is.empty(sub_poly) || !is.valid(sub_poly) || is.na(sub_poly))  {
      warning(paste("Polygon ID", nf$id[i], "is too small for the buffer distance."))
      sub_poly <- nf[j,]
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
  
  #Extract the EC layer
  extracted <- terra::extract(nf, all_points)
  
  all_points$point_number <- paste0("sample_", 1:dim(all_points)[1])
  all_points$field_name <- field_name
  all_points$female <- inbred
  all_points$recommended_nsample <- nsamples
  all_points$final_sample <- dim(all_points)[1]
  all_points$field_id <- folders[i]
  all_points$hac2_0250_label <- extracted$hac2_0250_label
  all_points$percentage <- extracted$percentage
  
  all_points$field_number <- unique(fields_vect$fieldNumber[which(fields_vect$feature_id == folders[i])])
  all_points <- all_points[,c("field_id", "field_number", "field_name", "recommended_nsample",
                              "final_sample", "point_number", "hac2_0250_label", "percentage")]
  
  #Convert the points back to original lat lon
  all_points <- terra::project(all_points, original_proj)
  
  #Simplified field:  field
  #save the visualizations:
  
  #Create a buffer
  buf_field <- terra::buffer(field, width = 50)
  
  my_cols <- adjustcolor(terrain.colors(10), alpha.f = 0.6)
  pdf(file = paste0(vis_dir, folder, ".pdf"), width = 12, height = 8)
  #par(mfrow = c(1,2), mar = c(2, 2, 8, 2), oma = c(2, 2, 4, 2))
  
  par(mfrow = c(1,1), mar = c(2, 2, 8, 2), oma = c(2, 2, 4, 2))
  #terra::plot(field_original, "hac2_0250_label", main = field_name)
  plot(buf_field, border = NA)
  terra::plot(field, "hac2_0250_label", main = field_name, col = my_cols,
              border = NA, add = TRUE)
  terra::plot(all_points, add = TRUE, col = "black")
  text(all_points, labels = "point_number", pos = 3, col = "black")
  mtext(paste0("Recommended samples:  ", nsamples), side = 3, line = 1, outer = TRUE)
  
  dev.off()
  
  points_list[[i]] <- all_points
  
  #Save the field shapefile
  shp_dir <- paste0(data_dir, "updated_field_shapefiles3/")
  writeVector(field, file = paste0(shp_dir, folder, ".shp"), overwrite = TRUE)
    
}


all_points <- do.call(rbind, points_list)
all_points <- as.data.frame(all_points, geom = "XY")

write.csv(all_points, file = paste0("../data/microenvironments/updated_sampling_design3.csv"))






