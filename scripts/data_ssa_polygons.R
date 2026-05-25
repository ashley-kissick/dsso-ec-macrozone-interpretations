
#data:
data_dir <- "../data/SSA/"


#SSA boundaries
eth <- terra::vect(paste0(data_dir, "political_shapefiles/eth_boundary.geojson"))
ken <- terra::vect(paste0(data_dir, "political_shapefiles/ken_boundary.geojson"))
ssa <- terra::vect(paste0(data_dir, "political_shapefiles/ssa_country_boundary.geojson"))


mwi <- sf::st_read(paste0(data_dir, "political_shapefiles/mwi_boundary.geojson"))
mwi <- mwi %>% st_collection_extract("POLYGON") %>% vect()

tza <- sf::st_read(paste0(data_dir, "political_shapefiles/tza_boundary.geojson"))
tza <- tza %>% st_collection_extract("POLYGON") %>% vect()

zmb <- sf::st_read(paste0(data_dir, "political_shapefiles/zmb_boundary.geojson"))
zmb <- zmb %>% st_collection_extract("POLYGON") %>% vect()


all_ssa <- rbind(eth, ken, mwi, tza, zmb)
usethis::use_data(all_ssa, overwrite = TRUE)



