# Make Base Layers

# Load data --------------------------------------------------------------------
for(i in 0:2){
  roi_sf <- gadm(country = "IRQ", level=i, path = tempdir()) %>% st_as_sf()
  
  saveRDS(roi_sf, 
          file.path(db_dir, "Data", "GADM", "RawData", paste0("gadm_", i, ".Rds")))
}

gadm_sp <- readRDS(file.path(db_dir, "Data", "GADM", "RawData", 
                             paste0("gadm_", 0, ".Rds"))) %>% as("Spatial")

for(size in c(5, 10, 20, 30, 40, 50)){
  
  hex_points <- spsample(gadm_sp, type = "hexagonal", cellsize = size/111.12)
  hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size/111.12)
  
  hex_grid$id <- 1:length(hex_grid)
  
  hex_grid <- hex_grid %>% st_as_sf()
  
  saveRDS(hex_grid, 
          file.path(db_dir, "Data", "Hexagons", "RawData", paste0("hex_", size, "km.Rds")))
  
}

