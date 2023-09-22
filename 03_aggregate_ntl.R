# Aggregate NTL

# Load data --------------------------------------------------------------------
for(i in 1:2){
  gadm_sf <- readRDS(file.path(db_dir, "Data", "GADM", "RawData", 
                               paste0("gadm_", i, ".Rds")))
  
  for(year_i in 2012:2022){
    r <- rast(file.path(db_dir, "Data", "NTL", "rasters", paste0("VNP46A4_t",year_i,".tif")))
    
    gadm_sf[[paste0("ntl_", year_i)]] <- exact_extract(r, gadm_sf, "mean")
  }
  
  saveRDS(gadm_sf, file.path(db_dir, "Data", "GADM", "FinalData", 
                            paste0("gadm_", i, "_ntl.Rds")))
}

for(size in c(5, 10, 20, 30, 40, 50)){
  hex_sf <- readRDS(file.path(db_dir, "Data", "Hexagons", "RawData", paste0("hex_", size, "km.Rds")))
  
  for(year_i in 2012:2022){
    r <- rast(file.path(db_dir, "Data", "NTL", "rasters", paste0("VNP46A4_t",year_i,".tif")))
    
    hex_sf[[paste0("ntl_", year_i)]] <- exact_extract(r, hex_sf, "mean")
  }
  
  saveRDS(hex_sf, file.path(db_dir, "Data", "Hexagons", "FinalData", 
                             paste0("hex_", size, "km_ntl.Rds")))
}

