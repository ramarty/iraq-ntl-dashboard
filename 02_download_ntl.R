# NTL Data

roi_sf <- readRDS(file.path(db_dir, "Data", "GADM", "RawData", paste0("gadm_", 0, ".Rds")))

bm_raster(roi_sf = roi_sf,
          product_id = "VNP46A4",
          date = 2012:2022,
          bearer = bearer,
          output_location_type = "file",
          file_dir = file.path(db_dir, "Data", "NTL", "rasters"))