# Make Data for Dashboard

# Polygon ----------------------------------------------------------------------
for(roi in c( paste0("gadm", 1:2), # 1:2
              paste0("hex", c(5, 10, 20, 30, 40, 50)))){  # 5, 10, 20, 30, 40, 50
  
  #### Load Data
  if(str_detect(roi, "gadm")){
    i <- roi %>% str_replace_all("gadm", "")
    roi_sf <- readRDS(file.path(db_dir, "Data", "GADM", "FinalData", 
                                paste0("gadm_", i, "_ntl.Rds")))
    
    roi_sf$name <- roi_sf[[paste0("NAME_", i)]]
    
  }
  
  if(str_detect(roi, "hex")){
    i <- roi %>% str_replace_all("hex", "")
    roi_sf <- readRDS(file.path(db_dir, "Data", "Hexagons", "FinalData", 
                                paste0("hex_", i, "km_ntl.Rds")))
    
    roi_sf <- roi_sf %>%
      dplyr::mutate(name = paste0("Cell ID ", id))
  }
  
  #### Log
  for(year in 2012:2022){
    roi_sf[[paste0("ntl_log_", year)]] <- log(roi_sf[[paste0("ntl_", year)]] + 1)
  }
  
  #### Make change variables
  for(year_1 in 2012:2022){
    for(year_2 in 2012:2022){
      if(year_1 >= year_2) next
      
      roi_sf[[paste0("ntl_change_", year_1, "_", year_2)]] <- roi_sf[[paste0("ntl_", year_2)]] - roi_sf[[paste0("ntl_", year_1)]]
      roi_sf[[paste0("ntl_growth_", year_1, "_", year_2)]] <- log(roi_sf[[paste0("ntl_", year_2)]]) - log(roi_sf[[paste0("ntl_", year_1)]])
      
    }
  }
  
  roi_sf$ntl_spark <- NA
  
  for(row_i in 1:nrow(roi_sf)){
    if( (row_i %% 500) == 0 ) print(row_i)
    
    roi_sf_i <- roi_sf[row_i,]
    
    ntl_values <- roi_sf_i %>%
      dplyr::select(paste0("ntl_", 2012:2022)) %>%
      st_drop_geometry() %>%
      as.numeric()
    
    roi_sf$ntl_spark[row_i] <- sparkline(ntl_values,
                                         type='bar',
                                         barColor="orange",
                                         chartRangeMin = min(ntl_values),
                                         chartRangeMax = max(ntl_values),
                                         width = 50,
                                         height = 100,
                                         tooltipChartTitle = "COVID-19 Cases",
                                         highlightLineColor = 'orange', 
                                         highlightSpotColor = 'orange') %>%
      as.character() #%>%
    #htmltools::as.tags()
  }
  
  #### Export Data
  saveRDS(roi_sf, file.path(git_dir, "data", paste0(roi, ".Rds")))
  
}

# Raster -----------------------------------------------------------------------
gadm0_sp <- readRDS(file.path(db_dir, "Data", "GADM", "RawData", paste0("gadm_", 0, ".Rds"))) %>%
  as("Spatial")

#### Levels / Logs
for(year_i in 2012:2022){
  print(year_i)
  
  r <- raster(file.path(db_dir, "Data", "NTL", "rasters", paste0("VNP46A4_t",year_i,".tif")))
  r <- r %>% crop(gadm0_sp) %>% mask(gadm0_sp)
  
  r_log <- r
  r_log[] <- log(r_log[]+1)
  
  writeRaster(r, file.path(git_dir, "data", paste0("raster_",year_i,".tif")))
  writeRaster(r_log, file.path(git_dir, "data", paste0("raster_log_",year_i,".tif")))
  
}

#### Make change variables
for(year_1 in 2012:2022){
  for(year_2 in 2012:2022){
    if(year_1 >= year_2) next
    print(paste(year_1, year_2))
    
    r1 <- raster(file.path(db_dir, "Data", "NTL", "rasters", paste0("VNP46A4_t",year_1,".tif")))
    r2 <- raster(file.path(db_dir, "Data", "NTL", "rasters", paste0("VNP46A4_t",year_2,".tif")))
    
    r <- r1
    r_growth <- r1
    r[] <- r2[] - r1[]
    r_growth[] <- log(r2[]) - log(r1[])
    
    r <- r %>% crop(gadm0_sp) %>% mask(gadm0_sp)
    r_growth <- r_growth %>% crop(gadm0_sp) %>% mask(gadm0_sp)
    
    writeRaster(r,        file.path(git_dir, "data", paste0("raster_change_",year_1,"_",year_2,".tif")))
    writeRaster(r_growth, file.path(git_dir, "data", paste0("raster_growth_",year_1,"_",year_2,".tif")))
    
  }
}

# Roads -----------------------------------------------------------------------
r78_sp <- readRDS(file.path(db_dir, "Data", "Project Roads", "RawData", "r7_r8ab.Rds"))  %>% st_as_sf()
gs_sp  <- readRDS(file.path(db_dir, "Data", "Project Roads", "RawData", "gs.Rds")) %>% st_as_sf()

r78_sp <- r78_sp %>%
  dplyr::rename(name = road) %>%
  dplyr::select(name)

osm_sf <- read_sf(file.path(db_dir, "Data", "OSM", "RawData", "gis_osm_roads_free_1.shp"))
osm_sf <- osm_sf[osm_sf$fclass %>% str_detect("motorway|trunk"),]
osm_sf <- osm_sf %>%
  dplyr::select(name)

saveRDS(r78_sp, file.path(git_dir, "data", "road_r78.Rds"))
saveRDS(gs_sp,  file.path(git_dir, "data", "road_gs.Rds"))
saveRDS(osm_sf, file.path(git_dir, "data", "osm_main.Rds"))


# 
# 
# 
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.DarkMatter) %>%
#   addPolylines(data = r78_sp, group = "R78ab", color = "darkorchid1", weight = 2) %>%
#   addPolylines(data = gs_sp, group = "Girsheen-Suheila", color = "darkorchid1", weight = 2) %>%
#   addPolylines(data = osm_sf, group = "Main Roads", color = "white", weight = 2) %>%
#   addLayersControl(
#     overlayGroups = c("R78ab", "Girsheen-Suheila", "Main Roads"),
#     options = layersControlOptions(collapsed = FALSE)
#   )
# 
# 
# 
# 
# 
# pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
#                     na.color = "transparent")
# 
# leaflet() %>% addTiles() %>%
#   addRasterImage(r, colors = pal, opacity = 0.8) %>%
#   addLegend(pal = pal, values = values(r),
#             title = "Surface temp")
# 
# 
# # Functions --------------------------------------------------------------------
# as.character.htmlwidget <- function(x, ...) {
#   htmltools::HTML(
#     htmltools:::as.character.shiny.tag.list(
#       htmlwidgets:::as.tags.htmlwidget(
#         x
#       ),
#       ...
#     )
#   )
# }
# 
# add_deps <- function(dtbl, name, pkg = name) {
#   tagList(
#     dtbl,
#     htmlwidgets::getDependency(name, pkg)
#   )
# }
# 
# roi_sf <- readRDS(file.path(git_dir, "data", paste0("hex50", ".Rds")))
# 
# v <- roi_sf$ntl_2022
# v <- v[!is.na(v)]
# v <- v[v != Inf]
# v <- v[v != -Inf]
# 
# pal_rev <- colorNumeric(
#   palette = "Spectral",
#   domain = c(-max(abs(v)), 0, max(abs(v))),
#   reverse = F
# )
# 
# pal <- colorNumeric(
#   palette = "Spectral",
#   domain = c(-max(abs(v)), 0, max(abs(v))),
#   reverse = T
# )
# 
# roi_sf$popup <- paste0("<h4> Trends in NTL for:<br>", roi_sf$name, "</h4>", 
#                        "<br>",
#                        roi_sf$ntl_spark)
# 
# library(leaflet.extras)
# library(sparkline)
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = roi_sf,
#               color = ~pal(ntl_2022)) %>%
#   addLegend("topright",
#             pal = pal_rev,
#             values = c(-max(abs(v)), 0, max(abs(v))),
#             title = "Legend",
#             labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
#             opacity = 1) 
# 
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = roi_sf,
#               label = ~lapply(paste0(popup), HTML),
#               popupOptions = popupOptions(minWidth = 50,
#                                           maxHeight = 150),
#               stroke = F,
#               smoothFactor = 0,
#               fillOpacity = 0.9,
#               color = ~pal(ntl_2022)) %>%
#   onRender("function(el,x) {
#       this.on('tooltipopen', function() {HTMLWidgets.staticRender();})
#     }") %>%
#   addLegend("topright",
#             pal = pal,
#             values = c(-max(abs(v)), 0, max(abs(v))),
#             title = "Legend",
#             #labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
#             opacity = 1) %>%
#   add_deps("sparkline") %>%
#   browsable()
# 
