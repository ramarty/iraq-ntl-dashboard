# Iraq Nighttime Lights Dashboard

# Packages ---------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(sf)
library(sp)
library(dplyr)
library(leaflet)
library(raster)
library(terra)
library(purrr)
library(stringr)
library(sparkline)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)
library(sparkline)

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  navbarPage("Iraq Nighttime Lights", id="nav",
             
             tabPanel("Map",
                      div(class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                          ),
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          uiOutput("change_map", width="100%", height="100%"),
                          
                          # Panel ----------------------------------------------
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 270, height = 400,
                                        #  style = "background-color: lightblue;
                                        
                                        style =  "overflow-y: scroll;",
                                        
                                        h3("Map Controls"),
                                        
                                        strong("Annual nighttime lights from 2012 to 2022. Data from NASA BlackMarble."),
                                        
                                        br(),
                                        br(),
                                        
                                        selectInput("unit", strong("Unit"), 
                                                    choices=c("Orig. NTL Res. (500m)",
                                                              "ADM 1", 
                                                              "ADM 2",
                                                              "Grid: 5km",
                                                              "Grid: 10km",
                                                              "Grid: 20km",
                                                              "Grid: 30km",
                                                              "Grid: 40km",
                                                              "Grid: 50km"), 
                                                    selected = "ADM 2", multiple = FALSE,
                                                    selectize = TRUE, width = NULL, size = NULL),
                                        
                                        tags$style(type="text/css", '#year_begin_map {width: 80px; margin-left:50px}'),
                                        tags$style(type="text/css", '#year_end_map {width: 80px; margin-left:100px;}'),
                                        
                                        selectInput("ntl_type", strong("Type"), 
                                                    choices=c("Levels", "Logs", "Change", "Growth Rate"), 
                                                    selected = "Logs", multiple = FALSE,
                                                    selectize = TRUE, width = NULL, size = NULL),
                                        
                                        uiOutput("ui_year")
                                        

                                        
                          )
                      )
             ),
             conditionalPanel("false", icon("crosshair"))
  )
)

# Server -----------------------------------------------------------------------
server = (function(input, output, session) {
  
  r78_sf <- readRDS(file.path(git_dir, "data", "road_r78.Rds"))
  gs_sf  <- readRDS(file.path(git_dir, "data", "road_gs.Rds"))
  osm_sf <- readRDS(file.path(git_dir, "data", "osm_main.Rds"))
  
  observe({
    
    # Map ------------------------------------------------------------------------
    output$change_map <- renderUI({
      
      # * Load data ------------------------------------------------------------
      if(input$unit == "ADM 1")      roi_sf <- readRDS(file.path("data", paste0("gadm1", ".Rds")))
      if(input$unit == "ADM 2")      roi_sf <- readRDS(file.path("data", paste0("gadm2", ".Rds")))
      if(input$unit == "Grid: 5km")  roi_sf <- readRDS(file.path("data", paste0("hex5", ".Rds")))
      if(input$unit == "Grid: 10km") roi_sf <- readRDS(file.path("data", paste0("hex10", ".Rds")))
      if(input$unit == "Grid: 20km") roi_sf <- readRDS(file.path("data", paste0("hex20", ".Rds")))
      if(input$unit == "Grid: 30km") roi_sf <- readRDS(file.path("data", paste0("hex30", ".Rds")))
      if(input$unit == "Grid: 40km") roi_sf <- readRDS(file.path("data", paste0("hex40", ".Rds")))
      if(input$unit == "Grid: 50km") roi_sf <- readRDS(file.path("data", paste0("hex50", ".Rds")))
      
      if(input$unit == "Orig. NTL Res. (500m)"){
      
        if(input$ntl_type == "Levels")      RASTER_FILE <- file.path(git_dir, "data", paste0("raster_",    input$year,".Rds"))
        if(input$ntl_type == "Logs")        RASTER_FILE <- file.path(git_dir, "data", paste0("raster_log_",input$year,".Rds"))
        if(input$ntl_type == "Change")      RASTER_FILE <- file.path(git_dir, "data", paste0("raster_change_",input$year_1,"_",input$year_2,".Rds"))
        if(input$ntl_type == "Growth Rate") RASTER_FILE <- file.path(git_dir, "data", paste0("raster_growth_",input$year_1,"_",input$year_2,".Rds"))
        
        print(RASTER_FILE)
        
        if(!file.exists(RASTER_FILE)){
          r <- raster()
        } else{
          r <- readRDS(RASTER_FILE)
        }
        

      }
      
      if(input$unit %>% str_detect("ADM")){
        stroke <- T
        weight <- 1
      } else{
        stroke <- F
        weight <- 0
      }
      
      # * Define variable ------------------------------------------------------
      if(input$unit != "Orig. NTL Res. (500m)"){
        
        if(input$ntl_type %in% "Levels"){
          roi_sf$ntl_var <- roi_sf[[paste0("ntl_", input$year)]]
        }
        
        if(input$ntl_type %in% "Logs"){
          roi_sf$ntl_var <- roi_sf[[paste0("ntl_log_", input$year)]]
        }
        
        if(input$ntl_type %in% "Change"){
          
          if(input$year_1 >= input$year_2){
            roi_sf$ntl_var <- NA
          } else{
            roi_sf$ntl_var <- roi_sf[[paste0("ntl_change_", input$year_1, "_", input$year_2)]]
          }
          
        }
        
        if(input$ntl_type %in% "Growth Rate"){
          
          if(input$year_1 >= input$year_2){
            roi_sf$ntl_var <- NA
          } else{
            roi_sf$ntl_var <- roi_sf[[paste0("ntl_growth_", input$year_1, "_", input$year_2)]]
          }
          
        }
        
        #### NTL Variable Range (for palette)
        v <- roi_sf$ntl_var
      } else{
        v <- values(r)
      }
      
      v <- v[!is.na(v)]
      v <- v[v != Inf]
      v <- v[v != -Inf]
      
      # * Palette --------------------------------------------------------------
      if(input$ntl_type %in% c("Change", "Growth Rate")){
        v_ntl <- c(max(abs(v)), 0, -max(abs(v)))
        
        pal <- colorBin("Spectral", 
                        v_ntl, 
                        11, 
                        pretty = T,
                        reverse = T,
                        na.color = "#00000000")
        
        pal_legend <- colorBin("Spectral", 
                               v_ntl, 
                               11, 
                               pretty = T,
                               reverse = F)
      } else{
        v_ntl <- unique(v)
        
        colors_for_pal <- c("black", "yellow", "red")
        
        pal <- colorBin(colors_for_pal, 
                        v_ntl, 
                        11, 
                        pretty = T,
                        na.color = "#00000000")
        
        pal_legend <- colorBin(colors_for_pal, 
                               v_ntl, 
                               11, 
                               pretty = T,
                               reverse = T)
      }
      
      # * Map: Polygon ---------------------------------------------------------
      if(input$unit != "Orig. NTL Res. (500m)"){
        roi_sf$popup <- paste0("<h4> Trends in NTL for:<br>", roi_sf$name, "</h4>", 
                               "<br>",
                               roi_sf$ntl_spark)
        
        l <- leaflet(height = "700px") %>%
          addProviderTiles(providers$CartoDB.DarkMatter) %>% 
          addPolygons(data = roi_sf,
                      label = ~lapply(paste0(popup), HTML),
                      popupOptions = popupOptions(minWidth = 200,
                                                  maxHeight = 150),
                      stroke = stroke,
                      weight = weight,
                      smoothFactor = 0,
                      fillOpacity = 0.8,
                      color = ~pal(roi_sf$ntl_var)) %>%
          addPolylines(data = osm_sf, group = "Main Roads", color = "gray", weight = 2, popup = ~name) %>%
          addPolylines(data = r78_sf, group = "R78ab", color = "purple", weight = 2, popup = ~name) %>%
          addPolylines(data = gs_sf, group = "Girsheen-Suheila", color = "purple", weight = 2, popup = ~name) %>%
          addLayersControl(
            overlayGroups = c("R78ab", "Girsheen-Suheila", "Main Roads"),
            options = layersControlOptions(collapsed = FALSE),
            position = "topleft"
          ) %>%
          hideGroup("Main Roads") %>%
          addLegend("topleft",
                    pal = pal_legend,
                    values = v_ntl,
                    title = "Legend",
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                    opacity = 1) %>%
          onRender("function(el,x) {
      this.on('tooltipopen', function() {HTMLWidgets.staticRender();})
    }") %>%
          add_deps("sparkline") %>%
          browsable()
        
      } else{
        
        factop <- function(x) {
          ifelse(is.na(x), 0, 1)
        }
        
        l <- leaflet(height = "700px") %>%
          addProviderTiles(providers$CartoDB.DarkMatter) %>%
          addRasterImage(r, colors = pal, opacity = 0.8) %>%
          addPolylines(data = osm_sf, group = "Main Roads", color = "gray", weight = 2, popup = ~name) %>%
          addPolylines(data = r78_sf, group = "R78ab", color = "purple", weight = 2, popup = ~name) %>%
          addPolylines(data = gs_sf, group = "Girsheen-Suheila", color = "purple", weight = 2, popup = ~name) %>%
          addLayersControl(
            overlayGroups = c("R78ab", "Girsheen-Suheila", "Main Roads"),
            options = layersControlOptions(collapsed = FALSE),
            position = "topleft"
          ) %>%
          hideGroup("Main Roads") %>%
          addLegend("topleft",
                    pal = pal_legend,
                    values = v_ntl,
                    title = "Legend",
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                    opacity = 1)
        
      }
      
      l
      
    })
    
    output$ui_year <- renderUI({
      
      tagList(
        
        if(input$ntl_type %in% c("Levels", "Logs")){
          selectInput("year", strong("Year"), 
                      choices=2012:2022, selected = 2012, multiple = FALSE,
                      selectize = TRUE, width = NULL, size = NULL)
        } else{
          div(style="display:flex",
              selectInput("year_1", strong("Year Start"), 
                          choices=2012:2022, selected = 2012, multiple = FALSE,
                          selectize = TRUE, width = NULL, size = NULL),
              selectInput("year_2", strong("Year End"), 
                          choices=2012:2022, selected = 2022, multiple = FALSE,
                          selectize = TRUE, width = NULL, size = NULL)
          )
        }
        
        
      )
    })
    
  })
})

# Run the app ------------------------------------------------------------------
shinyApp(ui, server)



