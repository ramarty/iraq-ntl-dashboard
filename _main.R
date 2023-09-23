# Iraq NTL Dashboard

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "robmarty"){
  db_dir <- "~/Dropbox/World Bank/IEs/Iraq NTL Dashboard"
  git_dir <- "~/Documents/Github/iraq-ntl-dashboard"
}

library(sf)
library(sp)
library(dplyr)
library(geodata)
library(leaflet)
library(blackmarbler)
library(raster)
library(terra)
library(exactextractr)
library(purrr)
library(stringr)
library(htmlwidgets)
library(htmltools)
library(sparkline)
library(htmlwidgets)

bearer <- read.csv("~/Desktop/bearer_bm.csv") %>% pull(token)
