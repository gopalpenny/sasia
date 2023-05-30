# plot_classes_KA

library(terra)
library(tidyterra)
library(tidyverse)
library(ggalluvial)
library(data.table)
library(sf)
source("src/plot/plot_classes_functions.R")

out_path <- ggp::fig_set_output("plot_classes_GCI")

#### Cauvery
cauvery_path <- "/Users/gopal/Projects/sasia/spatial/unmod/CauveryBasin/Cauvery_boundary5.shp"
cauvery_sf <- st_read(cauvery_path)
gci_path <- "~/Projects/Data/crops/GCI_QC/"
gci_kaveri <- read_gci_allyears_boundary(gci_path, cauvery_sf)
place_name = 'cauvery5'
plot_gci_all_years(place_name = place_name, gci_kaveri, cauvery_sf, 4)
plot_gci_change_all_years("cauvery5", gci_kaveri, cauvery_sf, 4)
plot_gci_ggalluvial("cauvery5", gci_kaveri, cauvery_sf)

####  Karnataka
gadm_ind_path <- "~/Projects/Data/gadm/IND_adm_shp/IND_adm1.shp"
ind_sf <- st_read(gadm_ind_path)
ka_sf <- ind_sf %>% dplyr::filter(grepl("Karnataka",NAME_1))
gci_path <- "~/Projects/Data/crops/GCI_QC/"
gci_karnataka <- read_gci_allyears_boundary(gci_path, ka_sf)
plot_gci_all_years("karnataka", gci_karnataka, ka_sf, 4)
plot_gci_change_all_years("karnataka", gci_karnataka, ka_sf, 4)
plot_gci_ggalluvial("karnataka", gci_karnataka, ka_sf)