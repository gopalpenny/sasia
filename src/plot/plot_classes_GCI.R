# plot_classes_KA

library(terra)
library(tidyterra)
library(tidyverse)
library(ggalluvial)
library(data.table)
library(sf)
source("src/plot/plot_classes_functions.R")

# torch <- import("torch")

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

#### TG HALLI
arkavathy_path <- "/Users/gopal/Projects/sasia/spatial/unmod/TGHalli/ArkavatiSubbasins.shp"
arkavathy_sf <- st_read(arkavathy_path)
gci_path <- "~/Projects/Data/crops/GCI_QC/"
gci_arkavathy <- read_gci_allyears_boundary(gci_path, arkavathy_sf)
plot_gci_all_years("arkavathy", gci_arkavathy, arkavathy_sf, out_path = out_path, nrow = 2)
plot_gci_change_all_years("arkavathy", gci_arkavathy, arkavathy_sf, out_path = out_path, nrow = 2)
plot_gci_ggalluvial("arkavathy", gci_arkavathy, arkavathy_sf, 
    out_path = out_path, frac_range = c(1e-4, 0.1))

#### Hesaraghatta
basin_path <- "/Users/gopal/Projects/sasia/spatial/unmod/TGHalli/Arkavathy_subwatersheds_ATREE.shp"
hesaraghatta_sf <- st_read(basin_path)
hesaraghatta_sf <- hesaraghatta_sf %>% dplyr::filter(Subcatch=="Hesaraghatta")
gci_path <- "~/Projects/Data/crops/GCI_QC/"
gci_hesaraghatta <- read_gci_allyears_boundary(gci_path, hesaraghatta_sf)
plot_gci_all_years("hesaraghatta", gci_hesaraghatta, hesaraghatta_sf, out_path = out_path, nrow = 4)
plot_gci_change_all_years("hesaraghatta", gci_hesaraghatta, hesaraghatta_sf, out_path = out_path, nrow = 4)
plot_gci_ggalluvial("hesaraghatta", gci_hesaraghatta, hesaraghatta_sf, 
    out_path = out_path, frac_range = c(1e-4, 0.1))