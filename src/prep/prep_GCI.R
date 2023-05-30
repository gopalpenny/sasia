
library(terra)
library(tidyterra)
library(tidyverse)
library(ggalluvial)
library(data.table)
library(sf)
source("src/plot/plot_classes_functions.R")

out_path <- ggp::fig_set_output("prep_GCI")

#### Cauvery
basin_path <- "/Users/gopal/Projects/sasia/spatial/unmod/TGHalli/ArkavatiSubbasins.shp"
basin_sf <- st_read(basin_path)
gci_path <- "~/Projects/Data/crops/GCI_QC/"
gci_basin <- read_gci_allyears_boundary(gci_path, basin_sf, type_func = as.numeric) %>%
    mask(basin_sf)

gci_cols <- names(gci_basin)[grepl("^gci2",names(gci_basin))]

gci_basin_dt <- as.data.table(as.data.frame(gci_basin))

gci_basin_dt[, (gci_cols) := lapply(.SD, as.numeric), .SDcols = gci_cols]
gci_basin_dt[, num_crops := rowSums(.SD), .SDcols = gci_cols]
gci_basin_crops <- gci_basin_dt[num_crops > 0, ]

# sapply(gci_basin_dt, class)


gci_basin_crops_mat <- as.integer(as.matrix(gci_basin_crops))
ggp::obj_size(gci_basin_crops_mat)


dim(gci_basin_dt)

table(gci_basin_crops$num_crops)
n <- 2000000
table(values(gci_basin$gci2001))
table()

is.factor(gci_basin$gci2001)
