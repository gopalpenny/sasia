# set python environment
# Sys.setenv(RETICULATE_PYTHON = "/Users/gopal/mambaforge/envs/dsgeom/bin/python")
# Sys.getenv("RETICULATE_PYTHON")
# reticulate::use_condaenv("/Users/gopal/mambaforge/envs/dsgeom/bin/python")
# library(reticulate)
# reticulate::py_config()

# load R packages
library(terra)
library(tidyterra)
library(tidyverse)
library(ggalluvial)
library(data.table)
library(sf)
source("src/plot/plot_classes_functions.R")

np <- reticulate::import("numpy")

out_path <- ggp::fig_set_output("prep_GCI")

#### Arkavathy

# Import basin and GCI raster data / mask to the basin boundary
basin_path <- "/Users/gopal/Projects/sasia/spatial/unmod/TGHalli/Arkavathy_subwatersheds_ATREE.shp"
basin_sf <- st_read(basin_path)
basin_sf <- basin_sf %>% dplyr::filter(Subcatch=="Hesaraghatta")
gci_path <- "~/Projects/Data/crops/GCI_QC/"
basin_name <- "Hesaraghatta"

save_gci_basin_numpy <- function(gci_path, basin_sf, basin_name) {
    gci_basin <- read_gci_allyears_boundary(gci_path, basin_sf, type_func = as.numeric) %>%
        mask(basin_sf)

    # Identify GCI columns
    gci_cols <- names(gci_basin)[grepl("^gci2",names(gci_basin))]

    # Extract cropping data to data.table
    gci_basin_dt <- as.data.table(as.data.frame(gci_basin))

    # Subset to remove rows with no crops in all years
    gci_basin_dt[, (gci_cols) := lapply(.SD, as.numeric), .SDcols = gci_cols]
    gci_basin_dt[, num_crops := rowSums(.SD), .SDcols = gci_cols]
    gci_basin_crops <- gci_basin_dt[num_crops > 0, ]

    # sapply(gci_basin_dt, class)
    gci_basin_crops_mat <- as.integer(as.matrix(gci_basin_crops))
    gci_basin_crops_np <- np$array(gci_basin_crops_mat, dtype = "int16")

    np$save(paste0("./data/format/gci/gci_",basin_name,"_crops_2001_2019.npy"), gci_basin_crops_np)

    cat("file output size:", ggp::obj_size(gci_basin_crops_np))
    # ggp::obj_size(gci_basin_crops_mat)
    # ggp::obj_size(gci_basin_crops_np)
    # typeof(gci_basin_crops_np)
}