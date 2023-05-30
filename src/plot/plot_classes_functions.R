# plot_classes_KA

library(terra)
library(tidyterra)
library(tidyverse)
library(ggalluvial)
library(data.table)
library(sf)


# # ADMIN BOUNDARIES
# gadm_ind_path <- "~/Projects/Data/gadm/IND_adm_shp/IND_adm1.shp"
# ind_sf <- st_read(gadm_ind_path)
# ka_sf <- ind_sf %>% dplyr::filter(grepl("Karnataka",NAME_1))
out_path <- ggp::fig_set_output("plot_classes_functions")


read_gci_crop <- function(gci_yr_path, crop_sf, type_func = NULL) {
  # gci_yr_path <- gci_filepaths[1]
  gci_yr_world <- rast(gci_yr_path)
  if (!is.null(type_func)) {
    gci_yr_world <- type_func(gci_yr_world)
  }
  gci_yr_crop <- terra::crop(gci_yr_world, vect(crop_sf))
  names(gci_yr_crop) <- gsub(".*GCI_([0-9]+).tif", "gci\\1", gci_yr_path)
  return(gci_yr_crop)
}

read_gci_allyears_boundary <- function(gci_path, crop_sf, type_func = NULL) {
  gci_filepaths <- list.files(gci_path, pattern = "GCI.*.tif$", full.names = TRUE) # nolint
  gci_filepaths <- gci_filepaths[!grepl("_QC_", gci_filepaths)]

  gci_ka_list <- lapply(gci_filepaths, read_gci_crop, crop_sf = crop_sf, type_func = type_func)
  gci_ka <- do.call(c, gci_ka_list)
  gci_ka
}



plot_gci_all_years <- function(place_name, gci_ka, crop_sf, nrow) {
  gci_colors <- c("forestgreen", "lightgreen", "gold2", "tan")

  p_gci_ka_all_years <- ggplot() +
    geom_spatraster(data = gci_ka, aes()) +
    geom_sf(data = crop_sf, aes(), fill = NA, color = "white") +
    scale_fill_manual(values = rev(gci_colors)) +
    facet_wrap(~lyr, nrow = nrow)
  cat("Saving maps of all years...")
  ggsave(paste0("GCI_",place_name,"_all	years.png"), p_gci_ka_all_years, path = out_path, width = 14, height = 10)

  p_gci_ka_all_years
}

plot_gci_change_all_years <- function(place_name, gci_ka, crop_sf, nrow) {
  gci_ka_diff_2001 <- gci_ka - gci_ka$gci2001
  p_gci_ka_all_years_diff <- ggplot() +
    geom_spatraster(data = gci_ka_diff_2001, aes()) +
    geom_sf(data = crop_sf, aes(), fill = NA, color = "gray") +
    scale_fill_gradient2() +
    facet_wrap(~lyr, nrow = 4)
  cat("Saving maps of diff of all years...")
  ggsave(paste0("GCI_",place_name,"_allyears_diff_2001.png"),
    p_gci_ka_all_years_diff, path = out_path, width = 14, height = 10)
  
  p_gci_ka_all_years_diff
}


#################################################################
#################################################################
#################################################################
#################################################################
#################################################################


# r_karur_change <- r_karur
#
# for (i in 2:nlayers(r_karur)) {
#   r_karur_change[[i]][r_karur[[1]] == r_karur[[i]]] <- NA
# }

#
# png(file.path(out_path, paste0("karur_crops_",6+2014,".png")), width = 1200, height = 800)
# plot(r_karur[[6]])
# dev.off()
#
# for (i in 1:nlayers(r_karur)){
#   png(file.path(out_path, paste0("karur_crops_change_",i+2014,".png")), width = 1200, height = 800)
#   plot(r_karur_change[[i]])
#   dev.off()
# }

plot_gci_ggalluvial <- function(place_name, gci_ka, crop_sf, frac_range = c(0.00001, 0.08)) {
  cat(paste0("Plotting alluvial plot...",place_name,"\n"))
  gci_ka_masked <- mask(gci_ka, crop_sf)
  # gci_ka_dt <- as.data.table(as.data.frame(gci_ka))
  gci_ka_dt <- as.data.table(as.data.frame(gci_ka_masked))

  gci_colnames <- names(gci_ka_masked)


  gci_ka_pattern <- gci_ka_dt[, .N, by = gci_colnames]
  gci_ka_pattern <- gci_ka_pattern[order(-N)]

  gci_ka_pattern[N > 1000, count := sum(N)]

  gci_ka_pattern[, count := sum(N)]
  gci_ka_pattern[, frac := N / count]
  gci_ka_pattern[, id := 1:.N]


  gci_ka_pattern_long <- melt(gci_ka_pattern, id.vars = c("N", "count", "frac", "id"), 
    measure_vars = gci_colnames, value.name = "crop")
  gci_ka_pattern_long[, year := gsub("gci", "", variable)]
  gci_ka_pattern_long[, avg_ci := mean(as.numeric(crop)), by = "id"]

  # gci_ka_pattern_long[frac > 0.001]

  # gci_ka_pattern[frac > 0.00001, .N]

  top10 <- gci_ka_pattern[1:10,]
  # print(gci_ka_pattern[1:10,])
  p_kaveri_alluvial <- ggplot(
    as_tibble(gci_ka_pattern_long[frac > frac_range[1] & frac < frac_range[2]]),
    aes(
      x = year, y = frac * 100, stratum = crop, alluvium = id,
      label = crop
    )
  ) +
    scale_fill_viridis_c() +
    # scale_fill_manual("Select\nmulti-year\npatterns",values = c("forestgreen","lightgreen","gold2","tan","gray"),
    #                   guide = guide_legend(order = 2)) +
    geom_flow(
      stat = "alluvium", lode.guidance = "frontback",
      color = "darkgray", aes(fill = avg_ci)
    ) +
    ggnewscale::new_scale_fill() +
    geom_stratum(aes(fill = crop)) +
    scale_fill_viridis_d()

  ggsave(paste0("gci_",place_name,"_alluvial_frac_gt_1e-5.png"), p_kaveri_alluvial, 
    width = 16, height = 12, path = out_path)

  p_kaveri_alluvial
}


if (FALSE) {
  cauvery_path <- "/Users/gopal/Projects/sasia/spatial/unmod/CauveryBasin/Cauvery_boundary5.shp"
  cauvery_sf <- st_read(cauvery_path)

  gci_path <- "~/Projects/Data/crops/GCI_QC/"
  gci_all <- read_gci_allyears_boundary(gci_path, cauvery_sf)

  place_name = 'cauvery5'
  plot_gci_all_years(place_name = place_name, gci_all, cauvery_sf, 4)
  plot_gci_change_all_years("cauvery5", gci_all, cauvery_sf, 4)

  plot_gci_ggalluvial("cauvery5", gci_all, cauvery_sf)
}