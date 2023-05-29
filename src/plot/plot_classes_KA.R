# plot_classes_KA

library(terra)
library(tidyterra)
library(tidyverse)
library(ggalluvial)
library(data.table)
library(sf)

out_path <- ggp::fig_set_output("plot_classes_KA")


# classy_paths <- c("/Users/gopal/Google Drive/_Research/Research projects/ML/classy/classy_downloads",
#                   "/Users/gopalpenny/Library/CloudStorage/GoogleDrive-gopalpenny@gmail.com/My Drive/_Research/Research projects/ML/classy/classy_downloads")
# classy_path <- classy_paths[file.exists(classy_paths)][1]
# karur_path <- file.path(classy_path,"karur_crops_2015_2021.tif")

gadm_ind_path <- "~/Projects/Data/gadm/IND_adm_shp/IND_adm1.shp"

ind_sf <- st_read(gadm_ind_path)
ka_sf <- ind_sf %>% dplyr::filter(grepl("Karnataka",NAME_1))

gci_path <- "~/Projects/Data/crops/GCI_QC/"


gci_filepaths <- list.files(gci_path, pattern = "GCI.*.tif$",full.names = TRUE)
gci_filepaths <- gci_filepaths[!grepl("_QC_",gci_filepaths)]

gci_yr_path <- gci_filepaths[1]

read_gci_crop <- function(gci_yr_path, crop_sf) {
  gci_yr_world <- rast(gci_yr_path)
  gci_yr_crop <- terra::crop(gci_yr_world, vect(crop_sf))
  names(gci_yr_crop) <- gsub(".*GCI_([0-9]+).tif","gci\\1",gci_yr_path)
  return(gci_yr_crop)
}

gci_ka_list <- lapply(gci_filepaths, read_gci_crop, crop_sf = ka_sf)
gci_ka <- do.call(c,gci_ka_list)

gci_colors <- c("forestgreen","lightgreen","gold2","tan")

p_gci_ka_all_years <- ggplot() + 
  geom_spatraster(data = gci_ka, aes()) +
  geom_sf(data = ka_sf, aes(), fill = NA, color = "white") +
  scale_fill_manual(values = rev(gci_colors)) +
  facet_wrap(~lyr, nrow = 3)
ggsave('GCI_Karnataka_allyears.png',p_gci_ka_all_years, path = out_path, width = 14, height = 10)

gci_ka_diff_2001 <- gci_ka - gci_ka$gci2001
p_gci_ka_all_years_diff <- ggplot() + 
  geom_spatraster(data = gci_ka_diff_2001, aes()) +
  geom_sf(data = ka_sf, aes(), fill = NA, color = "gray") +
  scale_fill_gradient2() +
  facet_wrap(~lyr, nrow = 3)
ggsave('GCI_Karnataka_allyears_diff_2001.png',p_gci_ka_all_years_diff, path = out_path, width = 14, height = 10)


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

gci_ka_dt <- as.data.table(as.data.frame(gci_ka))

gci_colnames <- names(gci_ka)


gci_ka_pattern <- gci_ka_dt[, .N, by = gci_colnames]
gci_ka_pattern <- gci_ka_pattern[order(-N)]

gci_ka_pattern[N > 1000, count := sum(N)]

# across(.cols = )

# ggplot(karur_dt) + geom_raster(aes(x,y, fill= plot_highlight))

karur_df_count_wide <- gci_ka_df %>% 
  # dplyr::select(-y,-y) %>%
  group_by(across(everything())) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(id=row_number()) %>%
  rowwise() %>%
  mutate(crop_order = paste(c_across(starts_with("gci")), collapse = "_")) %>%
  group_by(X0_crops) %>%
  arrange(X0_crops, desc(n)) %>%
  mutate(id_2015=row_number()) %>%
  filter(id_2015 <= 10)


gci_ka_pattern[N > 1000, count := sum(N)]

gci_ka_pattern[, count := sum(N)]
gci_ka_pattern[, frac := N / count]
gci_ka_pattern[, id := 1:.N]


gci_ka_pattern_long <- melt(gci_ka_pattern, id.vars = c("N","count","frac","id"), measure_vars = gci_colnames, value.name = "crop")
gci_ka_pattern_long[, year := gsub("gci","",variable)]
gci_ka_pattern_long[, avg_ci := mean(as.numeric(crop)), by = "id"]

gci_ka_pattern_long[frac > 0.001]

gci_ka_pattern[frac>0.00001, .N]
p_karur_alluvial <- ggplot(as_tibble(gci_ka_pattern_long[frac>0.00001 & frac < 0.08]),
       aes(x = year, y = frac * 100, stratum = crop, alluvium = id,
           label = crop)) +
  scale_fill_viridis_c() +
  # scale_fill_manual("Select\nmulti-year\npatterns",values = c("forestgreen","lightgreen","gold2","tan","gray"), 
  #                   guide = guide_legend(order = 2)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray", aes(fill = avg_ci)) +
  ggnewscale::new_scale_fill() +
  geom_stratum(aes(fill = crop)) +
  scale_fill_viridis_d()
p_karur_alluvial
  
  scale_fill_manual('Annual\ncropping\nintensity',values = c("forestgreen","lightgreen","gold2","tan"), 
                    guide = guide_legend(order = 1)) +
  # geom_text(stat = "stratum", aes(label = after_stat(stratum)))
  scale_x_continuous(breaks = 2015:2021) +
  scale_y_continuous("Area (ha)",labels = scales::label_number(scale_cut = scales::cut_si("")))+
  ggp::t_manu() %+replace% 
  theme(axis.title.x = element_blank(),legend.box = "horizontal",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10))
p_karur_alluvial

ggsave("karur_alluvial_frac_gt_1e-5.png", p_karur_alluvial, width = 16, height = 12, path = out_path)

ggp::obj_size(karur_df)


# Map  patterns of change
karur_dt <- data.table(karur_df)
karur_dt[, crop_order := paste(X0_crops,X1_crops,X2_crops,X3_crops,X4_crops,X5_crops,X6_crops, sep = "_")]

karur_dt[, plot_highlight := as.numeric(0)]
# karur_dt$plot_highlight <- as.numeric(0)
karur_dt[grepl("3_3_3",crop_order), plot_highlight:=4]
karur_dt[crop_order == "1_0_0_0_1_2_1", plot_highlight:=3]
karur_dt[crop_order == "1_0_0_0_1_1_1", plot_highlight:=2]
karur_dt[crop_order == "0_0_0_0_0_1_1", plot_highlight:=1]

r_karur_patterns <- r_karur[[1]]
values(r_karur_patterns) <- karur_dt$plot_highlight
writeRaster(r_karur_patterns, file.path(out_path, "karur_crop_patterns.tif"))
plot(r_karur_patterns)



# Map  patterns of change
karur_dt2 <- data.table(karur_df)
karur_dt2[, crop_order := paste(X0_crops,X1_crops,X2_crops,X3_crops,X4_crops,X5_crops,X6_crops, sep = "_")]

karur_dt2[, plot_highlight := as.numeric(0)]
karur_dt2[grepl("3_3_3",crop_order), plot_highlight:=4]
karur_dt2[grepl("2",crop_order), plot_highlight:=3]
karur_dt2[grepl("1_1_1",crop_order), plot_highlight:=2]
karur_dt2[grepl("0_0_0_0",crop_order), plot_highlight:=1]

r_karur_patterns2 <- r_karur[[1]]
values(r_karur_patterns2) <- karur_dt2$plot_highlight
writeRaster(r_karur_patterns2, file.path(out_path, "karur_crop_patterns2.tif"))
plot(r_karur_patterns2)



RStoolbox::ggR(r_karur_patterns2,geom_raster = TRUE) +
  geom_sf()

