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

karur_count <- karur_df_count_wide %>% 
  ungroup() %>%
  mutate(across(starts_with("X"),
                function(x) factor(x, levels = 3:0))) %>% #, labels = c("Triple","Double","Single","Fallow")))) %>%
  mutate(plot_highlight = dplyr::case_when(
           grepl("3_3_3", crop_order) ~ "-3-3-3-",
           grepl("2", crop_order) ~ "-2-",
           grepl("1_1_1", crop_order) ~ "-1-1-1-",
           grepl("0_0_0_0", crop_order) ~ "-0-0-0-0-",
           TRUE ~ "-x-"
         ),
         plot_highlight = factor(plot_highlight, levels = c("-3-3-3-", "-2-","-1-1-1-","-0-0-0-0-","-x-")),
         crop_2015 = X0_crops) %>%
  pivot_longer(cols = starts_with("X"), names_to = "varname", values_to = "crop") %>%
  group_by(id) %>%
  mutate(num_unique = length(unique(crop)),
         year = as.numeric(gsub("X([0-9]+)_.*","\\1",varname))+2015) %>%
  filter(num_unique > 1 | crop != "Fallow")

# grepl("0_0_0_0", karur_count$crop_order)


p_karur_alluvial <- ggplot(karur_count,
       aes(x = year, y = n * 900 / 1e4, stratum = crop, alluvium = id,
           label = crop)) +
  scale_fill_manual("Select\nmulti-year\npatterns",values = c("forestgreen","lightgreen","gold2","tan","gray"), 
                    guide = guide_legend(order = 2)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray", aes(fill = plot_highlight)) +
  ggnewscale::new_scale_fill() +
  geom_stratum(aes(fill = crop)) +
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

ggsave("karur_alluvial.png", p_karur_alluvial, width = 5, height = 2.25, path = out_path)

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

