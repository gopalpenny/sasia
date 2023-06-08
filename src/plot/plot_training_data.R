# This script is used to visualize spectrum of different land use classes from
# landsat imagery for training points from ground truth data


# Load libraries
library(tidyverse)
library(sf)

out_path <- ggp::fig_set_output("lulc_oli8")

# read in the data
vi_cols <- c("NDVI", "MNDWI", "EVI", "GCVI", "MODCRC", 
    "NBR1", "NBR2", "NDTI", "SAVI", "STI", "TVI")
training_ts <- read_csv("./data/format/training_landsat_2014_2015.csv") %>%
    dplyr::filter(clouds_shadows == 0) %>%
    mutate(
        NDVI = (nir - red) / (nir + red),
        MNDWI = (green - swir1) / (green + swir1),
        EVI = 2.5 * (nir - red) / (nir + 6 * red - 7 * blue + 1),
        GCVI = (nir / green) - 1,
        MODCRC = (swir1 - green) / (swir1 + green),
        NBR1 = (nir - swir1) / (nir + swir1),
        NBR2 = (nir - swir2) / (nir + swir2),
        NDTI = (swir1 - swir2) / (swir1 + swir2),
        SAVI = (nir - red) / (nir + red + 0.16),
        STI = swir1 / swir2,
        TVI = 60 * (nir - green) - 100 * (red - green)
    )
# lulc_gt <- read_sf('/Users/gopal/Library/CloudStorage/Dropbox/Berkeley/Research/MyResearch/2014ArkavathyIndia/Arkavathy shp file/ACCUWA_data/landuse/Tg_halli/Landuse_village_datapoints_15_01_2015.shp')
training_ids <- read_csv("./data/format/training_pt_ids.csv")

lulc <- training_ts %>% left_join(training_ids, by = "id")

# lulc %>% dplyr::filter(clouds_shadows==1)

veg_string <- paste0(
    "([Tt]omato)|([Bb]rin.al)|([Cc]hilli)|",
    "([Pp]otato)|([Cc]auliflower)|([Cc]abbage)|",
    "(Cucumber)|([Oo]nion)|([Gg]arlic)|([Gg]inger)|([Bb]eetroot)"
)
lulc <- lulc %>%
    mutate(crop_group = case_when(
        grepl("([Rr]agi)|([Bb]ean)|([Aa]varai)|(dal)|([Cc]han+a)|([Hh]orsegram)|([Mm]aize)|([Jj]owar)", Cur_status) ~ "Legume/millet",
        grepl("([Ff]allow)|([Pp]lough)|([Ww]asteland)|([Bb]arren)", Cur_status) ~ "Fallow/ploughed",
        grepl(veg_string, Cur_status) ~ "Vegetable",
        grepl("([Bb]anana)", Cur_status) ~ "Banana",
        grepl("([Pp]umpkin)|([Gg][oua]+rd)|([Ww]atermelon)", Cur_status) ~ "Gourd/melon",
        grepl("([Gg]rass)", Cur_status) ~ "Grass",
        grepl("([Gg]uava)|([Ss]apota)|([Mm]ango)", Cur_status) ~ "Orchard",
        grepl("([Ss]ilver oak)|([Cc]asuarina)|([hH]ebbevu)|([aA]cacia)|([tT]eak)|([Ee]ucal.p?tus)", Cur_status) ~ "Plantation",
        grepl("([Cc]oconut)|([Aa]recanut)", Cur_status) ~ "Palm",
        TRUE ~ Cur_status
    ))
# lulc %>% pull(crop_group) %>% unique() %>% sort()

lulc <- lulc %>%
    group_by(crop_group, image_id) %>%
    mutate(
        num_in_group = n(),
        mean_ndvi = mean(NDVI),
        mean_nir = mean(nir)
    )

lulc_crop_summary <- lulc %>%
    filter(image_id == "2015-01-13") %>%
    group_by(crop_group, image_id) %>%
    summarise(
        sd_ndvi = sd(NDVI),
        across(all_of(c("red", "green", "blue", "nir", "swir1", "swir2", "NDVI", "MNDWI")), mean),
        n = n()
    ) %>%
    arrange(NDVI) %>%
    ungroup() %>%
    mutate(crop = factor(crop_group, levels = crop_group))

lulc <- lulc %>%
    mutate(crop = factor(crop_group, levels = lulc_crop_summary$crop_group))


# %% Plot Training data for 2015-01-13
ggplot(lulc %>% dplyr::filter(image_id == "2015-01-13", num_in_group > 10)) +
    geom_boxplot(aes(crop, NDVI, color = mean_nir), outlier.shape = NA) +
    geom_point(aes(crop, NDVI, color = mean_nir),
        size = 2,
        position = position_jitter(), alpha = 0.5
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_NDVI_nir <- ggplot(mapping = aes(NDVI, nir, color = crop, shape = crop)) +
    geom_point(data = lulc %>% dplyr::filter(image_id == "2015-01-13", num_in_group > 10)) +
    geom_point(
        data = lulc_crop_summary %>% dplyr::filter(image_id == "2015-01-13", n > 10),
        size = 3, stroke = 2
    ) +
    scale_shape_manual(values = rep(c(0:5), 4))

p_NDVI_MNDWI <- ggplot(mapping = aes(NDVI, MNDWI, color = crop, shape = crop)) +
    geom_point(data = lulc %>% dplyr::filter(image_id == "2015-01-13", num_in_group > 10)) +
    geom_point(
        data = lulc_crop_summary %>% dplyr::filter(image_id == "2015-01-13", n > 10),
        size = 3, stroke = 2
    ) +
    scale_shape_manual(values = rep(c(0:5), 4))


# ggplot(lulc %>% dplyr::filter(image_id == "2015-01-13")) +
#     geom_histogram(aes(crop)) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))


# when **typing** text it gets formatted *like this\* as markdown.

### 
# lulc %>% dplyr::filter(image_id == "2015-01-13", crop == "cabbage")

crops_selected <- lulc_crop_summary %>%
    dplyr::filter(n > 10) %>%
    pull(crop)
ggplot(lulc %>% dplyr::filter(crop %in% crops_selected, NDVI > -0.2)) +
    geom_line(aes(image_id, NDVI, color = crop, group = id), alpha = 0.1) +
    stat_smooth(aes(image_id, NDVI, color = crop), se = FALSE, span = 0.05)+
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")# +
    # theme(axis.text.x = element_text(angle = 15, hjust = 1))


# lulc %>% dplyr::filter(crop %in% crops_selected, clouds_shadows == 1) %>% nrow()

# lulc_bands <- lulc %>%
#     dplyr::select(blue, green, red, nir, swir1, swir2) %>%
#     st_set_geometry(NULL) %>%
#     as.matrix()


lulc_vi <- lulc %>% 
    dplyr::filter(crop %in% crops_selected, NDVI > -0.2) %>% 
    ungroup() %>% 
    dplyr::select(all_of(c("id","image_id","crop",vi_cols))) %>%
    pivot_longer(cols = vi_cols, names_to = "vi", values_to = "value")

colors_all <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
crop_cols <- sample(colors_all, 15)

# pie(rep(1, 15), col=crop_cols)


ggplot(lulc_vi) +
    # geom_line(aes(image_id, NDVI, color = crop, group = id), alpha = 0.1) +
    stat_smooth(aes(image_id, value, color = crop), se = FALSE, span = 0.5)+
    scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
    # scale_color_manual(values = crop_cols) +
    facet_wrap(~vi, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



p_palm <- ggplot(lulc_vi %>% dplyr::filter(crop == "Palm", vi == "NDVI")) +
    # geom_line(aes(image_id, NDVI, color = crop, group = id), alpha = 0.1) +
    geom_abline(intercept = 0.2, slope = 0, color = "black", alpha = 0.5) +
    geom_point(aes(image_id, value, color = vi))+
    stat_smooth(aes(image_id, value, color = vi), se = FALSE, span = 0.25)+
    scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
    # scale_color_manual(values = crop_cols) +
    ylim(c(0.1,0.4)) +
    facet_wrap(~id) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


p_plantation <- ggplot(lulc_vi %>% dplyr::filter(crop == "Plantation", vi == "NDVI")) +
    # geom_line(aes(image_id, NDVI, color = crop, group = id), alpha = 0.1) +
    geom_abline(intercept = 0.2, slope = 0, color = "black", alpha = 0.5) +
    geom_point(aes(image_id, value, color = vi))+
    stat_smooth(aes(image_id, value, color = vi), se = FALSE, span = 0.25)+
    scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
    # scale_color_manual(values = crop_cols) +
    ylim(c(0.1,0.4)) +
    facet_wrap(~id) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_plantation

p_plantation
lulc_vi_veg_ids <- lulc_vi %>% dplyr::filter(grepl("Vegetable",crop), vi == "NDVI") %>%
    pull(id) %>% unique()
p_vegmil <- ggplot(lulc_vi %>% dplyr::filter(grepl("Vegetable",crop), vi == "NDVI") %>% sample_n(40)) +
    # geom_line(aes(image_id, NDVI, color = crop, group = id), alpha = 0.1) +
    geom_abline(intercept = 0.2, slope = 0, color = "black", alpha = 0.5) +
    geom_point(aes(image_id, value, color = vi))+
    stat_smooth(aes(image_id, value, color = vi), se = FALSE, span = 0.25)+
    scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
    # scale_color_manual(values = crop_cols) +
    ylim(c(0.1,0.4)) +
    facet_wrap(~id) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_vegmil


lulc_vi_millet_ids <- lulc_vi %>% dplyr::filter(grepl("illet",crop), vi == "NDVI") %>%
    pull(id) %>% unique()
p_millet <- ggplot(lulc_vi %>% dplyr::filter(vi == "NDVI", id %in% sample(lulc_vi_millet_ids,40))) +
    # geom_line(aes(image_id, NDVI, color = crop, group = id), alpha = 0.1) +
    geom_abline(intercept = 0.2, slope = 0, color = "black", alpha = 0.5) +
    geom_point(aes(image_id, value, color = vi))+
    stat_smooth(aes(image_id, value, color = vi), se = FALSE, span = 0.25)+
    scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
    # scale_color_manual(values = crop_cols) +
    ylim(c(0.1,0.4)) +
    facet_wrap(~id) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("crop_ts_millet.png",p_millet, path = out_path, width = 12, height = 10)
####################################################################################
####################################################################################
#################################################################################### 

library(MASS)
library(patchwork)
# lulc_tsne = tsne(lulc_bands, perplexity = 50, epoch_callback = ecb)

lulc_norm <- lulc %>% dplyr::mutate(across(
    all_of(c("blue", "green", "red", "nir", "swir1", "swir2")),
    scales::rescale
))


lda_lulc <- MASS::lda(crop ~ blue + green + red + nir + swir1 + swir2, data = lulc_norm)

# lda_lulc_predict <- predict(lda_lulc)

lulc_lda <- lulc %>% bind_cols(predict(lda_lulc)$x)
lulc_lda_summary <- lulc_lda %>%
    group_by(crop, num_in_group) %>%
    summarize(across(all_of(c("LD1", "LD2", "LD3", "LD4", "NDVI")), mean))


pLD12 <- ggplot(mapping = aes(LD1, LD2, color = crop, shape = crop)) +
    geom_point(data = lulc_lda %>% dplyr::filter(num_in_group > 2), alpha = 0.5) +
    geom_point(
        data = lulc_lda_summary %>% dplyr::filter(num_in_group > 2),
        size = 3, stroke = 2
    ) +
    scale_shape_manual(values = rep(c(0:5), 4))


pLD12_NDVI <- ggplot(mapping = aes(LD1, NDVI, color = crop, shape = crop)) +
    geom_point(data = lulc_lda %>% dplyr::filter(num_in_group > 2), alpha = 0.5) +
    geom_point(
        data = lulc_lda_summary %>% dplyr::filter(num_in_group > 2),
        size = 3, stroke = 2
    ) +
    scale_shape_manual(values = rep(c(0:5), 4))

pLD34 <- ggplot(mapping = aes(LD1, LD3, color = crop, shape = crop)) +
    geom_point(data = lulc_lda %>% dplyr::filter(num_in_group > 2), alpha = 0.5) +
    geom_point(
        data = lulc_lda_summary %>% dplyr::filter(num_in_group > 2),
        size = 3, stroke = 2
    ) +
    scale_shape_manual(values = rep(c(0:5), 4))

# install.packages("patchwork")

pLD12 / pLD34 / pLD12_NDVI + patchwork::plot_layout(guides = "collect")
