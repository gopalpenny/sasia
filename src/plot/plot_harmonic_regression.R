library(tidyverse)
t <- seq(0, 1, length.out = 1000)

cos3 <- cos(pi * 3 * t)
sin3 <- sin(pi * 3 * t)
cos6 <- cos(pi * 6 * t)
sin6 <- sin(pi * 6 * t)


lulc_ts <- read_csv("./data/format/lulc_training_ts.csv")
start_date <- as.Date("2014-05-01")

class_bands <- c("red", "nir", "swir1", "swir2", "NDVI", "GCVI")
lulc_ts_long <- lulc_ts %>%
    mutate(t_frac = as.numeric(date - start_date) / 365) %>%
    # dplyr::filter(crop %in% crops_selected, NDVI > -0.2) %>%
    ungroup() %>%
    dplyr::select(all_of(c("id", "t_frac", "date", "crop", class_bands))) %>%
    pivot_longer(cols = class_bands, names_to = "band", values_to = "value")

# lulc_ts_id <- lulc_ts_long %>% filter(id == 0, band == "NDVI")



get_harmonic_regression <- function(df) {
    harm_lm <- lm(value ~ cos3 + sin3 + cos6 + sin6, df)
    df <- tibble(
        coef = harm_lm$coefficients,
        names = gsub("[\\(\\)]", "", names(harm_lm$coefficients))
    ) %>%
        pivot_wider(names_from = names, values_from = coef)
    return(df)
}

lulc_ts_harmonics <- lulc_ts_long %>%
    mutate(
        cos3 = cos(pi * 3 * t_frac),
        cos3 = cos(pi * 3 * t_frac),
        sin3 = sin(pi * 3 * t_frac),
        cos6 = cos(pi * 6 * t_frac),
        sin6 = sin(pi * 6 * t_frac)
    ) %>%
    group_by(id, band, crop) %>%
    nest(data = c(t_frac, date, value, cos3, sin3, cos6, sin6)) %>%
    mutate(harmonics = lapply(data, get_harmonic_regression))
# summarize(get_harmonic_regression(value, cos3, sin3, cos6, sin6))



plot_band_crop_harmonic_regression <- function(lulc_ts_harmonics, crop_x, band_y) {
    # band_y <- "GCVI"
    # crop_x <- "Plantation"
    ids_crop_band <- lulc_ts_harmonics %>%
        dplyr::filter(band == band_y, crop == crop_x) %>%
        pull(id)

    ids_crop_band_select <- sample(ids_crop_band, min(c(30, length(ids_crop_band))))

    lulc_ts_id_harmonics <- lulc_ts_harmonics %>%
        dplyr::filter(id %in% ids_crop_band_select, band == band_y, crop == crop_x)

    lulc_ts_id_harmonics_pred <- lulc_ts_id_harmonics %>%
        unnest(cols = harmonics) %>%
        crossing(t = seq(0, 1, length.out = 500)) %>%
        mutate(pred = Intercept +
            cos3 * cos(pi * 3 * t) + sin3 * sin(pi * 3 * t) +
            cos6 * cos(pi * 6 * t) + sin6 * sin(pi * 6 * t))

    lulc_ts_id_obs <- lulc_ts_id_harmonics %>%
        unnest(cols = data)


    p_crop_band_harmonics <- ggplot() +
        geom_line(data = lulc_ts_id_harmonics_pred, aes(x = t, y = pred), color = "darkred") +
        geom_point(data = lulc_ts_id_obs, aes(x = t_frac, y = value), alpha = 0.5) +
        ylab(band_y) +
        ggtitle(crop_x) +
        coord_cartesian(ylim = c(0.1, 0.5)) +
        facet_wrap(~id, nrow = 5)

    return(p_crop_band_harmonics)
}

unique(lulc_ts_harmonics$crop)
plot_band_crop_harmonic_regression(lulc_ts_harmonics, "Plantation", "NDVI")
plot_band_crop_harmonic_regression(lulc_ts_harmonics, "Legume/millet", "NDVI")
plot_band_crop_harmonic_regression(lulc_ts_harmonics, "grapes", "NDVI")
plot_band_crop_harmonic_regression(lulc_ts_harmonics, "Palm", "NDVI")
plot_band_crop_harmonic_regression(lulc_ts_harmonics, "Vegetable", "NDVI")

# lulc_ts_id_harmonics$data[[1]]
# get_harmonic_regression(lulc_ts_id_harmonics$data[[1]])


coef_lm["cos3"]

df <- data.frame(
    t = t,
    cos3 = cos3,
    sin3 = sin3,
    cos6 = cos6,
    sin6 = sin6
)

df_long <- df %>%
    pivot_longer(
        cols = c(cos3, sin3, cos6, sin6),
        names_to = "variable", values_to = "value"
    )

ggplot(df_long) +
    geom_line(aes(x = t, y = value, color = variable))
