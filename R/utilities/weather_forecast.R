#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   weather_forecast.R
# Description:   Scrape weather.gov for weather data
# Author:        Brandon Monier
# Created:       2020-06-07 at 15:34:32
# Last Modified: 2020-06-09 at 15:18:21
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to obtain weather forecast
#    data for specific cities using weather.gov
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(dplyr, quietly = TRUE)
library(ggplot2)
library(lubridate)
library(magrittr)
library(patchwork)
library(rvest)
library(tidyr)
library(xml2)


## Get URL (Ithaca, NY example) ----

### Base
base <- "https://forecast.weather.gov/"

### City coordinates
city_coord <- "MapClick.php?lat=42.4446&lon=-76.5001"

### Data type
data_type <- "&lg=english&&FcstType=digital"

### Combine
con <- paste0(base, city_coord, data_type)


## Parameters ----

### Time zone
tz <- "America/New_York"

### Modified column names
cols <- c(
    "date",
    "hour",
    "temp_f",
    "dewpoint_f",
    "heat_index_f",
    "surface_wind_mph",
    "wind_dir",
    "gust",
    "sky_cover_perc",
    "precip_potential_perc",
    "relative_humidity_perc",
    "rain",
    "thunder"
)



# === Get weather data (table) ======================================

## Get raw data ----
df_wthr <- xml2::read_html(con) %>%
    rvest::html_nodes(xpath = "/html/body/table[6]") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::slice(-1) %>%
    .[-14, ] %>%
    split(c(rep(1, 13), rep(2, 13)))


## Split data based on overlap ----
df_wthr[[2]] <- df_wthr[[2]][, -1]
df_wthr <- do.call("cbind", df_wthr)


## Transpose data ----
df_wthr <- df_wthr %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-rowname) %>%
    tidyr::pivot_wider(
        names_from = rowname,
        values_from = value
    ) %>%
    dplyr::slice(-1) %>%
    dplyr::select(-1)


## Clean up header ----
colnames(df_wthr) <- cols


## Replace blank (e.g. `""`) with `NA` values ----
df_wthr <- df_wthr %>%
    apply(2, dplyr::na_if, "") %>%
    apply(2, dplyr::na_if, "--") %>%
    tibble::as_tibble()


## Fill in blanks and convert columns into correct data types ----
df_wthr <- df_wthr %>%
    tidyr::fill(date) %>%
    dplyr::mutate(
        date = lubridate::as_datetime(
            paste(date, hour),
            format = "%m/%d %H",
            tz = tz
        )
    ) %>%
    dplyr::mutate(
        wind_dir = factor(
            wind_dir,
            levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        ),
        rain = as.factor(rain),
        thunder = as.factor(thunder)
    ) %>%
    dplyr::select(-hour) %>%
    dplyr::mutate_if(is.character, as.numeric)


## Inspect data frame (debug) ----
print(df_wthr)



# === Visualize =====================================================

## Temperature ----
plt_temp <- df_wthr %>%
    dplyr::select(date, temp_f, dewpoint_f, heat_index_f) %>%
    tidyr::pivot_longer(c("temp_f", "dewpoint_f", "heat_index_f")) %>%
    dplyr::mutate(
        name = factor(
            name,
            levels = c("temp_f", "heat_index_f", "dewpoint_f"),
            labels = c("Temperature", "Heat index", "Dew point")
        )
    ) %>%
    ggplot() +
    aes(x = date, y = value, color = name) +
    geom_line(size = 1, na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    ylab("Temperature (F)") +
    theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom"
    )


## Wind speed ----
plt_wind <- df_wthr %>%
    dplyr::select(date, surface_wind_mph, wind_dir, gust) %>%
    tidyr::pivot_longer(c("surface_wind_mph", "gust")) %>%
    dplyr::mutate(
        name = factor(
            name,
            levels = c("surface_wind_mph", "gust"),
            labels = c("Surface wind", "Gusts")
        )
    ) %>%
    ggplot() +
    aes(x = date, y = value, color = name) +
    geom_line(size = 1, na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    ylab("Wind speed (mph)") +
    theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom"
    )


## Percentages ----
plt_perc <- df_wthr %>%
    dplyr::select(date, sky_cover_perc, precip_potential_perc, relative_humidity_perc) %>%
    tidyr::pivot_longer(c("sky_cover_perc", "precip_potential_perc", "relative_humidity_perc")) %>%
    dplyr::mutate(
        name = factor(
            name,
            levels = c("relative_humidity_perc", "precip_potential_perc", "sky_cover_perc"),
            labels = c("Relative humidity", "Precipitation potential", "Sky cover")
        )
    ) %>%
    ggplot() +
    aes(x = date, y = value, color = name) +
    geom_line(size = 1, na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    ylab("%") +
    theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom"
    )


## Plot all ----
plt_all <- (plt_temp) / (plt_wind) / (plt_perc)

start_stop <- c(
    df_wthr[[1, 1]],
    df_wthr[[48, 1]]
)

plt_all <-  plt_all +
    plot_annotation(
        title = "Weather forecast",
        subtitle = paste0(df_wthr[[1, 1]], " - ", df_wthr[[48, 1]]),
        caption = "source: weather.gov"
    )
print(plt_all)



