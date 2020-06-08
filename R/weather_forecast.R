#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   weather_forecast.R
# Description:   Scrape weather.gov for weather data
# Author:        Brandon Monier
# Created:       2020-06-07 at 15:34:32
# Last Modified: 2020-06-07 at 17:32:09
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to obtain weather forecast
#    data for specific cities using weather.gov
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(dplyr)
library(magrittr)
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
        date = as.factor(date),
        wind_dir = factor(
            wind_dir,
            levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        )
    ) %>%
    dplyr::mutate_if(is.character, as.numeric)


## Inspect data frame (debug) ----
print(df_wthr)















