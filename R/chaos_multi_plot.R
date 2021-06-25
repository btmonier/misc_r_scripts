#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   chaos_multi_plot.R
# Description:   Iterate through random Chaos Game permutations
# Author:        Brandon Monier
# Created:       2020-06-11 at 17:41:32
# Last Modified: 2020-06-11 at 17:57:56
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to generate random images
#    of Chaos Game permutations (e.g. number of sides, vertex
#    choices, r value, etc.) using Chaos Game methods built in pure
#    R.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(logging)
library(magrittr)


## Source functions ----
source("R/n_gon_generation.R")


## Parameters ----
chaos_points <- 1e5
img_path     <- "../../tmp/fig_output/chaos_game/"
file_pre     <- "chaos_game_gen_"
total_plots  <- 100



# === Create random images ==========================================

for (i in seq(101, 100 + total_plots)) {
    logging::loginfo(paste0("Creating image for iteration: ", i))

    tmp <- chaosGame(chaos = TRUE, iter = chaos_points)

    plt_chaos <- tmp$chaos_df %>%
        ggplot() +
        aes(x = x, y = y) +
        geom_point(size = 0.2, alpha = 0.25) +
        coord_equal() +
        theme_void()


    logging::loginfo(paste0("Saving image for iteration: ", i))

    pad_num <- formatC(
        x = i,
        width = nchar(total_plots),
        format = "d",
        flag = "0"
    )

    ggsave(
        filename = paste0(file_pre, pad_num, ".png"),
        plot = plt_chaos,
        device = "png",
        path = img_path,
        width = 6,
        height = 6,
        dpi = "retina"
    )

}


















