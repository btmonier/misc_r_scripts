#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   maze_generator.R
# Description:   Maze generation in R
# Author:        Brandon Monier
# Created:       2019-11-23 at 22:01:46
# Last Modified: 2019-11-23 at 22:02:55
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to generate mazes in R. We
#    will (hopefully) make a "depth-first search" algorithm...
#
#    NOTE:
#      i = row
#      j = column
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)
library(tibble)


## Global variables ----
cols <- 10
rows <- 10
w    <- 1
grid <- list()



# === Maze generation (I) ===========================================



setup <- function(width, height) {
    cols <- floor(width / w)
    rows <- floor(height / w)

    for (i in seq_len(rows)) {
        for (j in seq_len(cols)) {
            cell <- cell(i, j)
        }
    }
}


cell_lines <- function(i,
                       j,
                       w,
                       top = TRUE,
                       bottom = TRUE,
                       left = TRUE,
                       right = TRUE) {

    line_ls <- vector(mode = "list", length = i * j)

    tmp <- expand.grid(
        i = seq(1, i, w),
        j = seq(1, j, w)
    )

    names(line_ls) <- paste0("row", tmp$i, "_col", tmp$j)

    for (i in seq_len(i)) {
        for (j in seq_len(j)) {

            d_width <- 4

            if (left) {
                l_d <- c(i, j, i, j + w)
            } else {
                l_d <- rep(NA, d_width)
            }

            if (right) {
                r_d <- c(i + w, j, i + w, j + w)
            } else {
                r_d <- rep(NA, d_width)
            }

            if (top) {
                t_d <- c(i, j + w, i + w, j + w)
            } else {
                t_d <- rep(NA, d_width)
            }

            if (bottom) {
                b_d <- c(i, j, i + w, j)
            } else {
                b_d <- rep(NA, d_width)
            }



            line_ls[[paste0("row", i, "_col", j)]] <- tibble::tibble(
                # left right top bottom
                xlmin = c(l_d[1], r_d[1], t_d[1], b_d[1]),
                ylmin = c(l_d[2], r_d[2], t_d[2], b_d[2]),
                xlmax = c(l_d[3], r_d[3], t_d[3], b_d[3]),
                ylmax = c(l_d[4], r_d[4], t_d[4], b_d[4])
            )
        }
    }

    return(line_ls)

}


cell_shape <- function(i, j, w) {
    return(
        expand.grid(
            i = seq(1, i, w),
            j = seq(1, j, w)
        ) %>%
            tibble::as_tibble()
    )
}


## Visualize ----

ggplot() +
    geom_rect(
        mapping = aes(
            xmin = j,
            xmax = j + 1,
            ymin = i,
            ymax = i + 1
        ),
        data = cell_shape(i = rows, j = cols, w = w),
        fill = "#FF8F96",
    ) +
    geom_segment(
        mapping = aes(
            x    = xlmin,
            y    = ylmin,
            xend = xlmax,
            yend = ylmax
        ),
        data = cell_lines(i = rows, j = cols, w = w, right = F) %>%
            do.call("rbind", args = .)
    ) +
    xlab("x") +
    ylab("y") +
    scale_x_continuous(breaks = seq(1, 11, 1)) +
    scale_y_continuous(breaks = seq(1, 11, 1)) +
    coord_equal(xlim = seq_len(11), ylim = seq_len(11))









