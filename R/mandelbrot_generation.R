#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   mandlebrot_generation.R
# Description:   Visualize the Mandelbrot Set
# Author:        Brandon Monier
# Created:       2020-06-13 at 12:59:58
# Last Modified: 2020-06-13 at 14:23:35
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to code the Mandelbrot set
#    in base R.
#
#    The Mandelbrot Set (M-Set in short) is a fractal. It is plotted
#    on the complex plane. It is an example of how intricate
#    patterns can be formed from a simple math equation. It is
#    entirely self-similar. Within the fractal, there are
#    mini-Mandelbrot Sets, which have their own M-Sets, which have
#    their own M-Sets, which have their own M-sets, etc.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)


## Functions ----

### Mandelbrot calculation
mandelbrot <- function(c) {
    z <- 0
    n <- 0

    while (abs(z) <= 2 & n < max_iter) {
        z <- (z * z) + c
        n <- n + 1
    }

    return(n)
}


## Parameters ----

### Figure path
fig_path <- "../../tmp/fig_output/"

### Matrix size
width  <- 5e3
height <- floor(width * 0.67)

### Plot window
re_start <- -2
re_end   <- 1
im_start <- -1
im_end   <- 1

### Maximum iterations for Mandelbrot calculation
max_iter <- 100



# === Create Mandelbrot matrix data set =============================

## Initialize data set ----
c_mat <- matrix(0, nrow = height, ncol = width)


## Iterate ----
for (i in seq(0, height)) {
    for (j in seq(0, width)) {
        c <- complex(
            length.out = 1,
            real = re_start + (j / width) * (re_end - re_start),
            imaginary = im_start + (i / height) * (im_end - im_start)
        )
        m <- mandelbrot(c)

        c_mat[i, j] <- m
    }
}



# === Visualize =====================================================

## Create ggplot object ----
plt_mandelbrot <- c_mat %>%
    reshape2::melt() %>%
    ggplot() +
    aes(x = Var2, y = Var1, fill = value) +
    geom_tile() +
    scale_y_reverse() +
    coord_equal() +
    theme(
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        title = element_text(face = "bold", color = "#424242")
    )


## Save to disk ----
ggsave(
    filename = "mandelbrot_set.png",
    plot = plt_mandelbrot,
    device = "png",
    path = fig_path,
    width = 6,
    height = 4,
    dpi = "retina"
)









