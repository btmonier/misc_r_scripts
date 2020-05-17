#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   random_walk_snowflakes.R
# Description:   Snow flakes made using randowm walks
# Author:        Brandon Monier
# Created:       2020-05-16 at 21:13:49
# Last Modified: 2020-05-17 at 00:37:01
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to make radial symmetrical
#    patterns on a cartesian coordinate plane using point values
#    generated using a random walk procedure.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(logging)
library(magrittr)
library(scales)
library(tibble)


## Functions ----

### Rotate vector
rotate <- function(x, y, angle, r_class) {
    rad <- angle * (pi / 180)
    new_x <- x * cos(rad) - y * sin(rad)
    new_y <- y * cos(rad) + x * sin(rad)

    return(
        tibble::tibble(
            x = new_x,
            y = new_y,
            rot_class = r_class
        )
    )
}



# === Random walk data generation ===================================

## Parameters ----
iter <- 500
sig2 <- 10


## Make random walk ----
x <- seq(0, iter, 1)
w <- rnorm(n = length(x) - 1, sd = sqrt(sig2))
y <- c(0, cumsum(w)) %>% rev()


## Rotate ----
angles <- seq(0, 315, 45)
rw_sym_a <- list()
for (i in seq_along(angles)) {
    rw_sym_a[[i]] <- rotate(
        x = x,
        y = y,
        angle = angles[i],
        r_class = paste0("r_", angles[i])
    )
}
rw_sym_b <- list()
for (i in seq_along(angles)) {
    rw_sym_b[[i]] <- rotate(
        x = x,
        y = -y,
        angle = angles[i],
        r_class = paste0("-r_", angles[i])
    )
}
rw_final <- rbind(
    do.call("rbind", rw_sym_a),
    do.call("rbind", rw_sym_b)
)


## Visualize ----
max_x <- rw_final$x %>% max()
max_y <- rw_final$y %>% max()
plt_rw <- rw_final %>%
    ggplot() +
    aes(x, y) +
    # geom_point(size = 0.5) +
    geom_path(aes(color = rot_class)) +
    ylim(c(-max_x, max_x)) +
    xlim(c(-max_x, max_x)) +
    coord_equal() +
    theme_light() +
    theme(
        legend.position = "none"
    )
print(plt_rw)



# === Iterate =======================================================

## Parameters ----
total_plots <- 50
iter        <- 750
sig2        <- 15
angles      <- seq(0, 315, 45)
save_path   <- "../../tmp/fig_output/brownian_snowflakes/"
prefix      <- "rw_snow_"
colors      <- "#373B44"


## Creat and save plots to disk ----
for (plot in seq_len(total_plots)) {

    # Output logging information
    pad_num <- formatC(
        x = plot,
        width = nchar(total_plots),
        format = "d",
        flag = "0"
    )
    logging::loginfo(paste0("Creating image: ", pad_num, "..."))

    # Make random walk
    x <- seq(0, iter, 1)
    w <- rnorm(n = length(x) - 1, sd = sqrt(sig2))
    y <- c(0, cumsum(w)) %>% rev()

    # Create data with rotational symmetry
    rw_sym_a <- list()
    for (i in seq_along(angles)) {
        rw_sym_a[[i]] <- rotate(
            x = x,
            y = y,
            angle = angles[i],
            r_class = paste0("r_", angles[i])
        )
    }
    rw_sym_b <- list()
    for (i in seq_along(angles)) {
        rw_sym_b[[i]] <- rotate(
            x = x,
            y = -y,
            angle = angles[i],
            r_class = paste0("-r_", angles[i])
        )
    }
    rw_final <- rbind(
        do.call("rbind", rw_sym_a),
        do.call("rbind", rw_sym_b)
    )

    # Create plot
    max_x <- rw_final$x %>% max()
    max_y <- rw_final$y %>% max()
    plt_rw <- rw_final %>%
        ggplot() +
        aes(x, y) +
        geom_path(aes(color = rot_class)) +
        scale_color_manual(values = rep(colors, length(angles) * 2)) +
        ylim(c(-max_x, max_x)) +
        xlim(c(-max_x, max_x)) +
        coord_equal() +
        theme_light() +
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

    # Save to disk
    ggsave(
        filename = paste0(prefix, pad_num, ".png"),
        plot     = plt_rw,
        device   = "png",
        path     = save_path,
        width    = 6,
        height   = 6,
        units    = "in",
        dpi      = 300
    )
}


## Remove intermediate variables ----
rm(list = setdiff(ls(), lsf.str()))


