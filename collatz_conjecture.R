#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   collatz_conjecture.R
# Description:   Collatz conjecture formula
# Author:        Brandon Monier
# Created:       2019-10-28 at 17:18:44
# Last Modified: 2019-10-30 at 18:06:15
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to show and visualize the
#    Collatz Conjecture:
#    (https://en.wikipedia.org/wiki/Collatz_conjecture)
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)


## Make functions ----

### Collatz conjecture
collatz <- function(n) {
    if (n %% 2 == 0) {
        n / 2
    } else {
        (3 * n) + 1
    }
}

### Generate Collatz sequence
collatzSequence <- function(n) {
    c_vect <- n

    while (n != 1) {
        n <- collatz(n)
        c_vect <- c(c_vect, n)
    }
    return(c_vect)
}

### Coordinate rotation function
rotater <- function(px, py, cx = 0, cy = 0, angle) {

    angle <- angle * pi / 180

    s <- sin(angle)
    c <- cos(angle)

    x_rot <- c * (px - cx) - s * (py - cy) + cx
    y_rot <- s * (px - cx) - c * (py - cy) + cy

    return(cbind(x = x_rot, y = y_rot))
}



# === Unit tests ====================================================

## Parameters ----
n     <- 1e4
theta <- -100


## Iterate ----
tmp_coll <- vector(mode = "list", length = n)
for (j in seq_len(n)) {
    samp  <- collatzSequence(j)
    tmp_ls <- list(x = 0, y = 0)
    for (i in seq_along(samp)) {
        if (samp[i] %% 2 == 0) {
            theta2 <- theta
        } else {
            theta2 <- -theta
        }

        last_x <- tail(x = tmp_ls$x, n = 1)
        last_y <- tail(x = tmp_ls$y, n = 1)

        tmp_rot <- rotater(
            px = last_x,
            py = last_y + 1,
            cx = last_x + 1,
            cy = last_y,
            angle = theta2
        )

        tmp_ls$x[i + 1] <- tmp_rot[1]
        tmp_ls$y[i + 1] <- tmp_rot[2]
    }
    tmp_ls <- do.call("rbind", tmp_ls) %>%
        t() %>%
        tibble::as_tibble()
    tmp_coll[[j]] <- tmp_ls
}
tmp_coll <- do.call("rbind", tmp_coll)


## Visualize ----
tmp <- tmp_coll %>%
    ggplot() +
    aes(x, y) +
    geom_point(color = "black", shape = 46, alpha = 0.05) +
    theme_void() +
    theme(panel.background = element_rect(fill = "white"))


## Write to disk ----
ggsave(
    plot = last_plot(),
    filename = paste0("collatz_test_", Sys.Date(), ".png"),
    device = "png",
    width = 10,
    height = 10,
    units = "in",
    dpi = 500
)

