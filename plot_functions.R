#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   plot_functions.R
# Description:   Miscellaneous plot funcion tests
# Author:        Brandon Monier
# Created:       2019-02-12 at 10:47:47
# Last Modified: 2019-02-12 at 10:59:47
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to create easily deployable
#    functions for plot generation using only base R.
#--------------------------------------------------------------------

## Cross plot
plotr_cross <- function(len) {
    par(
        pty = "s",
        mar = rep(0.5, 4)
    )
    plot.new()
    plot.window(
        xlim = c(-len, len),
        ylim = c(-len, len)
    )

    for (i in seq_len(4)) {
        axis(
            side = i,
            pos = 0,
            col = "grey",
            labels = FALSE,
            at = seq(-len, len),
            lwd = 0.5,
            lwd.ticks = 1
        )
    }
}
plotr_cross(len = 10)


## Make isoceles trapezoids - proof
n <- 1
coords_1 <- list(
    x = c(-0.5 * n, 1.5 * n, n, 0),
    y = c(-sqrt(3) / 2 * n, -sqrt(3) / 2 * n, 0, 0)
)


## Plot shapes
rot_seq <- c(0, 120, 240)
for (i in rot_seq) {
    rot <- i * (pi / 180)
    coords_2 <- list(
        x = coords_1$x * cos(rot) - coords_1$y * sin(rot),
        y = coords_1$x * sin(rot) + coords_1$y * cos(rot)
    )
    polygon(x = coords_2$x, y = coords_2$y)
}



