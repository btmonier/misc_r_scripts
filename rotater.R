#--------------------------------------------------------------------
# Script Name:   rotater.R
# Description:   Rotation function with point options
# Author:        Brandon Monier
# Created:       2019-10-30 at 15:20:56
# Last Modified: 2019-10-30 at 15:23:33
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to generate a function
#    that will rotate a pair of coordinates {x, y} around either
#    (A) the origin (default) or (B) a specified point.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)


## Make rotational function ----
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
n     <- 30
theta <- -135
min   <- -n
max   <- n
samp  <- sample(x = seq_len(n), size = n, replace = FALSE)


## Iterate ----
tmp_ls <- list(x = 0, y = 0)
for (i in seq_len(n)) {
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
        cx = last_x,
        cy = last_y,
        angle = theta2
    )

    tmp_ls$x[i + 1] <- tmp_rot[1]
    tmp_ls$y[i + 1] <- tmp_rot[2]
}
tmp_ls <- do.call("rbind", tmp_ls) %>%
    t() %>%
    tibble::as_tibble()


## Sanity check ----

### Parameters
# min <- -3
# max <- abs(min)
# angle <- pi / 2

### Iterate
# tmp1 <- cbind(x = 0, y = 0)
# tmp2 <- rotater(0, 1, angle = angle)
# tmp3 <- rotater(px = tmp2[1], py = , cx = tmp2[1], cy = tmp2[2], angle = -angle)
# tmp4 <- rotater(0, 3, cx = tmp3[1], cy = tmp3[2], angle = angle)
# tmp5 <- rotater(0, 4, cx = tmp4[1], cy = tmp4[2], angle = -angle)


### Iterate (2)
# angle <- -135
# tmp1 <- rotater(0, 1, 0, 0, angle = angle)
# tmp2 <- rotater(tmp1[1], tmp1[2] + 1, tmp1[1], tmp1[2], angle = -angle)
# tmp3 <- rotater(tmp2[1], tmp2[2] + 1, tmp2[1], tmp2[2], angle = angle)



# sanity <- tibble::tibble(
#     x = c(0, tmp1[1], tmp2[1], tmp3[1]),
#     y = c(0, tmp1[2], tmp2[2], tmp3[2])
# )

tmp_ls %>%
    ggplot() +
    aes(x, y) +
    geom_path() +
    geom_point() +
    xlim(min, max) +
    ylim(min, max) +
    coord_fixed()



# ## Visualize ----
# tmp_ls %>%
#     t() %>%
#     tibble::as_tibble() %>%
#     ggplot() +
#     aes(x, y) +
#     geom_path() +
#     geom_point()












