#--------------------------------------------------------------------
# Script Name:   collatz_tests.R
# Description:   Rotation function with point options
# Author:        Brandon Monier
# Created:       2019-10-30 at 15:20:56
# Last Modified: 2019-11-06 at 16:30:44
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to generate a function
#    that will rotate a pair of coordinates {x, y} around either
#    (A) the origin (default) or (B) a specified point.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(dplyr)
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

### Make rotational function based on prior angle
rotater <- function(px, py, steps, angle, angle_prior) {
    angle <- angle + angle_prior
    angle <- angle * pi / 180
    x_rot <- steps * cos(angle)
    y_rot <- steps * sin(angle)

    return(
        list(
            x = x_rot + px,
            y = y_rot + py,
            angle = angle * 180 / pi
        )
    )
}



# === Unit test 01 (fail) ===========================================

# ## Parameters ----
# n     <- 27
# theta <- -90
# samp <- collatzSequence(n)
#
# ## Iterate ----
# tmp_ls <- list(x = 0, y = 0)
# for (i in seq_along(samp)) {
#     if (samp[i] %% 2 == 0) {
#         theta2 <- theta
#     } else {
#         theta2 <- -theta * 2
#     }
#
#     last_x <- tail(x = tmp_ls$x, n = 1)
#     last_y <- tail(x = tmp_ls$y, n = 1)
#
#     tmp_rot <- rotater(
#         px = last_x,
#         py = last_y + 1,
#         cx = last_x,
#         cy = last_y,
#         angle = theta2
#     )
#
#     tmp_ls$x[i + 1] <- tmp_rot[1]
#     tmp_ls$y[i + 1] <- tmp_rot[2]
# }
# tmp_ls <- do.call("rbind", tmp_ls) %>%
#     t() %>%
#     tibble::as_tibble()
#
#
# ## Visualize ----
#
# ### min/max parameters
# max <- max(
#     tmp_ls$x %>% abs() %>% max(),
#     tmp_ls$y %>% abs() %>% max()
# ) %>% ceiling()
# min <- -max
#
# ### plot
# tmp_ls %>%
#     ggplot() +
#     aes(x, y) +
#     geom_path() +
#     geom_point() +
#     xlim(min, max) +
#     ylim(min, max) +
#     coord_fixed()



# === Unit test 02 ==================================================

## Iterate manually ----
step <- 1
angle1 <- 45
angle2 <- -45
tmp1 <- rotater(0, 0, step, angle1, 0)
tmp2 <- rotater(tmp1$x, tmp1$y, step, angle2, tmp1$angle)
tmp3 <- rotater(tmp2$x, tmp2$y, step, angle1, tmp2$angle)
tmp4 <- rotater(tmp3$x, tmp3$y, step, angle1, tmp3$angle)

tmp_ls <- tibble::tibble(
    x = c(0, tmp1$x, tmp2$x, tmp3$x, tmp4$x),
    y = c(0, tmp1$y, tmp2$y, tmp3$y, tmp4$y)
)


## Visualize ----

### min/max parameters
max <- max(
    tmp_ls$x %>% abs() %>% max(),
    tmp_ls$y %>% abs() %>% max()
) %>% ceiling()
min <- -max

### plot
tmp_ls %>%
    ggplot() +
    aes(x, y) +
    geom_path() +
    geom_point() +
    xlim(min, max) +
    ylim(min, max) +
    coord_fixed()



# === Unit test 03 ==================================================

## Parameters ----
even_turn <- 30
odd_turn  <- -15

n     <- 7
samp  <- collatzSequence(n) %>% rev()
steps <- 1


## Iterate ----
tmp_ls <- list(x = 0, y = 0, angle = 0)
for (i in seq_along(samp)) {
    if (samp[i] %% 2 == 0) {
        theta <- even_turn
    } else {
        theta <- odd_turn
    }

    last_x <- tail(x = tmp_ls$x, n = 1)
    last_y <- tail(x = tmp_ls$y, n = 1)
    last_angle <- tail(x = tmp_ls$angle, n = 1)

    tmp_rot <- rotater(
        px          = last_x,
        py          = last_y,
        steps       = step,
        angle       = theta,
        angle_prior = last_angle
    )

    tmp_ls$x[i + 1] <- tmp_rot$x
    tmp_ls$y[i + 1] <- tmp_rot$y
    tmp_ls$angle[i + 1] <- tmp_rot$angle
}
tmp_ls <- do.call("rbind", tmp_ls) %>%
    t() %>%
    tibble::as_tibble()


## Visualize ----

### min/max parameters
max <- max(
    tmp_ls$x %>% abs() %>% max(),
    tmp_ls$y %>% abs() %>% max()
) %>% ceiling()
min <- -max

### plot
tmp_ls %>%
    ggplot() +
    aes(x, y) +
    geom_path() +
    geom_point() +
    xlim(min, max) +
    ylim(min, max) +
    coord_fixed()



# === Unit test 04 ==================================================

## Parameters ----
n    <- 2000
step <- 1
even_turn <- 5
odd_turn  <- -10

## Iterate ----
tmp_coll <- vector("list", length = n)
for (j in seq_len(n)) {
    tmp_ls <- list(x = 0, y = 0, angle = 0)
    samp <- collatzSequence(j) %>% rev()
    for (i in seq_along(samp)) {
        if (samp[i] %% 2 == 0) {
            theta <- even_turn
        } else {
            theta <- odd_turn
        }

        last_x <- tail(x = tmp_ls$x, n = 1)
        last_y <- tail(x = tmp_ls$y, n = 1)
        last_angle <- tail(x = tmp_ls$angle, n = 1)

        tmp_rot <- rotater(
            px          = last_x,
            py          = last_y,
            steps       = step,
            angle       = theta,
            angle_prior = last_angle
        )

        tmp_ls$x[i + 1] <- tmp_rot$x
        tmp_ls$y[i + 1] <- tmp_rot$y
        tmp_ls$angle[i + 1] <- tmp_rot$angle
    }
    tmp_ls <- do.call("rbind", tmp_ls) %>%
        t() %>%
        tibble::as_tibble() %>%
        dplyr::mutate(iter = j) %>%
        dplyr::mutate(iter = as.factor(.$iter))

    tmp_coll[[j]] <- tmp_ls
}
tmp_coll <- do.call("rbind", tmp_coll)


## Visualize ----

### min/max parameters
max <- max(
    tmp_coll$x %>% abs() %>% max(),
    tmp_coll$y %>% abs() %>% max()
) %>% ceiling()
min <- -max

### plot
tmp_coll %>%
    ggplot() +
    aes(x = x, y = y, fill = iter) +
    geom_path(aes(alpha = 0.01), size = 0.5) +
    xlim(min, max) +
    ylim(min, max) +
    coord_fixed() +
    theme(legend.position = "none")


