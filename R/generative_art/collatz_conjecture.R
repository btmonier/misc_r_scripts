#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   collatz_conjecture.R
# Description:   Collatz conjecture formula
# Author:        Brandon Monier
# Created:       2019-10-28 at 17:18:44
# Last Modified: 2019-11-03 at 12:09:22
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

## Make rotational function based on prior angle
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



# === Unit test 01 ==================================================

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



# === Unit test 02 ==================================================

## Parameters ----
n    <- 500
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
    theme_void() +
    theme(legend.position = "none")



# === Unit test 03 ==================================================

## Parameters ----
size <- 500
n    <- sample(seq_len(1e6), size = size, replace = FALSE)
step <- 1
even_turn <- 5
odd_turn  <- -10

## Iterate ----
tmp_coll <- vector("list", length = size)
for (j in seq_len(size)) {
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
    geom_path(aes(alpha = 0.001), color = "white") +
    theme_void() +
    theme(
        legend.position = "none",
        panel.background = element_rect(fill = "black")
    )



# === FINAL VISUALIZATION ===========================================

## Parameters ----
size <- 1e4
n    <- sample(seq_len(1e6), size = size, replace = FALSE)
step <- 1
even_turn <- 5
odd_turn  <- -10

## Iterate ----
tmp_coll <- vector("list", length = size)
for (j in seq_len(size)) {
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

### make plot object
coll_plot <- tmp_coll %>%
    ggplot() +
    aes(x = x, y = y, fill = iter) +
    geom_path(aes(alpha = 0.001), color = "white", size = 0.1) +
    theme_void() +
    theme(
        legend.position = "none",
        panel.background = element_rect(fill = "black")
    )

### Write to disk...
ggsave(
    plot = last_plot(),
    filename = paste0("collatz_plot_", Sys.Date(), ".png"),
    device = "png",
    width = 13,
    height = 10,
    units = "in",
    dpi = 500
)
