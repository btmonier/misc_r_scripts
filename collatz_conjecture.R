#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   collatz_conjecture.R
# Description:   Collatz conjecture formula
# Author:        Brandon Monier
# Created:       2019-10-28 at 17:18:44
# Last Modified: 2019-10-29 at 12:56:39
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


## Make function ----
collatz <- function(n) {
    if (n %% 2 == 0) {
        n / 2
    } else {
        (3 * n) + 1
    }
}


## Create vector of Collatz sequence ----
collatzSequence <- function(n) {
    c_vect <- n

    while (n != 1) {
        n <- collatz(n)
        c_vect <- c(c_vect, n)
    }
    return(c_vect)
}


n <- 1000
tmp_ls <- vector("list", length = n)
for (i in seq_len(n)) {
    tmp <- collatzSequence(i)
    tmp_ls[[i]]$seq <- tmp
    tmp_ls[[i]]$steps <- length(tmp)
}

tmp_ls[[100]]$seq %>%
    tibble::as_tibble() %>%
    ggplot() +
    aes(x = seq_along(value), y = value) +
    geom_line(stat = "identity") +
    labs(
        x = "Index",
        y = "Collatz value"
    ) +
    theme(
        axis.title = element_text(face = "bold")
    )


rotater <- function(x, y, theta) {
    theta <- (theta * pi / 180)
    return(
        list(
            x = ( x * cos(theta)) + (y * sin(theta)),
            y = (-x * sin(theta)) + (y * cos(theta))
        )
    )
}

x <- 0
y <- 0
n <- collatzSequence(n = 5)
incr <- 1
angle <- 60

tmp_ls <- list(x = x, y = y)
for(i in seq_along(n)) {
    if (n[i] %% 2 == 0) {
        angle <- angle
    } else {
        angle <- -angle
    }
    rot <- rotater(
        x = 0,
        y = i,
        theta = angle
    )
    tmp_ls$x[i + 1] <- rot$x
    tmp_ls$y[i + 1] <- rot$y + i
}

max_all <- max(c(max(tmp_ls$x %>% abs()), max(tmp_ls$y %>% abs())))
tmp_ls %>%
    tibble::as_tibble() %>%
    ggplot() +
    aes(x, y) +
    geom_path() +
    geom_point() +
    xlim(c(-max_all, max_all)) +
    ylim(c(-max_all, max_all)) +
    coord_equal()




