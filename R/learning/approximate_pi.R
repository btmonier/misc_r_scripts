#!/usr/bin/env Rscript

#------------------------------------------------------------------------------
# Title:   Approximate the value of PI
# Author:  Brandon Monier
# Created: 2020-05-23
#
# Approximate the value of PI using simple mathematic formulas.
#------------------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(dplyr)
library(ggplot2)
library(glue)
library(logging)
library(magrittr)
library(tibble)


## Parameters ----
r <- 20
iter <- 15e3


## Calculate PI ----
logging::loginfo(glue::glue("Approximating pi with {iter} data points."))
x <- runif(iter, -r, r)
y <- runif(iter, -r, r)
d <- (x * x) + (y * y)

pi_df <- tibble::tibble(
    x = x,
    y = y,
    d = d
) %>%
    dplyr::mutate(
        in_circle = ifelse(d < r * r, 1, 0) %>%
            as.factor()
    )
# print(pi_df)


## Visualize ----
logging::loginfo("Visualizing pi approximation.")
plt_pi <- pi_df %>%
    ggplot() +
    aes(x, y, color = in_circle) +
    geom_point(size = 0.5, alpha = 0.5) +
    xlim(-r, r) +
    ylim(-r, r) +
    coord_equal() +
    theme(
        legend.position = "none"
    )
print(plt_pi)


## Calculate pi ----
circle <- length(pi_df$in_circle[pi_df$in_circle == 1])
app_pi <- 4 * (circle / iter)
logging::loginfo(glue::glue("The value of pi is approximately {app_pi}."))
