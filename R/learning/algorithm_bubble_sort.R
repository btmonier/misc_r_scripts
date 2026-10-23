#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   algorithm_bubble_sort.R
# Description:   Bubble sorting algoritm
# Author:        Brandon Monier
# Created:       2020-05-25 at 11:45:51
# Last Modified: 2020-05-25 at 16:59:41
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to implement a bubble
#    sorting algorithm in base R and visualize with `gganimate`
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(gganimate)
library(ggplot2)
library(glue)
library(logging)
library(magrittr)


## Parameters ----
values <- 40
colors <- c("#6DD5FA", "#2C3E50")


## Bubble sort algorithm ----
bubbleSort <- function(x){
    n <- length(x)
    for(i in 1:(n - 1)){
        for(j in 1:(n - i)){
            if(x[j] > x[j + 1]){
                temp <- x[j]
                x[j] <- x[j + 1]
                x[j + 1] <- temp
            }
        }
    }
    return(x)
}


# === Visualize ordering ============================================

## Create "generational" data frame ----
logging::loginfo(glue::glue(" Creating visual data frame object for {values} variables."))
x <- sample(values)
n <- length(x)

gens <- list()
gens[[1]] <- tibble::tibble(
    index = seq(x),
    order = x,
    gen   = 1
)
for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
        gn <- length(gens)
        x2 <- gens[[gn]]$order
        if (x2[j] > x2[j + 1]) {
            tmp <- x2[j]
            x2[j] <- x2[j + 1]
            x2[j + 1] <- tmp
        }

        gens[[gn + 1]] <- tibble::tibble(
            index = seq(x2),
            order = x2,
            gen   = gn + 1
        )
    }

}
gens <- do.call("rbind", gens)

## Create static plot ----
logging::loginfo(" Creating `ggplot2` object.")
grad_colors <- grDevices::colorRampPalette(colors)(values)
plt_bubble <- gens %>%
    ggplot() +
    aes(x = index, y = order, fill = as.factor(order)) +
    geom_col() +
    scale_fill_manual(values = grad_colors) +
    theme(legend.position = "none")


## Animate ----

### Add `gganimate` layers
logging::loginfo(" Animating plot...")
anim_bubble <- plt_bubble +
    gganimate::transition_states(
        states = gen,
        transition_length = 3
    ) +
    ggplot2::labs(title = "Generation: {closest_state}")

### Create gif and save to disk
mod <- floor(values / 5) # <- testing dynamic frame generation
nframes <- gens$gen %>% unique() %>% length() * 2
gganimate::animate(
    plot = anim_bubble,
    nframes = nframes,
    fps = 30,
    width = 400,
    height = 300,
    renderer = gganimate::gifski_renderer("../../tmp/fig_output/bubble_sort.gif")
)


## Remove intermediate variables ----
# rm(list = setdiff(ls(), lsf.str()))


