#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   game_of_life.R
# Description:   Conway's "Game of Life"
# Author:        Brandon Monier
# Created:       2020-05-09 at 22:25:55
# Last Modified: 2020-05-10 at 03:22:35
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to code John Conway's Game
#    of Life in pure R. More info about the rules can be found here:
#
#      * https://mathworld.wolfram.com/GameofLife.html
#      * https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
#
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(dplyr)
library(gganimate)
library(ggplot2)
library(logging)
library(magrittr)
library(progress)


## Functions ----

### Get neighbor sums for a given cell
sumNeighbors <- function(m, i, j) {
    if (i == nrow(m) & j == ncol(m)) {
        sum(
            m[i    , j - 1],
            m[i - 1, j - 1],
            m[i - 1, j    ]
        )
    } else if (j == ncol(m)) {
        sum(
            m[i + 1, j - 1],
            m[i + 1, j    ],
            m[i    , j - 1],
            m[i - 1, j - 1],
            m[i - 1, j    ]
        )
    } else if (i == nrow(m)) {
        sum(
            m[i    , j - 1],
            m[i    , j + 1],
            m[i - 1, j - 1],
            m[i - 1, j    ],
            m[i - 1, j + 1]
        )
    } else{
        sum(
            m[i + 1, j - 1],
            m[i + 1, j    ],
            m[i + 1, j + 1],
            m[i    , j - 1],
            m[i    , j + 1],
            m[i - 1, j - 1],
            m[i - 1, j    ],
            m[i - 1, j + 1]
        )
    }
}

### Debug sum function
checker <- function(area) {
    for (i in seq_len(rows)) {
        for (j in seq_len(cols)) {
            message("[R: ", i, " | C: ", j, "] = ", sumNeighbors(area, i, j))
        }
    }
}

### Conway game rules
conway <- function(area) {
    rows <- nrow(area)
    cols <- ncol(area)
    area_new <- area
    for (i in seq_len(rows)) {
        for (j in seq_len(cols)) {
            tmp_sum <- sumNeighbors(area, i, j)
            cell <- area[i, j]
            if (cell == 1) {
                if (tmp_sum == 1 | tmp_sum == 0) {
                    area_new[i, j] <- 0
                } else if (tmp_sum >= 4) {
                    area_new[i, j] <- 0
                } else if (tmp_sum == 2 | tmp_sum == 3) {
                    area_new[i, j] <- 1
                }
            } else if (cell == 0) {
                if (tmp_sum == 3) {
                    area_new[i, j] <- 1
                }
            }

        }
    }
    return(area_new)
}



# === The Game of Life... ===========================================

## Random sample data ----

### Parameters
gen   <- 500       # number of generations
n_row <- 80        # number of rows in game area
n_col <- 80        # number of columns in game area
p_off <- 0.55      # probability weight for "off" states
p_on  <- 1 - p_off # probability weight for "on" state

### Initial seed area
area <- matrix(
    data = 0,
    nrow = n_row,
    ncol = n_col
)

### Create test area
area_test <- matrix(
    data = sample(
        x = c(0, 1),
        size = nrow(area) * ncol(area),
        replace = TRUE,
        prob = c(p_off, p_on)
    ),
    nrow = nrow(area),
    ncol = ncol(area)
)

### Create generations
message("--- --- ---")
logging::loginfo("Program start")
ls_conway_gen <- vector("list", gen)
pb <- progress_bar$new(
    format = " - Simulating generations [:bar] :percent eta: :eta",
    total = gen,
    clear = FALSE,
    width= 60
)
for (i in seq_len(gen)) {
    pb$tick()
    if (i == 1) {
        ls_conway_gen[[i]] <- area_test
    } else {
        ls_conway_gen[[i]] <- conway(ls_conway_gen[[i - 1]])
    }
}


## Visualize ----
message(" - Creating visualization...")
plt_conway <- ls_conway_gen %>%
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
    ) +
    gganimate::transition_states(L1) +
    labs(
        title = "John Conway's Game of Life",
        caption = "generation: {closest_state}"
    )


## Write to disk ----
message(" - Writing to disk...")
plt_conway %>%
    gganimate::anim_save(
        filename = "../../tmp/fig_output/game_of_life_test_4.gif",
        nframes = 2 * gen
    )
logging::loginfo("Program end")







