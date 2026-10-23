#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   maze_generator.R
# Description:   Maze generation in R
# Author:        Brandon Monier
# Created:       2019-11-23 at 22:01:46
# Last Modified: 2019-12-01 at 22:34:51
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to generate mazes in R. We
#    will (hopefully) make a "depth-first search" algorithm...
#
#    NOTE:
#      i = row
#      j = column
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)
library(tibble)


## Global variables ----
cols <- 15
rows <- 5
w    <- 1



# === Maze generation (I) ===========================================

## Cell data set ----
cell <- function(i,
                 j,
                 w       = 1,
                 top     = TRUE,
                 bottom  = TRUE,
                 left    = TRUE,
                 right   = TRUE,
                 visited = FALSE) {

    line_color <- "white"
    d_width <- 4

    fill_hex <- if (visited) "#ffbdbd" else "#8a8a8a"

    l_d <- if (left)   c(j    , i    , j    , i + w) else rep(NA, d_width)
    r_d <- if (right)  c(j + w, i    , j + w, i + w) else rep(NA, d_width)
    t_d <- if (top)    c(j    , i + w, j + w, i + w) else rep(NA, d_width)
    b_d <- if (bottom) c(j    , i    , j + w, i    ) else rep(NA, d_width)

    line_data = tibble::tibble(
        #         left    right   top     bottom
        xlmin = c(l_d[1], r_d[1], t_d[1], b_d[1]),
        ylmin = c(l_d[2], r_d[2], t_d[2], b_d[2]),
        xlmax = c(l_d[3], r_d[3], t_d[3], b_d[3]),
        ylmax = c(l_d[4], r_d[4], t_d[4], b_d[4])
    )

    shape_data = tibble::tibble(
        xmin = j,
        xmax = j + 1,
        ymin = i,
        ymax = i + 1,
        fill = fill_hex
    )

    return(
        list(
            line_data = line_data,
            shape_data = shape_data
        )
    )
}


## Randomly pick cell neighbor ---
pickNeighbor <- function(prev, prev_hist, rows, cols) {

    neighbors <- list(
        top = c(prev[1] + 1, prev[2]    ),
        bot = c(prev[1] - 1, prev[2]    ),
        lft = c(prev[1]    , prev[2] - 1),
        rgt = c(prev[1]    , prev[2] + 1)
    )

    filt <- lapply(neighbors, function(i) {
        all(i != 0 & i[1] <= rows & i[2] <= cols)
    }) %>%
        unlist()

    neighbors <- neighbors[filt]
    neighbors <- neighbors[!(neighbors %in% prev_hist)]

    if (length(neighbors) == 0) {
        the_one <- NULL
    } else {
        the_one <- neighbors[[sample(names(neighbors), 1)]]
    }

    return(
        list(
            the_one = the_one
        )
    )
}


## Navigate "random" path through grid ----
pathFinder <- function(rows, cols, visit = 1, empty = 0) {

    mat <- matrix(empty, nrow = rows, ncol = cols);
    mat_hist  <- list()
    prev_hist <- list()

    mat[1, 1] <- visit
    mat_hist[[1]] <- mat

    prev_hist[[1]] <- c(1, 1)
    tmp <- pickNeighbor(
        prev = c(1, 1),
        prev_hist = prev_hist,
        rows = rows,
        cols = cols
    )

    mat[tmp$the_one[1], tmp$the_one[2]] <- visit
    mat_hist[[2]] <- mat

    i <- 1
    repeat {
        prev_hist[[i + 1]] <- tmp$the_one
        tmp <- pickNeighbor(
            prev = tmp$the_one,
            prev_hist = prev_hist,
            rows = rows,
            cols = cols
        )

        if (is.null(tmp$the_one)) {
            # message("Got cornered!")
            break
        } else {
            # message("It's okay: ", i)
            # message("TMP... ", tmp)
            mat[tmp$the_one[1], tmp$the_one[2]] <- visit
            mat_hist[[i + 2]] <- mat
        }
        i <- i + 1
    }

    return(list(mat_hist = mat_hist, prev_hist = prev_hist))
}


## Show ----
cell_iterator <- function(rows, cols, path_hist, ...) {

    # Preamble
    grid_data <- matrix(data = list(), nrow = rows, ncol = cols)
    f1 <- function(x, y) all(x == y)

    # Generate path aesthetics
    for (i in seq_len(rows)) {
        for (j in seq_len(cols)) {

            if (any(mapply(f1, list(c(j, i)), path_hist))) {
                tmp_cell <- cell(i, j, visited = TRUE)
            } else {
                tmp_cell <- cell(i, j)
            }

            grid_data[i, j][[1]] <- tmp_cell
        }
    }

    # Grid data for ggplot2
    tmp_ln <- sapply(
        grid_data,
        "[[",
        "line_data",
        simplify = FALSE
    ) %>%
        do.call("rbind", .)

    tmp_sh <- sapply(
        grid_data,
        "[[",
        "shape_data",
        simplify = FALSE
    ) %>%
        do.call("rbind", .)

    # Return finalized grid data
    return(
        list(
            line_data = tmp_ln,
            shape_data = tmp_sh
        )
    )
}



# === Visualize =====================================================

## Path generation ----
path_hist <- pathFinder(rows = rows, cols = cols)$prev_hist


## Iterate ----
cell_all <- cell_iterator(rows = rows, cols = cols, path_hist = path_hist)


## Plot ----
cell_plot <- ggplot() +
    geom_rect(
        mapping = aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax
        ),
        fill = cell_all$shape_data$fill,
        data = cell_all$shape_data,
    ) +
    geom_segment(
        mapping = aes(
            x    = xlmin,
            y    = ylmin,
            xend = xlmax,
            yend = ylmax
        ),
        data = cell_all$line_data,
    ) +
    xlab("x") +
    ylab("y") +
    scale_x_continuous(breaks = seq(1, cols + 1, 1)) +
    scale_y_continuous(breaks = seq(1, rows + 1, 1)) +
    coord_equal(xlim = seq_len(cols + 1), ylim = seq_len(rows + 1))
cell_plot %>% print()



# === TEST ==========================================================

# ## Iterate through path example ----
# mat_hist <- pathFinder(rows = rows, cols = cols)$mat_hist
# for (i in seq_len(length(mat_hist))) {
#     par(mar = c(2, 2, 2, 2), pty = "s")
#     image(
#         mat_hist[[i]],
#         col = c("gray92", "#ff4949"),
#         xaxt = "n",
#         yaxt = "n",
#         main = paste0("Iteration: ", i)
#     )
#     box(col = "gray92")
# }


