#!/usr/bin/env Rscript

#------------------------------------------------------------------------------
# Title:   Maze generation in R
# Author:  Brandon Monier
# Created: 2019-11-23
#
# Generate mazes in R using a "depth-first search" (recursive backtracker)
# algorithm and render them with ggplot2 as a raster image layer.
#
# NOTE:
#   i = row
#   j = column
#
#   The maze is stored as a (2 * rows + 1) x (2 * cols + 1) pixel matrix
#   where 0 = wall and 1 = passage. Cell (i, j) lives at pixel (2i, 2j).
#------------------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)


## Global variables ----
rows <- 25
cols <- 40
seed <- NULL
wall_col <- "#212121"
path_col <- "white"
fig_path <- "."
fig_name <- paste0("maze_", rows, "x", cols, "_", Sys.Date(), ".png")


# === Maze generation ===============================================

## Unvisited neighbors of a cell ----
unvisitedNeighbors <- function(cell, visited) {
    offsets <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
    candidates <- lapply(offsets, function(o) cell + o)

    keep <- vapply(candidates, function(n) {
        n[1] >= 1 && n[1] <= nrow(visited) &&
            n[2] >= 1 && n[2] <= ncol(visited) &&
            !visited[n[1], n[2]]
    }, logical(1))

    return(candidates[keep])
}


## Recursive backtracker (iterative) ----
generateMaze <- function(rows, cols, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)

    maze <- matrix(0L, nrow = 2 * rows + 1, ncol = 2 * cols + 1)
    visited <- matrix(FALSE, nrow = rows, ncol = cols)

    start <- c(sample(rows, 1), sample(cols, 1))
    visited[start[1], start[2]] <- TRUE
    maze[2 * start[1], 2 * start[2]] <- 1L

    stack <- vector("list", rows * cols)
    stack[[1]] <- start
    top <- 1

    while (top > 0) {
        current <- stack[[top]]
        neighbors <- unvisitedNeighbors(current, visited)

        if (length(neighbors) == 0) {
            top <- top - 1
            next
        }

        nxt <- neighbors[[sample(length(neighbors), 1)]]
        visited[nxt[1], nxt[2]] <- TRUE
        maze[current[1] + nxt[1], current[2] + nxt[2]] <- 1L
        maze[2 * nxt[1], 2 * nxt[2]] <- 1L

        top <- top + 1
        stack[[top]] <- nxt
    }

    # Entrance (top-left) and exit (bottom-right)
    maze[2, 1] <- 1L
    maze[2 * rows, 2 * cols + 1] <- 1L

    return(maze)
}


# === Visualize =====================================================

## Matrix to long data frame ----
mazeToDf <- function(maze) {
    df <- expand.grid(y = seq_len(nrow(maze)), x = seq_len(ncol(maze)))
    df$value <- as.vector(maze)
    return(df)
}


## Raster plot ----
plotMaze <- function(maze_df, wall_col = "#212121", path_col = "white") {
    ggplot(maze_df, aes(x = x, y = y, fill = factor(value))) +
        geom_raster() +
        scale_fill_manual(values = c("0" = wall_col, "1" = path_col)) +
        scale_y_reverse() +
        coord_equal(expand = FALSE) +
        theme_void() +
        theme(legend.position = "none")
}


## Generate, plot, and save ----
maze <- generateMaze(rows = rows, cols = cols, seed = seed)

maze_plot <- maze %>%
    mazeToDf() %>%
    plotMaze(wall_col = wall_col, path_col = path_col)
maze_plot %>% print()

ggsave(
    filename = fig_name,
    plot     = maze_plot,
    device   = "png",
    path     = fig_path,
    width    = ncol(maze) / 10,
    height   = nrow(maze) / 10,
    dpi      = 150
)
