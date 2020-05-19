#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   dot_plot_basics.R
# Description:   Create a string dot plot in pure R
# Author:        Brandon Monier
# Created:       2020-05-19 at 14:01:16
# Last Modified: 2020-05-19 at 14:47:21
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to make an adjacency
#    matrix given a an input string and plot results using nothing
#    but base (i.e. pure) R.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)
library(reshape2)


## Parameters ----
# dna_input_a <- "ACTGATCGGAGGATAGGACGGGAGCGGAT"
# dna_input_b <- "ATCGGCGGATTTACGGATAGCGGCGATCG"


## Functions ----

### Create random string of sequence
makeDNA <- function(l = 20,
                    alphabet = c("A", "C", "G", "T"),
                    collapse = FALSE) {

    bio_str <- sample(alphabet, l, replace = TRUE)

    if (collapse) bio_str <- paste(bio_str, collapse = "")

    return(bio_str)

}

### Create dot plot matrix
dotPlot <- function(a, b) {
    ### Create adjacency matrix

    stopifnot(is.character(a))
    stopifnot(is.character(b))

    if (length(a) == 1 | length(b) == 1) {
        dna_a <- unlist(
            strsplit(a, split = "")
        )
        dna_b <- unlist(
            strsplit(b, split = "")
        )
    }

    dot_mat <- matrix(0, nrow = length(b), ncol = length(a))
    colnames(dot_mat) <- a
    rownames(dot_mat) <- b

    for (i in seq(nrow(dot_mat))) {
        for (j in seq(ncol(dot_mat))) {
            row <- colnames(dot_mat)[i]
            col <- colnames(dot_mat)[j]

            if (row == col) dot_mat[i, j] <- 1
        }
    }

    colnames(dot_mat) <- rownames(dot_mat) <- NULL

    return(dot_mat)
}



# === Test and visualize ============================================

## Make dot plot data ----
dot_mat <- dotPlot(
    a = makeDNA(100),
    b = makeDNA(100)
)


## Visualize ----
plt_dot <- dot_mat %>%
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
    )
print(plt_dot)


