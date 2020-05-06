#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   cell_automata.R
# Description:   Cellular automata
# Author:        Brandon Monier
# Created:       2020-05-05 at 21:01:29
# Last Modified: 2020-05-05 at 23:57:15
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to produce cellular
#    automation in R.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)
library(reshape2)


## Data ----

### Rule book
ls_rules <- list(
    "rule_30" = matrix(
        data = c(
            1, 1, 1, 0,
            1, 1, 0, 0,
            1, 0, 1, 0,
            1, 0, 0, 1,
            0, 1, 1, 1,
            0, 1, 0, 1,
            0, 0, 1, 1,
            0, 0, 0, 0
        ),
        ncol = 4, byrow = TRUE
    ),
    "rule_90" = matrix(
        data = c(
            1, 1, 1, 0,
            1, 1, 0, 1,
            1, 0, 1, 0,
            1, 0, 0, 1,
            0, 1, 1, 1,
            0, 1, 0, 0,
            0, 0, 1, 1,
            0, 0, 0, 0
        ),
        ncol = 4, byrow = TRUE
    ),
    "rule_110" = matrix(
        data = c(
            1, 1, 1, 0,
            1, 1, 0, 1,
            1, 0, 1, 1,
            1, 0, 0, 0,
            0, 1, 1, 1,
            0, 1, 0, 1,
            0, 0, 1, 1,
            0, 0, 0, 0
        ),
        ncol = 4, byrow = TRUE
    )
)

ls_pattern <- matrix(
    data = c(
        1, 1, 1,
        1, 1, 0,
        1, 0, 1,
        1, 0, 0,
        0, 1, 1,
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
    ),
    ncol = 3, byrow = TRUE
)


## Functions ----

### Convert integer number to bit
intToBitVect <- function(x, l = 8, p = 0){
    tmp <- rev(as.integer(intToBits(x)))
    id <- seq_len(match(1, tmp, length(tmp)) - 1)
    pad <- rep(p, l - length(tmp[-id]))
    return(c(pad, tmp[-id]))
}

### Generate k-mer subsets of given vector
makeKmers <- function(x) {
    i <- 1
    n_ind <- length(x)
    kmers <- vector("list", n_ind)
    repeat {
        if (i == 1) {
            kmers[[i]] <- x[c(n_ind, 1:2)]
        } else if (i > n_ind) {
            break()
        } else if (i == n_ind) {
            kmers[[i]] <- x[c(seq(i - 1, n_ind), 1)]
        } else {
            kmers[[i]] <- x[seq(i - 1, i + 1)]
        }

        i <- i + 1
    }

    return(kmers)
}

### Create new generation ----
evolver <- function(kmers, rule) {
    kmers %>%
        sapply(function(i) {
            rule[, 1:3] %>%
                apply(1, function(j) {
                    identical(i, j)
                }) %>%
                rule[., 4]
        })
}

### Wrapper ----
automata <- function(gen, n_ind, rule, pattern = NULL) {

    ## Initial pattern
    if (is.null(pattern))  {
        pattern_0 <- sample(c(0, 1), size = n_ind, replace = TRUE)
    } else {
        pattern_0 <- pattern
        n_ind <- length(pattern_0)
    }

    all_generations <- vector("list", gen)
    i <- 1
    repeat {
        if (i == 1) {
            all_generations[[i]] <- pattern_0
        } else if (i > gen) {
            break()
        } else {
            all_generations[[i]] <- all_generations[[i - 1]] %>%
                makeKmers(n_ind) %>%
                evolver(rule)
        }
        i <- i + 1
    }

    return(
        all_generations %>%
            do.call("rbind", .)
    )
}



## Test ----

### Generate
rule <- ls_rules$rule_110

pattern <- c(rep(0, 30), 1, rep(0, 30))
auto_res <- automata(
    gen = 150,
    rule = rule,
    n_ind = 30,
    pattern = pattern
)

### Visualize
auto_res %>%
    reshape2::melt() %>%
    ggplot() +
    aes(x = Var2, y = Var1, fill = value) +
    geom_tile() +
    scale_y_reverse() +
    coord_equal() +
    theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_blank()
    )


