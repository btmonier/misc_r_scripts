#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   cell_automata.R
# Description:   Cellular automata
# Author:        Brandon Monier
# Created:       2020-05-05 at 21:01:29
# Last Modified: 2020-05-09 at 23:59:07
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
library(progress)
library(reshape2)


## Data ----
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
evolver <- function(kmers, rule, ls_pattern = ls_pattern) {

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
automata <- function(gen, rule, pattern = NULL, n_ind = 31) {

    if (rule > 255) stop("Rule ID cannot exceed 255")

    ## Initial pattern
    if (is.null(pattern))  {
        pattern_0 <- c(rep(0, (n_ind - 1) * 0.5 ), 1, rep(0, (n_ind - 1) * 0.5))
    } else {
        pattern_0 <- pattern
    }
    rule <- cbind(ls_pattern, intToBitVect(rule))
    all_generations <- vector("list", gen)
    i <- 1
    repeat {
        if (i == 1) {
            all_generations[[i]] <- pattern_0
        } else if (i > gen) {
            break()
        } else {
            all_generations[[i]] <- all_generations[[i - 1]] %>%
                makeKmers() %>%
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
rule <- sample(0:255, 1)
auto_res <- automata(
    gen = 110,
    rule = rule,
    n_ind = 121
)

### Visualize
auto_res %>%
    reshape2::melt() %>%
    ggplot() +
    aes(x = Var2, y = Var1, fill = value) +
    geom_tile() +
    scale_y_reverse() +
    coord_equal() +
    labs(
        title = paste("Rule:", rule)
    ) +
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



# ## Iterate ----
# rules <- 0:255
# pb <- progress_bar$new(
#     format = "  Generating rules [:bar] :percent eta: :eta",
#     total = length(rules),
#     clear = FALSE,
#     width= 60
# )
# for (i in rules) {
#
#     pb$tick()
#
#     auto_res <- automata(
#         gen = 110,
#         rule = i,
#         n_ind = 121
#     )
#
#     ### Visualize
#     auto_res %>%
#         reshape2::melt() %>%
#         ggplot() +
#         aes(x = Var2, y = Var1, fill = value) +
#         geom_tile() +
#         scale_y_reverse() +
#         coord_equal() +
#         labs(
#             title = paste("Rule:", i)
#         ) +
#         theme(
#             axis.text = element_blank(),
#             axis.title.x = element_blank(),
#             axis.title.y = element_blank(),
#             axis.ticks = element_blank(),
#             legend.position = "none",
#             plot.title = element_text(hjust = 0.5),
#             panel.background = element_blank(),
#             title = element_text(face = "bold", color = "#424242")
#         )
#
#     ggsave(
#         filename = paste0("../../tmp/fig_output/rule_", i, ".png"),
#         width = 5,
#         height = 5,
#         units = "in"
#     )
# }









