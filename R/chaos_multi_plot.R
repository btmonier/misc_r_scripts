#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   chaos_multi_plot.R
# Description:   Iterate through random Chaos Game permutations
# Author:        Brandon Monier
# Created:       2020-06-11 at 17:41:32
# Last Modified: 2022-09-07 at 09:24:35
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to generate random images
#    of Chaos Game permutations (e.g. number of sides, vertex
#    choices, r value, etc.) using Chaos Game methods built in pure
#    R.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(logging)
library(magrittr)
library(optparse)


## Parse CL arguments ----
optionList <- list(
    make_option(
        opt_str = c("-o", "--out"),
        type    = "character",
        default = NULL,
        help    = "Image output directory [default %default]"
    ),
    make_option(
        opt_str = c("-p", "--prefix"),
        type    = "character",
        default = "chaos_game_gen_",
        help    = "Prefix to for image files [default %default]"
    ),
    make_option(
        opt_str = c("-n", "--nimage"),
        type    = "integer",
        default = 10,
        help    = "Number of images to generate [default %default]"
    ),
    make_option(
        opt_str = c("-d", "--npoints"),
        type    = "integer",
        default = 1e5,
        help    = "Number of points to plot for each image [default %default]"
    )
)


## Parse arguments ----
optParser <- OptionParser(option_list = optionList)
opt       <- parse_args(optParser)


## Get parameters ----
chaos_points <- opt$npoints
img_path     <- opt$out
file_pre     <- opt$prefix
total_plots  <- opt$nimage


## Source functions ----
logging::loginfo("Sourcing external file...")
tryCatch(
    suppressWarnings(source("n_gon_generation.R")),
    error = function(e) {
        logging::logerror("File does not exist")
        stop("stopping script", call. = FALSE, domain = NA)
    }
)



# === Create random images ==========================================

meta_df <- vector("list", length = total_plots)

for (i in seq(1, total_plots)) {

    pad_num <- formatC(
        x = i,
        width = nchar(total_plots),
        format = "d",
        flag = "0"
    )

    logging::loginfo(paste0("Creating image for iteration: ", pad_num))

    tmp <- chaosGame(chaos = TRUE, iter = chaos_points)

    plt_chaos <- tmp$chaos_df %>%
        ggplot() +
        aes(x = x, y = y) +
        geom_point(size = 0.1, alpha = 0.25) +
        coord_equal() +
        theme_void()

    logging::loginfo(paste0("Saving image for iteration: ", pad_num))

    ggsave(
        filename = paste0(file_pre, pad_num, ".png"),
        plot     = plt_chaos,
        device   = "png",
        path     = img_path,
        width    = 6,
        height   = 6,
        dpi      = "retina"
    )

    meta_df[[i]] <- data.frame(
        file_name    = paste0(file_pre, pad_num, ".png"),
        n_sides      = tmp$sides,
        r_value      = tmp$r,
        vert_choices = paste(tmp$choices, collapse = ","),
        seq_type     = tmp$seq
    )
}


logging::loginfo(paste0("Saving metadata..."))
meta_df <- do.call(rbind, meta_df)
data.table::fwrite(meta_df, paste0(img_path, "metadata.csv"))


