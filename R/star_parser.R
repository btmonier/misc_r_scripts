#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   star_parser.R
# Description:   Parse STAR alignment statistics files
# Author:        Brandon Monier
# Created:       2020-03-06 at 13:39:29
# Last Modified: 2020-03-06 at 14:58:39
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to parse the weird
#    statistical outputs from the STAR aligner program. Data will
#    be parsed into a more machine-readable format.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(magrittr)
library(tibble)


## Load data ----
con <- file("data/star_statistics")
stats <- readLines(con = con)
stats_ls <- list(stats)


# === Data parsing ==================================================

## Parse names ----
# pure_names <- gsub(pattern = "^output/", replacement = "", x = files)
# pure_names <- gsub(pattern = "_CKD.*$", replacement = "", x = pure_names)
pure_names <- c("test_sample")


## Set up variables ----
map_speed_ls       <- vector(mode = "list", length = length(stats_ls))
uniquely_mapped_ls <- vector(mode = "list", length = length(stats_ls))
mapped_multiple_ls <- vector(mode = "list", length = length(stats_ls))
mapped_too_many_ls <- vector(mode = "list", length = length(stats_ls))
unmapped_short_ls  <- vector(mode = "list", length = length(stats_ls))
unmapped_other_ls  <- vector(mode = "list", length = length(stats_ls))


## Loop ----
for (i in seq_len(length(stats_ls))) {

    # Subset for each taxa
    tmp <- stats_ls[[i]]


    # Parse

    ## Mapping speed
    tmp_map_speed <- tmp[grep("Mapping speed, Million", tmp)]
    tmp_map_speed <- gsub("^.*\\t", "", tmp_map_speed) %>% as.numeric()


    ## Total reads
    tmp_total <- tmp[grep("Number of input reads", tmp)]
    tmp_total <- gsub("^.*\\t", "", tmp_total) %>% as.numeric()

    ## Uniquely mapped reads
    tmp_uniq <- tmp[grep("Uniquely mapped reads number", tmp)]
    tmp_uniq <- gsub("^.*\\t", "", tmp_uniq) %>% as.numeric()

    ## Mapped to multiple loci
    tmp_map_mult <- tmp[grep("Number of reads mapped to multiple loci", tmp)]
    tmp_map_mult <- gsub("^.*\\t", "", tmp_map_mult) %>% as.numeric()

    ## Mapped to too many loci
    tmp_map_too  <- tmp[grep("Number of reads mapped to too many", tmp)]
    tmp_map_too <- gsub("^.*\\t", "", tmp_map_too) %>% as.numeric()

    ## Unmapped: too short
    tmp_unmap_short <- tmp[grep("% of reads unmapped: too short", tmp)]
    tmp_unmap_short <- gsub("^.*\\t", "", tmp_unmap_short)
    tmp_unmap_short <- gsub("%", "", tmp_unmap_short) %>% as.numeric() * 0.01
    tmp_unmap_short <- tmp_total * tmp_unmap_short

    ## Unmapped: other
    tmp_unmap_other <- tmp[grep("% of reads unmapped: other", tmp)]
    tmp_unmap_other <- gsub("^.*\\t", "", tmp_unmap_other)
    tmp_unmap_other <- gsub("%", "", tmp_unmap_other) %>% as.numeric() * 0.01
    tmp_unmap_other <- tmp_total * tmp_unmap_other


    # Populate
    map_speed_ls[[i]]       <- tmp_map_speed
    uniquely_mapped_ls[[i]] <- tmp_uniq
    mapped_multiple_ls[[i]] <- tmp_map_mult
    mapped_too_many_ls[[i]] <- tmp_map_too
    unmapped_short_ls[[i]]  <- tmp_unmap_short
    unmapped_other_ls[[i]]  <- tmp_unmap_other
}


## Coerce to vector ----
map_speed_ls       <- do.call(what = "rbind", map_speed_ls)
uniquely_mapped_ls <- do.call(what = "rbind", uniquely_mapped_ls)
mapped_multiple_ls <- do.call(what = "rbind", mapped_multiple_ls)
mapped_too_many_ls <- do.call(what = "rbind", mapped_too_many_ls)
unmapped_short_ls  <- do.call(what = "rbind", unmapped_short_ls)
unmapped_other_ls  <- do.call(what = "rbind", unmapped_other_ls)



# === Finalize ======================================================

## Pass vectors to data frame ----
star_stats <- tibble::tibble(
    sample          = pure_names,
    map_speed       = map_speed_ls,
    uniquely_mapped = uniquely_mapped_ls,
    mapped_multiple = mapped_multiple_ls,
    mapped_too_many = mapped_too_many_ls,
    unmapped_short  = unmapped_short_ls,
    unmapped_other  = unmapped_other_ls
)













