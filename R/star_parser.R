#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   star_parser.R
# Description:   Parse STAR alignment statistics files
# Author:        Brandon Monier
# Created:       2020-03-06 at 13:39:29
# Last Modified: 2020-03-20 at 12:29:55
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to parse the weird
#    statistical outputs from the STAR aligner program. Data will
#    be parsed into a more machine-readable format.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)
library(tibble)


## Parameters ----
# path <- "~/Temporary/sorghum_rna_stats/star_stats"
# pattern <- "/home/btmonier/Temporary/sorghum_rna_stats/star_stats/"
path <- "C:/Users/brand/Documents/tmp/sorghum_rna_statistics/star_stats"     # <- Windows path
pattern <- "C:/Users/brand/Documents/tmp/sorghum_rna_statistics/star_stats/" # <- Windows path
meta_path <- "C:/Users/brand/Documents/tmp/sorghum_rna_statistics/"          # <- Windows path


## Load data ----

### Get files
files <- list.files(
    path = path,
    recursive  = TRUE,
    pattern    = "\\.out$",
    full.names = TRUE
)

### Read text from each file
stats_ls <- lapply(X = seq_len(length(files)), FUN = function(i) {
    readLines(con = files[i])
})

### Read metadata
metadata <- readr::read_csv(file = paste0(meta_path, "/", "sorghum_rna_metadata.csv"))



# === Data parsing ==================================================

## Parse names ----
pure_names <- gsub(
    pattern = pattern,
    replacement = "",
    x = files
)
# pure_names <- gsub(pattern = "^data/star_stats/", replacement = "", x = files)
pure_names <- gsub(pattern = "_CKD.*$", replacement = "", x = pure_names)
names(stats_ls) <- pure_names


## Set up variables ----
map_speed_ls       <- vector(mode = "numeric", length = length(stats_ls))
uniquely_mapped_ls <- vector(mode = "numeric", length = length(stats_ls))
mapped_multiple_ls <- vector(mode = "numeric", length = length(stats_ls))
mapped_too_many_ls <- vector(mode = "numeric", length = length(stats_ls))
unmapped_short_ls  <- vector(mode = "numeric", length = length(stats_ls))
unmapped_other_ls  <- vector(mode = "numeric", length = length(stats_ls))
total_reads        <- vector(mode = "numeric", length = length(stats_ls))


## Loop ----
for (i in seq_len(length(stats_ls))) {

    message("Parsing sample: ", pure_names[i])

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
    map_speed_ls[i]       <- tmp_map_speed
    total_reads[i]        <- tmp_total
    uniquely_mapped_ls[i] <- tmp_uniq
    mapped_multiple_ls[i] <- tmp_map_mult
    mapped_too_many_ls[i] <- tmp_map_too
    unmapped_short_ls[i]  <- tmp_unmap_short
    unmapped_other_ls[i]  <- tmp_unmap_other
}


## Pass vectors to data frame ----
star_data <- tibble::tibble(
    sample          = pure_names,
    map_speed       = map_speed_ls,
    total_reads     = total_reads,
    uniquely_mapped = uniquely_mapped_ls,
    mapped_multiple = mapped_multiple_ls,
    mapped_too_many = mapped_too_many_ls,
    unmapped_short  = unmapped_short_ls,
    unmapped_other  = unmapped_other_ls
)


## Filter ----

### Sorghum panel
star_data_clean <- star_data %>%
    dplyr::filter(
        !stringr::str_detect(sample, "B73|BLANK|BT623|BTx623|s_|20_")
    ) %>%
    dplyr::arrange(-total_reads)

### Checks
star_data_checks <- star_data %>%
    dplyr::filter(
        stringr::str_detect(sample, "B73|BLANK|BT623|BTx623|s_|20_")
    ) %>%
    dplyr::arrange(-total_reads)


## Remove variables ----
rm(
    map_speed_ls,
    total_reads,
    uniquely_mapped_ls,
    mapped_multiple_ls,
    mapped_too_many_ls,
    unmapped_short_ls,
    unmapped_other_ls,
    tmp_map_mult,
    tmp_map_speed,
    tmp_map_too,
    tmp_total,
    tmp_uniq,
    tmp_unmap_other,
    tmp_unmap_short
)


## Write data to disk ----
write.csv(
    x = star_data,
    file = "data/sorghum_star_stats.csv",
    row.names = FALSE
)



# === Visualize =====================================================

## Convert data to long format ----
star_data_clean$sample_id <- seq_len(nrow(star_data_clean)) %>%
    as.numeric()
long_star <- star_data_clean %>%
    tidyr::gather(key = "map_type", value = "value", 4:8)


## Reorder factor levels ----
long_star$map_type <- factor(
    x = long_star$map_type,
    levels = c(
        "uniquely_mapped",
        "mapped_multiple",
        "mapped_too_many",
        "unmapped_short",
        "unmapped_other"
    )
)


## Plot STAR alignment scores ----
star_scores <- long_star %>%
    ggplot() +
    aes(x = sample_id, y = value, fill = map_type) +
    geom_area(position = position_stack(reverse = TRUE)) +
    scale_fill_manual(
        values = c("#4579A6", "#77B4EE", "#E1AB76", "#BA0948", "#780505"),
        name = "",
        breaks = c(
            "uniquely_mapped",
            "mapped_multiple",
            "mapped_too_many",
            "unmapped_short",
            "unmapped_other"
        ),
        labels = c(
            "Uniquely mapped",
            "Mapped to multiple loci",
            "Mapped to too many loci",
            "Unmapped: too short",
            "Unmapped: other"
        )
    ) +
    xlab("Sorghum samples arranged by total input reads") +
    ylab("Number of reads") +
    ggtitle("STAR alignment scores") +
    expand_limits( x = c(0,NA), y = c(0,NA)) +
    scale_y_continuous(labels = function(l) {
        paste0(round(l / 1e6, 1), "M")
    }) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    )


## Save plot to disk ----
ggsave(
    filename = "C:/Users/brand/Documents/tmp/sorghum_rna_statistics/star_scores.png",
    plot = star_scores, device = "png",width = 10, height = 5, dpi = "retina"
)


