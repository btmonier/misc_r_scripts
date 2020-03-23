# Sorghum RNA sanity checks (TERRA-MEPP)


# === Preamble ======================================================

## Load packages ----
library(dplyr)
library(forcats)
library(ggplot2)
library(magrittr)
library(tibble)


## Parameters ----

### Temp only (Windows machine)
parent_path <- "C:/Users/brand/Documents/tmp/sorghum_rna_statistics/"
setwd(parent_path)


## Load data ----
metadata       <- readr::read_csv(file = "data/sorghum_rna_metadata.csv")
star_stats     <- readr::read_csv(file = "data/sorghum_star_stats.csv")
rna_data_sizes <- do.call(
    rbind,
    lapply(
        X = list.files(
            path = "data/",
            pattern = "_rawdata(1)?.csv$",
            full.names = TRUE
        ),
        FUN = readr::read_csv,
        col_names = c("file", "date", "size_b")
    )
)



# === Process data ==================================================

## Clean size data ----
rna_data_sizes <- rna_data_sizes %>%
    dplyr::mutate(file = gsub("/.*fq.gz$", "", file)) %>%
    dplyr::filter(!stringr::str_detect(file, "Undetermined")) %>%
    dplyr::mutate(size_mb = size_b / 1e6) %>%
    dplyr::slice(match(metadata$corrected_novogene_id, file))


## Parse STAR stats ----
star_order <- star_stats %>%
    dplyr::slice(match(metadata$corrected_novogene_id, sample)) %>%
    dplyr::mutate(
        coord_id     = metadata$coord_id,
        id_correct   = metadata$corrected_novogene_id,
        entry        = metadata$corrected_entry_id,
        growth       = metadata$growth,
        plate_x      = metadata$plate_x,
        plate_y      = metadata$plate_y,
        file_size_mb = rna_data_sizes$size_mb
    )


## Get plate data ----
plate_id <- "4_"
plate_view <- star_order %>%
    dplyr::filter(stringr::str_detect(coord_id, plate_id)) %>%
    dplyr::select(
        id_correct,
        entry,
        uniquely_mapped,
        file_size_mb,
        plate_x,
        plate_y
    )



# === Visualize data ================================================

## Visualize plates with values ----
plate_view %>%
    ggplot() +
    aes(x = plate_x, y = forcats::fct_rev(plate_y)) +
    geom_tile(aes(fill = file_size_mb)) +
    geom_text(
        aes(label = paste0(id_correct, "\n", "(", entry, ")")),
        size = 2.5
    ) +
    scale_fill_gradient(low = "#DCEDC8", high = "#1A237E") +
    scale_x_continuous(limits = c(0.5, 12.5), breaks = 1:12) +
    xlab("Plate coordinate: x") +
    ylab("Plate coordinate: y")


