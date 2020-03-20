# Saturation curve test


## Set WD ----
path <- "C:/Users/brand/Documents/tmp/sorghum_rna_statistics/htseq_counts"     # <- Windows path
pattern <- "C:/Users/brand/Documents/tmp/sorghum_rna_statistics/htseq_counts/" # <- Windows path


## Load packages ----
library(dplyr)
library(ggplot2)
library(readr)
# library(RNAseQC) # <- GitHub
library(tidyr)


## Load data ----
files <- list.files(path = path, pattern = "\\.counts$", full.names = TRUE)
files <- files[-grep("20_trimmed|B73|BLANK|BT623|BTx|s_", files)]


# files <- files[1:10]
myfiles <- lapply(files, function(i) {
        readr::read_tsv(
            file = i,
            col_names = c("gene_id", "counts"),
            quote = "_",
            n_max = 34128
            # n_max = 25
        )
    }
)


## Combine data ----
count_all <- dplyr::bind_cols(myfiles)
count_all <- count_all[, -grep("gene_id[[:digit:]]", names(count_all))]


## Modify names ----
pure_names <- gsub(pattern = pattern, replacement = "", x = files)
pure_names <- gsub(pattern = "_CKD.*$", replacement = "", x = pure_names)
colnames(count_all)[-1] <- pure_names


## Convert to matrix ----
count_mat <- count_all[, -1] %>% as.matrix()

write.csv(
    x = count_all, 
    file = "data/sorghum_htseq_counts_all.csv", 
    row.names = FALSE
)


## Plot ----
ndepth <- 15
sat <- RNAseQC::estimate_saturation(
    counts = count_mat,
    min_counts = 2,
    verbose = TRUE,
    ndepths = ndepth
)

write.csv(
    x = sat,
    file = "data/sorghum_saturation_values.csv",
    row.names = FALSE
)


sat_picks <- c(
    "PI570254",
    "PI651168",
    "PI525653",
    "PI329524",
    "PI452693",
    "PI665142",
    "PI525941",
    "NSL67920",
    "PI330125",
    "PI571022"
)
sat_pattern <- paste0(sat_picks, sep = "|", collapse = "")
sat_pattern <- gsub("\\|$", "", sat_pattern)
# sample_intensity <- c(
#     rep(sat_picks[1],  ndepth + 1),
#     rep(sat_picks[2],  ndepth + 1),
#     rep(sat_picks[3],  ndepth + 1),
#     rep(sat_picks[4],  ndepth + 1),
#     rep(sat_picks[5],  ndepth + 1),
#     rep(sat_picks[6],  ndepth + 1),
#     rep(sat_picks[7],  ndepth + 1),
#     rep(sat_picks[8],  ndepth + 1),
#     rep(sat_picks[9],  ndepth + 1),
#     rep(sat_picks[10], ndepth + 1)
# )
sat_filt <- sat %>% 
    dplyr::filter(stringr::str_detect(sample, sat_pattern))
# sat_filt$s_int <- sample_intensity
sat_filt$sample <- factor(
    sat_filt$sample, 
    levels = sat_picks
)
colfunc <- colorRampPalette(c("#DCEDC8", "#1A237E"))

satplot <- sat_filt %>% 
    ggplot() +
    aes(x = depth, y = sat, color = sample) +
    labs(x = "Reads", y = "Genes above threshold") +
    geom_line(size = 1.2, alpha = 0.9) +
    scale_color_manual(
        values = colfunc(length(sat_picks)), 
        breaks = sat_picks,
        labels = c(
            "~ 1M", "~ 2M", "~ 3M", "~4M", "~5M",
            "~ 6M", "~ 7M", "~ 8M", "~9M", "~10M"
        )
    ) +
    xlab("Number of reads") +
    ylab("# of genes with at least 2 reads") +
    scale_x_continuous(labels = function(l) {
        paste0(round(l/1e6,1),"M")
    }) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank())
satplot

ggsave(
    filename = "C:/Users/brand/Documents/tmp/sorghum_rna_statistics/saturation.png", 
    plot = satplot,device = "png",width = 8, height = 5, dpi = "retina"
)






















