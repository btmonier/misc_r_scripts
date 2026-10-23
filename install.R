#------------------------------------------------------------------------------
# Title:         Package Installation
# Author:        Brandon Monier
# Description:   Install the packages used by the scripts in this repository.
#                Packages that are already installed are skipped.
#------------------------------------------------------------------------------

install_missing <- function(pkgs, installer) {
    missing <- pkgs[!(pkgs %in% rownames(installed.packages()))]
    if (length(missing)) installer(missing)
}


# CRAN
cran_packages <- c(
    "cli",
    "dartR",
    "data.table",
    "dplyr",
    "gganimate",
    "ggplot2",
    "glue",
    "googledrive",
    "googlesheets4",
    "gridExtra",
    "here",
    "igraph",
    "jsonlite",
    "keypress",
    "logging",
    "lubridate",
    "magick",
    "magrittr",
    "microbenchmark",
    "openxlsx",
    "optparse",
    "patchwork",
    "pbapply",
    "pdftools",
    "pheatmap",
    "progress",
    "RColorBrewer",
    "Rcpp",
    "RCurl",
    "readr",
    "readxl",
    "remotes",
    "reshape2",
    "rJava",
    "rvest",
    "scales",
    "shiny",
    "stringr",
    "tibble",
    "tictoc",
    "tidyr",
    "tidyverse",
    "xml2"
)
install_missing(cran_packages, install.packages)


# Bioconductor
## DESeq (used by R/bioinformatics/rna_seq.R) is no longer on Bioconductor.
bioc_packages <- c(
    "airway",
    "GenomicFeatures",
    "GEOquery",
    "QUBIC",
    "SNPRelate"
)
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install_missing(bioc_packages, BiocManager::install)


# GitHub
## fastRCluster (used by R/generative_art/fastr_test.R) needs a FastR install.
github_packages <- c(
    croix   = "btmonier/croix",
    rTASSEL = "maize-genetics/tassel-r"
)
for (pkg in names(github_packages)) {
    install_missing(pkg, function(x) remotes::install_github(github_packages[[x]]))
}
