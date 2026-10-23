#------------------------------------------------------------------------------
# Title:   Test RNA-seq Script (Bioconductor)
# Author:  Brandon Monier
# Created: 2016-11-08
#------------------------------------------------------------------------------

#---------
# Preamble
#---------

# Load packages
suppressPackageStartupMessages({
    library(DESeq)
    library(GEOquery)
    library(airway)
    library(ggplot2)
    library(dplyr)
    library(tidyr)
})

# Get GEO data
dir <- system.file("extdata", package = "airway")
geofile <- file.path(dir, "GSE52778_series_matrix.txt")
gse <- getGEO(filename = geofile)


#-------------
# Data munging
#-------------

# Parse GEO matrix phenotypic data
pdata <- pData(gse)[, grepl("characteristics", names(pData(gse)))]

# Edit column names
names(pdata) <- c("treatment", "tissue", "ercc_mix", "cell", "celltype")

# Parse and 'clean' columns to new data frame
pdataclean <- data.frame(
    treatment = sub("treatment: (.*)", "\\1", pdata$treatment),
    cell      = sub("cell line: (.*)", "\\1", pdata$cell),
    row.names = rownames(pdata)
)

# Create new columns for treatments
pdataclean$dex <- ifelse(
    grepl("Dex", pdataclean$treatment),
    "trt",
    "untrt"
)

pdataclean$albut <- ifelse(
    grepl("Albut", pdataclean$treatment),
    "trt",
    "untrt"
)

# Additional edits to 'clean' data frame
pdataclean$SampleName <- rownames(pdataclean)
pdataclean$treatment <- NULL

# Get SRA run id information
srafile <- file.path(dir, "SraRunInfo_SRP033351.csv")
srp <- read.csv(srafile)
srpsmall <- srp[, c(
    "Run", "avgLength", "Experiment", "Sample", "BioSample", "SampleName"
)]

# Merge SRA and pheotypic data into new data frame
coldata <- merge(pdataclean, srpsmall, by = "SampleName")
rownames(coldata) <- coldata$Run
coldata <- coldata[coldata$albut == "untrt", ]

# Remove 'albut' treatment
coldata$albut <- NULL

tbl_df(coldata)
