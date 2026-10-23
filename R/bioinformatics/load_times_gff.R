#------------------------------------------------------------------------------
# Title:   GFF Load Times
# Author:  Brandon Monier
#
# Benchmark loading a compressed and an uncompressed GFF file with
# GenomicFeatures.
#------------------------------------------------------------------------------

library(GenomicFeatures)
library(microbenchmark)
library(ggplot2)
library(dplyr)

gffPath <- "path/to/Zm-B73-REFERENCE-NAM-5.0_Zm00001eb.1.gff3.gz"
gffPathNoZip <- "path/to/Zm-B73-REFERENCE.gff3"

results <- microbenchmark(
    suppressMessages(makeTxDbFromGFF(gffPath)),
    suppressMessages(makeTxDbFromGFF(gffPathNoZip)),
    times = 10
)

autoplot(results)
boxplot(
    results,
    names = c("compressed", "no zip"),
    xlab = ""
)

results %>%
    mutate(
        expr = factor(expr, labels = c("Compression (GZip)", "No Compression")),
        time = time / 1e9
    ) %>%
    ggplot() +
    aes(x = expr, y = time) +
    ylab("Time (s)") +
    geom_boxplot() +
    theme(axis.title.x = element_blank())
