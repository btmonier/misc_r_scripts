# Get raw, filtered counts of SNP hits in DB

## Load packages ----
library(data.table)
library(glue)
library(logging)


## Load files and parameters ----
files <- list.files(
    path = "../Downloads/trip_data",
    full.names = TRUE
)
chromIDs <- gsub("^.*trip_data/|_fast_.*$", "", files)
tissueID <- unique(gsub("^.*results_|_Kremling.*$", "", files))


## Process data ----
loginfo(glue("Processing data for tissue: {tissueID}"))
countsAll <- lapply(seq_along(files), function(i) {
    loginfo(glue("-- Processing chromosome: {chromIDs[i]}..."))
    tmp <- data.table::fread(files[i], select = c("seqid", "snp_coord"), showProgress = TRUE)
    tmp$Marker <- paste0(tmp$seqid, "_", tmp$snp_coord)
    
    loginfo("-- Getting counts...")
    setkey(tmp, "Marker")
    counts <- tmp[, .(count = .N), by = Marker]
    counts <- counts[, c("snp_seqid", "snp_coord") := tstrsplit(Marker, "_", fixed = TRUE)]
    counts$snp_seqid <- as.numeric(counts$snp_seqid)
    counts$snp_coord <- as.numeric(counts$snp_coord)
    counts <- counts[, c(3, 4, 2)]
    colnames(counts) <- c("snp_seqid", "snp_coord", "num_trait")
    return(counts)
})
countsAll <- do.call("rbind", countsAll)
setorder(countsAll, snp_seqid, snp_coord)


## Write data to disk ----
loginfo("-- Writing count data to disk...")
fileID <- glue("count_data_all_{tissueID}.csv")
fwrite(countsAll, file = glue("tmp/{fileID}"))
