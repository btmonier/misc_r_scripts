library(tictoc)

# ## Preamble ----
# gffPath <- "Downloads/Zm-B73-REFERENCE-NAM-5.0_Zm00001eb.1.gff3"
# gffFileCon <- file(gffPath, "r")
# gffLines <- readLines(gffFileCon)
# close(gffFileCon)


# ## Take 1 (14 seconds) ----
# tic()
# ids <- c()
# for (line in gffLines) {
#     fields <- line |> strsplit("\t") |> unlist()
#     
#     if (length(fields) == 1) next
#     
#     feature <- fields[3]
#     attributes <- fields[9]
#     
#     if (feature == "gene") {
#         featureId <- gsub("^ID=|;.*$", "", attributes)
#         ids <- c(id, featureId)
#     }
# }
# toc()


# ## Take 2 (10 seconds) ----
# tic()
# ids <- vector("character", length(gffLines))
# for (i in seq_along(gffLines)) {
#     line <- gffLines[i]
#     fields <- line |> strsplit("\t") |> unlist()
#     
#     if (length(fields) == 1) next
#     
#     feature <- fields[3]
#     attributes <- fields[9]
#     
#     if (feature == "gene") {
#         featureId <- gsub("^ID=|;.*$", "", attributes)
#         ids[i] <- featureId
#     }
# }
# ids <- ids[ids != ""]
# toc()


# ## Take 3 (5.1 seconds) ----
# tic()
# ids <- vector("character", length(gffLines))
# featureType <- "\tgene\t"
# for (i in seq_along(gffLines)) {
#     line <- gffLines[i]
#     if (grepl(featureType, line)) {
#         ids[i] <- gsub("^.*ID=|;.*", "", line)
#     }
# }
# ids <- ids[ids != ""]
# toc()


# ## Take 4 (5.1 seconds) ----
# tic()
# ids <- vector("character", length(gffLines))
# featureType <- "\tgene\t"
# for (i in seq_along(gffLines)) {
#     line <- gffLines[i]
#     if (grepl(featureType, line)) {
#         ids[i] <- line
#     }
# }
# ids <- ids[ids != ""]
# ids <- gsub("^.*ID=|;.*", "", ids)
# toc()


# ## Take 5 (5.06 seconds) ----
# tic()
# ids <- vector("character", length(gffLines))
# featureType <- "\tgene\t"
# for (i in seq_along(gffLines)) {
#     if (grepl(featureType, gffLines[i])) ids[i] <- gffLines[i]
# }
# ids <- ids[ids != ""]
# ids <- gsub("^.*ID=|;.*", "", ids)
# toc()


# ## Take 6 (1.07 seconds) ----
# tic()
# featureType <- "gene"
# featureType <- paste0("\t", featureType, "\t")
# ids <- gffLines[grepl(featureType, gffLines)]
# ids <- gsub("^.*ID=|;.*", "", ids)
# toc()


## Take FINAL (2.172 seconds) ----
tictoc::tic()
gffPath <- "Downloads/Zm-B73-REFERENCE-NAM-5.0_Zm00001eb.1.gff3"
featureType <- "gene"

gffFileCon <- file(gffPath, "r")
gffLines <- readLines(gffFileCon)
close(gffFileCon)
rm(gffFileCon)
featureType <- paste0("\t", featureType, "\t")
ids <- gsub("^.*ID=|;.*", "", gffLines[grepl(featureType, gffLines)])
tictoc::toc()


