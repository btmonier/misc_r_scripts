library(data.table)


markerData <- fread("../../Downloads/geno.csv", header = TRUE)
metaData   <- fread("../../Downloads/info_markers.csv")
file <- "test.txt"

metaData$alt_name <- paste0("S", metaData$Chrom_Sorghum_v301, "_", metaData$ChromPos_Sorghum_v301)

metaData$alt_name <- ifelse(
    test = grepl("super_", metaData$alt_name),
    yes  = gsub("^Ssuper_", "Ssuper", metaData$alt_name),
    no   = paste0("S", metaData$Chrom_Sorghum_v301, "_", metaData$ChromPos_Sorghum_v301)
)
metaData$CloneID <- as.character(metaData$CloneID)


colnames(markerData)[-1] <- metaData$alt_name[match(colnames(markerData)[-1], metaData$CloneID)]
colnames(markerData)[1] <- "<Marker>"


write("<Numeric>", file = file)
fwrite(
    x         = markerData, 
    file      = file, 
    sep       = "\t", 
    append    = TRUE, 
    na        = NA, 
    col.names = TRUE, 
    quote     = FALSE
)

