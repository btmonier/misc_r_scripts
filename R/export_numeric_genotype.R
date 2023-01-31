library(magrittr)

genotype_matrix <- data.frame(Taxa = c("B73", "Mo17", "Ky21"), Geno1 = c(0,1,1), Geno2 = c(0,0,1))


exportNumericOutput <- function(df, file, taxaCol = "Taxa") {
    if (!taxaCol %in% colnames(df)) {
        stop("Taxa ID column: ", "'", taxaCol, "'", " not found.")
    }

    typeString <- "<Numeric>"
    headerString <- paste(
        "<Marker>", paste(
            colnames(df)[colnames(df) != taxaCol],
            collapse = "\t"
        ),
        sep = "\t"
    )

    myFile <- file(file, "w")
    writeLines(typeString, myFile, sep = "\n")
    writeLines(headerString, myFile, sep = "\n")

    write.table(df, myFile, quote = FALSE, row.names = FALSE, col.names = FALSE)

    close(myFile)
}


outputFileName <- "temp_geno.txt"

exportNumericOutput(genotype_matrix, file = outputFileName)

readLines(outputFileName) %>% cat(sep = "\n")

rTASSEL::readGenotypeTableFromPath(outputFileName)

