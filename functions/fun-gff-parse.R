#------------------------------------------------------------------------------
# Title:         Function - GFF3 Parser
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-05-09 16:57:48 CDT
# Last Modified: 2018-05-17 09:08:04 CDT
#------------------------------------------------------------------------------

# Packages needed:
#   - library(ape)
#   - library(stringr)
#   - library(dplyr)


# Parameters:
#   - gff......... GFF3 file location 
#   - cts......... count matrix (row names = IDs; column names = samples)
#   - type........ GFF3 type column identifier
#   - attribute... GFF3 attribute


# Get `%>%` operator:
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

parseGFF3 <- function(gff, cts, 
                      type = c(
                        "gene", "transcript", "exon", "CDS", 
                        "start_codon", "stop_codon", "UTR"
                      ), 
                      attribute = c(
                        "ID", "gene_id", "transcript_id", "gene_name", 
                        "transcript_name"
                      )) {

    type.choice <- c(
        "gene", "transcript", "exon", "CDS", 
        "start_codon", "stop_codon", "UTR"
    )

    attr.choice <- c(
        "ID", "gene_id", "transcript_id", "gene_name", 
        "transcript_name"
    )

    if (missing(type) || !type %in% type.choice) {
        stop(
            paste0(
                "\n\n", "Please specify a correct GFF3 type:", "\n",
                "  - gene\n", "  - transcript\n", "  - exon\n", "  - CDS\n",
                "  - start_codon\n", "  - stop_codon\n", "  - UTR\n\n"
            )
        )
    } else if (missing(attribute) || !attribute %in% attr.choice) {
        stop(
            paste0(
                "\n\n", "Please specify a correct GFF3 attribute:", "\n",
                "  - ID\n", "  - gene_id\n", "  - transcript_id\n", 
                "  - gene_name\n", "  - transcript_name\n\n"
            )
        )        
    }

    message("Parsing GFF3 file. This may take a while...")

    tmp <- ape::read.gff(gff)
    tmp <- tmp[which(tmp$type == type), ]

    tmp2 <- stringr::str_split_fixed(tmp$attributes, ";", n = Inf)
    tmp2 <- data.frame(tmp2)
    tmp2[] <- lapply(tmp2, as.character)
    colnames(tmp2) <- tmp2[1, ]

    att <- paste0(attribute, "=")
    tmp3 <- tmp2[, grep(att, names(tmp2), value = TRUE)]
    tmp3 <- gsub(pattern = att, replacement = "", tmp3)
    
    tmp[[attribute]] <- tmp3
    tmp <- tmp[, !(names(tmp) %in% "attributes")]
    tmp4 <- row.names(cts)

    tmp5 <- tmp[tmp[[attribute]] %in% tmp4, ]
    tmp5[[attribute]] <- as.factor(tmp5[[attribute]])
    tmp5[["length_kbp"]] <- tmp5[["end"]] - tmp5[["start"]]

    tmp6 <- tmp5 %>% 
        dplyr::group_by_(attribute) %>% 
        dplyr::summarize(length_kbp = mean(length_kbp, na.rm = TRUE))

    tmp6$length_kbp <- tmp6$length_kbp / 1000

    return(tmp6)
}