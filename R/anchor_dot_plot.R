## Libraries ----
library(ggplot2)


## Function ----
anchorDotPlot <- function(
    anchorPath,
    querySeqId,
    refSeqId,
    queryLab = NULL,
    refLab = NULL,
    colorId    = c("strand", "score")
) {
    ## Workflow ----
    if (is.null(queryLab)) queryLab <- "Query"
    if (is.null(refLab)) refLab <- "Reference"

    colorId <- match.arg(colorId)
    if (colorId == "score") {
        scaleUnit <- scale_color_viridis_c()
    } else {
        scaleUnit <- NULL
    }

    tmpData <- read.table(file = anchorPath, head = TRUE)

    tmpData <- tmpData[which(tmpData$refChr   %in% refSeqId), ]
    tmpData <- tmpData[which(tmpData$queryChr %in% querySeqId), ]

    toMb <- function(x) x / 1e6


    p <- ggplot(data = tmpData) +
        aes(x = queryStart, y = referenceStart, color = .data[[colorId]]) +
        geom_point(size = 0.3) +
        scale_y_continuous(labels = toMb) +
        scale_x_continuous(labels = toMb) +
        facet_grid(
            rows = vars(refChr),
            col = vars(queryChr),
            scales = "free",
            space = "free"
        ) +
        scaleUnit +
        xlab(paste(queryLab, "(Mbp)")) +
        ylab(paste(refLab, "(Mbp)"))
    # theme(legend.position = "none")

    return(p)
}















