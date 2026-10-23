
## Parameters
charLen <- 25
v <- runif(100)

textBoxPlot <- function(v, charLen = 25) {
    m   <- mean(v)
    minV <- min(v)
    maxV <- max(v)
    lenV <- sum(abs(c(minV, maxV)))
    loQ <- quantile(v)[[2]]
    upQ <- quantile(v)[[4]]

    mUse   <- round((sum(abs(c(m, minV))) / lenV) * charLen, 0)
    loQUse <- round((sum(abs(c(loQ, minV))) / lenV) * charLen, 0)
    upQUse <- round((sum(abs(c(upQ, minV))) / lenV) * charLen, 0)

    paste0(
        "|",
        strrep("-", loQUse - 2),
        "[",
        strrep(" ", (mUse - loQUse) - 1),
        ":",
        strrep(" ", (upQUse - mUse) - 1),
        "]",
        strrep("-", (charLen - upQUse) - 1),
        "|"
    )
}
