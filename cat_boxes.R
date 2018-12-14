#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   test.R
# Description:   Make a console box
# Author:        Brandon Monier
# Created:       2018-11-21 at 11:45:12
# Last Modified: 2018-11-21 at 13:23:07
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    Make a console box of current console dimensions
#--------------------------------------------------------------------

# Get column dimensions
colwidth <- NA
lmar <- 5
boxheight <- 10

if (is.na(colwidth)) {
    colwidth <- as.numeric(Sys.getenv("COLUMNS"))
    colwidth <- round(colwidth * 0.8)
} else {
    colwidth <- colwidth
}

if (is.na(lmar)) {
    lmar <- round(colwidth * 0.05)
    lmar <- strrep(" ", lmar)
} else {
    lmar <- strrep(" ", lmar)
}

if (is.na(boxheight)) {
    boxheight <- 5
} else {
    boxheight <- boxheight
}

boxtb <- paste0( 
    lmar,
    "+",
    strrep("-", colwidth - 2),
    "+",
    "\n"
)

boxside <- paste0(
    lmar,
    "|",
    strrep(" ", colwidth - 2),
    "|",
    "\n"
)

boxout <- c()
for (i in 1:boxheight) {
    boxout[i] <- boxside
}
boxout[1] <- boxtb
boxout[boxheight] <- boxtb
cat(boxout, sep = "")

