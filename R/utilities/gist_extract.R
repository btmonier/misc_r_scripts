#------------------------------------------------------------------------------
# Title:   Pull Data From GitHub Gists
# Author:  Brandon Monier
# Created: 2016-11-15
#------------------------------------------------------------------------------

#---------
# Preamble
#---------

# Load packages
library(RCurl)
library(dplyr)


#----------------------
# Data frame extraction
#----------------------

# A 'simple' data frame (.csv)
url <- getURL(
    paste0(
        "https://gist.githubusercontent.com/btmonier/",
        "c55f9332bcc685663452ae21cdc955c9/raw/",
        "7ec2e1600358c5bcb8eebcbcd0fdeee1cf06a28a/test.txt"
    )
)

test.01 <- read.csv(text = url, header = TRUE)
tbl_df(test.01)

# A more complicated data frame (tab separated)
url <- getURL(
    paste0(
        "https://gist.githubusercontent.com/stephenturner/",
        "806e31fce55a8b7175af/raw/",
        "1a507c4c3f9f1baaa3a69187223ff3d3050628d4/results.txt"
    )
)

test.02 <- read.table(text = url, header = TRUE)
tbl_df(test.02)
