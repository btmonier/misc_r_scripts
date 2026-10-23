#!/usr/bin/env Rscript

#------------------------------------------------------------------------------
# Title:   Return highest and lowest numbers
# Author:  Brandon Monier
# Created: 2018-11-20
#
# Make a function that given a string of numbers, will return a string that
# will contain the highest and lowest values within the string. Kata...
#------------------------------------------------------------------------------

# High-low function (requires stringr to run)
highAndLow <- function(string) {
    string <- as.numeric(stringr::str_split(string, " ")[[1]])
    string <- sort(string)
    sorts <- c(
        string[length(string)],
        string[1]
    )
    return(sorts)
}

numbs <- "-88 7 3 -3 5"

highAndLow(numbs)
