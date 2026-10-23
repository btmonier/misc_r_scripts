#------------------------------------------------------------------------------
# Title:   Rarefaction Curves
# Author:  Brandon Monier
# Created: 2016-07-13
#------------------------------------------------------------------------------

#-------
# Prelim
#-------

# Load packages
library(ggplot2)
library(reshape2)

# Load data frame
df.rar <- read.csv("ADD LOCATION HERE")
df.rar[1:5, 1:5]


#-------------
# Data munging
#-------------

# 'Melt' data frame
df.rar.melt <- melt(
    df.rar,
    id.vars = "numsampled",
    variable.name = "category"
)

# Grep unique entities with regex
df.rar.melt.unique <- df.rar.melt[grepl("unique", df.rar.melt$category), ]


#--------------
# Visualization
#--------------

# Line graph
ggplot(
    data = df.rar.melt.unique
) +
    aes(
        x     = numsampled,
        y     = value,
        color = category
    ) +
    geom_line() +
    geom_path(
        size = 1
    ) +
    theme(
        legend.position = "none"
    )
