# Title:         Package Manager for R installs
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Last Modified: 04.13.16

# 'Pack Man' function
pack.man <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Package list
packages <- c('ggplot2',
              'tidyr',
              'dplyr',
              'plyr',
              'sjPlot',
              'Rmisc',
              'agricolae',
              'ggmap',
              'data.table',
              'gridExtra',
              'agridat',
              'minque',
              'vegan',
              'gplots',
              'RColorBrewer',
              'devtools',
              'RCircos',
              'circlize',
              'reshape2',
              'venneuler',
              'knitr')
pack.man(packages)

# Github dependencies
devtools::install_github("vqv/ggbiplot")
  
# Bioconductor
  
source('http://bioconductor.org/biocLite.R')
biocLite(pkgs = c("EBImage", "DESeq2", "edgeR"))

