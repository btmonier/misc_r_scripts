#------------------------------------------------------------------------------
# Title:         Package Installation
# Author:        Brandon Monier
# Created:		 2018-05-09 13:31:00 CDT
# Last Modified: 2018-05-25 11:22:53 CDT
#------------------------------------------------------------------------------

# CRAN
packages <- c(
	"agricolae",
	"agridat",
	"backports",
	"circlize",
	"cowsay",
	"crosstalk",
	"data.table",
	"devtools",
	"digest",
	"fields",
	"geneplotter",
	"GGally",
	"ggmap",
	"ggrepel",
	"gplots",
	"ggpubr",
	"gridExtra",
	"gtools",
	"Hmisc",
	"knitr",
	"locfit",
	"magick"
	"openxlsx",
	"pheatmap",
	"plotly",
	"profvis",
	"progress",
	"psych",
	"qtlcharts",
	"sjPlot",
	"RColorBrewer",
	"rmarkdown",
	"Rmisc",
	"RCircos",
	"Rcpp",
	"reshape2",
	"revealjs",
	"tidyverse",
	"tools",
	"shiny",
	"shinyBS",
	"shinycssloaders",
	"shinythemes",
	"vegan",
	"venneuler",
	"xlsx"
)
np <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(np)) install.packages(np)



# BioConductor
bio.packages <- c(
	"Biobase",
	"BiocStyle",
	"DESeq2",
	"edgeR",
	"GenomicRanges",
	"phyloseq"
	"QUBIC"
)
np <- bioc.packages[!(bioc.packages %in% installed.packages()[, "Package"])]
source("https://bioconductor.org/biocLite.R")
if (length(np)) biocLite(np)



# GitHub
devtools::install_github("vqv/ggbiplot")
