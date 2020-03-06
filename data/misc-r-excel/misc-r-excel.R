#------------------------------------------------------------------------------
# Title:         R to Excel...
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-03-22 12:57:45 CDT
# Last Modified: EDIT <`r Sys.time()` after exit>
#------------------------------------------------------------------------------

# Preamble

## Set WD
setwd("D:/Box Sync/misc-scripts-r/misc-r-excel")

## Load packages
library(openxlsx)



# Add data to cell (openxlsx)
wb <- loadWorkbook("template.xlsx")



# USER GENERATED MATERIAL ----

## Series Info 
ser_01 <- c(
	"A test title",
	"A test summary of text...",
	"An overall design..."
)

ser_02 <- c(
	"Brandon",
	"Adam",
	"Qin"
)

ser_03 <- c(
	"Supp file",
	"SRA code thing"
)


## Samples Info
sam_01 <- c(
	"Sample 01",
	"Sample 02",
	"Sample 03"
)


## 




# General titles
meta_01 <- c(
	"# High-throughput sequencing metadata template (version 2.1).",
	"# All fields in this template must be completed.",
	paste(
		"# Templates containing example data are found in the METADATA",
		"EXAMPLES spreadsheet tabs at the foot of this page."
	),
	paste(
		"# Field names (in blue on this page) should not be edited. Hover",
		"over cells containing field names to view field content guidelines."
	),
	paste(
		"# Human data. If there are patient privacy concerns regarding making",
		"data fully public through GEO, please submit to NCBI's dbGaP",
		"(http://ww.ncbi.nlm.nih.gov/gap/) database. dbGaP has controlled",
		"access mechanisms and is an appropriate resource for hosting",
		"sensitive patient data."
	),
	"",
	"SERIES",
	"# This section describes the overall experiment"
)

meta_02 <- c(
	"",
	"SAMPLES",
	paste(
		"# This section lists and describes each of the biological Samples",
		"under investgation, as well as any protocols that are specific to",
		"individual Samples."
	),
	paste(
		"# Additional \"processed data file\" or \"raw file\" columns",
		"may be included"
	),
	"Sample name"
)

meta_03 <- c(
	"",
	"PROTOCOLS",
	paste(
		"Any of the protocols below which are applicable to only a subset of",
		"the SAMPLES section instead."
	),
	"growth protocol",
	"treatment protocol",
	"extract protocol",
	"library construction protocol",
	"library strategy"
)

meta_04 <- c(
	"",
	"DATA PROCESSING PIPELINE"
	paste(
		"# Data processing steps include base-calling, alignment, filtering,",
		"peak-calling, generation of normalized abundance measurements etc..."
	),
	paste(
		"# For each step provide a description, as wll as software name,",
		"version, parameters, if applicable."
	),
	"# Include additional steps, as necessary."
)

meta_05 <- c(
	"",
	paste(
		"# For each file listed in the \"processed data\" file columns of the",
		"SAMPLES section, provide additional information below."
	),
	"PROCESSED DATA FILES",
	"file name"
)

meta_06 <- c(
	"",
	paste(
		"# For each file listed in the raw file columns of the SAMPLES", 
		"section provide additional information below."
	),
	"RAW FILES",
	"file name"
)

meta_06 <- c(
	"",
	paste(
		"# For paired-end experiments, list the 2 associated raw files,",
		"and provide average insert size and standard deviation,"
		"if known. For SOLiD experiments, list the 4 file names",
		"(include \"file name 3\" and \"file name 4\" columns)"
	),
	"PAIRED-END EXPERIMENTS",
	"file name 1"
)

ser_00_title <- c(
	"title",
	"summary",
	"overall design",
	rep("contributor", length(ser_02)),
	"supplementary file",
	"SRA_center_name_code"
)









master_01 <- c(
	meta_01,
	ser_00_title,
	meta_02
)

# Write data
writeData(
	wb,
	sheet = "METADATA TEMPLATE ",
	x = master_01,
	startRow = 1,
	startCol = 1
)


saveWorkbook(wb, "template.xlsx", overwrite = TRUE)

