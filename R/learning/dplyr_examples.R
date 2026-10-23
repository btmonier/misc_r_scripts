#------------------------------------------------------------------------------
# Title:   An introduction to dplyr
# Author:  Brandon Monier
# Created: 2016-08-10
#------------------------------------------------------------------------------

#-------
# Prelim
#-------

# Convert dataframe to tbl_df (wrapper)
df.tran <- tbl_df(df.tran)

df.tran


#----------------
# Basic functions
#----------------

# Filter
filter(df.tran, grass.genus == "panicum")

filter(df.tran, grass.genus == "panicum", symbiont == "myc")
