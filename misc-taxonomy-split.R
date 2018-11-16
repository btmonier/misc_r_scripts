#-----------------------------------------------------#
# Title:  Taxonomy string split with regex filters    #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   07.19.16                                    #
#-----------------------------------------------------#

#-------
# Prelim
#-------

df.tax <- read.delim('location')


#-------------
# Data munging
#-------------

# Extract taxonomy variable and break it apart '(n);'
tax <- strsplit(as.character(df.tax$Taxonomy), '\\([^\\)]+\\);')

# Row bind
tmp <- do.call(rbind, tax)

# New dataframe
df.tax.new <- data.frame(df.tax[1:2], do.call(rbind, tax))

View(df.tax.new)