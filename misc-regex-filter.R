#-----------------------------------------------------#
# Title:  Regex Filtration Example                    #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   06.21.16                                    #
#-----------------------------------------------------#

# Filter main data frame to retain only samples that contain ONE or TWO
# digit identifiers after 'AMV4_5NF_'
tmp2 <- 'AMV4_5NF_[0-9][0-9]$|AMV4_5NF_[0-9]$'

# Use 'grepl' to filter data frame using custom filter
filt <- stability[grepl(tmp2, stability$V1),]

# Certify that new data frame contains filtered units
dim(filt)

# Save to new file
write.table(
  filt, 
  'stability.files', 
  quote     = FALSE, 
  row.names = FALSE,
  col.names = FALSE
)

# Filter files in linux using grep:
# select ONLY entries with ONE or TWO digit identifiers:
# ls | grep "^AMV4_5NF_[0-9]_.*.fastq$\|^AMV4_5NF_[0-9][0-9]_.*.fastq$"

# scp multiple files to cluster:
# ls | grep "^AMV4_5NF_[0-9]_.*.fastq$\|^AMV4_5NF_[0-9][0-9]_.*.fastq$" > files.txt
# tar cvzf - -T files.txt | ssh monierb@blackjack tar xzf -


#-----------------------------------------------
# Stability file generation (diazotroph primers)
#-----------------------------------------------
#
# grep files to groupname.txt
# ls | grep "F2_R6_L[0-9][0-9]_.*._R1.fastq" | cut -f1-3 -d "_" > groupnames.txt
# 
# grep files for forward.txt
# ls | grep "F2_R6_L[0-9][0-9]_.*._R1.fastq" > forward.txt
# 
# grep files for reverse.txt
# ls | grep "F2_R6_L[0-9][0-9]_.*._R2.fastq" > reverse.txt
#
# paste it all together (stability.files)
# paste groupnames.txt forward.txt > temp.txt
# paste temp.txt reverse.txt > stability.files
