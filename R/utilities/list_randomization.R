#-----------------------------------------------------#
# Title:  Presentation Randomization                  #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   01.12.17                                    #
#-----------------------------------------------------#

#---------
# Preamble
#---------

# Make participant list
list <- c('Brandon Monier', 'Vincent Peta', 'Arjun Kafle', 'Alex Soupir', 
          'Merritt Burch', 'Janice Eibensteiner', 'Heike Bucking')

# Randomize list
samp <- data.frame(sample(list, size = length(list), replace = FALSE))

# Misc
colnames(samp) <- c('Order')
samp