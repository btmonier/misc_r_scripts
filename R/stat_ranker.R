#-----------------------------------------------------#
# Title:  Ranking Function for Statistical Outputs    #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   05.08.17                                    #
#-----------------------------------------------------#

ranker <- function(data) {
  tmp <- read.csv(data, header = TRUE)
  tmp$aov.sig <- 'NS'
  tmp$aov.sig[tmp$ANOVA < 0.1] <- '*'
  tmp$aov.sig[tmp$ANOVA < 0.05] <- '**'
  tmp$aov.sig[tmp$ANOVA < 0.01] <- '***'
  
  tmp$kru.sig <- 'NS'
  tmp$kru.sig[tmp$Kruskal < 0.1] <- '*'
  tmp$kru.sig[tmp$Kruskal < 0.05] <- '**'
  tmp$kru.sig[tmp$Kruskal < 0.01] <- '***'
  
  write.csv(tmp, data, row.names = FALSE)
}



