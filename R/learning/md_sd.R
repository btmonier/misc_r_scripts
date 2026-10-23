#-----------------------------------------------------#
# Title:  Mean Dev VS. Standard Dev                   #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   01.13.17                                    #
#-----------------------------------------------------#

#---------
# Preamble
#---------

# Create simulations
list <- list()

for(i in 1:250) {
  list[[i]] <- sample(1:100, 10, replace = TRUE)
}


md <- function(x) {
  m <- mean(x)
  d <- abs(x - m)
  m <- mean(d)
  return(m)
}

md.ls <- lapply(list, md)
sd.ls <- lapply(list, sd)

md.df <- do.call(rbind, lapply(md.ls, data.frame, stringsAsFactors = FALSE))
sd.df <- do.call(rbind, lapply(sd.ls, data.frame, stringsAsFactors = FALSE))

tot.df <- cbind(md.df, sd.df)

colnames(tot.df) <- c('md', 'sd')

head(tot.df)


#--------------
# Visualization
#--------------

# PDF
pdf('tmp.pdf', width = 10, height = 7.5)

plot(tot.df, xlim = c(0, 40), ylim = c(0, 40), xlab = 'Mean Deviation',
     ylab = 'Standard Deviation', pch = 18)
abline(lm(tot.df$sd ~ tot.df$md), col = 'blue', lwd = 3)
lines(lowess(tot.df), col = 'red', lwd = 2, lty = 2)

dev.off()