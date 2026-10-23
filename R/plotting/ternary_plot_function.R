#-----------------------------------------------------#
# Title:  Ternary Plotting Function                   #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   01.05.17                                    #
#-----------------------------------------------------#

#---------
# Preamble
#---------

# Create working data frames...
df.01 <- data.frame(
  x = c(32.4, 21.1, 72.4, 33.3, 0.3, 70.5),
  y = c(21.3, 13.4, 5.2, 30.3, 1.2, 29.1),
  z = c(46.3, 65.5, 22.4, 36.4, 98.5, 0.4)
)

df.02  <- data.frame(
  x = c( 0, 0, 1, 0.1, 0.6, 0.2 ),
  y = c( 0, 1, 0, 0.3, 0.2, 0.8 ),
  z = c( 1, 0, 0, 0.6, 0.2, 0.0 )
)


#-------------
# The function
#-------------

ternplot <- function(df, file = 'tmp.pdf', ...) {
  x <- (df[1] + 2 * df[2]) / (2 * (df[1] + df[2] + df[3]))
  y <- (sqrt(3) * df[1]) / (2 * (df[1] + df[2] + df[3]))
  xy.df <- data.frame(x, y)
  colnames(xy.df) <- c('x', 'y')
  
  pdf(file, width = 6.5, height = 7)
  
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), pch = 16, bty = 'n', 
       xlab = '', ylab = '')
  
  # 'Major' grid lines list
  maj.s1 <- c(seq(0.1, 0.9, 0.1), rev(seq(0.55, 0.95, 0.05)),
              seq(0.05, 0.45, 0.05)) 
  maj.s2 <- c(rep(0, 9), seq(0.0866, 0.779, 0.086),
              seq(0.0866, 0.779, 0.086))
  maj.s3 <- c(seq(0.55, 0.95, 0.05), seq(0.05, 0.45, 0.05),
              seq(0.1, 0.9, 0.1))
  maj.s4 <- c(rev(seq(0.0866, 0.779, 0.086)), seq(0.0866, 0.779, 0.086),
              rep(0, 9))
  
  ls.maj <- list(maj.s1, maj.s2, maj.s3, maj.s4)
  
  # 'Minor' grid lines list
  min.s1 <- c(seq(0.025, 0.475, 0.05), rev(seq(0.05, 0.95, 0.1)))
  min.s2 <- c(seq(0.043, 0.8224, 0.086), rep(0, 10))
  min.s3 <- c(seq(0.05, 0.95, 0.1), rev(seq(0.525, 0.975, 0.05)))
  min.s4 <- c(rep(0, 10), seq(0.043, 0.8224, 0.0866))
  
  ls.min <- list(min.s1, min.s2, min.s3, min.s4)
  
  segments(ls.maj[[1]], ls.maj[[2]], ls.maj[[3]], ls.maj[[4]], 
           lty = 5, col = 'grey')
  
  segments(ls.min[[1]], ls.min[[2]], ls.min[[3]], ls.min[[4]],
           lty = 1, col = 'lightgrey', lwd = 0.5)
  
  polygon(c(0, 0.5, 1), c(0, sqrt(3)/2, 0), lwd = 2)
  points(xy.df, pch = 16)
  
  dev.off()
  
}

.terngrid <- function() {
  # 'Major' grid lines list
  maj.s1 <- c(seq(0.1, 0.9, 0.1), rev(seq(0.55, 0.95, 0.05)),
              seq(0.05, 0.45, 0.05)) 
  maj.s2 <- c(rep(0, 9), seq(0.0866, 0.779, 0.086),
              seq(0.0866, 0.779, 0.086))
  maj.s3 <- c(seq(0.55, 0.95, 0.05), seq(0.05, 0.45, 0.05),
              seq(0.1, 0.9, 0.1))
  maj.s4 <- c(rev(seq(0.0866, 0.779, 0.086)), seq(0.0866, 0.779, 0.086),
              rep(0, 9))
  
  maj.df <- data.frame(maj.s1, maj.s2, maj.s3, maj.s4)
  
  ls.maj <- list(maj.s1, maj.s2, maj.s3, maj.s4)
  
  # 'Minor' grid lines list
  min.s1 <- c(seq(0.025, 0.475, 0.05), rev(seq(0.05, 0.95, 0.1)))
  min.s2 <- c(seq(0.043, 0.8224, 0.086), rep(0, 10))
  min.s3 <- c(seq(0.05, 0.95, 0.1), rev(seq(0.525, 0.975, 0.05)))
  min.s4 <- c(rep(0, 10), seq(0.043, 0.8224, 0.0866))
  
  min.df <- data.frame(min.s1, min.s2, min.s3, min.s4)
  
  ls.min <- list(min.s1, min.s2, min.s3, min.s4)
  ls.tot <- list(ls.min, ls.maj)
  return(ls.tot)
}


#----------------------
# Ternary plot template
#----------------------

# 'Major' grid lines list
maj.s1 <- c(seq(0.1, 0.9, 0.1), rev(seq(0.55, 0.95, 0.05)),
            seq(0.05, 0.45, 0.05)) 
maj.s2 <- c(rep(0, 9), seq(0.0866, 0.779, 0.086),
            seq(0.0866, 0.779, 0.086))
maj.s3 <- c(seq(0.55, 0.95, 0.05), seq(0.05, 0.45, 0.05),
            seq(0.1, 0.9, 0.1))
maj.s4 <- c(rev(seq(0.0866, 0.779, 0.086)), seq(0.0866, 0.779, 0.086),
            rep(0, 9))

ls.maj <- list(maj.s1, maj.s2, maj.s3, maj.s4)


# 'Minor' grid lines list
min.s1 <- c(seq(0.025, 0.475, 0.05), rev(seq(0.05, 0.95, 0.1)))
min.s2 <- c(seq(0.043, 0.8224, 0.086), rep(0, 10))
min.s3 <- c(seq(0.05, 0.95, 0.1), rev(seq(0.525, 0.975, 0.05)))
min.s4 <- c(rep(0, 10), seq(0.043, 0.8224, 0.0866))

ls.min <- list(min.s1, min.s2, min.s3, min.s4)


# Grid template
pdf('tmp.pdf', width = 6.5, height = 7)
plot(0:1, 0:1, type = 'n', bty = 'n')

segments(ls.maj[[1]], ls.maj[[2]], ls.maj[[3]], ls.maj[[4]], 
         lty = 2, col = 'grey')

segments(ls.min[[1]], ls.min[[2]], ls.min[[3]], ls.min[[4]],
         lty = 1, col = 'lightgrey', lwd = 0.5)

polygon(c(0, 0.5, 1), c(0, 0.8660254, 0), lwd = 2)

dev.off()



#----------------------------
# Return Cartesian conversion
#----------------------------

terndf <- function(df) {
  x <- (df[1] + 2 * df[2]) / (2 * (df[1] + df[2] + df[3]))
  y <- (sqrt(3) * df[1]) / (2 * (df[1] + df[2] + df[3]))
  xy.df <- data.frame(x, y)
  colnames(xy.df) <- c('x', 'y')
  return(xy.df)
}

pyth <- function(a, c) {
  b <- sqrt((c * c) - (a * a))
  return(b)
}









