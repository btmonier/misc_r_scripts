#-----------------------------------------------------#
# Title:  Return plots within functions               #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   11.18.16                                    #
#-----------------------------------------------------#

# Basic calculation (square value)
square.it <- function(x) {
  square <- x * x
  return(square)
}

# Functions not save to global environment
fun1 <- function(x) {
  2 + x - 1
}

# Functions calling other functions
my.fun <- function(X.matrix, y.vec, z.scalar) {
  sq.scalar <- square.it(z.scalar)
  mult      <- X.matrix %*% y.vec
  final     <- mult * sq.scalar
  return(final)
}


# Dummy function - call ggplot2 package
tmp <- function() {
  df <- data.frame(a = c(1, 2, 3, 4, 5), b = c(1, 2, 3, 4, 5))
  x  <- ggplot(data = df, aes(x = a, y = b)) + geom_point()
  print(x)
  return(df)
}

# Plot function - make blue plot
blue.plot <- function(x, y, ...) {
  plot(x, y, col = 'blue', ...)
}