#-----------------------------------------------------#
# Title:  Use R as a Graphing Calculator              #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   12.28.16                                    #
#-----------------------------------------------------#

#---------
# Preamble
#---------

# Load packages
require(ggplot2)


#------------------
# Plot one function
#------------------

# Generate sequence of numbers...
x <- seq(-5, 5, by = 0.1)

# Create function...
f <- function(x) {x^2}

# Plot the function by 'x'
ggplot(data.frame(x), aes(x)) + 
  geom_line(aes(y = f(x))) +
  ylim(c(0, 25)) +
  xlim(c(-5, 5)) +
  theme_light()


#------------------------
# Plot multiple functions
#------------------------

# Make sequence...
x <- seq(-10, 10, by = 0.1)

# Create function...
f <- function(x) {sin(x)}
g <- function(x) {cos(x)}

# Plot the function by 'x'
ggplot(data.frame(x), aes(x)) +
  geom_path(aes(y = f(x)), color = 'red') +
  geom_path(aes(y = g(x)), color = 'blue') +
  xlim(c(-10, 10)) +
  theme_light()


#-----------------
# Make your own...
#-----------------

# Make sequence...
x <- seq(-5, 5, by = 0.05)

# Create function...
f <- function(x) {
  sin(x)
}

# Plot
ggplot(data.frame(x), aes(x)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_path(aes(y = f(x)), color = 'red', size = 1) +
  scale_x_continuous(breaks = vec.breaks, labels = vec.expr) +
  theme_light()


#--------------
# Miscellaneous
#--------------

# Draw a circle
circleFun <- function(center = c(0,0),diameter = 2, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

x <- circleFun()

ggplot(x, aes(x, y)) + geom_path()

# pi symbols...
vec.breaks <- seq(from = -7*pi/2, to = 7*pi/2, by = pi/2)
pi.halfs <- c(paste(expression(pi), "/2"),
              paste(seq(from = 3, to = 21, by = 2), "*" , expression(pi), "/2"))
pi.fulls <- c(paste(expression(pi)),
              paste(seq(from = 2, to = 11, by = 1), "*" , expression(pi)))
vec.expr <- parse(text = c(rbind(pi.halfs, pi.fulls)))[1:7]


