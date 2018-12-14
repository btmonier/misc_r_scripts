#------------------------------------------------------------------------------
# Title:  Generate an Avatar
# Author: Brandon Monier (brandon.monier@sdstate.edu)
# Date:   12.15.17
#------------------------------------------------------------------------------

# Preamble

## Create sequence
av.seq <- c(
	0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 1, 1, 1, 1, 1, 1, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 1, 1, 0, 1, 1, 1, 1, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 1, 1, 1, 1, 0, 1, 1, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 1, 1, 1, 1, 1, 1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0
)

## Create matrix
av.mat <- matrix(
	data = av.seq,
	nrow = 9,
	ncol = 9,
	byrow = TRUE
)

## Rotate matrix
rotate <- function(x) {
	t(apply(x, 2, rev))
}
av.mat <- rotate(av.mat)


par(mar = c(5, 2, 4, 2), pty = "s")
image(
	av.mat, 
	col = c("#333333", "#ff4949"), 
	xaxt = "n", 
	yaxt = "n", 
	main = "btmonier.github.io",
	col.main = "#333333"
)



# S4 experimentation

## Create "avatar" class
setClass(
	Class = "avatarMatrix",
	representation = representation(data = "ANY"),
	validity = function(object) {
		if (class(object@data) != "matrix") {
			return(
				paste(
					"This is a", class(object@data), "class. Please use only matrices!"
				)
			)
		} else {
			return(TRUE)
		}
	}
)

## Create load function
avatarFromMatrix <- function(obj) {
	nobj <- new("avatarMatrix", data = obj)
	return(nobj)
}



# Avatar 2: Electric Boogaloo

## Create sequence
av.seq <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 1, 0, 1, 0, 1, 0,
    0, 1, 0, 1, 0, 1, 0, 1, 0,
    0, 1, 0, 0, 0, 1, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 1, 0, 0, 0, 1, 0,
    0, 1, 0, 1, 0, 1, 0, 1, 0,
    0, 1, 0, 1, 0, 1, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0
)

## Create matrix
av.mat <- matrix(
    data = av.seq,
    nrow = 9,
    ncol = 9,
    byrow = TRUE
)

## Rotate matrix
rotate <- function(x) {
    t(apply(x, 2, rev))
}
av.mat <- rotate(av.mat)


par(mar = c(5, 2, 4, 2), pty = "s")
image(
    av.mat, 
    col = c("#333333", "#33e03f"), 
    xaxt = "n", 
    yaxt = "n", 
    main = "btmonier.github.io",
    col.main = "#333333"
)
