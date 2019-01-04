W <- 20
grid <- matrix(NA, nrow = W, ncol = W)
grid[W/2 + c(0, 1), W/2 + c(0, 1)] = 0

diffuse <- function(p) {
    count = 0
    #
    while (TRUE) {
        p = p + moves[sample(M, 1),]
        #
        count = count + 1
        #
        # Black boundary conditions
        #
        if (p$x > W | p$y > W | p$x < 1 | p$y < 1) return(NA)
        #
        # Check if it sticks (to nearest neighbour)
        #
        if (p$x < W && !is.na(grid[p$x+1, p$y])) break
        if (p$x > 1 && !is.na(grid[p$x-1, p$y])) break
        if (p$y < W && !is.na(grid[p$x, p$y+1])) break
        if (p$y > 1 && !is.na(grid[p$x, p$y-1])) break
    }
    #
    return(c(p, count = count))
}

library(foreach)

# Number of particles per batch
#
PBATCH <- 5000
#
# Select starting position
#
phi = runif(PBATCH, 0, 2 * pi)
#
x = round((1 + cos(phi)) * W / 2 + 0.5)
y = round((1 + sin(phi)) * W / 2 + 0.5)
#
particles <- data.frame(x, y)

result = foreach(n = 1:PBATCH) %do% diffuse(particles[n,])

lapply(result, function(p) {if (length(p) == 3) grid[p$x, p$y] <<- p$count})


par(pty = "s", mar = c(0.5, 0.5, 0.5, 0.5))
plot(
    x = NA,
    y = NA,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    xlab = "",
    ylab = "",
    pch = 16,
    bg = NA,
    fg = NA,
    axes = FALSE
)
abline(h = 0, v = 0, col = "gray60")

tri <- data.frame(
    x = c(0, 0.5, 0.5),
    y = c(0, 0, 0.5)
)

rotater <- function(x, y, theta = pi/6) {
    x <- (x) * cos(theta) + (y) * sin(theta)
    y <- (y) * cos(theta) - (x) * sin(theta)
    return(list(x = x, y = y))
}

triRotated <- rotater(tri$x, tri$y, theta = -pi/6)
polygon(x = tri$x, y = tri$y)
polygon(x = triRotated$x, y = triRotated$y)



dev.off()


