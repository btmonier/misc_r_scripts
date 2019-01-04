


dev.off()
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
# abline(h = 0, v = 0, col = "gray60")

tri <- data.frame(
    x = c(0, 0.5, 0.5),
    y = c(0, 0, 0.5)
)
polygon(x = tri$x, y = tri$y)

rotater <- function(x, y, theta = pi/6) {
    x <- (x) * cos(theta) + (y) * sin(theta)
    y <- (y) * cos(theta) - (x) * sin(theta)
    return(list(x = x, y = y))
}

triRotated <- rotater(tri$x, tri$y, theta = -pi/2)
polygon(x = triRotated$x, y = triRotated$y)

# triRotated <- rotater(tri$x, tri$y, theta = -pi)
# polygon(x = triRotated$x, y = triRotated$y)



