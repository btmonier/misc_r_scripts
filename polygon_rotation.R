


dev.off()
par(pty = "s", mar = c(0.5, 0.5, 0.5, 0.5))
plot(
    x = NA,
    y = NA,
    xlim = c(-1.5, 1.5),
    ylim = c(-1.5, 1.5),
    xlab = "",
    ylab = "",
    pch = 16,
    bg = NA,
    fg = NA,
    axes = FALSE
)
abline(h = 0, v = 0, col = "gray60")

tri <- matrix(c(0, 1, 1, 0, 0, 1), nrow = 2, byrow = TRUE)
polygon(x = tri[1, ], y = tri[2, ])


triRotated <- matrix(c(c(0, 0, -1), c(0, 1, 1)), nrow = 2, byrow = TRUE)


theta <- 90
thetaRad <- theta * pi / 180
x <- 1
y <- 0
round(x*cos(thetaRad) - y*sin(thetaRad), 0)
round(x*sin(thetaRad) + y*cos(thetaRad), 0)





rotater <- function(x, y, theta = 30) {
    thetaRad <- theta * pi / 180
    x <- round(x * cos(thetaRad) - y * sin(thetaRad), 0)
    y <- round(x * sin(thetaRad) + y * cos(thetaRad), 0)
    return(matrix(c(x, y), nrow = 2, byrow = TRUE))
}

triRotated2 <- rotater(tri[1, ], tri[2, ], theta = 90)

tri
triRotated
triRotated2

# polygon(x = tri[1, ], y = tri[2, ])
# polygon(x = triRotated2[1, ], y = triRotated2[2, ])



