# Clifford attractor test

## Function ----
clifford <- function(points, a, b, c, d) {
    x <- y <- vector("numeric", points)

    for (i in seq_len(points)) {
        if (i == 1) {
            x[i] <- sin(a * y[i] + c * cos(a * x[i]))
            y[i] <- sin(b * x[i] + d * cos(b * y[i]))
        } else {
            x[i] <- sin(a * y[i - 1] + c * cos(a * x[i - 1]))
            y[i] <- sin(b * x[i - 1] + d * cos(b * y[i - 1]))
        }

    }

    return(
        data.frame(
            x = x,
            y = y
        )
    )
}


## Parameters ----
points <- 20e6
a      <- -1.5
b      <- 1.5
c      <- 1.5
d      <- 1.6


## Time tests ----
iters <- 20
times <- vector("numeric", iters)
for (i in seq_len(iters)) {
    tmp <- system.time(clifford(points, a, b, c, d))
    it <- formatC(i, width = 2, format = "d", flag = "0")
    message("- iteration ", it, " time: ", tmp[[1]])
    times[i] <- tmp[[1]]
}

message("---")
message("MEAN.... ", mean(times))
message("STDEV... ", stats::sd(times))


