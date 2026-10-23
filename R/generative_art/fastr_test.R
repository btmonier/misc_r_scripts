# https://en.wikipedia.org/wiki/Clifford_A._Pickover

## Load packages ----
library(Rcpp)
library(fastRCluster)
library(microbenchmark)
library(parallel)

## Test function (strange attractors) ----
clifford <- function(points, a, b, c, d, x0 = 0, y0 = 0) {
    x <- y <- vector("numeric", points)

    x[1] <- x0
    y[1] <- y0

    for (i in seq(2, points)) {
        x[i] <- sin(a * y[i - 1] + c * cos(a * x[i - 1]))
        y[i] <- sin(b * x[i - 1] + d * cos(b * y[i - 1]))
    }

    return(
        data.frame(
            x = x,
            y = y
        )
    )
}

## Parameters ----
points <- 50e6
a      <- -1.5
b      <- 1.5
c      <- 1.5
d      <- 1.6


## FastR preamble ----
fastNode <- fastRCluster::makeFastRCluster()
parallel::clusterExport(fastNode, c("points", "a", "b", "c", "d", "clifford"))


## Get FastR times ----
system.time(
    fastRCluster::fastr(
        cl = fastNode,
        code = clifford(points, a, b, c, d)
    )
)


## Compare to base R ----
system.time(
    clifford(points, a, b, c, d)
)


## Benchmark ----
cliff_times <- microbenchmark::microbenchmark(
    FastR = fastRCluster::fastr(
        cl = fastNode,
        code = clifford(points, a, b, c, d)
    ),
    BaseR = clifford(points, a, b, c, d),
    times = 25
)


