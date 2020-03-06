# === 3D plots ======================================================

### Perlin noise generation
perlinNoise <- function(
    n = 5,   m = 7,    # Size of the grid for the vector field
    N = 100, M = 100   # Dimension of the image
) {
    # For each point on this n*m grid, choose a unit 1 vector
    vector_field <- apply(
        array( rnorm( 2 * n * m ), dim = c(2,n,m) ),
        2:3,
        function(u) u / sqrt(sum(u^2))
    )
    f <- function(x,y) {
        # Find the grid cell in which the point (x,y) is
        i <- floor(x)
        j <- floor(y)
        stopifnot( i >= 1 || j >= 1 || i < n || j < m )
        # The 4 vectors, from the vector field, at the vertices of the square
        v1 <- vector_field[,i,j]
        v2 <- vector_field[,i+1,j]
        v3 <- vector_field[,i,j+1]
        v4 <- vector_field[,i+1,j+1]
        # Vectors from the point to the vertices
        u1 <- c(x,y) - c(i,j)
        u2 <- c(x,y) - c(i+1,j)
        u3 <- c(x,y) - c(i,j+1)
        u4 <- c(x,y) - c(i+1,j+1)
        # Scalar products
        a1 <- sum( v1 * u1 )
        a2 <- sum( v2 * u2 )
        a3 <- sum( v3 * u3 )
        a4 <- sum( v4 * u4 )
        # Weighted average of the scalar products
        s <- function(p) 3 * p^2 - 2 * p^3
        p <- s( x - i )
        q <- s( y - j )
        b1 <- (1-p)*a1 + p*a2
        b2 <- (1-p)*a3 + p*a4
        (1-q) * b1 + q * b2
    }
    xs <- seq(from = 1, to = n, length = N+1)[-(N+1)]
    ys <- seq(from = 1, to = m, length = M+1)[-(M+1)]
    outer( xs, ys, Vectorize(f) )
}

### Random number matrix
randMat <- function(v, s) {
    matrix(
        data = sample(
            x = v,
            size = s,
            replace = TRUE
        ),
        nrow = sqrt(s),
        ncol = sqrt(s)
    )
}


## Parameters
N = 50
M = 400
perl <- perlinNoise(N = N, M = M, n = 7, m = 15)
x <- seq_len(N)
y <- seq_len(M)
z <- perl * 10


## Perspective function (iterate)
iter <- M - 50
for (i in seq(0, iter)) {
    cat("Processing file:", sprintf(paste0("%0", nchar(iter), "d"), i), "\n")
    png(
        filename = paste0(
            "perl_",
            sprintf(paste0("%0", nchar(iter), "d"), i),
            ".png"
        ), res = 150, height = 4, width = 4, units = "in"
    )
    par(bg = "#1c1c1c", mar = c(0, 0, 1, 0))
    persp(
        x = x[seq(1, N)],
        y = y[seq(1 + i, 50 + i)],
        z = z[seq(1, N), seq(1 + i, 50 + i)],
        box = FALSE,
        phi = 30,
        scale = FALSE,
        ltheta = -120,
        border = "#5e9bff",
        col = "#1c1c1c",
        col.main = "#5e9bff"
        # main = "THERE IS NOBODY HERE",
        # family = "VCR OSD Mono"
    )
    dev.off()
}


## Make gif
list.files(pattern = "*.png", full.names = T) %>%
    map(image_read) %>% # reads each path file
    image_join() %>% # joins image
    image_animate(fps = 25) %>% # animates, can opt for number of loops
    image_write("perlin_noise.gif") # write to current dir
