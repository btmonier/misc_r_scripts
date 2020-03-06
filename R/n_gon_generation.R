#------------------------------------------------------------------------------
# Title:         Function - n-gon Generator
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-05-21 15:36:05 CDT
# Last Modified: 2018-08-07 15:16:58 CDT
#------------------------------------------------------------------------------


# n-gon generator
.ngon <- function(n, s = 1, vert) {
    rho <- (2 * pi) / n
    h <- 0.5 * s * tan((pi / 2) - (pi / n))
    r <-  sqrt(h^2 + (s / 2) ^ 2)
    sRho <-  ifelse(n %% 2 == 0, (pi / 2 - rho/2), pi / 2)
    cumRho <- cumsum(c(sRho, rep(rho, n)))
    cumRho <- ifelse(cumRho > 2 * pi, cumRho - 2 * pi, cumRho)

    x <- r * cos(cumRho)
    y <- r * sin(cumRho)

    if (missing(vert)) {
        x <- replace(x, abs(x) < 0.001, 0)
        y <- replace(y, abs(y) < 0.001, 0)
        return(list(x = x, y = y))
    } else {
        x <- replace(x, abs(x) < 0.001, 0)[seq_len(n)]
        y <- replace(y, abs(y) < 0.001, 0)[seq_len(n)]
        return(list(x = x[vert], y = y[vert]))
    }
}

# Modulo with offset
.mod <- function(x, m) {
    t1 <- floor(x / m)
    t2 <- x - t1 * m
    return(replace(t2, t2 == 0, m))
}


# The chaos game...
chaosGame <- function(sides = 3,
                      r = 0.5,
                      iter = 10000,
                      seq = c("random", "restrict"),
                      vertex_choice = NULL,
                      chaos = FALSE) {
    if (sides < 3) {
        stop("3 or more sides are needed to generate polygons.")
    }
    if (r > 1 || r < 0) {
        stop("r variable needs to be in range of 0 and 1.")
    }

    seq_types <- c("random", "restrict")
    if (missing(seq)) {
        seq <- "random"
    } else if (!seq %in% seq_types) {
        stop("Incorrect sequence type. Use \"random\" or \"restrict\".")
    }

    if (seq == "random" && is.null(vertex_choice)) {
        cho <- seq_len(sides)
    } else if (seq == "random" && length(vertex_choice) < sides) {
        warning(
            paste(
                "Restricted vertices will not work with random sampling.",
                "Using all vertices..."
            )
        )
        cho <- seq_len(sides)
    } else if (seq == "restrict" && is.null(vertex_choice)) {
        message(
            paste(
                "Vertex restriction not implemented.",
                "Using all vertices..."
            )
        )
        cho <- seq_len(sides)
    } else if (seq == "restrict" && !is.vector(vertex_choice)) {
        stop("Please use vector for vertex choices.")
    } else if (seq == "restrict" && max(vertex_choice) > sides && !chaos) {
        stop("Max vertex restriction must not exceed number of sides.")
    } else {
        cho <- vertex_choice
    }

    if (isTRUE(chaos)) {
        message("Chaos has been implemented. All options will be random!")
        sides <- sample(3:10, 1)
        r <- 0.5
        cho <- sort(sample(seq_len(sides), sample(seq(2, sides), 1)))
        seq <- "restrict"
    }

    n <- .mod(cumsum(sample(cho, iter, replace = TRUE)), sides)

    ## Generate points
    tmp <- .ngon(sides)
    v_x <- tmp$x
    v_y <- tmp$y

    cat("Generating points...\n")
    pb <- utils::txtProgressBar(
        min = 0,
        max = iter,
        style = 3,
        width = 25,
        char = ":"
    )

    x <- y <- 0
    pltx <- plty <- list()

    for (i in seq_len(iter)) {
        x <- x + (v_x[n[i]] - x) * (1 - r)
        y <- y + (v_y[n[i]] - y) * (1 - r)
        pltx[[i]] <- x
        plty[[i]] <- y
        utils::setTxtProgressBar(pb, i)
        if (i == iter) cat("\n")
    }

    chaos <- list(
        chaos_df = tibble::tibble(
            x = unlist(pltx),
            y = unlist(plty)
        ),
        sides = sides,
        r = r,
        iter = iter,
        seq = seq,
        choices = cho
    )
    class(chaos) <- "chaosDF"
    chaos
}


print.chaosDF <- function(x, ...) {
    cat("A Chaos Game data set:", x$iter, "x 2\n\n")

    if (length(x[[1]][["x"]]) > 10) {
        tmp <- data.frame(
            x = x[[1]][["x"]][1:10],
            y = x[[1]][["y"]][1:10]
        )
        print(tmp)
        cat("... with", length(x[[1]][["x"]]) - 10, "more rows\n")
    } else {
        tmp <- data.frame(
            x = x[[1]][["x"]],
            y = x[[1]][["y"]]
        )
        print(tmp)
    }
}


summary.chaosDF <- function(x, ...) {
    cat("Chaos game metadata:\n")
    cat("  Points............",      x$iter,  "\n")
    cat("  Polygon sides.....",      x$sides, "\n")
    cat("  r value...........",      x$r,     "\n")
    cat("  Vertex choices....", "{", x$cho,   "}\n")
    cat("  Sequencing type...",      x$seq,   "\n")
}

boxplot.chaosDF <- function(x, ...) {
    par(pty = "s")
    boxplot(
        x$chaos_df,
        main = "Chaos Game Data",
        xlab = "Coordinate",
        ylab = "Vertex position",
        col = c("#cccccc", "#4286f4")
    )
}



plot.chaosDF <- function(x, ...) {
    par(mar = c(5, 0, 3, 0), pty = "s")
    plot(
        x[[1]],
        x[[2]],
        bty = "n",
        xaxt = "n",
        yaxt = "n",
        ann = FALSE,
        pch = 15,
        cex = 0.1,
        axes = FALSE,
        ...
    )
    title(
        main = paste0(x$sides, "-gon Fractal"),
        xlab = paste0(
            "Sides: ", x$sides,
            "  |  Points: ", x$iter,
            "  |  r: ", x$r
        ),
        ...
    )

}
