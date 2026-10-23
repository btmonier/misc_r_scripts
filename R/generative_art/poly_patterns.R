#------------------------------------------------------------------------------
# Title:         Polygon Pattern Generation
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-04-30 13:44:10 CDT
# Last Modified: 2018-06-06 15:42:05 CDT
#------------------------------------------------------------------------------

# Polygon data frames
ngon_prime <- function(x = 2, y = 2, col1 = "#4d4d4d", col2 = "#add8e6") {
    df.tri1 <- lapply(seq(0, y - 1), function(j) {
        lapply(seq(0, x - 1), function(i) {
            tmp1 <- data.frame(
                x = c(0, 0, 1) + i,
                y = c(0, 1, 1) + j
            )
            list(tmp1)
        })
    })
    df.tri1 <- unlist(df.tri1, recursive = FALSE)
    df.tri1 <- unlist(df.tri1, recursive = FALSE)

    df.tri2 <- lapply(seq(0, y - 1), function(j) {
        lapply(seq(0, x - 1), function(i) {
            tmp1 <- data.frame(
                x = c(0, 1, 1) + i,
                y = c(0, 0, 1) + j
            )
            list(tmp1)
        })
    })
    df.tri2 <- unlist(df.tri2, recursive = FALSE)
    df.tri2 <- unlist(df.tri2, recursive = FALSE)

    col1 <- sample(c(col1, col2), x * y, replace = TRUE)
    col2 <- sample(c(col1, col2), x * y, replace = TRUE)

    df.ls <- list(df.tri1, df.tri2, col1, col2)

    structure(df.ls, x = x, y = y, class = "ngon")
}



# Generate polygons
print.ngon <- function(ngon_data, ...) {
    x <- attr(ngon_data, "x")
    y <- attr(ngon_data, "y")

    par(mar = c(0, 0, 0, 0), pty = "s", bg = NA)
    plot.new()
    plot.window(
        xlim = c(0, x),
        ylim = c(0, y),
        xaxs = "i",
        yaxs = "i"
    )

    df.tri1 <- ngon_data[[1]]
    df.tri2 <- ngon_data[[2]]

    col1 <- ngon_data[[3]]
    col2 <- ngon_data[[4]]

    for (i in seq_len(length(df.tri1))) {
        polygon(
            x = df.tri1[[i]]$x,
            y = df.tri1[[i]]$y,
            col = col1[i],
            border = NA
        )
        polygon(
            x = df.tri2[[i]]$x,
            y = df.tri2[[i]]$y,
            col = col2[i],
            border = NA
        )
    }
}



# Iterate
quiltr <- function(sym = c("reflect", "rotate"),
                   iter = 1,
                   keep.seeds = FALSE,
                   ...) {
    sym_type <- c("reflect", "rotate")
    if (missing(sym) || !sym %in% sym_type) {
        stop("Incorrect symmetry parameter.")
    } 

    pad <- nchar(iter)
    pad <- paste0("%0", pad, "d")

    message("Generating seeds...")
    for (i in seq_len(iter)) {
        tmp <- ngon_prime(...)
        png(
            paste0("seed-", sprintf(pad, i), ".png"),
            width     = 3,
            height    = 3,
            units     = "in",
            res       = 200,
            pointsize = 4
        )
        par(mar = c(0, 0, 0, 0))
        print(tmp)
        dev.off()
    }

    v <- list()
    for (i in seq_len(iter)) {
        v[[i]] <- magick::image_read(
            paste0("seed-", sprintf(pad, i), ".png")
        )
    }

    if (sym == "rotate") {
        for (i in seq_len(iter)) {
            message(
                paste0(
                    "Generating rotated quilt: ", sprintf(pad, i),
                    " / ", iter
                )
            )
            png(
                paste0("quilt-rotate-", sprintf(pad, i), ".png"),
                width     = 3,
                height    = 3,
                units     = "in",
                res       = 200,
                pointsize = 4
            )
            par(
                mfrow = c(2, 2),
                mar = c(0, 0, 0, 0),
                oma = c(0, 0, 0, 0),
                pty = "m"
            )
            plot(v[[i]])
            plot(magick::image_rotate(v[[i]], 90))
            plot(magick::image_rotate(v[[i]], 270))
            plot(magick::image_rotate(v[[i]], 180))
            dev.off()
        }
    } else if (sym == "reflect") {
        for (i in seq_len(iter)) {
            message(
                paste0(
                    "Generating reflected quilt: ", 
                    sprintf(pad, i),
                    " / ", 
                    iter
                )
            )
            seed <- magick::image_read(
                paste0("seed-", sprintf(pad, i), ".png")
            )
            png(
                paste0("quilt-reflect-", sprintf(pad, i), ".png"),
                width     = 3,
                height    = 3,
                units     = "in",
                res       = 200,
                pointsize = 4
            )
            par(
                mfrow = c(2, 2),
                mar = c(0, 0, 0, 0),
                oma = c(0, 0, 0, 0),
                pty = "m"
            )
            plot(v[[i]])
            plot(magick::image_flop(v[[i]]))
            plot(magick::image_flip(v[[i]]))
            plot(magick::image_flip(magick::image_flop(v[[i]])))
            dev.off()
        }
    }

    if (isTRUE(keep.seeds)) {
        message("Finished!")
    } else {
        message("Deleting seeds...")
        file.remove(
            grep(
                pattern = "^seed-",
                x = list.files(),
                value = TRUE
            )
        )
        message("Finished!")
    }
    message(paste0("\nPatterns found at: ", getwd()))
}
