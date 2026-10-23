width <- 15
height <- 15

player <- list(
    px = 4,
    py = 4,
    char = "*"
)

m <- matrix(".", height, width)

printMat <- function(m) {
    for (i in 1:nrow(m)) {
        message(m[i, ])
    }
}



draw <- function(m) {
    shell("cls")
    printMat(m)
}

update <- function(m) {
    while (TRUE) {

        x <- keypress::keypress()
        px <- player[["px"]]
        py <- player[["py"]]

        if (x == "left") {
            px <- px - 1
        } else if (x == "right") {
            px <- px + 1
        }

        m[py, px] <- player[["char"]]
        draw(m)
    }
}


main <- function(m) {
    update(m)
}

main(m)
