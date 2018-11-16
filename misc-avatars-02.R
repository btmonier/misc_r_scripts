#-----------------------------------------------------------------------------
# Title:         Avatar Generation 02
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-03-02 23:24:35 CST
# Last Modified: 2018-03-02 23:26:10 CST
#-----------------------------------------------------------------------------

# Random avatar generator function

## Make function
ran.av <- function(rcolor = FALSE, width = 9, main = NULL) {
    ## Validity of width
    if (width < 5) {
        stop(
            "Please enter a 'width' value >= 5"
        )
    }
    
    ## Make universal variables
    nr <- width - 2
    
    ## Make first half of matrix
    if (width %% 2 != 0) {
        nc <- (width - 3) * 0.5
        ne <- nr * nc
        nm <- matrix(
            data = sample(c(0, 1), ne, TRUE),
            nrow = nr,
            ncol = nc
        )
    } else {
        nc <- (width - 4) * 0.5
        ne <- nr * nc
        nm <- matrix(
            data = sample(c(0, 1), ne, TRUE),
            nrow = nr,
            ncol = nc
        )
    }
    
    ## Flip first half
    mn <- nm[, c(nc:1), drop = FALSE]
    
    ## Make "mid rib"
    if (width %% 2 != 0) {
        midm <- matrix(
            data = sample(c(0, 1), nr, TRUE),
            nrow = nr,
            ncol = 1,
            byrow = TRUE
        )
    } else {
        midm <- matrix(
            data = sample(c(0, 1), nr, TRUE),
            nrow = nr,
            ncol = 1,
            byrow = TRUE
        )
        midm <- cbind(midm, midm)
    }
    
    ## Bind portions together
    fm <- cbind(nm, midm, mn)
    fm <- rbind(c(rep(0, nr)), fm, c(rep(0, nr)))
    fm <- cbind(c(rep(0, width)), fm, c(rep(0, width)))
    
    ## Rotate matrix
    rotate <- function(x) {
        t(apply(x, 2, rev))
    }
    fmr <- rotate(fm)
    
    if (!isTRUE(rcolor)) {
        col <- c("gray92", "#ff4949")
    } else {
        qual_col_pals <- 
            RColorBrewer::brewer.pal.info[
                RColorBrewer::brewer.pal.info$category == "qual",]
        col_vector = unlist(
            mapply(
                RColorBrewer::brewer.pal, 
                qual_col_pals$maxcolors, rownames(qual_col_pals)
            )
        )
        col <- sample(col_vector, 1)
        col <- c("gray92", col)
        
    }
    
    ## Image generation
    par(mar = c(2, 2, 2, 2), pty = "s")
    ran.av <- image(
        fmr, 
        col = col, 
        xaxt = "n", 
        yaxt = "n",
        main = main
    )
    box(col = "gray92")
    return(ran.av)
}
