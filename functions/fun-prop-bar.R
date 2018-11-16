#------------------------------------------------------------------------------
# Title:         Function - Proportional Usage Text Bar
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-05-07 11:04:16 CDT
# Last Modified: 2018-05-17 09:08:18 CDT
#------------------------------------------------------------------------------

makeTextBar <- function(
    use, tot, char.use = "#", char.rem = " ", char.len = 25, 
    char.end = c("[", "]")
) {
    prop.use <- use / tot
    prop.rem <- 1 - prop.use

    num.use <- round(prop.use * char.len, 0)

    if (num.use == 0) {
        num.use <- 1
    } else if (num.use == char.len) {
        num.use <- char.len - 1
    } else {
        num.use
    }

    num.rem <- char.len - num.use

    text.bar <- paste0(
        char.end[1], 
        paste(rep(char.use, num.use), collapse = ""), 
        paste(rep(char.rem, num.rem), collapse = ""), 
        char.end[2]
    )
    
    return(text.bar)
} 
