#------------------------------------------------------------------------------
# Title:         Function - Proportional Usage Text Bar
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-05-07 11:04:16 CDT
# Last Modified: 2018-12-14 17:31:10 EST
#------------------------------------------------------------------------------

makeTextBar <- function(use, 
                        total, 
                        charUse = "#", 
                        charRem = " ", 
                        charLen = 25, 
                        charEnd = c("[", "]")) {
    # Generates a text-based proportion bar for downstream applications
    #
    # Args:
    #   use:     Number of units used in totalal size.
    #   total:   Total number units in totalal size.
    #   charUse: Text character to display used portion of bar. Default is
    #            "#".
    #   charRem: Text character to display unused portion of bar. Default is
    #            " ".
    #   charLen: Total number of characters used for bar. Default is 25 units.
    #   charEnd: "Fancy" end pieces if your want them. Defaults to 
    #            c("[", "]").
    #
    # Returns:
    #   A vector containing strings.

    propUse <- use / total
    propRem <- 1 - propUse

    numUse <- round(propUse * charLen, 0)

    if (use == 0) {
        numUse <- 0
    } else if (numUse == 0) {
        numUse <- 1
    } else if (numUse == charLen) {
        numUse <- charLen - 1
    } else {
        numUse
    }

    numRem <- charLen - numUse

    text.bar <- paste0(
        charEnd[1], 
        paste(rep(charUse, numUse), collapse = ""), 
        paste(rep(charRem, numRem), collapse = ""), 
        charEnd[2]
    )
    
    return(text.bar)
} 
