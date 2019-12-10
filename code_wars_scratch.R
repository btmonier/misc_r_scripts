# === CW 001 ===+

library(testthat)

duplicate_count <- function(text) {
    text <- tolower(text)
    text <- strsplit(text, "")
    text <- unlist(text)
    text <- table(text)
    text <- text[text >= 2]
    return(length(text))
}

testthat::test_that("Sample Tests", {
    expect_equal(duplicate_count(""), 1)
    expect_equal(duplicate_count("abcdeaa"), 1)
    expect_equal(duplicate_count("Indivisibilities"), 2)
}) %>% print()



# === CW 002 ====
bouncingBall <- function(h, bounce, window) {
    if (h <= 0) {
        return(-1)
    }
    
    if (bounce <= 0 | bounce >= 1) {
        return(-1)
    }
    
    if (window >= h) {
        return(-1)
    }
    
    
    bounce_data <- list()
    i <- TRUE
    while (i) {
        new_height <- bounce * h
        if (new_height <= window) {
            new_height <- bounce * new_height
            message("bounce at ", new_height, " meters...")
            i <- TRUE
        } else {
            message("stop!")
            i <- F
        }
        # message("bounce...")
    }
    return(2)
}













