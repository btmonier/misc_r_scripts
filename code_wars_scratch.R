# === CW 001 ========================================================

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



# === CW 002 ========================================================
bouncingBall <- function(h, bounce, window) {
    
    ## Logic
    if (h <= 0) {
        return(-1)
    }
    
    if (bounce <= 0 | bounce >= 1) {
        return(-1)
    }
    
    if (window >= h) {
        return(-1)
    }
    
    ## Iterate through variables while new height > window height
    new_height <- h * bounce
    fall_data <- 1 # will always fall at start...
    bounce_data <- 0
    while (new_height > window) {
        new_height <- bounce * new_height
        fall_data <- fall_data + 1
        bounce_data <- bounce_data + 1
    }
    return(fall_data + bounce_data)
}



# === CW 003 ========================================================

dig_power <- function(n, p) {
    if (n < 0 || p < 0) return(-1)
    
    num_string <- as.numeric(strsplit(as.character(n), "")[[1]])
    
    powers <- seq_len(num_string) + (p - 1)
    
    form_iter <- num_string ^ powers
    
    sum_iter <- sum(form_iter)
    return(num_string)
}







