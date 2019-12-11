# === CW 001 ========================================================

## My attempt ----
duplicate_count <- function(text) {
    text <- tolower(text)
    text <- strsplit(text, "")
    text <- unlist(text)
    text <- table(text)
    text <- text[text >= 2]
    return(length(text))
}



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

## My attempt ----
group_by_commas <- function(n) {
    
    ## Logic
    if (n < 0 || n >= 2147483647) return(-1)
    if (n < 1000) return(as.character(n))
    
    ## Reverse string and split with , every 3 elements
    n <- as.integer(n)
    n <- paste0(rev(strsplit(as.character(n), "")[[1]]), collapse = "")
    n <- gsub("(.{3})", "\\1,", n)
    n <- paste0(rev(strsplit(as.character(n), "")[[1]]), collapse = "")
    n <- gsub("^,", "", n)
    
    ## Return reversed
    return(n)
}


## Best attempt ----
group_by_commas <- function(n){
    format(n, big.mark=',', scientific=F)
}



# === CW 004 ========================================================

## My attempt ----
get_count <- function(input_str) {
    vowels <- c("a", "e", "i", "o", "u")
    input_str <- strsplit(input_str, "")[[1]]
    length(input_str[input_str %in% vowels])
}


## Best attempt ----
get_count <- function(input_str){
    nchar(gsub("[^aeiou]", "", input_str))
}











