## Problem 1 ----

library(magrittr)

limit <- 1000
n <- 1:limit - 1

n[n %% 3 == 0 | n %% 5 == 0] %>% sum() %>% paste0("Prob 001: ", .) %>% print()


## Problem 2 ----

fib <- function(n) {
    if(n <= 1) {
        return(n)
    } else {
        return(fib(n - 1) + fib(n - 2))
    }
}

limit <- 100
v <- sapply(seq_len(limit - 1), fib) %>%
    .[. %% 2 == 0] %>% 
    sum() %>% 
    paste0("Prob 002: ", .) %>% 
    print()


