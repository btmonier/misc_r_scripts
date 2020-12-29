## Problem 1 ----

library(magrittr)

limit <- 1000
n <- 1:limit - 1

n[n %% 3 == 0 | n %% 5 == 0] %>% sum() %>% paste0("Prob 001: ", .) %>% print()


## Problem 2 ----
x <- 0
y <- 1
fib <- c()
while (x < 4000000 & y < 4000000){
    x <- x + y
    y <- x + y
    fib = c(fib, x, y)
}
fib %>% sum() %>% paste0("Prob 002: ", .) %>% print()


## Problem 3 ----
pf <- c()
limit <- 13195
n <- 1

isPrime <- function(n) {
    if (n <= 1) return(FALSE)
    
    for (i in 2:n) {
        if (n %% i == 0) return(FALSE)
        return(TRUE)
    }
    
}

for (i in 2:limit) {
    if (limit %% i == 0) {
        pf[n] <- i
        n <- n + 1
    }
        
}

