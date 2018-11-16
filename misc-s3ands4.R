# S3 and S4 object test

# Integers and factors... ----
x <- rep(0:1, c(10, 20))
y <- as.factor(x)



# Generic functions ----
summary(x)
summary(y)

print(x)
print(y)

plot(x)
plot(y)



# S3 programming ----
myx <- x
class(myx) <- "myvector"

print.myvector <- function(x, ...) {
  cat("This is your vector:\n")
  cat(paste(x[1:5]), "...\n")
}

summary.myvector <- function(x, ...) {
  cat("\nSUMMARY STATISTICS \n")
  cat("   Unique values : ", paste(unique(x)), "\n")
  cat("   Length        : ", length(x), "\n")
  cat("   Integer       : ", is.integer(x), "\n\n")
}



# Another S3 exampple ----
calculator <- function(x, ...) UseMethod("calculator") 

calculator.default <- function(x, y, ...) {
  add <- list()
  add$x <- x
  add$y <- y
  add$sum <- sum(x, y)
  class(add) <- "calculator"
  add
}

print.calculator <- function(x, ...) {
  cat("Your calculation results:\n")
  cat("   Variable (x) :", paste(x$x), "\n")
  cat("   Variable (y) :", paste(x$y), "\n")
  cat("   Formula      :", paste0("'", x$x, " + ", x$y, "'\n"))
  cat("   Calc. result :", paste(sum(x$sum)), "\n")
}