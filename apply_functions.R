# test

seq(1, 16)

m <- matrix(seq(1, 16), 4, 4)
m

m <- matrix(1:16, 4, 4)
m

apply(m, 1, min)
apply(m, 2, min)

m <- array(seq(32), dim = c(4, 4, 4))
m
str(m)
dim(m)

apply(m , 1, sum)
apply(m , 2, sum)

sum(1, 5, 9, 13, 17, 21, 25, 29)
sum(1:4, 17:20)

apply(m, c(1, 2), sum)


ls <- list(a = 1, b = 1:3, c = 10:100)
ls

lapply(ls, FUN = length)
