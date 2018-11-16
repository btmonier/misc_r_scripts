#-------------------------------------------------------------------------------
# Title:  Let's Learn S4 Object Programming...                             
# Author: Brandon Monier (brandon.monier@sdstate.edu) 
# Date:   09.19.17                                    
#-------------------------------------------------------------------------------

# A BMI example

## "Normal" programming...
weight <- 85
size <- 1.84

(bmi <- weight / size ^ 2)


## "Normal" programming - BMI with two people
weight.me <- 85
size.me <- 1.84

weight.her <- 62
size.her <- 1.60

(bmi.me <- weight.me / size.me ^ 2)
(bmi.her <- weight.me / size.her ^ 2) # Note the error here


## Object programming - BMI with two people
setClass("BMI", representation(weight = "numeric", size = "numeric"))

setMethod("show", "BMI",
  function(object) {
    cat("BMI =", object@weight / (object@size ^ 2), "\n")
  }
)

(myBMI <- new("BMI", weight = 85, size = 1.84))
(herBMI <- new("BMI", weight = 62, size = 1.20))


## Error prevention with object programming
(weight <- "Hello")
new("BMI", weight = "Hello", size = 1.84) # Note error here



# Validity checking with "coherence inspectors"

## "Normal" programming without control
size.me <- -1.84

## Object programming with control
setValidity("BMI",
  function(object) {
    if(object@size < 0) {
      return("This is a negative size.")
    } else {
      return(TRUE)
    }
  }
)

new("BMI", weight = 85, size = size.me)



# Inheritance (father to son?)

## Define "heir"
setClass("BMIplus", representation(sex = "character"), contains = "BMI")

(male <- new("BMIplus", size = 1.76, weight = 84, sex = "Male"))



# S4 with microarray data (Gatto 2013)

## Microarry with a matrix of size n x m (n = probe, m = samples) - Traditional
n <- 10
m <- 6

marray <- matrix(rnorm(n * m, 10, 5), ncol = m)

pmeta <- data.frame(sampleID = 1:m,
                    condition = rep(c("WT", "MUT"), each = 3))

rownames(pmeta) <- colnames(marray) <- LETTERS[1:m]

fmeta <- data.frame(geneID = 1:n,
                    pathway = sample(LETTERS, n, replace = TRUE))

rownames(fmeta) <- rownames(marray) <- paste0("probe", 1:n)

maexp <- list(marray = marray,
              fmeta  = fmeta,
              pmeta  = pmeta)
rm(marray, fmeta, pmeta)
str(maexp)

wt <- maexp$pmeta[, "condition"] == "WT"
maexp$marray["probe8", wt]


## Microarray example - S4
MArray <- setClass("MArray", slots = c(marray = "matrix",
                                       fmeta  = "data.frame",
                                       pmeta  = "data.frame"))

MArray() # An empty object
MArray(marray = 1:2) # Note error

ma <- MArray(marray = maexp[[1]],
             pmeta  = maexp[["pmeta"]],
             fmeta  = maexp[["fmeta"]])
class(ma); ma
ma@marray # Access individual slots with "@" symbol


## MArray methods
show # generic

setMethod("show",
          signature = "MArray",
          definition = function(object) {
            cat("An object of class ", class(object), "\n", sep = "")
            cat(" ", nrow(object@marray), " features by ",
                ncol(object@marray), " samples.\n", sep = "")
            invisible(NULL)
          })

setGeneric("marray", function(object, ...) standardGeneric("marray"))
setGeneric("fmeta", function(object, ...) standardGeneric("fmeta"))
setGeneric("pmeta", function(object, ...) standardGeneric("pmeta"))

setMethod("marray", "MArray",
          function(object) object@marray)
setMethod("fmeta", "MArray",
          function(object) object@fmeta)
setMethod("pmeta", "MArray",
          function(object) object@pmeta)


## Subset MArray
setMethod("[", "MArray",
          function(x, i, j, drop = "missing") {
            .marray <- x@marray[i, j]
            .pmeta <- x@pmeta[j, ]
            .fmeta <- x@fmeta[i, ]
            MArray(marray = .marray,
                    fmeta  = .fmeta,
                    pmeta  = .pmeta)
          })


## Validity check MArray
setValidity("MArray", function(object) {
  msg <- NULL
  valid <- TRUE
  if (nrow(marray(object)) != nrow(fmeta(object))) {
    valid <- FALSE
    msg <- c(msg,
             "Number of data and feature meta-data rows must be identical.")
  }
  if (ncol(marray(object)) != nrow(pmeta(object))) {
    valid <- FALSE
    msg <- c(msg,
             "Number of data rows and sample meta-data columns must be identical.")
  }
  if (!identical(rownames(marray(object)), rownames(fmeta(object)))) {
    valid <- FALSE
    msg <- c(msg,
             "Data and feature meta-data row names must be identical.")
  }
  if (!identical(colnames(marray(object)), rownames(pmeta(object)))) {
    valid <- FALSE
    msg <- c(msg,
             "Data row names and sample meta-data columns names must be identical.")
  }
  if (valid) TRUE else msg
})


## Replacement methods for MArray
ma@marray <- 1 # Note error

(broken <- ma)

broken@marray <- matrix(1:9, 3); broken

validObject(broken)

setGeneric("marray<-",
           function(object, value) standardGeneric("marray<-"))
setGeneric("pmeta<-",
           function(object, value) standardGeneric("pmeta<-"))
setGeneric("fmeta<-",
           function(object, value) standardGeneric("fmeta<-"))

setMethod("marray<-", "MArray",
          function(object, value) {
            object@marray <- value
            if (validObject(object))
              return(object)
})
setMethod("pmeta<-", "MArray",
          function(object, value) {
            object@pmeta <- value
            if (validObject(object))
              return(object)
})
setMethod("fmeta<-", "MArray",
          function(object, value) {
            object@fmeta <- value
            if (validObject(object))
              return(object)
})

tmp <- matrix(rnorm(n * m, 10, 5), ncol = m)
marray(ma) <- tmp

colnames(tmp) <- LETTERS[1:m]
rownames(tmp) <- paste0("probe", 1:n)
marray(ma) <- tmp

pmeta(ma)$sex <- rep(c("M", "F"), 3)
pmeta(ma)



# S4 polygon example (Andrea Spano)

## setClass
setClass("rolygon", representation(n = "numeric", s = "numeric"))


## setMethod
setMethod(f = "plot", signature = "rolygon", 
          definition = function(x, y){
            object = x
            s = object@s
            n = object@n
            rho = (2*pi)/n
            h = .5*s*tan((pi/2)-(pi/n))
            r = sqrt(h^2+(s/2)^2)
            sRho = ifelse(n %% 2 == 0, 
                          (pi/2- rho/2), pi/2)
            cumRho = cumsum(c(sRho, rep(rho, n)))
            cumRho = ifelse(cumRho > 2*pi, 
                            cumRho-2*pi, cumRho)
            x = r*cos(cumRho)
            y = r*sin(cumRho)
            par(pty = "s")
            plot(x, y, type = "n", xlab = "", ylab = "") 
            lines(x, y, col = "red", lwd = 2)
            points(0, 0, pch = 16, col = "red")
            grid()
            invisible(NULL)      
          })


## rolygon
plot(new("rolygon", n = 5, s = 2))  


## f
f <-  function(x) {
  g = function(y){x+y}
  g
}


## f1
f1 <- f(x = 1)
f1(y = 3)


## ls
ls(env=environment(f1))
get("x", env=environment(f1))


## environment
environment(f1)$x <- 0 
f1(1)


## f99
f99 <- f(99)
f99(y = 1)


## rolygon
rolygon <- function(n) {
  
  # Define rolygon class    
  setClass("rolygon", representation(
    n = "numeric", s = "numeric"))
  
  # Define a plot method for object of class rolygon
  setMethod(f = "plot", signature = "rolygon", 
            definition = function(x, y){
              object <-  x
              s <-  object@s
              n <- object@n
              pi <- base::pi
              rho <-  (2*pi)/n
              h <-  .5*s*tan((pi/2)-(pi/n))
              r <-  sqrt(h^2+(s/2)^2)
              sRho <-  ifelse(n %% 2 == 0, 
                              (pi/2- rho/2), pi/2)
              cumRho <-  cumsum(c(sRho, rep(rho, n)))
              cumRho <-  ifelse(cumRho > 2*pi, 
                                cumRho-2*pi, cumRho)
              x <-  r*cos(cumRho)
              y <-  r*sin(cumRho)
              par(pty = "s")
              plot(x, y, type = "n", xlab = "", 
                   ylab = "") 
              lines(x, y, col = "red", lwd = 2)
              points(0, 0, pch = 16, col = "red")
              grid()
              invisible(NULL)      
            })
  
  # Define a function that returns an object 
  # of class rolygon
  f <- function(s){new("rolygon", n = n, 
                       s = s)}
  
  # Return the newly created function
  return(f)  
}


## heptagon
heptagon <- rolygon(n = 7)
e1 <- heptagon(1)
plot(e1)  


## circumference
circumference <- rolygon(n = 10^4)
plot(circumference(s = base::pi/10^4))



# Hadley save meee

## Classes and instances

setClass("Person", representation(name = "character", age = "numeric"))
setClass("Employee", representation(boss = "Person"), contains = "Person")

brandon <- new("Person", name = "Brandon", age = 31)



cat("\nBegin OOP with S4 demo \n\n")

cat("Creating Person p1 with initialize values \n\n")
p1 <- new("Person")  # could use p1 <- Person
display(p1)  # could use print(p1)

cat("Setting p1 fields directly \n\n")
p1@empID <- as.integer(65565)
p1@lastName <- "Adams"
p1@hireDate <- "2010-09-15"
p1@payRate <- 43.21
display(p1)

cat("Calling yearsService \n\n")
tenure <- yearsService(p1)
cat("Person p1 tenure = ", tenure, " years \n")

cat("Making a value-copy of p1 using '<-' \n\n")
p2 <- p1

cat("\nEnd OOP with S4 demo \n\n")



# Leisch package creation with S4

## A linear model example
linmodEst <- function(x, y)
{
  ## compute QR-decomposition of x
  qx <- qr(x)
  
  ## compute (x'x)^(-1) x'y
  coef <- solve.qr(qx, y)
  
  ## degrees of freedom and standard deviation of residuals
  df <- nrow(x)-ncol(x)
  sigma2 <- sum((y - x %*% coef)^2) / df
  
  ## compute sigma^2 * (x'x)^-1
  vcov <- sigma2 * chol2inv(qx$qr)
  colnames(vcov) <- rownames(vcov) <- colnames(x)
  list(coefficients = coef,
       vcov = vcov,
       sigma = sqrt(sigma2),
       df = df)
}

data(cats, package = "MASS")
linmodEst(cbind(1, cats$Bwt), cats$Hwt)















