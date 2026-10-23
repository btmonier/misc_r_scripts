#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   mat_algebra.R
# Description:   Matrix Algebra using R
# Author:        Brandon Monier
# Created:       2019-01-09 at 21:56:54
# Last Modified: 2019-01-09 at 23:10:12
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to generate answers to
#    some matrix algebra problems from the Tuscon Plant Breeding
#    Institute workshops.
#--------------------------------------------------------------------

# === Question 1 ====================================================

## Make matrix
A <- matrix(
    data = c(
        3, 4, 4, 6, 9, 2, 
        -1, -6, 1, 1, 1, 
        -10, 2, 9, 2, -1
    ), 
    nrow = 4, 
    byrow = TRUE
)

## Make results matrix
C <- matrix(
    data = c(
        -10, 20, 2, -10
    ), 
    nrow = 4, 
    byrow = TRUE
)

## Calculate products of x1 ... x4
result1 <- solve(A) %*% C


# === Question 2 ====================================================

## Variance matrix
V <- matrix(
    data = c(
        10, -5, 10,
        -5, 20, 0,
        10, 0, 30
    ),
    nrow = 3
)
c1 <- matrix(data = c(1, -2, 5))
c2 <- matrix(data = c(0, 6, -4))

## Get results
result2 <- t(c1) %*% V %*% c1
result3 <- t(c2) %*% V %*% c2
result4 <- t(c1) %*% V %*% c2


# === Output for terminal ===========================================
cat("=== Question 1 ==========", "\n")
cat("The product of question 1:", "\n")
print(result1)
cat("\n")
cat("=== Question 2 ==========", "\n")
cat("The variance of Y1 index....... ", result2, "[ t(c1) %*% V %*% c1 ]", "\n")
cat("The variance of Y2 index.......", result3, "[ t(c2) %*% V %*% c2 ]", "\n")
cat("The co-variance of Y1 and Y2...", result4, "[ t(c1) %*% V %*% c2 ]", "\n")
