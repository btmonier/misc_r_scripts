# test script for package similarity

list.of.packages <- c("ggplot2", "Rcpp", "abctools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require('ggplot2')

df <- data.frame(treatment = factor(rep(c('A', 'B'), each = 1000)),
                 variable = c(rnorm(1000), rnorm(1000, mean = 2)))

ggplot(df, aes(x = variable, fill = treatment)) +
  geom_histogram(binwidth = .2, alpha = .5, position = "identity")