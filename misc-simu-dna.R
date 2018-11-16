
# COUNT THOSE NUCLEOTIDES, YO...

# Make a nucleotide counter...
count.it <- function(x) {
  nuc <- c('a', 'c', 'g', 't')
  ls.tmp <- list()
  for (i in nuc) {
    ls.tmp[[i]] <- lengths(regmatches(x, gregexpr(i, x)))
  }
  df.dna <- do.call('rbind', ls.tmp)
  return(t(df.dna))
}

# Make a simulation data frame...
simu.dna <- function(n, bp, percent = FALSE){
  nuc <- c('a', 'c', 'g', 't')
  ls.tmp <- list()
  for(i in 1:n) {
    ls.tmp[[i]] <- paste(sample(nuc, bp, replace = T), collapse = '')
  }
  tmp <- lapply(ls.tmp, count.it)
  tmp.df <- as.data.frame(do.call('rbind', tmp))
  
  if (!isTRUE(percent)) {
    return(tmp.df)
  } else {
    return(tmp.df / rowSums(tmp.df))
  }
}

