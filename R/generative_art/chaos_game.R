#-----------------------------------------------------#
# Title:  The Chaos Game                              #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   05.08.17                                    #
#-----------------------------------------------------#






.dice.tern <- function() {
  alpha <- c(0, 0)
  beta  <- c(0.5, sqrt(3)/2)
  gamma <- c(1, 0)
  tmp <- sample(1:3, 1)
  if(tmp == 1) {
    return(data.frame(x = alpha[1], y = alpha[2]))
  } 
  if(tmp == 2) {
    return(data.frame(x = beta[1], y = beta[2]))
  }
  if(tmp == 3) {
    return(data.frame(x = gamma[1], y = gamma[2]))
  }
}

.dice.four <- function() {
  a <- c(0, 0)
  b <- c(0, 1)
  c <- c(1, 1)
  d <- c(1, 0)
  tmp <- sample(1:4, 1)
  if(tmp == 1) {
    return(data.frame(x = a[1], y = a[2]))
  } 
  if(tmp == 2) {
    return(data.frame(x = b[1], y = b[2]))
  }
  if(tmp == 3) {
    return(data.frame(x = c[1], y = c[2]))
  }
  if(tmp == 4) {
    return(data.frame(x = d[1], y = d[2]))
  }
}

.dice.five <- function() {
  a <- c(0.5, 0)
  b <- c(1.5, 0)
  c <- c(1.8122, 0.951)
  d <- c(1, 1.539)
  e <- c(0.1878, 0.951)
  tmp <- sample(1:5, 1)
  if(tmp == 1) {
    return(data.frame(x = a[1], y = a[2]))
  } 
  if(tmp == 2) {
    return(data.frame(x = b[1], y = b[2]))
  }
  if(tmp == 3) {
    return(data.frame(x = c[1], y = c[2]))
  }
  if(tmp == 4) {
    return(data.frame(x = d[1], y = d[2]))
  }
  if(tmp == 5) {
    return(data.frame(x = e[1], y = e[2]))
  }
}


# .ternplot <- function(df, file = 'tmp.pdf', point) {
#   tri <- data.frame(x = c(0, 0.5, 1), y = c(0, sqrt(3)/2, 0))
#   pdf(file, width = 6.5, height = 7)
#   
#   plot(NA, xlim = c(0, 1), ylim = c(0, 1), pch = 16, bty = 'n', 
#        xlab = '', ylab = '')
# 
#   polygon(tri, lwd = 2)
#   points(df, pch = 16, cex = point)
#   
#   dev.off()
# }
# 
# .fourplot <- function(df, file = 'tmp.pdf', point){
#   square <- data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0))
#   pdf(file, width = 6.5, height = 7)
#   
#   plot(NA, xlim = c(0, 1), ylim = c(0, 1), pch = 16, bty = 'n', 
#        xlab = '', ylab = '')
#   
#   polygon(square, lwd = 2)
#   points(df, pch = 16, cex = point)
#   
#   dev.off()
# }
# 
# .fiveplot <- function(df, file = 'tmp.pdf', point){
#   pent <- data.frame(x = c(0.5, 1.5, 1.8122, 1, 0.1878), 
#                      y = c(0, 0, 0.951, 1.539, 0.951))
#   pdf(file, width = 7, height = 7)
#   
#   plot(NA, xlim = c(0, 2), ylim = c(0, 1.6), pch = 16, bty = 'n', 
#        xlab = '', ylab = '')
#   
#   polygon(pent, lwd = 2)
#   points(df, pch = 16, cex = point)
#   
#   dev.off()
# }



  
.chaos.df <- function(n, r, shape) {
  message('Generating index...')
  
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3, width = 40)
  s <- c(0.5, 0)
 
  ls1 <- list() 
  for (i in 1:n) {
    if (shape == 'triangle') {
      ls1[[i]] <- .dice.tern()
    }
    if (shape == 'square') {
      ls1[[i]] <- .dice.four()
    }
    if (shape == 'pentagon') {
      ls1[[i]] <- .dice.five()
    }
  }
  ls2 <- list()
  for (i in 2:n) {
    ls2[[1]] <- data.frame(x = r * sum(c(s[[1]], ls1[[1]][1, 1])), 
                           y = r * sum(c(s[[2]], ls1[[1]][1, 2])))
    
    ls2[[i]] <- data.frame(x = r * sum(c(ls1[[i]][1, 1], ls2[[i - 1]][1, 1])),
                           y = r * sum(c(ls1[[i]][1, 2], ls2[[i - 1]][1, 2])))
    utils::setTxtProgressBar(pb, i)
  }
  message('Wrapping things up...')
  
  tmp <- data.table::rbindlist(ls2)
  
  message('Done!\n')
  return(tmp)
}




fractus <- function(n, r, shape, point = NULL, title = TRUE) {

  if(is.null(point)) {
    point = 0.5
  }
  if(isTRUE(title)) {
    m.label <- paste0('n = ', n, ', r = ', r)
  } else {
    m.label <- NULL
  }
  
  graphics::plot(.chaos.df(n, r, shape), bty = 'n', xaxt = 'n', yaxt = 'n', 
                 pch = 16, xlab = '', ylab = '', cex = point, main = m.label)
}













