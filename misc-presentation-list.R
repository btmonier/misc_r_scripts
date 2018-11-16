#-----------------------------------------------------#
# Title:  Presentation List Generation                #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   08.21.17                                    #
#-----------------------------------------------------#

# Current list
people <- c("Sierra Ash", "Jeff Bartel", "Janice Eibensteiner", "Arjun Kafle",
            "Brandon Monier", "Vincent Peta", "Rachel Raths", "Alex Soupir",
            "Jaya Yakha")


# Generate list (Heike wants to go first)
present <- function(p) {
  tmp <- sample(x = p, size = length(p))
  tmp <- data.frame(Presentation.Order = c("Heike Bucking", tmp))
  return(tmp)
}


# Prove it's not RIGGED
ls <- list()
for(i in 1:5){
  ls[[i]] <- present(people)
  names(ls) <- paste0("Trial.", seq_along(ls))
}; ls


# The actual list
present(people)


# Sample proof
vec <- c()
name <- c("a", "b", "c", "d", "e")
iter <- 1000000

for (i in 1:iter) {
  vec[i] <- sample(name, size = 1, replace = TRUE)
}

barplot(prop.table(table(vec)), 
        main = paste("sample iteration =", iter))