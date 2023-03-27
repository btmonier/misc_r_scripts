set.seed(1215)
library(magrittr)

antHill <- c(
    "Aimee",
    "Ana",
    "Germano",
    "Jingjing",
    "Merritt",
    "Michelle",
    "Nick",
    "Renyu",
    "Sara",
    "Zack"
)

antHillRand <- sample(antHill, length(antHill), replace = FALSE)



msg <- paste0("Presentation order for ", Sys.Date(), ":\n")
cat(msg)
antHillRand %>%
    paste("*", .) %>%
    cat(sep = "\n")




