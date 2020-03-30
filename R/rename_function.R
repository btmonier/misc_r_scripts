library(magrittr)
library(dplyr)

## Make test data frame ----
test <- tibble(
    some = 1:5,
    cool = letters[1:5],
    data = 6:10,
    to = 1:5,
    test = 1:5
)


## Function ----
renamer <- function(df, n_last, vec2){
    nms <- names(df) %>% `[`(seq(from = ncol(df) - n_last + 1, to = ncol(df)))
    vec <- setNames(object = nms, vec2)
    df %>%
        dplyr::rename(
            !!!vec
        )
}

## Test ----
test %>% renamer(3, c("stuff", "to", "try"))
