library(readr)


ps4gPath <- "~/Downloads/10taxa_ropebwt_ps4g.txt"

ps4g <- read_tsv(
    ps4gPath,
    comment = "#",
    col_types = c("ccdd")
)


nums <- ps4g$gameteSet |>
    as.list() |>
    lapply(
        \(x) {
            strsplit(x, ",") |> unlist() |> as.integer()
        }
    )


uniqueNums <- nums |> unlist() |> unique()


lines <- readLines(ps4gPath)

header_lines <- grep("^#", lines, value = TRUE)

start_idx <- grep("^#gamete", header_lines)
end_idx <- tail(grep("^#[^#]", header_lines), 1) # last comment line before data

# Subset that section
table_lines <- header_lines[start_idx:end_idx]

# Remove the leading '#' from each line
table_clean <- stringr::str_remove(table_lines, "^#")

# Read the cleaned text into a tibble
header_tbl <- read_table(
    paste(table_clean, collapse = "\n"),
    col_types = cols(
        gamete = col_character(),
        gameteIndex = col_integer(),
        count = col_double()
    )
)


header_tbl$gameteIndex






