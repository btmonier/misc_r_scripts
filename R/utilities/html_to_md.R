library(rvest)
library(stringr)

# 2. Define input/output paths
input_file  <- ""
output_file <- ""  # e.g. inside your MkDocs `docs/` folder

# 3. Read the HTML document
doc <- read_html(input_file)

# 4. Extract page title (<h1>)
page_title <- doc %>%
    html_node("h1") %>%
    html_text(trim = TRUE)

# 5. Extract each version heading (<h3>) and its following list items (<ul><li>)
version_nodes <- doc %>% html_nodes("h3")
versions <- version_nodes %>% html_text(trim = TRUE)
change_lists <- version_nodes %>%
    map(~ {
        node <- .x %>% html_node(xpath = "following-sibling::ul[1]")
        if (is.na(node)) return(character(0))
        node %>% html_nodes("li") %>% html_text(trim = TRUE)
    })

# 6. Build Markdown lines
md <- c(
    # YAML front‐matter for MkDocs
    "---",
    sprintf('title: "%s"', str_replace_all(page_title, '"', '\\"')),
    "---",
    "",
    # Markdown header
    paste0("# ", page_title),
    ""
)

# 7. Append each version section
for (i in seq_along(versions)) {
    md <- c(md,
            paste0("## ", versions[i]),  # use H2 under the main H1
            ""
    )
    if (length(change_lists[[i]]) > 0) {
        md <- c(md, paste0("- ", change_lists[[i]]), "")
    }
}

# 8. Write out to a .md file
writeLines(md, output_file)
message("Written Markdown to: ", output_file)


